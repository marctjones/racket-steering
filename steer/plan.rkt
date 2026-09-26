#lang racket/base
;; Checked bulk plans for `steer import`. A plan is a file (or stdin) of forms:
;;
;;   (task "Add CSV export"
;;     #:id csv                         ; local label, referable from #:after in the same plan
;;     #:goal "Why and what, in a sentence or two"
;;     #:after (schema T3)              ; labels in this plan or existing task ids
;;     #:check "raco test report/export-test.rkt"   ; repeatable, or a list
;;     #:anchor "report/export.rkt#export-csv"
;;     #:touch "report/"  #:priority 1  #:tag export)
;;
;; Everything is validated before anything is written: unknown keywords, wrong value shapes,
;; duplicate or unknown labels, and cycles, each with a line:col and a fix. All or nothing.
(require racket/list racket/string "common.rkt" "srcread.rkt" "store.rkt" "tasks.rkt")
(provide parse-plan resolve-plan)

(define keywords '("id" "goal" "after" "check" "anchor" "touch" "priority" "tag"))

;; → (values specs errors). A spec is a hasheq with 'title 'label 'goal 'after 'checks 'anchors
;; 'touches 'priority 'tags 'line 'col; 'after holds raw references (strings).
(define (parse-plan text source-name)
  (define errors '())
  (define (err! stx kind msg [fix #f])
    (set! errors (cons (finding 'error kind msg #:file source-name
                                #:line (and stx (syntax-line stx)) #:col (and stx (add1 (or (syntax-column stx) 0)))
                                #:fix fix)
                       errors)))
  (define forms
    (with-handlers ([exn:fail:read?
                     (λ (e)
                       (define loc (let ([ls (exn:fail:read-srclocs e)]) (and (pair? ls) (car ls))))
                       (set! errors (list (finding 'error 'read-error (car (string-split (exn-message e) "\n"))
                                                   #:file source-name
                                                   #:line (and loc (srcloc-line loc))
                                                   #:col (and loc (srcloc-column loc) (add1 (srcloc-column loc)))
                                                   #:fix "check parentheses and string quotes; `steer syntax FILE` locates unbalanced forms")))
                       '())])
      (define-values (fs _lang _text) (read-racket-source text #:source source-name))
      fs))
  (define specs
    (for*/list ([f forms]
                [s (in-value (parse-task-form f err!))]
                #:when s)
      s))
  (values specs (reverse errors)))

(define (parse-task-form f err!)
  (define l (syntax->list f))
  (cond
    [(not (and l (pair? l) (eq? (syntax-e (car l)) 'task)))
     (err! f 'not-a-task (format "expected (task \"title\" #:key value ...), got ~a" (clip (format "~s" (syntax->datum f)) 60))
           "every top-level form must start with `task`")
     #f]
    [(or (null? (cdr l)) (not (string? (syntax-e (cadr l)))))
     (err! f 'missing-title "a task needs a string title right after `task`" "(task \"Short imperative title\" ...)")
     #f]
    [else
     (define title (string-trim (syntax-e (cadr l))))
     (when (string=? title "") (err! (cadr l) 'missing-title "title is empty"))
     (let loop ([xs (cddr l)]
                [spec (hasheq 'title title 'label #f 'goal #f 'after '() 'checks '() 'anchors '()
                              'touches '() 'priority 2 'tags '()
                              'line (syntax-line f) 'col (add1 (or (syntax-column f) 0)))])
       (cond
         [(null? xs) spec]
         [(not (keyword? (syntax-e (car xs))))
          (err! (car xs) 'expected-keyword (format "expected a #:keyword, got ~s" (syntax->datum (car xs)))
                (format "keywords: ~a" (string-join (map (λ (k) (string-append "#:" k)) keywords) " ")))
          spec]
         [(null? (cdr xs))
          (err! (car xs) 'missing-value (format "~a has no value" (syntax-e (car xs))))
          spec]
         [else
          (define kw (keyword->string (syntax-e (car xs))))
          (define v (cadr xs))
          (define d (syntax->datum v))
          (define (strings-of what)             ; one string/symbol or a list of them
            (define items (if (list? d) d (list d)))
            (cond [(andmap (λ (x) (or (string? x) (symbol? x))) items) (map (λ (x) (format "~a" x)) items)]
                  [else (err! v 'bad-value (format "#:~a expects ~a" kw what)) '()]))
          (define spec*
            (case kw
              [("id")
               (cond [(and (symbol? d) (not (regexp-match? #px"^[Tt][0-9]+$" (symbol->string d))))
                      (hash-set spec 'label (symbol->string d))]
                     [else (err! v 'bad-label (format "#:id must be a symbol that does not look like a task id, got ~s" d)
                                 "use a short word like csv-export")
                           spec])]
              [("goal") (if (string? d) (hash-set spec 'goal d)
                            (begin (err! v 'bad-value "#:goal expects a string") spec))]
              [("after") (hash-set spec 'after (append (hash-ref spec 'after) (strings-of "labels or task ids")))]
              [("check") (hash-set spec 'checks (append (hash-ref spec 'checks)
                                                        (if (or (string? d) (and (list? d) (andmap string? d)))
                                                            (if (list? d) d (list d))
                                                            (begin (err! v 'bad-value "#:check expects a shell command string or a list of them") '()))))]
              [("anchor") (hash-set spec 'anchors (append (hash-ref spec 'anchors) (strings-of "\"path#name\" strings")))]
              [("touch") (hash-set spec 'touches (append (hash-ref spec 'touches) (strings-of "paths")))]
              [("priority") (if (and (exact-integer? d) (<= 0 d 9)) (hash-set spec 'priority d)
                                (begin (err! v 'bad-value "#:priority expects an integer 0-9 (0 = most urgent)") spec))]
              [("tag") (hash-set spec 'tags (append (hash-ref spec 'tags) (strings-of "symbols")))]
              [else (err! (car xs) 'unknown-keyword (format "unknown keyword #:~a" kw)
                          (let ([c (closest kw keywords)])
                            (if (pair? c) (format "did you mean #:~a?" (car c))
                                (format "keywords: ~a" (string-join (map (λ (k) (string-append "#:" k)) keywords) " ")))))
                    spec]))
          (loop (cddr xs) spec*)]))]))

;; Resolve labels and references against the existing store; assign ids; check for cycles.
;; → (values new-tasks errors label->id)
(define (resolve-plan specs existing source-name)
  (define by-id (index existing))
  (define errors '())
  (define (err! spec kind msg [fix #f])
    (set! errors (cons (finding 'error kind msg #:file source-name #:line (hash-ref spec 'line)
                                #:col (hash-ref spec 'col) #:fix fix)
                       errors)))
  (define first-n (id-number (next-id existing)))
  (define ids (for/list ([i (in-naturals first-n)] [_ specs]) (format "T~a" i)))
  (define labels
    (for/fold ([m (hash)]) ([s specs] [id ids])
      (define lab (hash-ref s 'label))
      (cond [(not lab) m]
            [(hash-ref m lab #f) (err! s 'duplicate-label (format "label ~a is used twice" lab)) m]
            [else (hash-set m lab id)])))
  (define known-refs (append (hash-keys labels) (hash-keys by-id)))
  (define tasks
    (for/list ([s specs] [id ids])
      (define after
        (remove-duplicates
         (for*/list ([r (hash-ref s 'after)]
                     [resolved (in-value
                                (cond [(regexp-match? #px"^[Tt]?[0-9]+$" r)
                                       (define tid (normalize-id r))
                                       (if (hash-ref by-id tid #f) tid
                                           (begin (err! s 'unknown-task (format "#:after ~a: no existing task ~a" r tid)
                                                        (did-you-mean tid (hash-keys by-id) #:else "check `steer list --status all`"))
                                                  #f))]
                                      [(hash-ref labels r #f)]
                                      [else (err! s 'unknown-label (format "#:after ~a: no task in this plan has #:id ~a" r r)
                                                  (did-you-mean r known-refs #:else "labels are defined with #:id"))
                                            #f]))]
                     #:when resolved)
           resolved)))
      (when (member id after) (err! s 'self-dependency (format "~a depends on itself" (or (hash-ref s 'label) id))))
      (hasheq 'id id 'title (hash-ref s 'title) 'status 'open 'priority (hash-ref s 'priority)
              'goal (hash-ref s 'goal) 'after after 'checks (hash-ref s 'checks)
              'anchor-refs (hash-ref s 'anchors) 'touches (hash-ref s 'touches) 'tags (hash-ref s 'tags))))
  (define cyc (and (null? errors)
                   (find-cycle (for/fold ([m by-id]) ([t tasks]) (hash-set m (hash-ref t 'id) t)))))
  (when cyc
    (define id->label (for/hash ([(l i) (in-hash labels)]) (values i l)))
    (define shown (map (λ (i) (hash-ref id->label i i)) cyc))
    (define spec (or (for/first ([s specs] [id ids] #:when (member id cyc)) s) (car specs)))
    (err! spec 'cycle (string-append "plan has a dependency cycle: " (string-join shown " → "))
          "remove one of these #:after references"))
  (values tasks (reverse errors) labels))
