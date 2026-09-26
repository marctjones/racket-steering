#lang racket/base
;; Shared plumbing for every steer command: errors, replies, rendering, argument parsing, time.
;; Output rule (note 03): short, stable, located, with a suggested fix; never a raw stack trace.
(require racket/list racket/string json (only-in racket/date find-seconds))
(provide (struct-out exn:steer) fail!
         (struct-out reply) make-reply
         finding finding->text reply->text reply->jsexpr ->jsexpr
         current-json? current-full? current-agent current-limit current-root-override
         parse-args opt-ref opt-list opt-ids
         now-iso iso->seconds age-text
         closest did-you-mean
         clip one-line plural)

;; ---------------------------------------------------------------------------------------------
;; Errors. `code` is the process exit code: 2 = usage/refusal by input, 3 = internal/store problem.

(struct exn:steer exn:fail (kind code hint))

(define (fail! kind msg #:hint [hint #f] #:code [code 2])
  (raise (exn:steer msg (current-continuation-marks) kind code hint)))

;; ---------------------------------------------------------------------------------------------
;; Global options (set by main from flags that may appear anywhere on the command line).

(define current-json? (make-parameter #f))
(define current-full? (make-parameter #f))
(define current-agent (make-parameter "claude"))
(define current-limit (make-parameter #f))
(define current-root-override (make-parameter #f))

;; ---------------------------------------------------------------------------------------------
;; Replies: every command returns one. `text` is the compact rendering, `data` the JSON payload.

(struct reply (tool ok? text data findings next))

(define (make-reply tool text [data (hasheq)] #:ok? [ok? #t] #:findings [findings '()] #:next [next '()])
  (reply tool ok? text data findings next))

(define (finding severity kind message
                 #:file [file #f] #:line [line #f] #:col [col #f] #:task [task #f] #:fix [fix #f]
                 #:detail [detail #f] #:edit [edit #f])
  ;; `edit` is the machine-applicable form of `fix` (see syntax-check.rkt apply-edit)
  (for/hasheq ([(k v) (in-hash (hasheq 'severity severity 'kind kind 'message message
                                       'file file 'line line 'col col 'task task 'fix fix
                                       'detail detail 'edit edit))]
               #:when v)
    (values k v)))

(define (finding->text f)
  (define loc
    (cond [(hash-ref f 'file #f)
           (string-append " " (hash-ref f 'file)
                          (if (hash-ref f 'line #f) (format ":~a" (hash-ref f 'line)) "")
                          (if (hash-ref f 'col #f) (format ":~a" (hash-ref f 'col)) ""))]
          [(hash-ref f 'task #f) (string-append " " (hash-ref f 'task))]
          [else ""]))
  (string-append (symbol->string (hash-ref f 'severity)) " " (symbol->string (hash-ref f 'kind)) loc
                 ": " (hash-ref f 'message)
                 (if (hash-ref f 'fix #f) (string-append " → " (hash-ref f 'fix)) "")
                 (if (hash-ref f 'detail #f)
                     (string-append "\n" (indent-block (hash-ref f 'detail) "    "))
                     "")))

(define (indent-block s pre)
  (string-join (for/list ([l (string-split s "\n" #:trim? #f)]) (string-append pre l)) "\n"))

(define default-finding-cap 5)

(define (capped-findings fs)
  (define cap (or (current-limit) (if (current-full?) +inf.0 default-finding-cap)))
  (if (> (length fs) cap)
      (values (take fs (inexact->exact cap)) (- (length fs) (inexact->exact cap)))
      (values fs 0)))

(define (reply->text r)
  (define-values (fs more) (capped-findings (reply-findings r)))
  (string-join
   (filter (λ (s) (and s (not (string=? s ""))))
           (list (reply-text r)
                 (and (pair? fs) (string-join (map finding->text fs) "\n"))
                 (and (> more 0) (format "(+~a more findings; add --full or --limit N)" more))
                 (and (pair? (reply-next r)) (string-append "next: " (string-join (reply-next r) " | ")))))
   "\n"))

(define (reply->jsexpr r elapsed-ms)
  (define-values (fs more) (capped-findings (reply-findings r)))
  (->jsexpr (hasheq 'tool (reply-tool r) 'ok (reply-ok? r) 'elapsed_ms elapsed-ms
                    'data (reply-data r) 'findings fs 'truncated (> more 0)
                    'next (reply-next r))))

;; Racket data → jsexpr: symbols become strings, paths strings, exact non-integers floats.
;; Keys are snake_case everywhere (claimed-by → claimed_by, found? → found) so field names are stable.
(define (json-key k)
  (string->symbol (regexp-replace* #rx"-" (regexp-replace #rx"[?!]$" (format "~a" k) "") "_")))

(define (->jsexpr v)
  (cond [(hash? v) (for/hasheq ([(k x) (in-hash v)]) (values (json-key k) (->jsexpr x)))]
        [(list? v) (map ->jsexpr v)]
        [(pair? v) (list (->jsexpr (car v)) (->jsexpr (cdr v)))]
        [(vector? v) (map ->jsexpr (vector->list v))]
        [(symbol? v) (symbol->string v)]
        [(path? v) (path->string v)]
        [(void? v) 'null]
        [(and (number? v) (exact? v) (not (integer? v))) (exact->inexact v)]
        [(or (string? v) (boolean? v) (number? v)) v]
        [else (format "~s" v)]))

;; ---------------------------------------------------------------------------------------------
;; Argument parsing. spec = list of (flag kind), kind ∈ 'bool 'one 'many.
;; Unknown flags fail with a did-you-mean hint rather than a usage dump.

(define (parse-args cmd argv spec #:min [lo 0] #:max [hi #f] #:usage [usage #f])
  (define kinds (for/hash ([s spec]) (values (car s) (cadr s))))
  (define (usage-hint) (or usage (format "steer help ~a" cmd)))
  (let loop ([args argv] [pos '()] [opts (hasheq)] [only-pos? #f])
    (cond
      [(null? args)
       (define ps (reverse pos))
       (when (< (length ps) lo)
         (fail! 'usage (format "`steer ~a` needs ~a positional argument~a, got ~a" cmd lo (plural lo)
                               (length ps))
                #:hint (usage-hint)))
       (when (and hi (> (length ps) hi))
         (fail! 'usage (format "`steer ~a` takes at most ~a positional argument~a, got ~a: ~s" cmd hi
                               (plural hi) (length ps) ps)
                #:hint (string-append "quote multi-word text; " (usage-hint))))
       (values ps opts)]
      [else
       (define a (car args))
       (cond
         [only-pos? (loop (cdr args) (cons a pos) opts #t)]
         [(equal? a "--") (loop (cdr args) pos opts #t)]
         [(and (> (string-length a) 2) (string-prefix? a "--"))
          (define m (regexp-match #rx"^(--[^=]+)(?:=(.*))?$" a))
          (define name (cadr m))
          (define inline (caddr m))
          (define kind (hash-ref kinds name #f))
          (unless kind
            (fail! 'usage (format "unknown flag ~a for `steer ~a`" name cmd)
                   #:hint (did-you-mean name (hash-keys kinds) #:else (usage-hint))))
          (define key (string->symbol (substring name 2)))
          (case kind
            [(bool)
             (when inline (fail! 'usage (format "flag ~a takes no value" name) #:hint (usage-hint)))
             (loop (cdr args) pos (hash-set opts key #t) #f)]
            [else
             (define-values (val rest)
               (cond [inline (values inline (cdr args))]
                     [(pair? (cdr args)) (values (cadr args) (cddr args))]
                     [else (fail! 'usage (format "flag ~a needs a value" name) #:hint (usage-hint))]))
             (loop rest pos
                   (if (eq? kind 'one)
                       (hash-set opts key val)
                       (hash-update opts key (λ (l) (append l (list val))) '()))
                   #f)])]
         [(and (> (string-length a) 1) (char=? (string-ref a 0) #\-)
               (not (regexp-match? #rx"^-[0-9]" a)))
          (fail! 'usage (format "unknown option ~a for `steer ~a` (flags use two dashes)" a cmd)
                 #:hint (usage-hint))]
         [else (loop (cdr args) (cons a pos) opts #f)])])))

(define (opt-ref opts key [default #f]) (hash-ref opts key default))
(define (opt-list opts key) (hash-ref opts key '()))

;; Id lists may be repeated flags or comma/space separated: --after T1,T2 --after T3
(define (opt-ids opts key)
  (for*/list ([v (opt-list opts key)]
              [s (string-split v #rx"[, ]+")]
              #:unless (string=? s ""))
    s))

;; ---------------------------------------------------------------------------------------------
;; Suggestions

(define (edit-distance a b)
  ;; optimal string alignment: insert, delete, substitute, and swap of adjacent chars all cost 1
  (define la (string-length a))
  (define lb (string-length b))
  (define d (for/vector ([i (add1 la)]) (make-vector (add1 lb) 0)))
  (define (at i j) (vector-ref (vector-ref d i) j))
  (define (put! i j v) (vector-set! (vector-ref d i) j v))
  (for ([i (add1 la)]) (put! i 0 i))
  (for ([j (add1 lb)]) (put! 0 j j))
  (for* ([i (in-range 1 (add1 la))] [j (in-range 1 (add1 lb))])
    (define cost (if (char=? (string-ref a (sub1 i)) (string-ref b (sub1 j))) 0 1))
    (put! i j (min (add1 (at (sub1 i) j)) (add1 (at i (sub1 j))) (+ (at (sub1 i) (sub1 j)) cost)))
    (when (and (> i 1) (> j 1)
               (char=? (string-ref a (sub1 i)) (string-ref b (- j 2)))
               (char=? (string-ref a (- i 2)) (string-ref b (sub1 j))))
      (put! i j (min (at i j) (add1 (at (- i 2) (- j 2)))))))
  (at la lb))

;; The best candidates within a length-scaled distance (only those tied for best).
(define (closest s candidates #:max [n 3])
  (define limit (max 2 (quotient (string-length s) 3)))
  (define scored (for*/list ([c candidates]
                             [dist (in-value (edit-distance (string-downcase s) (string-downcase c)))]
                             #:when (<= dist limit))
                   (cons dist c)))
  (cond [(null? scored) '()]
        [else
         (define best (apply min (map car scored)))
         (define tied (map cdr (filter (λ (p) (= (car p) best)) scored)))
         (take tied (min n (length tied)))]))

(define (did-you-mean s candidates #:else [otherwise #f])
  (define c (closest s candidates))
  (cond [(pair? c) (format "did you mean ~a?" (string-join c " or "))]
        [otherwise otherwise]
        [else #f]))

;; ---------------------------------------------------------------------------------------------
;; Text helpers

(define (one-line s) (string-normalize-spaces (regexp-replace* #rx"[\r\n]+" s " ")))

(define (clip s n)
  (if (<= (string-length s) n) s (string-append (substring s 0 (max 0 (- n 1))) "…")))

(define (plural n) (if (= n 1) "" "s"))

;; ---------------------------------------------------------------------------------------------
;; Time: ISO-8601 UTC strings in the store; ages for humans and models.

(define (pad2 n) (if (< n 10) (format "0~a" n) (number->string n)))

(define (now-iso [secs (current-seconds)])
  (define d (seconds->date secs #f))
  (format "~a-~a-~aT~a:~a:~aZ" (date-year d) (pad2 (date-month d)) (pad2 (date-day d))
          (pad2 (date-hour d)) (pad2 (date-minute d)) (pad2 (date-second d))))

(define (iso->seconds s)
  (define m (and (string? s) (regexp-match #px"^(\\d{4})-(\\d\\d)-(\\d\\d)T(\\d\\d):(\\d\\d):(\\d\\d)Z$" s)))
  (and m
       (let ([n (map string->number (cdr m))])
         (find-seconds (list-ref n 5) (list-ref n 4) (list-ref n 3)
                       (list-ref n 2) (list-ref n 1) (list-ref n 0) #f))))

(define (age-text iso)
  (define s (iso->seconds iso))
  (cond [(not s) "?"]
        [else
         (define d (max 0 (- (current-seconds) s)))
         (cond [(< d 90) "just now"]
               [(< d 5400) (format "~am ago" (quotient d 60))]
               [(< d 129600) (format "~ah ago" (quotient d 3600))]
               [else (format "~ad ago" (quotient d 86400))])]))
