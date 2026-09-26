#lang racket/base
;; `steer doctor`: integrity checks for .steer/, and safe repairs for what branch merges do to it.
;;   steer doctor                      check the store; exit 1 when it finds problems
;;   steer doctor --against REF        also find task ids that REF (a git branch/tag/commit) uses for a
;;                                     *different* task: the "both branches created T58" collision
;;   steer doctor --fix                repair what is safe: resequence duplicate event numbers, and with
;;                                     --against renumber OUR colliding tasks (run it on the branch you
;;                                     are about to merge, against the branch you merge into)
;; The event log is merged with git's `union` driver (.gitattributes), so concurrent appends never
;; conflict, but two branches then hand out the same sequence numbers; `since N` needs them unique.
(require (for-syntax racket/base) racket/list racket/string racket/file racket/port racket/system
         "common.rkt" "store.rkt" "tasks.rkt")
(provide cmd-doctor duplicate-seqs resequence collision-plan rename-task-id stale-claims)

;; ---------------------------------------------------------------------------------------------
;; Pure helpers

;; sequence numbers that occur more than once, or are out of order
(define (duplicate-seqs events)
  (define seen (make-hasheqv))
  (for/list ([e events] #:when (begin0 (hash-ref seen (car e) #f) (hash-set! seen (car e) #t))) (car e)))

(define (unordered? events)
  (for/or ([a events] [b (if (pair? events) (cdr events) '())]) (>= (car a) (car b))))

;; events renumbered 1..n in file order
(define (resequence events)
  (for/list ([e events] [i (in-naturals 1)]) (cons i (cdr e))))

;; A collision is an id both sides define for different tasks (different creation time or title).
;; ours, theirs: id → task. New ids start above the highest id on either side.
;; → list of (old-id . new-id), in old-id order
(define (collision-plan ours theirs)
  (define clash
    (sort (for/list ([(id t) (in-hash ours)]
                     #:when (let ([u (hash-ref theirs id #f)])
                              (and u (not (and (equal? (task-ref t 'created) (task-ref u 'created))
                                               (equal? (task-ref t 'title) (task-ref u 'title)))))))
            id)
          < #:key id-number))
  (define top (for/fold ([m 0]) ([id (append (hash-keys ours) (hash-keys theirs))]) (max m (id-number id))))
  (for/list ([id clash] [n (in-naturals (add1 top))]) (cons id (format "T~a" n))))

;; Rename `old` to `new` inside one task: its own id, and any reference in `after`.
(define (rename-task-id t mapping)
  (define (m id) (let ([p (assoc id mapping)]) (if p (cdr p) id)))
  (task-set t 'id (m (task-ref t 'id)) 'after (map m (task-ref t 'after '()))))

;; active tasks not touched for `days` days: → list of (id . age-in-days)
(define (stale-claims tasks days [now (current-seconds)])
  (for*/list ([t tasks] #:when (eq? (task-ref t 'status) 'active)
              [s (in-value (iso->seconds (task-ref t 'updated "")))]
              #:when (and s (> (- now s) (* days 86400))))
    (cons (task-ref t 'id) (quotient (- now s) 86400))))

;; ---------------------------------------------------------------------------------------------
;; Reading the store without trusting it

(define (read-all-tasks root)                      ; → (values tasks-by-id findings)
  (define dir (tasks-dir root))
  (define findings '())
  (define-syntax-rule (note! arg ...) (set! findings (cons (finding arg ...) findings)))
  (define tasks
    (for/fold ([acc (hash)]) ([f (if (directory-exists? dir) (sort (directory-list dir) string<? #:key path->string) '())])
      (define name (path->string f))
      (define path (build-path dir f))
      (cond
        [(regexp-match #rx"^(T[0-9]+)[.]rktd$" name)
         => (λ (m)
              (define text (file->string path))
              (cond
                [(regexp-match? #rx"(?m:^(<<<<<<<|=======|>>>>>>>)( |$))" text)
                 (note! 'error 'conflict-marker (format "~a contains git conflict markers" name)
                        #:file (string-append ".steer/tasks/" name) #:fix "resolve the merge (keep one side of each conflict), then rerun `steer doctor`")
                 acc]
                [else
                 (with-handlers ([exn:steer? (λ (e)
                                               (note! 'error 'corrupt-task (exn-message e) #:file (string-append ".steer/tasks/" name)
                                                      #:fix (or (exn:steer-hint e) "restore the file from git"))
                                               acc)])
                   (define t (parse-task-text text name))
                   (cond
                     [(not (equal? (task-ref t 'id) (cadr m)))
                      (note! 'error 'id-mismatch (format "~a holds task ~a" name (task-ref t 'id))
                             #:file (string-append ".steer/tasks/" name) #:fix "rename the file or the id so they agree")
                      acc]
                     [else (hash-set acc (task-ref t 'id) t)]))]))]
        [else
         (note! 'warning 'stray-file (format "unexpected file in .steer/tasks: ~a" name)
                #:file (string-append ".steer/tasks/" name) #:fix "delete it (merge leftovers like .orig and .rej are safe to remove)")
         acc])))
  (values tasks (reverse findings)))

(define (git root . args)                          ; → (values exit-code stdout)
  (define out (open-output-string))
  (define code (parameterize ([current-directory root] [current-output-port out] [current-error-port (open-output-nowhere)])
                 (if (find-executable-path "git") (apply system*/exit-code (find-executable-path "git") args) 127)))
  (values code (get-output-string out)))

;; tasks of another git ref: id → task
(define (tasks-at-ref root ref)
  (define-values (c0 _o) (git root "rev-parse" "--verify" "--quiet" (string-append ref "^{commit}")))
  (unless (eqv? c0 0)
    (fail! 'bad-ref (format "~a is not a git ref in this repository" ref) #:hint "use a branch, tag or commit, e.g. `--against main`"))
  (define-values (c1 listing) (git root "ls-tree" "-r" "--name-only" ref "--" ".steer/tasks"))
  (for/fold ([acc (hash)]) ([path (string-split listing "\n")]
                            #:when (regexp-match? #rx"/T[0-9]+[.]rktd$" path))
    (define-values (c text) (git root "show" (string-append ref ":" path)))
    (with-handlers ([exn:steer? (λ (e) acc)])
      (define t (parse-task-text text (string-append ref ":" path)))
      (hash-set acc (task-ref t 'id) t))))

;; ---------------------------------------------------------------------------------------------
;; Command

(define (cmd-doctor argv)
  (define-values (_ o) (parse-args "doctor" argv '(("--against" one) ("--fix" bool)) #:max 0
                                   #:usage "steer doctor [--against GIT-REF] [--fix]"))
  (define root (find-root))
  (define ref (opt-ref o 'against))
  (define fix? (opt-ref o 'fix))
  (define (run)
    (define-values (by-id read-findings) (read-all-tasks root))
    (define graph (graph-findings by-id))
    (define events (read-events root))
    (define dup (duplicate-seqs events))
    (define stale (stale-claims (hash-values by-id) (config-ref root 'stale-claim-days 7)))
    (define theirs (and ref (tasks-at-ref root ref)))
    (define plan (if theirs (collision-plan by-id theirs) '()))
    (define fixed '())
    (define (fixed! s) (set! fixed (cons s fixed)))
    ;; --fix: resequence events; renumber our colliding tasks (never touches theirs)
    (when (and fix? (or (pair? dup) (unordered? events)))
      (write-events! root (resequence events))
      (fixed! (format "resequenced ~a events (cursors handed out earlier no longer match)" (length events))))
    (when (and fix? (pair? plan))
      (for ([p plan])
        (delete-task! root (car p)))
      (for ([t (hash-values by-id)])
        (define t* (rename-task-id t plan))
        (unless (equal? t t*) (save-task! root t*)))
      (for ([p plan]) (append-event! root 'renumber (cdr p) (format "was ~a (collided with ~a)" (car p) ref)))
      (fixed! (format "renumbered ~a: ~a" (string-append (number->string (length plan)) " task" (plural (length plan)))
                      (string-join (for/list ([p plan]) (format "~a → ~a" (car p) (cdr p))) ", "))))
    (define findings
      (append
       read-findings
       (if (and (not fix?) (or (pair? dup) (unordered? events)))
           (list (finding 'warning 'duplicate-seq
                          (format "~a event sequence number~a repeat or run backwards (two branches appended events)"
                                  (length dup) (plural (length dup)))
                          #:file ".steer/events.rktd" #:fix "`steer doctor --fix` resequences the log; `since N` needs unique numbers"))
           '())
       (if (not fix?)
           (for/list ([p plan])
             (finding 'error 'id-collision
                      (format "~a is a different task on ~a (ours: \"~a\"; theirs: \"~a\")" (car p) ref
                              (clip (task-ref (hash-ref by-id (car p)) 'title) 40) (clip (task-ref (hash-ref theirs (car p)) 'title) 40))
                      #:task (car p) #:fix (format "`steer doctor --against ~a --fix` renumbers ours to ~a" ref (cdr p))))
           '())
       (filter (λ (f) (memq (hash-ref f 'kind) '(cycle missing-dependency dropped-dependency))) graph)
       (for/list ([s stale])
         (finding 'warning 'stale-claim (format "~a has been active for ~a days" (car s) (cdr s)) #:task (car s)
                  #:fix (format "`steer release ~a` if the agent is gone" (car s))))))
    (define errors (filter (λ (f) (eq? (hash-ref f 'severity) 'error)) findings))
    (make-reply "doctor"
                (string-join
                 (append (list (format "~a task~a, ~a event~a~a: ~a problem~a"
                                       (hash-count by-id) (plural (hash-count by-id)) (length events) (plural (length events))
                                       (if ref (format ", compared with ~a" ref) "")
                                       (length findings) (plural (length findings))))
                         (for/list ([f (reverse fixed)]) (string-append "fixed: " f)))
                 "\n")
                (hasheq 'tasks (hash-count by-id) 'events (length events) 'problems (length findings)
                        'fixed (reverse fixed) 'renumbered (for/list ([p plan]) (hasheq 'from (car p) 'to (cdr p))))
                #:ok? (null? errors) #:findings findings))
  (if fix? (with-store-lock root run) (run)))
