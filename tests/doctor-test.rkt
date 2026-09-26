#lang racket/base
;; `steer doctor`: pure helpers, and the real scenario it exists for: two git branches that each create
;; a task with the same id, merged with the union driver for the event log.
(require rackunit racket/list racket/string racket/file racket/port racket/runtime-path
         "../steer/doctor.rkt" "../steer/tasks.rkt" (only-in "../steer/common.rkt" iso->seconds))

;; ---------------------------------------------------------------------------------------------
;; pure

(define (t id title created #:after [after '()] #:status [status 'open] #:updated [updated "2026-01-01T00:00:00Z"])
  (hasheq 'id id 'title title 'created created 'after after 'status status 'updated updated))

(check-equal? (duplicate-seqs '((1 a) (2 b) (2 c) (3 d) (3 e) (3 f))) '(2 3 3))
(check-equal? (duplicate-seqs '((1 a) (2 b))) '())
(check-equal? (resequence '((1 a) (1 b) (5 c) (2 d))) '((1 a) (2 b) (3 c) (4 d)))

(let* ([ours (hash "T1" (t "T1" "base" "c1") "T2" (t "T2" "ours" "c2") "T3" (t "T3" "ours 2" "c3" #:after '("T2")))]
       [theirs (hash "T1" (t "T1" "base" "c1") "T2" (t "T2" "theirs" "cX") "T4" (t "T4" "theirs 4" "cY"))])
  (check-equal? (collision-plan ours theirs) '(("T2" . "T5")) "T2 differs; new ids start above the highest id on either side")
  (check-equal? (collision-plan ours ours) '() "identical stores never collide")
  (check-equal? (collision-plan ours (hash "T2" (t "T2" "ours" "c2"))) '() "same title and creation time: the same task")
  (let ([renamed (rename-task-id (hash-ref ours "T3") '(("T2" . "T5")))])
    (check-equal? (hash-ref renamed 'after) '("T5") "references follow the rename")
    (check-equal? (hash-ref renamed 'id) "T3"))
  (check-equal? (hash-ref (rename-task-id (hash-ref ours "T2") '(("T2" . "T5"))) 'id) "T5"))

(check-equal? (stale-claims (list (t "T1" "a" "c" #:status 'active #:updated "2026-01-01T00:00:00Z")
                                  (t "T2" "b" "c" #:status 'open #:updated "2026-01-01T00:00:00Z")
                                  (t "T3" "c" "c" #:status 'active #:updated "2026-01-09T00:00:00Z"))
                            7 (iso->seconds "2026-01-10T00:00:00Z"))
              '(("T1" . 9)) "only active tasks older than the limit")

;; ---------------------------------------------------------------------------------------------
;; two branches

(define-runtime-path main-rkt "../steer/main.rkt")
(define git? (and (find-executable-path "git") (find-executable-path "racket") #t))

(define dir (make-temporary-directory "steer-doctor~a"))
(define (run cmd . args)
  (define-values (p out in err)
    (parameterize ([current-directory dir])
      (apply subprocess #f #f 'stdout cmd args)))
  (close-output-port in)
  (define text (port->string out))
  (subprocess-wait p) (close-input-port out)
  (values (subprocess-status p) text))
(define (steer . args) (apply run (find-executable-path "racket") (path->string main-rkt) args))
(define (git . args) (apply run (find-executable-path "git") args))

(cond
  [(not git?) (eprintf "doctor-test: SKIPPED git scenario: git or racket not found\n")]
  [else
   (for ([kv '(("GIT_AUTHOR_NAME" . "t") ("GIT_AUTHOR_EMAIL" . "t@example.com")
               ("GIT_COMMITTER_NAME" . "t") ("GIT_COMMITTER_EMAIL" . "t@example.com"))])
     (putenv (car kv) (cdr kv)))
   (call-with-values (λ () (git "init" "-q" "-b" "main")) void)
   (call-with-output-file (build-path dir ".gitattributes")
     (λ (o) (void (write-string ".steer/events.rktd merge=union\n" o))))
   (call-with-values (λ () (steer "init")) void)
   (call-with-values (λ () (steer "add" "base" "--check" "true")) void)
   (call-with-values (λ () (git "add" "-A")) void)
   (call-with-values (λ () (git "commit" "-q" "-m" "base")) void)
   ;; branch b creates T2
   (call-with-values (λ () (git "checkout" "-q" "-b" "b")) void)
   (call-with-values (λ () (steer "add" "from branch b" "--check" "true")) void)
   (call-with-values (λ () (git "add" "-A")) void)
   (call-with-values (λ () (git "commit" "-q" "-m" "b adds T2")) void)
   ;; branch a (from main) also creates T2, plus a task that depends on it
   (call-with-values (λ () (git "checkout" "-q" "main")) void)
   (call-with-values (λ () (git "checkout" "-q" "-b" "a")) void)
   (call-with-values (λ () (steer "add" "from branch a" "--check" "true")) void)
   (call-with-values (λ () (steer "add" "after a" "--after" "T2" "--check" "true")) void)
   (call-with-values (λ () (git "add" "-A")) void)
   (call-with-values (λ () (git "commit" "-q" "-m" "a adds T2 and T3")) void)

   (let-values ([(c o) (steer "doctor")])
     (check-equal? c 0 o)
     (check-regexp-match #rx"3 tasks.*0 problems" o "no comparison, nothing wrong yet"))
   (let-values ([(c o) (steer "doctor" "--against" "nosuchbranch")])
     (check-equal? c 2 o)
     (check-regexp-match #rx"not a git ref" o))
   (let-values ([(c o) (steer "doctor" "--against" "b")])
     (check-equal? c 1 o)
     (check-regexp-match #rx"id-collision T2.*different task on b.*from branch a.*from branch b" o)
     (check-regexp-match #rx"renumbers ours to T4" o "b has T1,T2; a has T1,T2,T3: next free id is T4"))
   (let-values ([(c o) (steer "doctor" "--against" "b" "--fix")])
     (check-equal? c 0 o)
     (check-regexp-match #rx"renumbered 1 task: T2 → T4" o))
   (let-values ([(c o) (steer "show" "T3")])
     (check-regexp-match #rx"after: T4" o "the task that waited for T2 now waits for T4"))
   (let-values ([(c o) (steer "show" "T4")])
     (check-regexp-match #rx"from branch a" o))
   (let-values ([(c o) (steer "doctor" "--against" "b")])
     (check-equal? c 0 (string-append "collision resolved: " o)))
   (call-with-values (λ () (git "add" "-A")) void)
   (call-with-values (λ () (git "commit" "-q" "-m" "a: renumber")) void)
   ;; the merge now applies cleanly (event log via the union driver), leaving duplicate event numbers
   (let-values ([(c o) (git "merge" "-q" "--no-edit" "b")])
     (check-equal? c 0 (format "merge is clean after renumbering: ~a" o)))
   (let-values ([(c o) (steer "doctor")])
     (check-equal? c 0 o)
     (check-regexp-match #rx"duplicate-seq" o "union-merged events repeat sequence numbers"))
   (let-values ([(c o) (steer "doctor" "--fix")])
     (check-equal? c 0 o)
     (check-regexp-match #rx"resequenced [0-9]+ events" o))
   (let-values ([(c o) (steer "doctor")])
     (check-regexp-match #rx"4 tasks.*0 problems" o "T1, T2 (b), T3, T4 (a): all present after the merge"))
   ;; a conflict marker and a stray file are reported, not crashed on
   (call-with-output-file (build-path dir ".steer" "tasks" "T3.rktd") #:exists 'truncate
     (λ (o) (void (write-string "<<<<<<< HEAD\n((id \"T3\"))\n=======\n((id \"T3\"))\n>>>>>>> b\n" o))))
   (call-with-output-file (build-path dir ".steer" "tasks" "T2.rktd.orig") (λ (o) (void (write-string "x" o))))
   (let-values ([(c o) (steer "doctor")])
     (check-equal? c 1 o)
     (check-regexp-match #rx"conflict-marker" o)
     (check-regexp-match #rx"stray-file" o))])

(delete-directory/files dir)
