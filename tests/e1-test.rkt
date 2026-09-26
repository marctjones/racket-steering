#lang racket/base
;; E1 failure logger: classification (pure) and recording through the real CLI in a scratch store.
(require rackunit racket/list racket/string racket/file racket/port racket/runtime-path json
         "../steer/failures.rkt")

;; ---------------------------------------------------------------------------------------------
;; taxonomy

(check-equal? (classify 'doc 'unknown-id) 'unbound-id)
(check-equal? (classify 'doc 'not-in-racket) 'unbound-id)
(check-equal? (classify 'syntax 'unclosed-form) 'syntax)
(check-equal? (classify 'hook-post-edit 'mismatched-closer) 'syntax)
(check-equal? (classify 'import 'read-error) 'plan-error "the same reader error in a plan is a plan error, not a syntax error")
(check-equal? (classify 'import 'unknown-keyword) 'plan-error)
(check-equal? (classify 'import 'cycle) 'plan-error)
(check-equal? (classify 'graph 'cycle) 'drift)
(check-equal? (classify 'done 'check-failed) 'check-failed)
(check-equal? (classify 'api 'removed-export) 'api-break)
(check-equal? (classify 'steer 'usage) 'tool-misuse)
(check-equal? (classify 'checkpoint 'incomplete-checkpoint) 'tool-misuse)
(check-equal? (classify 'steer 'internal) 'tool-failure)
(check-equal? (classify 'stale 'stale-anchor) 'drift)
(check-equal? (classify 'x 'never-heard-of-it) 'other)
(check-regexp-match #rx"steer syntax" (class-hint 'syntax))
(check-regexp-match #rx"steer doc" (class-hint 'unbound-id))

;; ---------------------------------------------------------------------------------------------
;; through the CLI

(define-runtime-path main-rkt "../steer/main.rkt")
(define dir (make-temporary-directory "steer-e1~a"))
(define (steer #:in [input ""] #:env [env '()] . args)
  (for ([kv env]) (putenv (car kv) (cdr kv)))
  (define-values (p out in err)
    (parameterize ([current-directory dir])
      (apply subprocess #f #f 'stdout (find-executable-path "racket") (path->string main-rkt) args)))
  (write-string input in) (close-output-port in)
  (define text (port->string out))
  (subprocess-wait p) (close-input-port out)
  (for ([kv env]) (putenv (car kv) ""))
  (values (subprocess-status p) text))
(define (failures . args) (let-values ([(c o) (apply steer "--json" "failures" args)]) (string->jsexpr o)))
(define (log-lines) (let ([f (build-path dir ".steer" "failures.rktd")]) (if (file-exists? f) (file->lines f) '())))
(define (ignore . _) (void))

(let-values ([(c o) (steer "failures")])
  (check-equal? c 2 "no store yet: a usage error, and nothing is written")
  (check-false (directory-exists? (build-path dir ".steer"))))

(call-with-values (λ () (steer "init")) ignore)
(let-values ([(c o) (steer "failures")])
  (check-equal? c 0 o)
  (check-regexp-match #rx"no failures recorded" o))

(call-with-values (λ () (steer "add" "one" "--afer" "T1")) ignore)                                  ; misuse: typo'd flag
(call-with-values (λ () (steer "claim" "T99")) ignore)                                                ; misuse: unknown task
(call-with-values (λ () (steer "import" "-" #:in "(task \"A\" #:chek \"x\")")) ignore)                ; plan error
(call-with-values (λ () (steer "add" "needs check")) ignore)                                          ; warning: no --check
(call-with-values (λ () (steer "add" "fails" "--check" "false")) ignore)
(call-with-values (λ () (steer "claim" "T2")) ignore)
(call-with-values (λ () (steer "done" "T2")) ignore)                                                  ; check-failed
(call-with-values (λ () (steer "add" "ok" "--check" "true")) ignore)                                  ; no finding: nothing recorded

(let* ([j (failures)]
       [classes (for/hash ([c (hash-ref (hash-ref j 'data) 'classes)]) (values (hash-ref c 'class) (hash-ref c 'count)))])
  (check-equal? (hash-ref classes "tool-misuse") 3 "typo'd flag, unknown task, and the `no-check` warning")
  (check-equal? (hash-ref classes "plan-error") 1)
  (check-equal? (hash-ref classes "check-failed") 1)
  (check-false (hash-ref classes "syntax" #f)))

(let-values ([(c o) (steer "failures")])
  (check-regexp-match #rx"largest class: tool-misuse → usage hints" o)
  (check-regexp-match #rx"by class: tool-misuse 3" (string-replace o "by class: " "by class: ")))
(let-values ([(c o) (steer "failures" "--by" "kind")])
  (check-regexp-match #rx"unknown-flag|usage 1" o))
(check-equal? (hash-ref (hash-ref (failures "--class" "check-failed") 'data) 'total) 1 "filter by class")
(check-equal? (hash-ref (hash-ref (failures "--who" "nobody") 'data) 'total) 0 "filter by agent")
(check-true (> (hash-ref (hash-ref (failures "--who" "claude") 'data) 'total) 0))

;; messages are clipped and carry no file contents; the log is not committed
(check-true (andmap (λ (l) (< (string-length l) 500)) (log-lines)))
(check-regexp-match #rx"failures.rktd" (file->string (build-path dir ".steer" ".gitignore")))

;; the hook records syntax errors as they happen
(call-with-output-file (build-path dir "bad.rkt") (λ (o) (void (write-string "#lang racket/base\n(define (f x)\n  x\n\n(define y 1)\n" o))))
(let-values ([(c o) (steer "hook" "post-edit" #:in "{\"tool_input\":{\"file_path\":\"bad.rkt\"}}")])
  (check-equal? c 2))
(check-true (> (hash-ref (hash-ref (failures "--class" "syntax") 'data) 'total) 0) "hook-detected paren errors are logged")

;; opt-out, and recording never changes a command's result
(define before (length (log-lines)))
(let-values ([(c o) (steer "claim" "T99" #:env '(("STEER_NO_LOG" . "1")))])
  (check-equal? c 2 "same result")
  (check-equal? (length (log-lines)) before "nothing recorded with STEER_NO_LOG"))

(delete-directory/files dir)
