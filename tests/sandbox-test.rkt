#lang racket/base
;; T11: the sandboxed executor (catalog C4). Every case here was checked against racket/sandbox's ACTUAL
;; behavior first (a probe script, not assumed from memory/docs) - two real surprises found that way:
;; a CPU-time limit raises exn:fail:resource synchronously (resource='time), but a MEMORY limit kills
;; the sandbox's custodian out-of-band and surfaces only as a plain exn:fail, "evaluator: terminated
;; (out-of-memory)" - classify() in sandbox.rkt handles both shapes, and this file pins both.
(require rackunit racket/runtime-path racket/string racket/file "../steer/sandbox.rkt")

(define-runtime-path project-root ".")

;; ---------------------------------------------------------------------------------------------
;; normal eval: runs, produces its own stdout, no false positive from any limit/guard

(let ([r (run-sandboxed "#lang racket/base\n(displayln \"hello from the sandbox\")"
                         #:project-root project-root)])
  (check-eq? (sandbox-result-outcome r) 'ok)
  (check-true (string-contains? (sandbox-result-stdout r) "hello from the sandbox")))

;; ---------------------------------------------------------------------------------------------
;; infinite loop: the CPU-time limit fires, classified 'timeout, well inside the outer backstop

(let ([r (run-sandboxed "#lang racket/base\n(let loop () (loop))"
                         #:project-root project-root #:timeout 1 #:memory-mb 20)])
  (check-eq? (sandbox-result-outcome r) 'timeout)
  (check-true (< (sandbox-result-elapsed-ms r) 8000) "should be caught by the inner limit, not the +10s outer backstop"))

;; ---------------------------------------------------------------------------------------------
;; allocation bomb: the memory limit fires, classified 'memory (the asymmetric exn shape above)

(let ([r (run-sandboxed "#lang racket/base\n(let loop ([acc '()]) (loop (cons (make-bytes 1000000 0) acc)))"
                         #:project-root project-root #:timeout 8 #:memory-mb 10)])
  (check-eq? (sandbox-result-outcome r) 'memory))

;; ---------------------------------------------------------------------------------------------
;; network access: refused by our own sandbox-network-guard, not a bare exn

(let ([r (run-sandboxed "#lang racket/base\n(require racket/tcp)\n(tcp-connect \"example.com\" 80)"
                         #:project-root project-root)])
  (check-eq? (sandbox-result-outcome r) 'refused))

;; ---------------------------------------------------------------------------------------------
;; write outside the granted temp dir: refused by racket/sandbox's own path-permissions, not a bare
;; exn. Targets a fresh path of our own (never something like /etc/hosts - a real system file must
;; never be the target here even as a negative test, in case a path-permissions bug ever let it
;; through) that is deliberately NOT under the write-dir run-sandboxed grants internally.

(let* ([outside-dir (make-temporary-file "steer-sandbox-test-outside~a" 'directory)]
       [outside-path (build-path outside-dir "should-not-be-written.txt")])
  (let ([r (run-sandboxed (format "#lang racket/base\n(call-with-output-file ~s (lambda (o) (write 1 o)))"
                                   (path->string outside-path))
                           #:project-root project-root)])
    (check-eq? (sandbox-result-outcome r) 'refused)
    (check-false (file-exists? outside-path) "the write must not have happened"))
  (delete-directory outside-dir))

;; ---------------------------------------------------------------------------------------------
;; read IS allowed under the granted project root (the opposite case: prove the read allow-list works,
;; not just that everything is denied)

(let ([r (run-sandboxed (format "#lang racket/base\n(displayln (file-exists? ~s))"
                                 (path->string (path->complete-path "sandbox-test.rkt" project-root)))
                         #:project-root project-root)])
  (check-eq? (sandbox-result-outcome r) 'ok)
  (check-true (string-contains? (sandbox-result-stdout r) "#t") "the project root must be readable"))
