#lang racket/base
;; Sandboxed executor (catalog C4): run untrusted Racket code with time/memory limits, no network, and
;; a filesystem restricted to read-only under the project root plus write-only under one temp
;; directory. Foundation for A2/A3 and for hardening `steer api`'s own worker.
;;
;; Two layers, not one: `racket/sandbox`'s custodian-based limits (sandbox-eval-limits, sandbox-network-
;; guard, sandbox-path-permissions) run INSIDE a worker process and catch the common case (an infinite
;; loop, a memory-hungry program, a stray `(tcp-connect ...)`) cleanly, with a real value/error back.
;; The worker itself runs in the INSTALLED `racket`, not this binary - same reason as api.rkt/doc.rkt:
;; a `raco exe` binary does not embed the full collects tree a `#lang`-headed target program needs, and
;; a genuinely separate OS process can be killed outright if the inner limits are somehow bypassed (an
;; unsafe FFI call, say) - the "OS isolation" half of this task's goal. Building an OS-level sandbox
;; (macOS `sandbox-exec` is deprecated; a namespace/cgroup variant is Linux-only) is left for a later
;; task if the subprocess-kill backstop ever proves insufficient; v1 does not attempt it.
(require racket/string racket/file racket/port "common.rkt")
(provide (struct-out sandbox-result) run-sandboxed)

;; outcome: 'ok | 'timeout | 'memory | 'refused | 'exn
;; value: the module's last expression printed with `~v`, or #f
;; elapsed-ms: wall time for the whole worker process, including startup - not just the eval itself
(struct sandbox-result (outcome value stdout stderr elapsed-ms) #:prefab)

(define worker-source #<<WORKER
#lang racket/base
(require racket/sandbox racket/port)
(define args (vector->list (current-command-line-arguments)))
(define input-path (list-ref args 0))
(define timeout-s (string->number (list-ref args 1)))
(define memory-mb (string->number (list-ref args 2)))
(define project-root (list-ref args 3))
(define write-dir (list-ref args 4))

(sandbox-eval-limits (list timeout-s memory-mb))
(sandbox-network-guard (lambda args (error 'sandbox-refused "network access refused")))
(sandbox-path-permissions
 (list (list 'exists write-dir) (list 'write write-dir) (list 'delete write-dir)
       (list 'read project-root)))
(define out (open-output-string))
(define err (open-output-string))
(sandbox-output out)
(sandbox-error-output err)

;; racket/sandbox reports the two eval-limits asymmetrically, checked directly (not assumed from
;; docs): a TIME violation raises exn:fail:resource with resource='time, synchronously, from the call
;; that hit the limit - but a MEMORY violation kills the sandbox's custodian out-of-band and the
;; calling thread only ever sees a plain exn:fail, "evaluator: terminated (out-of-memory)", with no
;; exn:fail:resource wrapper at all.
(define (classify e)
  (define m (exn-message e))
  (cond [(and (exn:fail:resource? e) (eq? (exn:fail:resource-resource e) 'time)) 'timeout]
        [(and (exn:fail:resource? e) (eq? (exn:fail:resource-resource e) 'memory)) 'memory]
        [(regexp-match? #rx"out-of-memory" m) 'memory]
        ;; network: our own sandbox-network-guard raises this. filesystem: racket/sandbox's own
        ;; sandbox-path-permissions denial always reads "OP: access denied for PATH" (`open-input-file`,
        ;; `open-output-file`, `delete-file`, ...) - both are policy refusals, not program bugs.
        [(regexp-match? #rx"sandbox-refused|access denied" m) 'refused]
        [else 'exn]))

(define result
  (with-handlers ([exn:fail? (lambda (e) (list (classify e) (exn-message e)))])
    ;; a bare string is one of make-module-evaluator's OWN valid inputs too - it means "this string
    ;; IS the module's source text", not "read the file at this path" - so the path must be converted
    ;; explicitly, or a file path passed as a string here reads back as a syntax error instead of
    ;; running the file (checked directly: this exact mistake was made and caught while building this).
    (define ev (make-module-evaluator (string->path input-path)))
    (define v (call-in-sandbox-context ev (lambda () (void))))
    (kill-evaluator ev)
    (list 'ok #f)))

(write (list (car result) (cadr result) (get-output-string out) (get-output-string err)))
WORKER
  )

;; Runs `code` (a full `#lang`-headed Racket program, as text) in the sandboxed worker.
;; #:project-root: the only directory tree the sandboxed code may READ.
;; #:timeout/#:memory-mb: racket/sandbox's own limits, in seconds and megabytes.
;; An outer wall-clock timeout (timeout + 10s grace) backstops the worker process itself in case the
;; inner limit is bypassed; on that backstop firing, the worker is killed and outcome is 'timeout.
(define (run-sandboxed code #:project-root project-root #:timeout [timeout 10] #:memory-mb [memory-mb 64])
  (define racket (or (getenv "STEER_RACKET") (let ([p (find-executable-path "racket")]) (and p (path->string p)))))
  (unless racket
    (fail! 'no-racket "the sandboxed executor needs an installed `racket`" #:hint "install Racket, or set STEER_RACKET" #:code 3))
  (define dir (make-temporary-directory "steer-sandbox~a"))
  (define write-dir (build-path dir "write"))
  (make-directory* write-dir)
  (define input-path (build-path dir "input.rkt"))
  (call-with-output-file input-path (lambda (o) (write-string code o)))
  (define worker (build-path dir "worker.rkt"))
  (call-with-output-file worker (lambda (o) (write-string worker-source o)))
  (define t0 (current-inexact-milliseconds))
  (define-values (p out in err)
    (apply subprocess #f #f #f racket (path->string worker)
           (list (path->string input-path) (number->string timeout) (number->string memory-mb)
                 (path->string (path->complete-path project-root)) (path->string write-dir))))
  (close-output-port in)
  (define out-text (box ""))
  (define err-text (box ""))
  (define t1 (thread (lambda () (set-box! out-text (port->string out)))))
  (define t2 (thread (lambda () (set-box! err-text (port->string err)))))
  (define finished? (sync/timeout (+ timeout 10) p))
  (unless finished? (subprocess-kill p #t))
  (subprocess-wait p)
  (thread-wait t1) (thread-wait t2)
  (close-input-port out) (close-input-port err)
  (define elapsed (- (current-inexact-milliseconds) t0))
  (delete-directory/files dir)
  (cond
    [(not finished?) (sandbox-result 'timeout #f (unbox out-text) (unbox err-text) elapsed)]
    [else
     (define d (with-handlers ([exn:fail? (lambda (e) #f)]) (read (open-input-string (unbox out-text)))))
     (cond
       [(not (and (list? d) (= (length d) 4)))
        (sandbox-result 'exn #f (unbox out-text) (unbox err-text) elapsed)]
       [else (sandbox-result (car d) (cadr d) (caddr d) (cadddr d) elapsed)])]))
