#lang racket/base
;; C1 (raw-error retry) and C2-lite (steer-syntax-feedback retry) on top of the C0 completions/results
;; that eval-local.rkt already produced. This is the cheap "does steering help" proxy from note 04's
;; T15, using only steer syntax --fix (A1, already built) — not the unbuilt A2/A3 checkers.
;;
;; Pipeline (turn 1 = C0, already run via eval-local.rkt prompts/score):
;;   racket scripts/retry-eval.rkt eligibility RESULTS1.jsonl ELIG.jsonl
;;   racket scripts/retry-eval.rkt turn2-prompts c1      PROMPTS1.jsonl RESULTS1.jsonl ELIG.jsonl OUT.jsonl
;;   racket scripts/retry-eval.rkt turn2-prompts c2lite   PROMPTS1.jsonl RESULTS1.jsonl ELIG.jsonl OUT.jsonl
;;   .venv-mlx/bin/python scripts/mlx_generate.py OUT.jsonl COMPLETIONS2.jsonl
;;   racket scripts/eval-local.rkt score OUT.jsonl COMPLETIONS2.jsonl RESULTS2.jsonl     (reused as-is)
;;   racket scripts/retry-eval.rkt combine RESULTS1.jsonl RESULTS2.jsonl COMBINED.jsonl
;;   racket scripts/eval-local.rkt report COMBINED.jsonl                                 (reused as-is)
;;   racket scripts/retry-eval.rkt subset-report ELIG.jsonl COMBINED-C1.jsonl COMBINED-C2LITE.jsonl
(require racket/list racket/string racket/file racket/format racket/system racket/port json
         "eval-local.rkt")

(define steer-bin (path->string (build-path (current-directory) "build" "steer")))

(define (read-jsonl f) (for/list ([l (file->lines f)] #:unless (string=? (string-trim l) "")) (string->jsexpr l)))
(define (write-jsonl f rows) (call-with-output-file f #:exists 'truncate (λ (o) (for ([r rows]) (write-json r o) (newline o)))))

;; ---------------------------------------------------------------------------------------------
;; eligibility: does `steer syntax` find an actual (bracket/read) error in this failing completion?
;; Only such tasks are a fair test of C2-lite (A1 has no way to help unbound-id or semantic failures).

(define (steer-syntax-findings code)
  (define tmp (make-temporary-file "steer-elig~a.rkt"))
  (call-with-output-file tmp #:exists 'truncate (λ (o) (write-string code o)))
  (define out (with-output-to-string (λ () (system* steer-bin "syntax" (path->string tmp) "--json"))))
  (delete-file tmp)
  (with-handlers ([exn:fail? (λ (e) '())])
    (hash-ref (string->jsexpr out) 'findings '())))

(define (cmd-eligibility args)
  (define results1 (read-jsonl (car args)))
  (define rows
    (for/list ([r results1] #:unless (hash-ref r 'pass))
      (define findings (steer-syntax-findings (hash-ref r 'code "")))
      (define errs (filter (λ (f) (equal? (hash-ref f 'severity) "error")) findings))
      (hasheq 'id (hash-ref r 'id) 'eligible (pair? errs)
              'finding-kinds (map (λ (f) (hash-ref f 'kind)) errs))))
  (write-jsonl (cadr args) rows)
  (printf "eligibility: ~a/~a failing turn-1 tasks have a steer-syntax-detectable error\n"
          (length (filter (λ (r) (hash-ref r 'eligible)) rows)) (length rows)))

;; ---------------------------------------------------------------------------------------------
;; turn2-prompts

;; Runs steer syntax --fix on a temp copy and returns its report with the temp path replaced by a
;; plain "solution.rkt" so the model sees the same wording it would from steer on its own checkout.
(define (steer-fix-feedback code)
  (define tmp (make-temporary-file "steer-fix~a.rkt"))
  (call-with-output-file tmp #:exists 'truncate (λ (o) (write-string code o)))
  (define raw (with-output-to-string (λ () (system* steer-bin "syntax" (path->string tmp) "--fix"))))
  (define out (string-replace raw (path->string tmp) "solution.rkt"))
  (define fixed (file->string tmp))
  (delete-file tmp)
  (values out fixed))

(define (retry-prompt orig-prompt code header feedback)
  (string-append orig-prompt
                 "\n\n## Your previous attempt (did not pass)\n```racket\n" code "\n```\n\n"
                 "## " header "\n" feedback
                 "\n\nFix your solution. Reply with the complete corrected file in a single ```racket code block."))

(define (cmd-turn2-prompts args)
  (define condition (car args))
  (define prompts1 (for/hash ([p (read-jsonl (cadr args))]) (values (hash-ref p 'id) p)))
  (define results1 (read-jsonl (caddr args)))
  (define elig (for/hash ([e (read-jsonl (cadddr args))]) (values (hash-ref e 'id) (hash-ref e 'eligible))))
  (define out-path (list-ref args 4))
  (define rows
    (for/list ([r results1] #:unless (hash-ref r 'pass)
               #:when (case condition
                        [("c1") #t]
                        [("c2lite") (hash-ref elig (hash-ref r 'id) #f)]
                        [else (error 'turn2-prompts "unknown condition ~a" condition)]))
      (define id (hash-ref r 'id))
      (define p (hash-ref prompts1 id))
      (define code (hash-ref r 'code ""))
      (define-values (header feedback)
        (case condition
          [("c1")
           (values "Compiler/test error"
                   (if (string=? (hash-ref r 'detail "") "")
                       (format "your solution ~a." (if (equal? (hash-ref r 'result) "timeout") "timed out (60s)" "failed"))
                       (hash-ref r 'detail)))]
          [("c2lite")
           (define-values (fix-text _fixed) (steer-fix-feedback code))
           (values "`steer syntax --fix` report (a located structural error plus a verified repair)" fix-text)]))
      (hasheq 'id id 'kind (hash-ref p 'kind) 'mutation (hash-ref p 'mutation #f)
              'prompt (retry-prompt (hash-ref p 'prompt) code header feedback))))
  (write-jsonl out-path rows)
  (printf "wrote ~a turn-2 prompts (~a) for condition ~a\n" (length rows) out-path condition))

;; ---------------------------------------------------------------------------------------------
;; combine: condition pass = turn1 pass, or (turn1 fail AND retried AND turn2 pass). Token/time cost
;; is summed across the turns actually spent.

(define (cmd-combine args)
  (define results1 (read-jsonl (car args)))
  (define results2 (for/hash ([r (read-jsonl (cadr args))]) (values (hash-ref r 'id) r)))
  (define rows
    (for/list ([r1 results1])
      (define id (hash-ref r1 'id))
      (cond
        [(hash-ref r1 'pass) (hash-set* r1 'turns 1 'retried #f)]
        [(hash-ref results2 id #f)
         => (λ (r2)
              (hash-set* r2 'turns 2 'retried #t
                         'completion_tokens (+ (hash-ref r1 'completion_tokens 0) (hash-ref r2 'completion_tokens 0))
                         'prompt_tokens (+ (hash-ref r1 'prompt_tokens 0) (hash-ref r2 'prompt_tokens 0))
                         'seconds (+ (hash-ref r1 'seconds 0) (hash-ref r2 'seconds 0))
                         'turn1_result (hash-ref r1 'result)))]
        [else (hash-set* r1 'turns 1 'retried #f)])))
  (write-jsonl (caddr args) rows)
  (printf "combined ~a tasks -> ~a\n" (length rows) (caddr args)))

(define (hash-set* h . kvs)
  (let loop ([h h] [kvs kvs])
    (if (null? kvs) h (loop (hash-set h (car kvs) (cadr kvs)) (cddr kvs)))))

;; ---------------------------------------------------------------------------------------------
;; subset-report: head-to-head of C1 vs C2-lite on the syntax-error subset only.

(define (cmd-subset-report args)
  (define elig-ids (for/list ([e (read-jsonl (car args))] #:when (hash-ref e 'eligible)) (hash-ref e 'id)))
  (define elig-set (for/hash ([i elig-ids]) (values i #t)))
  (define c1 (for/hash ([r (read-jsonl (cadr args))]) (values (hash-ref r 'id) r)))
  (define c2 (for/hash ([r (read-jsonl (caddr args))]) (values (hash-ref r 'id) r)))
  (printf "\nsyntax-error subset: ~a tasks whose turn-1 (C0) failure `steer syntax` actually flags\n" (length elig-ids))
  (printf "| condition | n | pass | 95% CI |\n|---|---|---|---|\n")
  (for ([cond-name (list "C1 (raw error retry)" "C2-lite (steer syntax retry)")]
        [tbl (list c1 c2)])
    (define sub (for/list ([i elig-ids] #:when (hash-ref tbl i #f)) (hash-ref tbl i)))
    (define n (length sub))
    (define k (length (filter (λ (r) (hash-ref r 'pass)) sub)))
    (define-values (lo hi) (wilson k n))
    (printf "| ~a | ~a | ~a | ~a–~a |\n" cond-name n (if (zero? n) "–" (pct (/ k n))) (pct lo) (pct hi))))

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (case (and (pair? args) (car args))
    [("eligibility") (cmd-eligibility (cdr args))]
    [("turn2-prompts") (cmd-turn2-prompts (cdr args))]
    [("combine") (cmd-combine (cdr args))]
    [("subset-report") (cmd-subset-report (cdr args))]
    [else (eprintf "usage: racket scripts/retry-eval.rkt eligibility|turn2-prompts|combine|subset-report ...\n") (exit 2)]))
