#lang racket/base
;; Baseline calibration for a model on the sample tasks (note 04 condition C0: the model alone, one
;; attempt, no tools). Splits the work so each half does what it is good at:
;;   racket scripts/eval-local.rkt prompts OUT.jsonl [--seed S] [--multipl N] [--t1 N] [--t2 N] [--t3 N]
;;   .venv-mlx/bin/python scripts/mlx_generate.py OUT.jsonl COMPLETIONS.jsonl        (model runs here)
;;   racket scripts/eval-local.rkt score PROMPTS.jsonl COMPLETIONS.jsonl RESULTS.jsonl
;;   racket scripts/eval-local.rkt report RESULTS.jsonl
;; Only the `tasks` split is used (never `holdout`). MultiPL-E tasks are HumanEval only: MBPP's one-line
;; specs are vague and its tests encode Python conventions, so its pass rate mostly measures guessing them. Scoring is `raco make` (compiles?) then `raco test`
;; (plain `racket file.rkt` exits 0 even when rackunit checks fail, so it cannot be the judge).
;; Stages are reported separately: no-compile, wrong, timeout, pass.
(require racket/list racket/string racket/file racket/port racket/format racket/math racket/runtime-path json
         "corpus.rkt")

(provide eval-dir read-meta meta-ref prompt-for task-dirs extract-code judge wilson median pct report-results run-cmd)

(define-runtime-path here ".")
(define eval-dir (simplify-path (build-path here 'up "samples" "eval" "tasks")))

(define (read-meta dir) (call-with-input-file (build-path dir "task.rktd") read))
(define (meta-ref m k [d #f]) (let ([p (assq k m)]) (if p (cadr p) d)))

;; ---------------------------------------------------------------------------------------------
;; prompts

(define (fence lang s) (string-append "```" lang "\n" (string-trim s "\n" #:left? #f) "\n```"))

(define (prompt-for dir meta)
  (define kind (meta-ref meta 'kind))
  (define fmt (meta-ref meta 'format))
  (define (slurp f) (file->string (build-path dir f)))
  (cond
    [(eq? fmt 'multipl-e)
     (string-append "Complete the following Racket program. Write the complete definition of the function (and any "
                    "helper definitions it needs) in a single ```racket code block. Do not write tests or example calls.\n\n"
                    (fence "racket" (slurp "prompt.rkt")))]
    [(eq? kind 'T3)
     (define sol (meta-ref meta 'solution-file))
     (define test (for/first ([f (directory-list dir)] #:when (regexp-match? #rx"-test[.]rkt$" (path->string f))) (path->string f)))
     (string-append "The Racket file `" sol "` below is meant to pass the tests, but it does not. Fix it. Reply with the complete "
                    "corrected `" sol "` in a single ```racket code block.\n\n"
                    "## Task\n" (slurp "instructions.md") "\n\n## `" sol "` (broken)\n" (fence "racket" (slurp sol))
                    "\n\n## Tests `" test "`\n" (fence "racket" (slurp test)))]
    [else
     (define sol (meta-ref meta 'solution-file))
     (define test (for/first ([f (directory-list dir)] #:when (regexp-match? #rx"-test[.]rkt$" (path->string f))) (path->string f)))
     (string-append "Implement this Racket exercise. Reply with the complete contents of `" sol "` in a single ```racket code block.\n\n"
                    "## Instructions\n" (slurp "instructions.md") "\n\n## Starting file `" sol "`\n" (fence "racket" (slurp sol))
                    "\n\n## Tests `" test "` (they will be run against your solution)\n" (fence "racket" (slurp test)))]))

(define (task-dirs)
  (for/list ([d (directory-list eval-dir #:build? #t)] #:when (file-exists? (build-path d "task.rktd"))) d))

(define (cmd-prompts args)
  (define (opt k def) (let ([m (member k args)]) (if (and m (pair? (cdr m))) (string->number (cadr m)) def)))
  (define seed (let ([m (member "--seed" args)]) (if (and m (pair? (cdr m))) (cadr m) "c0")))
  (define out (car args))
  (define dirs (task-dirs))
  (define (pick pred n label)
    (define pool (filter (λ (d) (pred (read-meta d))) dirs))
    (seeded-sample pool n (string-append seed "/" label) #:key path->string))
  (define chosen
    (append (pick (λ (m) (and (eq? (meta-ref m 'format) 'multipl-e) (regexp-match? #rx"^humaneval" (meta-ref m 'id)))) (opt "--multipl" 40) "multipl")
            (pick (λ (m) (and (eq? (meta-ref m 'kind) 'T1) (eq? (meta-ref m 'format) 'exercism))) (opt "--t1" 30) "t1")
            (pick (λ (m) (eq? (meta-ref m 'kind) 'T2)) (opt "--t2" 20) "t2")
            (pick (λ (m) (and (eq? (meta-ref m 'kind) 'T3) (eq? (meta-ref m 'mutation) 'paren))) (quotient (opt "--t3" 40) 2) "t3p")
            (pick (λ (m) (and (eq? (meta-ref m 'kind) 'T3) (eq? (meta-ref m 'mutation) 'unbound))) (quotient (opt "--t3" 40) 2) "t3u")))
  (call-with-output-file out #:exists 'truncate
    (λ (o)
      (for ([d chosen])
        (define m (read-meta d))
        (write-json (hasheq 'id (meta-ref m 'id) 'kind (symbol->string (meta-ref m 'kind))
                            'mutation (let ([x (meta-ref m 'mutation)]) (and x (symbol->string x)))
                            'prompt (prompt-for d m))
                    o)
        (newline o))))
  (printf "wrote ~a prompts to ~a\n" (length chosen) out))

;; ---------------------------------------------------------------------------------------------
;; scoring

(define (extract-code text)
  (define blocks (regexp-match* #px"```[a-zA-Z0-9_+-]*\\s*\n(.*?)```" text #:match-select cadr))
  (cond [(pair? blocks) (argmax string-length blocks)]
        [else (let ([m (regexp-match #px"```[a-zA-Z0-9_+-]*\\s*\n(.*)$" text)])   ; an unterminated block
                (if m (cadr m) text))]))

(define (run-cmd dir timeout . args)
  (define-values (p out in _err)
    (parameterize ([current-directory dir]) (apply subprocess #f #f 'stdout (find-executable-path "raco") args)))
  (close-output-port in)
  (define buf (box ""))
  (define t (thread (λ () (set-box! buf (port->string out)))))
  (define done? (sync/timeout timeout p))
  (unless done? (subprocess-kill p #t))
  (subprocess-wait p)
  (thread-wait t)
  (close-input-port out)
  (values (if done? (subprocess-status p) 'timeout) (unbox buf)))

;; → (values 'pass|'wrong|'no-compile|'timeout detail-text)
;; detail-text is the tail of the relevant raco output: the compile error for no-compile, the test
;; failure for wrong, "" for pass/timeout. Used to build C1's raw-error retry feedback.
(define (judge dir-of-task meta code)
  (define tmp (make-temporary-directory "steer-eval~a"))
  (define fmt (meta-ref meta 'format))
  (define file
    (cond
      [(eq? fmt 'multipl-e)
       (define prog (string-append (if (regexp-match? #rx"^#lang" (string-trim code #:right? #f)) "" "#lang racket\n")
                                   code "\n" (file->string (build-path dir-of-task "tests.rkt"))))
       (call-with-output-file (build-path tmp "prog.rkt") (λ (o) (void (write-string prog o))))
       "prog.rkt"]
      [else
       (for ([f (directory-list dir-of-task)]
             #:unless (member (path->string f) (list "task.rktd" "reference.rkt" "instructions.md" (meta-ref meta 'solution-file))))
         (copy-file (build-path dir-of-task f) (build-path tmp f)))
       (call-with-output-file (build-path tmp (meta-ref meta 'solution-file)) (λ (o) (void (write-string code o))))
       (for/first ([f (directory-list dir-of-task)] #:when (regexp-match? #rx"-test[.]rkt$" (path->string f))) (path->string f))]))
  (define-values (c1 o1) (run-cmd tmp 60 "make" file))
  (define-values (result detail)
    (cond [(eq? c1 'timeout) (values 'timeout "")]
          [(not (eqv? c1 0)) (values 'no-compile (head-text o1))]
          [else (define-values (c2 o2) (run-cmd tmp 60 "test" file))
                (cond [(eq? c2 'timeout) (values 'timeout "")]
                      [(eqv? c2 0) (values 'pass "")]
                      [else (values 'wrong (head-text o2))])]))
  (delete-directory/files tmp)
  (values result detail))

;; HARNESS BUG (found while scoring real model completions for T9, not the synthetic fixtures this
;; scorer was validated on): `raco make`'s read/compile errors put the real message on the FIRST
;; line, followed by a long "context...\n  <continuation marks>" stack dump; `raco test` prints each
;; failure's name/location/params at the point it happens, then repeats aggregate "N success(es)..."
;; counts afterwards. tail-text (steer/checks.rkt) keeps the LAST ~20 lines/1500 chars, which for
;; both of these is exactly the noise (stack frames, or repeated counts) and drops the one thing a
;; retry prompt needs — the actual error. head-text keeps the front instead, and for raco make also
;; drops everything from "context..." onward since it is never useful feedback.
(define (head-text s #:lines [max-lines 20] #:chars [max-chars 1500])
  (define ls (filter (λ (l) (not (regexp-match? #px"^\\s*$" l)))
                     (string-split (regexp-replace* #rx"\e\\[[0-9;]*[A-Za-z]" s "") "\n")))
  (define ls* (let loop ([l ls] [acc '()])
                 (cond [(null? l) (reverse acc)]
                       [(regexp-match? #px"^\\s*(compilation )?context\\.\\.\\.\\s*$" (car l)) (reverse acc)]
                       [else (loop (cdr l) (cons (car l) acc))])))
  (define head (if (> (length ls*) max-lines) (take ls* max-lines) ls*))
  (define clipped (for/list ([l head]) (if (> (string-length l) 200) (string-append (substring l 0 199) "…") l)))
  (let loop ([ls clipped])
    (define t (string-join ls "\n"))
    (if (and (> (string-length t) max-chars) (pair? (cdr ls))) (loop (reverse (cdr (reverse ls)))) t)))

(define (read-jsonl f) (for/list ([l (file->lines f)] #:unless (string=? (string-trim l) "")) (string->jsexpr l)))

(define (cmd-score args)
  (define prompts (for/hash ([p (read-jsonl (car args))]) (values (hash-ref p 'id) p)))
  (define comps (read-jsonl (cadr args)))
  (define dirs (for/hash ([d (task-dirs)]) (values (meta-ref (read-meta d) 'id) d)))
  (define results
    (let ([sem (make-semaphore 4)])
      (define threads
        (for/list ([c comps])
          (thread-with-result
           (λ ()
             (semaphore-wait sem)
             (begin0
               (let*-values ([(id) (hash-ref c 'id)] [(d) (hash-ref dirs id)] [(m) (read-meta d)]
                             [(p) (hash-ref prompts id)]
                             [(code) (extract-code (hash-ref c 'completion ""))]
                             [(r detail) (if (string=? (string-trim code) "") (values 'no-compile "(empty completion / no code block found)") (judge d m code))])
                 (hasheq 'id id 'kind (hash-ref p 'kind) 'mutation (hash-ref p 'mutation #f) 'result (symbol->string r)
                         'pass (eq? r 'pass) 'completion_tokens (hash-ref c 'completion_tokens 0)
                         'prompt_tokens (hash-ref c 'prompt_tokens 0) 'seconds (hash-ref c 'seconds 0)
                         'code code 'detail detail))
               (semaphore-post sem))))))
      (map thread-result threads)))
  (call-with-output-file (caddr args) #:exists 'truncate
    (λ (o) (for ([r results]) (write-json r o) (newline o))))
  (printf "scored ~a completions → ~a\n" (length results) (caddr args))
  (report-results results))

;; tiny thread-with-result helper
(struct tr (thread box))
(define (thread-with-result thunk)
  (define b (box #f))
  (tr (thread (λ () (set-box! b (thunk)))) b))
(define (thread-result t) (thread-wait (tr-thread t)) (unbox (tr-box t)))

;; ---------------------------------------------------------------------------------------------
;; report

(define (wilson k n [z 1.96])
  (if (zero? n) (values 0 0)
      (let* ([p (/ k n)] [d (+ 1 (/ (* z z) n))]
             [c (/ (+ p (/ (* z z) (* 2 n))) d)]
             [h (/ (* z (sqrt (+ (/ (* p (- 1 p)) n) (/ (* z z) (* 4 n n))))) d)])
        (values (max 0 (- c h)) (min 1 (+ c h))))))

(define (median l) (if (null? l) 0 (list-ref (sort l <) (quotient (length l) 2))))
(define (pct x) (~a (~r (* 100.0 x) #:precision 0) "%"))

(define (report-results rs)
  (define groups
    (list (cons "MultiPL-E (write function)" (λ (r) (regexp-match? #rx"^(humaneval|mbpp)" (hash-ref r 'id))))
          (cons "Exercism T1 (write)" (λ (r) (and (equal? (hash-ref r 'kind) "T1") (regexp-match? #rx"^exercism" (hash-ref r 'id)))))
          (cons "Exercism T2 (library use)" (λ (r) (equal? (hash-ref r 'kind) "T2")))
          (cons "T3 repair: deleted paren" (λ (r) (equal? (hash-ref r 'mutation) "paren")))
          (cons "T3 repair: unbound name" (λ (r) (equal? (hash-ref r 'mutation) "unbound")))
          (cons "all" (λ (r) #t))))
  (printf "\n| set | n | pass | 95% CI | no-compile | wrong | timeout | median tokens out |\n|---|---|---|---|---|---|---|---|\n")
  (for ([g groups])
    (define sub (filter (cdr g) rs))
    (define n (length sub))
    (define (cnt k) (length (filter (λ (r) (equal? (hash-ref r 'result) k)) sub)))
    (define-values (lo hi) (wilson (cnt "pass") n))
    (printf "| ~a | ~a | ~a | ~a–~a | ~a | ~a | ~a | ~a |\n" (car g) n
            (if (zero? n) "–" (pct (/ (cnt "pass") n))) (pct lo) (pct hi)
            (cnt "no-compile") (cnt "wrong") (cnt "timeout") (median (map (λ (r) (hash-ref r 'completion_tokens)) sub)))))

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (case (and (pair? args) (car args))
    [("prompts") (cmd-prompts (cdr args))]
    [("score") (cmd-score (cdr args))]
    [("report") (report-results (read-jsonl (cadr args)))]
    [else (eprintf "usage: racket scripts/eval-local.rkt prompts|score|report ...\n") (exit 2)]))
