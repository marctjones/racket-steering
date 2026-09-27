#lang racket/base
;; Reading test-runner output (milestone XL1): so `steer done` can show *which* test failed, where, and why,
;; instead of the last 20 lines of a log. Note 12 found the raw tail loses the first failures of a
;; `dotnet test` run, clips its `file:line` at 200 characters, and reads well for pytest only because `-q`
;; happens to end with a summary. Parsers here run on the *whole* output (checks.rkt keeps up to 1 MiB) and
;; return one shape for every runner:
;;   runner   'pytest | 'dotnet | 'rackunit | #f (unknown: the caller falls back to the tail)
;;   failures list of (hasheq 'id 'file 'line 'col 'message), in the order the runner reported them
;;   summary  "5 failed, 3 passed" or #f
;; The runner is taken from the command (`pytest`, `dotnet test`, `raco test`) and, failing that, from the
;; output's own signature. The formats were taken from real runs (tests/fixtures/test-output): pytest 9.1,
;; dotnet 10 with xunit, Racket 9.3.
(require racket/list racket/string (only-in "common.rkt" plural))
(provide parse-test-output annotate-result detect-runner
         parse-diagnostics parse-msbuild parse-python-traceback)

(define (clip s n) (if (<= (string-length s) n) s (string-append (substring s 0 (max 0 (- n 1))) "…")))

(define (lines-of output) (map (λ (l) (string-trim l "\r" #:left? #f)) (string-split output "\n" #:trim? #f)))

(define (detect-runner cmd output)
  (cond
    [(regexp-match? #px"(?i:\\bpytest\\b|\\bpy\\.test\\b)" cmd) 'pytest]
    [(regexp-match? #px"\\bdotnet\\s+(?:test|vstest)\\b" cmd) 'dotnet]
    [(regexp-match? #px"\\braco\\s+test\\b" cmd) 'rackunit]
    [(regexp-match? #rx"(?m:^=+ (?:FAILURES|short test summary info|test session starts) =+$)" output) 'pytest]
    [(regexp-match? #rx"(?m:^(?:Failed|Passed)!  - Failed:)" output) 'dotnet]
    [(regexp-match? #rx"(?m:^raco test: )" output) 'rackunit]
    [else #f]))

(define (fail-record id file line col message)
  (hasheq 'id id 'file file 'line line 'col col 'message (clip (string-trim (or message "")) 200)))

;; ---------------------------------------------------------------------------------------------
;; pytest: `FAILED id - message` summary lines, and a block per failure with `file:line: ErrorType`

(define (parse-pytest lines)
  (define summary
    (for*/list ([l lines] [m (in-value (regexp-match #px"^(FAILED|ERROR) (\\S+?)(?: - (.*))?$" l))] #:when m)
      (list (cadr m) (caddr m) (cadddr m))))
  (define (header-name l) (let ([m (regexp-match #px"^_{3,} (.+?) _{3,}$" l)]) (and m (cadr m))))
  (define (section-end? l) (regexp-match? #px"^={3,}" l))
  ;; blocks: (name . lines) between `___ name ___` headers
  (define blocks
    (let loop ([ls lines] [cur #f] [acc '()])
      (cond
        [(null? ls) (reverse (if cur (cons cur acc) acc))]
        [(header-name (car ls))
         => (λ (name) (loop (cdr ls) (cons name '()) (if cur (cons cur acc) acc)))]
        [(and cur (section-end? (car ls))) (loop (cdr ls) #f (cons cur acc))]
        [cur (loop (cdr ls) (cons (car cur) (cons (car ls) (cdr cur))) acc)]     ; keep name first, lines after
        [else (loop (cdr ls) #f acc)])))
  (define (block-info b)
    (define name (car b))
    (define body (reverse (cdr b)))
    (define loc (for/last ([l body] #:when (let ([m (regexp-match #px"^(\\S+?):(\\d+): (\\S.*)$" l)]) (and m (not (regexp-match? #rx"^in " (cadddr m)))))) l))
    (define lm (and loc (regexp-match #px"^(\\S+?):(\\d+): " loc)))
    (define e-line (for/first ([l body] #:when (regexp-match? #px"^E\\s+\\S" l)) (string-trim (substring l 1))))
    (list name (and lm (cadr lm)) (and lm (string->number (caddr lm))) e-line))
  (define infos (map block-info (filter (λ (b) (pair? (cdr b))) blocks)))
  ;; join a summary entry to its block: `file.py::Class::test[p]` ↔ header `Class.test[p]`
  (define (node->header nodeid) (string-join (cdr (string-split nodeid "::")) "."))
  (define from-summary
    (for/list ([s summary])
      (define info (findf (λ (i) (equal? (car i) (node->header (cadr s)))) infos))
      (fail-record (cadr s) (or (and info (cadr info)) (car (string-split (cadr s) "::")))
                   (and info (caddr info)) #f (or (and (caddr s) (not (string=? (caddr s) "")) (caddr s)) (and info (cadddr info)) (car s)))))
  (define from-blocks-only
    (for/list ([i infos] #:unless (for/or ([s summary]) (equal? (node->header (cadr s)) (car i))))
      (fail-record (car i) (cadr i) (caddr i) #f (or (cadddr i) "failed"))))
  (append from-summary from-blocks-only))

(define (pytest-summary lines)
  (define m (for/last ([l lines] #:when (regexp-match #px"^=*\\s*((?:\\d+ (?:failed|passed|skipped|errors?|xfailed|xpassed|deselected)(?:, )?)+) in [\\d.]+s" l))
              (regexp-match #px"((?:\\d+ (?:failed|passed|skipped|errors?|xfailed|xpassed|deselected)(?:, )?)+) in" l)))
  (and m (string-trim (cadr m) #px",\\s*$")))

;; ---------------------------------------------------------------------------------------------
;; dotnet test (VSTest): `  Failed Ns.Class.Method [3 ms]`, `  Error Message:`, `  Stack Trace:` with `in file:line N`

(define (parse-dotnet lines)
  (let loop ([ls lines] [acc '()])
    (cond
      [(null? ls) (reverse acc)]
      [(regexp-match #px"^\\s+Failed (.+?) \\[[^\\]]*\\]\\s*$" (car ls))
       => (λ (m)
            (define-values (block rest) (splitf-at (cdr ls) (λ (l) (not (or (regexp-match? #px"^\\s+Failed .+ \\[[^\\]]*\\]\\s*$" l)
                                                                            (regexp-match? #px"^(?:Failed|Passed)!  -" l)
                                                                            (regexp-match? #px"^\\[xUnit\\.net .*\\[FAIL\\]" l))))))
            (define msg-lines
              (let* ([after (memf (λ (l) (regexp-match? #px"^\\s+Error Message:" l)) block)]
                     [body (if after (cdr after) '())])
                (filter (λ (l) (not (string=? (string-trim l) "")))
                        (takef body (λ (l) (not (regexp-match? #px"^\\s+Stack Trace:" l)))))))
            (define at (for/first ([l block] #:when (regexp-match? #px"^\\s+at .+ in .+:line \\d+\\s*$" l))
                         (regexp-match #px"^\\s+at .+ in (.+?):line (\\d+)\\s*$" l)))
            (loop rest (cons (fail-record (cadr m) (and at (cadr at)) (and at (string->number (caddr at))) #f
                                          (string-join (map string-trim (take msg-lines (min 2 (length msg-lines)))) " | "))
                             acc)))]
      [else (loop (cdr ls) acc)])))

(define (dotnet-summary lines)
  (define m (for/last ([l lines] #:when (regexp-match? #px"^(?:Failed|Passed)!  - Failed:" l))
              (regexp-match #px"Failed:\\s+(\\d+), Passed:\\s+(\\d+), Skipped:\\s+(\\d+)" l)))
  (and m (format "~a failed, ~a passed, ~a skipped" (cadr m) (caddr m) (cadddr m))))

;; ---------------------------------------------------------------------------------------------
;; rackunit (`raco test`): blocks between lines of dashes, FAILURE or ERROR, then `field: value` lines

(define (parse-rackunit lines)
  (define blocks                                       ; lists of lines between dash rules
    (let loop ([ls lines] [cur '()] [acc '()])
      (cond [(null? ls) (reverse (if (pair? cur) (cons (reverse cur) acc) acc))]
            [(regexp-match? #px"^-{10,}$" (car ls)) (loop (cdr ls) '() (if (pair? cur) (cons (reverse cur) acc) acc))]
            [else (loop (cdr ls) (cons (car ls) cur) acc)])))
  (for*/list ([b blocks]
              [kind-pos (in-value (index-where b (λ (l) (member l '("FAILURE" "ERROR")))))]
              #:when kind-pos)
    (define kind (list-ref b kind-pos))
    (define case-name (and (> kind-pos 0) (string-trim (car b))))
    (define fields
      (for*/hash ([l (drop b (add1 kind-pos))] [m (in-value (regexp-match #px"^(name|location|message|actual|expected|params|tolerance):\\s+(.*)$" l))] #:when m)
        (values (cadr m) (caddr m))))
    (define loc (let ([l (hash-ref fields "location" #f)]) (and l (regexp-match #px"^(.+?):(\\d+):(\\d+)$" l))))
    (define (unquote-str s) (if (and s (regexp-match? #px"^\".*\"$" s)) (substring s 1 (sub1 (string-length s))) s))
    (define msg (unquote-str (hash-ref fields "message" #f)))
    (define check (hash-ref fields "name" #f))
    (define text
      (cond
        [(equal? kind "ERROR")
         (or (for/first ([l (drop b (add1 kind-pos))] #:unless (string=? (string-trim l) "")) (string-trim l)) "error")]
        [(and (hash-ref fields "actual" #f) (hash-ref fields "expected" #f))
         (format "~a~a: got ~a, expected ~a" (if msg (string-append msg " — ") "") (or check "check")
                 (hash-ref fields "actual") (hash-ref fields "expected"))]
        [msg (format "~a: ~a" (or check "check") msg)]
        [else (format "~a failed~a" (or check "check") (let ([p (hash-ref fields "params" #f)]) (if p (string-append " with " p) "")))]))
    (fail-record (or case-name (and check (if loc (format "~a at ~a:~a" check (cadr loc) (caddr loc)) check)) "test")
                 (and loc (cadr loc)) (and loc (string->number (caddr loc))) (and loc (string->number (cadddr loc))) text)))

(define (rackunit-summary lines)
  (or (for/last ([l lines] #:when (regexp-match? #px"^\\d+/\\d+ test failures" l))
        (let ([m (regexp-match #px"^(\\d+)/(\\d+) test failures" (for/last ([x lines] #:when (regexp-match? #px"^\\d+/\\d+ test failures" x)) x))])
          (format "~a of ~a failed" (cadr m) (caddr m))))
      (for/last ([l lines] #:when (regexp-match? #px"^\\d+ tests? passed" l)) l)))

;; ---------------------------------------------------------------------------------------------

(define (parse-test-output cmd output)
  (define runner (detect-runner cmd output))
  (define lines (lines-of output))
  (case runner
    [(pytest) (hasheq 'runner runner 'failures (parse-pytest lines) 'summary (pytest-summary lines))]
    [(dotnet) (hasheq 'runner runner 'failures (parse-dotnet lines) 'summary (dotnet-summary lines))]
    [(rackunit) (hasheq 'runner runner 'failures (parse-rackunit lines) 'summary (rackunit-summary lines))]
    [else (hasheq 'runner #f 'failures '() 'summary #f)]))

;; a run-check result plus what the parser found; the (large) raw output is dropped
(define (annotate-result r)
  (define p (parse-test-output (hash-ref r 'cmd) (hash-ref r 'output "")))
  (hash-set* (hash-remove r 'output) 'runner (hash-ref p 'runner) 'failures (hash-ref p 'failures) 'summary (hash-ref p 'summary)))


;; ---------------------------------------------------------------------------------------------
;; Build/import-time diagnostics (T48): failures a test runner never got to report, because the code
;; did not compile or import. Same shape as a test failure (id file line message), so `steer done`
;; treats "it didn't build" the same way as "a test failed": located, capped, not a raw log.

;; MSBuild: `path(line,col): error CS1234: message [project]`. `id` is the diagnostic code.
(define (parse-msbuild lines)
  (define hits
    (for*/list ([l lines]
                [m (in-value (regexp-match #px"^(.+?)\\((\\d+)(?:,(\\d+))?\\): (error|warning) (\\S+): (.+?)(?: \\[.*\\])?$" l))]
                #:when m)
      (fail-record (list-ref m 5) (list-ref m 1) (string->number (list-ref m 2))
                   (and (list-ref m 3) (string->number (list-ref m 3))) (list-ref m 6))))
  (remove-duplicates hits (λ (a b) (equal? (list (hash-ref a 'file) (hash-ref a 'line) (hash-ref a 'id)) (list (hash-ref b 'file) (hash-ref b 'line) (hash-ref b 'id))))))

;; MSBuild lists each diagnostic twice (once as it happens, once in the trailing summary): count unique
;; (file, line, code) pairs, same key as parse-msbuild's own dedup, so the headline and the findings agree.
(define (msbuild-summary lines)
  (define (keys sev)
    (remove-duplicates
     (for*/list ([l lines] [m (in-value (regexp-match #px"^(.+?)\\((\\d+)(?:,\\d+)?\\): (error|warning) (\\S+):" l))]
                 #:when (and m (equal? (list-ref m 3) sev)))
       (list (list-ref m 1) (list-ref m 2) (list-ref m 4)))))
  (define errs (length (keys "error")))
  (define warns (length (keys "warning")))
  (and (> (+ errs warns) 0) (format "~a error~a, ~a warning~a" errs (plural errs) warns (plural warns))))

;; Python traceback: the *last* `File "f", line N, in scope` frame (where the raised code actually is)
;; plus the final `ExceptionType: message` line. A traceback may repeat during exception chaining
;; ("The above exception was the direct cause..."); each occurrence is one failure, in order.
(define (parse-python-traceback lines)
  (define starts (for/list ([l lines] [i (in-naturals)] #:when (regexp-match? #px"^Traceback \\(most recent call last\\):" l)) i))
  (define ends (append (cdr starts) (list (length lines))))
  (for/list ([s starts] [e ends])
    (define block (take (drop lines s) (- e s)))
    (define frames (for*/list ([l block] [m (in-value (regexp-match #px"^  File \"(.+?)\", line (\\d+), in (\\S+)" l))] #:when m) m))
    (define last-frame (and (pair? frames) (last frames)))
    (define exc-line (for/last ([l block] #:when (regexp-match? #px"^[A-Za-z_][A-Za-z0-9_.]*(Error|Exception|Warning)?: " l)) l))
    (define exc-m (and exc-line (regexp-match #px"^(\\S+): (.*)$" exc-line)))
    (fail-record (and exc-m (cadr exc-m)) (and last-frame (cadr last-frame)) (and last-frame (string->number (caddr last-frame)))
                 #f (or (and exc-m (caddr exc-m)) (and exc-line (string-trim exc-line)) "unhandled exception"))))

(define (python-traceback-summary lines)
  (define n (length (filter (λ (l) (regexp-match? #px"^Traceback \\(most recent call last\\):" l)) lines)))
  (and (> n 0) (format "~a unhandled exception~a" n (plural n))))

(define (looks-like-msbuild? output) (regexp-match? #px"(?m:^.+\\(\\d+(?:,\\d+)?\\): (?:error|warning) \\S+:)" output))
(define (looks-like-python-traceback? output) (regexp-match? #rx"(?m:^Traceback \\(most recent call last\\):)" output))

;; Diagnostics from a check that never got as far as running tests (a build or import failure). Only
;; called when parse-test-output found nothing, so it never shadows an actual test result.
(define (parse-diagnostics output)
  (define lines (lines-of output))
  (cond
    [(looks-like-msbuild? output) (hasheq 'runner 'msbuild 'failures (parse-msbuild lines) 'summary (msbuild-summary lines))]
    [(looks-like-python-traceback? output) (hasheq 'runner 'python-traceback 'failures (parse-python-traceback lines) 'summary (python-traceback-summary lines))]
    [else (hasheq 'runner #f 'failures '() 'summary #f)]))
