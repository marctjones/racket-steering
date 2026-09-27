#lang racket/base
;; T31: `steer spec check|render` through the real CLI: output, findings, exit codes, --lax, --json, the cap.
(require rackunit racket/list racket/string racket/file racket/port racket/runtime-path json)

(define-runtime-path main-rkt "../steer/main.rkt")
(define dir (make-temporary-directory "steer-spec~a"))

(define (steer #:in [input ""] . args)
  (define-values (p out in err)
    (parameterize ([current-directory dir])
      (apply subprocess #f #f 'stdout (find-executable-path "racket") (path->string main-rkt) args)))
  (write-string input in) (close-output-port in)
  (define text (port->string out))
  (subprocess-wait p) (close-input-port out)
  (values (subprocess-status p) text))

(define-syntax-rule (expect code rx call)
  (let-values ([(c t) call])
    (check-equal? c code (format "exit code for ~a\n~a" 'call t))
    (check-regexp-match rx t)))

(define good "The export SHALL write a header row.\nWHEN the input is empty, the export SHALL write only the header row.\n")

;; ---------------------------------------------------------------------------------------------
;; check

(let-values ([(c o) (steer "spec" "check" "-" #:in good)])
  (check-equal? c 0 o)
  (check-regexp-match #rx"^2 criteria: 2 ok, 0 refused \\(0 errors, 0 warnings\\)" o)
  (check-regexp-match #rx"L1 ubiquitous: export → write a header row" o "the structured form")
  (check-regexp-match #rx"L2 event: export → write only the header row  \\(when: the input is empty\\)" o))
(let-values ([(c o) (steer "spec" "check" "-" #:in "The tool SHALL exit.\n")])
  (check-regexp-match #rx"^1 criterion: 1 ok" o "singular"))

(define mixed "# criteria\nThe export SHALL write a header row.\n\nThe export should write one row per record.\nExports are fast.\nThe tool SHALL retry as needed.\n")
(let-values ([(c o) (steer "spec" "check" "-" #:in mixed)])
  (check-equal? c 1 o)
  (check-regexp-match #rx"4 criteria: 1 ok, 3 refused \\(3 errors, 0 warnings\\)" o)
  (check-regexp-match #rx"error weak-modal <stdin>:4:12: `should` is not allowed.*→ write SHALL instead of `should`" o "located, with a fix")
  (check-regexp-match #rx"error bad-start <stdin>:5:1: " o)
  (check-regexp-match #rx"error hedge <stdin>:6:22: `as needed`" o "lint refusals are reported too")
  (check-regexp-match #rx"next: fix the refused lines" o)
  (check-false (regexp-match? #rx"L4|L5|L6 " o) "refused lines are not listed as structured"))

;; a file argument names the file in findings
(define f (build-path dir "goal.txt"))
(call-with-output-file f #:exists 'truncate (λ (o) (void (write-string "The export should write x.\n" o))))
(let-values ([(c o) (steer "spec" "check" "goal.txt")])
  (check-equal? c 1)
  (check-regexp-match #rx"error weak-modal goal.txt:1:12" o))

;; --lax: refusals from the lint become warnings; grammar errors stay errors
(let-values ([(c o) (steer "spec" "check" "-" "--lax" #:in "The tool SHALL retry as needed.\n")])
  (check-equal? c 0 o)
  (check-regexp-match #rx"1 criterion: 1 ok, 0 refused \\(0 errors, 1 warning\\)" o)
  (check-regexp-match #rx"warning hedge" o))
(let-values ([(c o) (steer "spec" "check" "-" "--lax" #:in "The tool should exit.\n")])
  (check-equal? c 1 "the grammar is not relaxed"))
;; a warning alone does not fail
(let-values ([(c o) (steer "spec" "check" "-" #:in "The tool SHALL exit within 5.\n")])
  (check-equal? c 0 o)
  (check-regexp-match #rx"warning bare-quantity" o))

;; nothing to check is a problem, not a pass
(let-values ([(c o) (steer "spec" "check" "-" #:in "")])
  (check-equal? c 1 o)
  (check-regexp-match #rx"no-criteria.*no criteria in <stdin>.*The export SHALL write a header row" o))
(let-values ([(c o) (steer "spec" "check" "-" #:in "# just a comment\n\n")]) (check-equal? c 1))

;; the cap and --full / --limit (shared protocol)
(define eight (string-join (for/list ([i 8]) "Exports are fast.") "\n"))
(let-values ([(c o) (steer "spec" "check" "-" #:in eight)])
  (check-equal? (length (regexp-match* #rx"error bad-start" o)) 5)
  (check-regexp-match #rx"\\(\\+3 more findings; add --full or --limit N\\)" o))
(let-values ([(c o) (steer "spec" "check" "-" "--full" #:in eight)])
  (check-equal? (length (regexp-match* #rx"error bad-start" o)) 8))
(let-values ([(c o) (steer "spec" "check" "-" "--limit" "2" #:in eight)])
  (check-equal? (length (regexp-match* #rx"error bad-start" o)) 2))

;; ---------------------------------------------------------------------------------------------
;; render

(let-values ([(c o) (steer "spec" "render" "-" #:in "the export shall  WRITE a header row .\nwhen idle, the tool shall exit.\n")])
  (check-equal? c 0 o)
  (check-equal? o "The export SHALL write a header row.\nWHEN idle, the tool SHALL exit.\n"))
;; the output is itself valid input
(let*-values ([(c o) (steer "spec" "render" "-" #:in good)]
              [(c2 o2) (steer "spec" "check" "-" #:in o)])
  (check-equal? c2 0 o2))
;; lines that do not parse are reported; the rest still render
(let-values ([(c o) (steer "spec" "render" "-" #:in "The export SHALL write x.\nExports are fast.\n")])
  (check-equal? c 1 o)
  (check-regexp-match #rx"^The export SHALL write x\\.\n" o)
  (check-regexp-match #rx"error bad-start <stdin>:2:1" o))

;; ---------------------------------------------------------------------------------------------
;; --json (note 03 protocol)

(let-values ([(c o) (steer "--json" "spec" "check" "-" #:in mixed)])
  (define j (string->jsexpr o))
  (check-equal? c 1)
  (check-equal? (hash-ref j 'tool) "spec")
  (check-false (hash-ref j 'ok))
  (define d (hash-ref j 'data))
  (check-equal? (list (hash-ref d 'ok) (hash-ref d 'refused) (hash-ref d 'errors)) '(1 3 3))
  (check-equal? (hash-ref (car (hash-ref d 'criteria)) 'canonical) "The export SHALL write a header row.")
  (check-equal? (map (λ (f) (hash-ref f 'kind)) (hash-ref j 'findings)) '("weak-modal" "bad-start" "hedge")))
(let-values ([(c o) (steer "--json" "spec" "render" "-" #:in good)])
  (define j (string->jsexpr o))
  (check-equal? c 0)
  (check-equal? (map (λ (x) (hash-ref x 'shape)) (hash-ref (hash-ref j 'data) 'criteria)) '("ubiquitous" "event")))

;; ---------------------------------------------------------------------------------------------
;; usage errors: exit 2 with a hint

(expect 2 #rx"needs 2 positional arguments" (steer "spec"))
(expect 2 #rx"unknown spec action lint.*check \\| render" (steer "spec" "lint" "-"))
(expect 2 #rx"no such file nope.txt.*`-`" (steer "spec" "check" "nope.txt"))
(expect 2 #rx"unknown flag --strict" (steer "spec" "check" "-" "--strict" #:in good))
(expect 2 #rx"at most 2 positional" (steer "spec" "check" "a" "b" "c"))

;; help
(expect 0 #rx"steer spec check\\|render FILE\\|-.*\n  The <system> SHALL <verb> <what>\\..*WHEN <trigger>.*IF <trigger>, THEN.*base form: write, return, refuse"
        (steer "help" "spec"))
(expect 0 #rx"spec +acceptance criteria" (steer "help"))

;; ---------------------------------------------------------------------------------------------
;; failures reach the E1 logger under their own class (measured in M3)

(let-values ([(c o) (steer "init")]) (check-equal? c 0 o))
(call-with-values (λ () (steer "spec" "check" "-" #:in mixed)) void)
(let ([j (let-values ([(c o) (steer "--json" "failures" "--class" "criteria-error")]) (string->jsexpr o))])
  (check-equal? (hash-ref (hash-ref j 'data) 'total) 3 "weak-modal, bad-start and hedge are logged as criteria-error"))

(delete-directory/files dir)
