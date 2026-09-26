#lang racket/base
;; End-to-end: drive the real CLI as a process, the way an agent does, and check output and exit
;; codes. Uses `racket steer/main.rkt` by default; set STEER_BIN to test a built executable.
(require rackunit racket/port racket/file racket/string racket/runtime-path json)

(define-runtime-path main-rkt "../steer/main.rkt")
(define dir (make-temporary-directory "steer-cli~a"))

(define (steer #:in [input ""] . args)
  (define bin (getenv "STEER_BIN"))
  (define-values (p out in err)
    (parameterize ([current-directory dir])
      (if bin
          (apply subprocess #f #f 'stdout bin args)
          (apply subprocess #f #f 'stdout (find-executable-path "racket") (path->string main-rkt) args))))
  (write-string input in)
  (close-output-port in)
  (define text (port->string out))
  (subprocess-wait p)
  (close-input-port out)
  (values (subprocess-status p) text))

(define-syntax-rule (expect code rx call)
  (let-values ([(c t) call])
    (check-equal? c code (format "exit code for ~a\n~a" 'call t))
    (check-regexp-match rx t)))

(expect 0 #rx"^steer [0-9]+\\.[0-9]+\\.[0-9]+\n$" (steer "--version"))
(expect 2 #rx"no .steer directory" (steer "list"))
(expect 0 #rx"created" (steer "init"))
(expect 0 #rx"added T1 \\[ready\\]" (steer "add" "First" "--check" "test -f one.txt"))
(expect 2 #rx"did you mean --after" (steer "add" "Second" "--afer" "T1"))
(expect 0 #rx"added T2 \\[blocked\\]" (steer "add" "Second" "--after" "T1" "--check" "true"))
(expect 1 #rx"plan rejected: 2 problems" (steer "import" "-" #:in "(task \"A\" #:id a #:after (b))\n(task \"B\" #:id b #:after (a) #:chek \"x\")"))
(expect 0 #rx"imported 2 tasks" (steer "import" "-" #:in "(task \"C\" #:id c #:after (T2) #:check \"true\")\n(task \"D\" #:after (c))"))
(expect 0 #rx"critical path \\(4\\): T1 → T2 → T3 → T4" (steer "graph"))
(expect 0 #rx"claimed T1" (steer "next" "--claim"))
(expect 1 #rx"blocked" (steer "claim" "T2"))
(expect 2 #rx"missing: --next" (steer "checkpoint" "T1" "--did" "half"))
(expect 0 #rx"checkpoint #[0-9]+ saved" (steer "checkpoint" "T1" "--did" "wrote half" "--next" "write the rest"))
(expect 1 #rx"not done: 1 of 1 check failed" (steer "done" "T1"))
(call-with-output-file (build-path dir "one.txt") void)
(expect 0 #rx"T1 done \\(1 check passed\\); now ready: T2" (steer "done" "T1"))
(expect 1 #rx"has no acceptance check" (steer "done" "T4"))
(expect 1 #rx"claimed by claude, not other" (let () (steer "claim" "T2") (steer "--agent" "other" "done" "T2")))
(expect 0 #rx"YOUR ACTIVE TASK\nT2" (steer "resume"))
(expect 0 #rx"events since #0" (steer "since" "0"))
(expect 2 #rx"unknown command lsit → did you mean list\\?" (steer "lsit"))

;; JSON protocol: stable top-level fields
(let-values ([(c t) (steer "--json" "show" "T2")])
  (define j (string->jsexpr t))
  (check-equal? c 0)
  (check-equal? (sort (map symbol->string (hash-keys j)) string<?) '("data" "elapsed_ms" "findings" "next" "ok" "tool" "truncated"))
  (check-equal? (hash-ref (hash-ref (hash-ref j 'data) 'task) 'derived) "active"))

;; hooks
(call-with-output-file (build-path dir "bad.rkt") (λ (o) (void (write-string "#lang racket/base\n(define (f x)\n  x\n\n(define y 1)\n" o))))
(expect 2 #rx"missing \\) at the end of line 3 \\(verified" (steer "hook" "post-edit" #:in "{\"tool_input\":{\"file_path\":\"bad.rkt\"}}"))
(expect 0 #rx"fixed bad.rkt: inserted \\) at line 3" (steer "syntax" "--fix" "bad.rkt"))
(expect 0 #rx"^ok bad.rkt \\(2 forms\\)" (steer "syntax" "bad.rkt"))
(expect 0 #rx"^$" (steer "hook" "post-edit" #:in "{\"tool_input\":{\"file_path\":\"one.txt\"}}"))
(expect 0 #rx"steer resume" (steer "hook" "session-start"))

;; skills are embedded and installable
(expect 0 #rx"steer-tasks: installed" (steer "skills" "install"))
(check-true (file-exists? (build-path dir ".claude" "skills" "steer-tasks" "SKILL.md")))
(expect 0 #rx"steer-tasks: up to date" (steer "skills" "install"))

(delete-directory/files dir)
