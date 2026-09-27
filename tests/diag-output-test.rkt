#lang racket/base
;; T48: MSBuild and Python traceback diagnostics for checks that never reached their tests (a build or
;; import error). Fixtures are sanitized real runs: a broken build of GuardClauses (two different C#
;; errors) and two real Python tracebacks.
(require rackunit racket/list racket/string racket/file racket/port racket/runtime-path json
         "../steer/testparse.rkt")

(define-runtime-path fixdir "fixtures/test-output")
(define (fixture name) (file->string (build-path fixdir name)))

;; ---------------------------------------------------------------------------------------------
;; MSBuild: `path(line,col): error CSnnnn: message [project]`, deduplicated (MSBuild repeats each
;; diagnostic once as it happens and once in the trailing per-project summary)

(let ([p (parse-diagnostics (fixture "dotnet-build-fail.txt"))])
  (check-equal? (hash-ref p 'runner) 'msbuild)
  (check-equal? (hash-ref p 'summary) "1 error, 0 warnings" "not 2: the diagnostic is printed twice in the real log")
  (define fs (hash-ref p 'failures))
  (check-equal? (length fs) 1)
  (check-equal? (hash-ref (car fs) 'id) "CS1026")
  (check-regexp-match #rx"GuardAgainstEmptyOrWhiteSpaceExtensions\\.cs$" (hash-ref (car fs) 'file))
  (check-equal? (hash-ref (car fs) 'line) 26)
  (check-equal? (hash-ref (car fs) 'message) ") expected" "the message text, without the trailing [project] noise"))

(let ([p (parse-diagnostics (fixture "dotnet-build-fail2.txt"))])
  (check-equal? (hash-ref p 'summary) "1 error, 2 warnings")
  (define fs (hash-ref p 'failures))
  (check-equal? (map (λ (f) (hash-ref f 'id)) fs) '("CS0103" "CS8603" "CS8777") "one error then its two warnings, in file order")
  (check-equal? (hash-ref (car fs) 'line) 98)
  (check-regexp-match #rx"'GuardXyz' does not exist" (hash-ref (car fs) 'message))
  (check-equal? (hash-ref (cadr fs) 'col) 16 "column, when MSBuild gives one"))

;; a diagnostic line without a project suffix, and with a column, still parses
(check-equal? (map (λ (f) (list (hash-ref f 'file) (hash-ref f 'line) (hash-ref f 'col) (hash-ref f 'id)))
                   (hash-ref (parse-diagnostics "a/b.cs(3,5): error CS0001: bad thing\n") 'failures))
              '(("a/b.cs" 3 5 "CS0001")))
(check-equal? (hash-ref (car (hash-ref (parse-diagnostics "a/b.cs(3): error CS0002: no column\n") 'failures)) 'col) #f)
(check-equal? (parse-diagnostics "Build succeeded.\n    0 Warning(s)\n    0 Error(s)\n") (hasheq 'runner #f 'failures '() 'summary #f))

;; ---------------------------------------------------------------------------------------------
;; Python tracebacks: the exception type as the id, the *last* frame (where it actually happened,
;; not where it was first called from), the exact message text

(let ([p (parse-diagnostics (fixture "py-traceback.txt"))])
  (check-equal? (hash-ref p 'runner) 'python-traceback)
  (check-equal? (hash-ref p 'summary) "1 unhandled exception")
  (define fs (hash-ref p 'failures))
  (check-equal? (length fs) 1)
  (check-equal? (hash-ref (car fs) 'id) "ZeroDivisionError")
  (check-equal? (hash-ref (car fs) 'file) "/work/helper.py" "the frame where the error happened, not main_demo.py which just called it")
  (check-equal? (hash-ref (car fs) 'line) 2)
  (check-equal? (hash-ref (car fs) 'message) "division by zero"))

(let ([p (parse-diagnostics (fixture "py-import-error.txt"))])
  (check-equal? (hash-ref p 'summary) "1 unhandled exception")
  (define f (car (hash-ref p 'failures)))
  (check-equal? (hash-ref f 'id) "ModuleNotFoundError")
  (check-equal? (hash-ref f 'file) "/work/bad_import.py")
  (check-equal? (hash-ref f 'line) 1)
  (check-regexp-match #rx"No module named 'nonexistent_module_xyz'" (hash-ref f 'message)))

;; chained exceptions: two Traceback blocks, each counted, each pointing at its own last frame
(define chained #<<CHAINED
Traceback (most recent call last):
  File "/work/a.py", line 2, in inner
    raise ValueError("bad")
ValueError: bad

The above exception was the direct cause of the following exception:

Traceback (most recent call last):
  File "/work/a.py", line 5, in outer
    inner()
RuntimeError: outer failed
CHAINED
  )
(let ([p (parse-diagnostics chained)])
  (check-equal? (hash-ref p 'summary) "2 unhandled exceptions")
  (check-equal? (map (λ (f) (hash-ref f 'id)) (hash-ref p 'failures)) '("ValueError" "RuntimeError"))
  (check-equal? (map (λ (f) (hash-ref f 'line)) (hash-ref p 'failures)) '(2 5)))

;; a message that happens to look like "Word: text" is not mistaken for a traceback
(check-equal? (parse-diagnostics "Note: something happened\n") (hasheq 'runner #f 'failures '() 'summary #f))
;; something recognisable as neither: no crash, nothing found
(check-equal? (parse-diagnostics "some random output\nwith no structure\n") (hasheq 'runner #f 'failures '() 'summary #f))

;; ---------------------------------------------------------------------------------------------
;; through `steer done`: diagnostics only when parse-test-output found nothing (never shadowing a
;; real test result), a distinct finding kind, capped, and it reaches the E1 failure log under its
;; own class so a build-vs-test-failure distinction survives to note 04's taxonomy

(define-runtime-path main-rkt "../steer/main.rkt")
(define dir (make-temporary-directory "steer-diag~a"))
(define (steer #:in [input ""] . args)
  (define-values (p out in err)
    (parameterize ([current-directory dir])
      (apply subprocess #f #f 'stdout (find-executable-path "racket") (path->string main-rkt) args)))
  (write-string input in) (close-output-port in)
  (define text (port->string out))
  (subprocess-wait p) (close-input-port out)
  (values (subprocess-status p) text))
(define (script! name text)
  (call-with-output-file (build-path dir name) #:exists 'truncate (λ (o) (void (write-string (format "#!/bin/sh\ncat <<'SH_EOF'\n~aSH_EOF\nexit 1\n" text) o))))
  (file-or-directory-permissions (build-path dir name) #o755))

(call-with-values (λ () (steer "init")) void)
(script! "build.sh" (fixture "dotnet-build-fail2.txt"))
(call-with-values (λ () (steer "add" "t" "--check" "sh build.sh")) void)
(call-with-values (λ () (steer "claim" "T1")) void)
(let-values ([(c o) (steer "done" "T1")])
  (check-equal? c 1 o)
  (check-regexp-match #rx"error check-failed T1: `sh build.sh` exited 1 after [0-9.]+s: 1 error, 2 warnings" o)
  (check-equal? (length (regexp-match* #rx"error diag-failed" o)) 3)
  (check-regexp-match #rx"error diag-failed .*GuardAgainstNullExtensions\\.cs:98: CS0103: The name 'GuardXyz'" o)
  (check-false (regexp-match? #rx"test-failed" o) "a build failure is its own kind, not disguised as a test failure"))

(script! "run.py.sh" (fixture "py-traceback.txt"))
(call-with-values (λ () (steer "add" "t2" "--check" "sh run.py.sh")) void)
(call-with-values (λ () (steer "claim" "T2")) void)
(let-values ([(c o) (steer "done" "T2")])
  (check-equal? c 1)
  (check-regexp-match #rx"error diag-failed /work/helper\\.py:2: ZeroDivisionError: division by zero" o))

(let ([j (let-values ([(c o) (steer "--json" "failures" "--class" "build-failed")]) (string->jsexpr o))])
  (check-equal? (hash-ref (hash-ref j 'data) 'total) 4 "3 diag-failed on T1 + 1 on T2"))
(let ([j (let-values ([(c o) (steer "--json" "failures" "--class" "check-failed")]) (string->jsexpr o))])
  (check-equal? (hash-ref (hash-ref j 'data) 'total) 2 "the two check-failed headlines, not reclassified"))

(delete-directory/files dir)
