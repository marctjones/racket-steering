#lang racket/base
;; T47: parsing pytest, dotnet test and rackunit output (note 12: dotnet loses the first failures and clips
;; file:line; pytest reads well only with -q). Fixtures are sanitized real runs (tests/fixtures/test-output).
(require rackunit racket/list racket/string racket/file racket/port racket/runtime-path json
         "../steer/testparse.rkt" "../steer/checks.rkt")

(define-runtime-path fixdir "fixtures/test-output")
(define (fixture name) (file->string (build-path fixdir name)))

;; ---------------------------------------------------------------------------------------------
;; runner detection: from the command first, from the output's own signature otherwise

(check-equal? (detect-runner "python3 -m pytest -q" "") 'pytest)
(check-equal? (detect-runner "pytest tests/" "") 'pytest)
(check-equal? (detect-runner "py.test -k foo" "") 'pytest)
(check-equal? (detect-runner "dotnet test proj" "") 'dotnet)
(check-equal? (detect-runner "dotnet vstest x.dll" "") 'dotnet)
(check-equal? (detect-runner "raco test t.rkt" "") 'rackunit)
(check-equal? (detect-runner "make check" (fixture "pytest-plain.txt")) 'pytest "unknown command, recognised by the output")
(check-equal? (detect-runner "make check" (fixture "dotnet-fail.txt")) 'dotnet)
(check-equal? (detect-runner "make check" (fixture "rackunit-fail.txt")) 'rackunit)
(check-false (detect-runner "echo hi" "just some text") "an unknown runner is #f, not a guess")

;; ---------------------------------------------------------------------------------------------
;; pytest: id, file:line and the message, for every failure, in the order pytest reported them; both
;; `-q` (a short summary that this steer version already read acceptably) and plain output agree

(for ([f '("pytest-q.txt" "pytest-plain.txt")])
  (define p (parse-test-output "python3 -m pytest" (fixture f)))
  (check-equal? (hash-ref p 'runner) 'pytest f)
  (check-equal? (hash-ref p 'summary) "5 failed, 3 passed" f)
  (define fs (hash-ref p 'failures))
  (check-equal? (length fs) 5 f)
  (check-equal? (map (λ (x) (hash-ref x 'id)) fs)
                '("test_demo.py::test_wrong_value" "test_demo.py::test_message" "test_demo.py::TestGroup::test_in_class"
                  "test_demo.py::TestGroup::test_param[2-3]" "test_demo.py::test_error_in_setup")
                f)
  (check-equal? (map (λ (x) (hash-ref x 'line)) fs) '(14 23 28 32 36) f)
  (check-true (andmap (λ (x) (equal? (hash-ref x 'file) "test_demo.py")) fs))
  (check-regexp-match #rx"assert \\(3, 4\\) == \\(3, 5\\)" (hash-ref (car fs) 'message))
  (check-regexp-match #rx"strings differ" (hash-ref (cadr fs) 'message) "the assertion message, not just AssertionError")
  (check-regexp-match #rx"RuntimeError: boom" (hash-ref (list-ref fs 4) 'message) "an error outside an assert still gets a message"))

;; every message is on one line, short, and never empty
(for ([f (hash-ref (parse-test-output "pytest" (fixture "pytest-plain.txt")) 'failures)])
  (check-false (regexp-match? #rx"\n" (hash-ref f 'message)))
  (check-true (< (string-length (hash-ref f 'message)) 210))
  (check-true (> (string-length (hash-ref f 'message)) 0)))

;; a run with only passes: no failures, no crash
(check-equal? (hash-ref (parse-test-output "pytest" "3 passed in 0.01s\n") 'failures) '())

;; ---------------------------------------------------------------------------------------------
;; dotnet test: both failures recovered (the first is not lost), file:line from the stack trace, and the
;; message is the real assertion text, not clipped mid-sentence

(let ([p (parse-test-output "dotnet test proj" (fixture "dotnet-fail.txt"))])
  (check-equal? (hash-ref p 'runner) 'dotnet)
  (check-equal? (hash-ref p 'summary) "2 failed, 30 passed, 0 skipped")
  (define fs (hash-ref p 'failures))
  (check-equal? (length fs) 2 "both failures, including the first one a tail alone would have lost")
  (check-equal? (hash-ref (car fs) 'id) "GuardClauses.UnitTests.GuardAgainstNullOrEmpty.ThrowsCustomExceptionWhenSuppliedGivenEmptyStringSpan")
  (check-equal? (hash-ref (car fs) 'line) 62)
  (check-regexp-match #rx"GuardAgainstNullOrEmpty\\.cs$" (hash-ref (car fs) 'file))
  (check-regexp-match #rx"No exception was thrown" (hash-ref (car fs) 'message))
  (check-regexp-match #rx"typeof\\(System\\.Exception\\)" (hash-ref (car fs) 'message) "the Expected: line too, not clipped")
  (check-equal? (hash-ref (cadr fs) 'line) 55)
  (check-regexp-match #rx"typeof\\(System\\.ArgumentException\\)" (hash-ref (cadr fs) 'message)))

;; ---------------------------------------------------------------------------------------------
;; rackunit (`raco test`): the check name, file:line, and a readable message for equal/true/exn/within,
;; plus an ERROR block (an exception, not a failed assertion)

(let ([p (parse-test-output "raco test demo-test.rkt" (fixture "rackunit-fail.txt"))])
  (check-equal? (hash-ref p 'runner) 'rackunit)
  (check-equal? (hash-ref p 'summary) "5 of 7 failed")
  (define fs (hash-ref p 'failures))
  (check-equal? (length fs) 5)
  (check-equal? (hash-ref (first fs) 'line) 7)
  (check-regexp-match #rx"got 4, expected 5" (hash-ref (first fs) 'message))
  (check-regexp-match #rx"arithmetic is broken" (hash-ref (first fs) 'message) "the check's own message")
  (check-equal? (hash-ref (second fs) 'id) "a named case" "a named test-case is the id, not the bare check")
  (check-regexp-match #rx"failed" (hash-ref (second fs) 'message))
  (check-equal? (hash-ref (third fs) 'line) 11)
  (check-regexp-match #rx"No exception raised" (hash-ref (third fs) 'message))
  (check-equal? (hash-ref (fourth fs) 'line) 12)
  (check-regexp-match #rx"got 1.0, expected 1.5" (hash-ref (fourth fs) 'message))
  (check-equal? (hash-ref (fifth fs) 'id) "an error")
  (check-regexp-match #rx"boom: kaput 42" (hash-ref (fifth fs) 'message) "an ERROR block, not a FAILURE"))

;; ---------------------------------------------------------------------------------------------
;; annotate-result: the run-check shape plus the parse, output dropped

(let ([r (annotate-result (hasheq 'cmd "python3 -m pytest -q" 'ok #f 'exit 1 'secs 1.0 'tail "x" 'output (fixture "pytest-q.txt")))])
  (check-equal? (hash-ref r 'runner) 'pytest)
  (check-equal? (length (hash-ref r 'failures)) 5)
  (check-false (hash-ref r 'output #f) "the raw output is not carried forward")
  (check-equal? (hash-ref r 'ok) #f "the original fields survive"))

;; ---------------------------------------------------------------------------------------------
;; through `steer done`: at most 3 failing tests shown, located, with the summary in the headline

(define-runtime-path main-rkt "../steer/main.rkt")
(define dir (make-temporary-directory "steer-tp~a"))
(define (steer #:in [input ""] . args)
  (define-values (p out in err)
    (parameterize ([current-directory dir])
      (apply subprocess #f #f 'stdout (find-executable-path "racket") (path->string main-rkt) args)))
  (write-string input in) (close-output-port in)
  (define text (port->string out))
  (subprocess-wait p) (close-input-port out)
  (values (subprocess-status p) text))

(call-with-values (λ () (steer "init")) void)
(call-with-output-file (build-path dir "run.sh") #:exists 'truncate
  (λ (o) (void (write-string (format "#!/bin/sh
cat <<'EOF'
~aEOF
exit 1
" (fixture "pytest-plain.txt")) o))))
(file-or-directory-permissions (build-path dir "run.sh") #o755)
(call-with-values (λ () (steer "add" "t" "--check" "sh run.sh")) void)
(call-with-values (λ () (steer "claim" "T1")) void)
(let-values ([(c o) (steer "done" "T1")])
  (check-equal? c 1 o)
  (check-regexp-match #rx"error check-failed T1: `sh run.sh` exited 1 after [0-9.]+s: 5 failed, 3 passed" o "the summary in the headline")
  (check-equal? (length (regexp-match* #rx"error test-failed" o)) 3 "capped at 3")
  (check-regexp-match #rx"error test-failed test_demo.py:14: test_demo.py::test_wrong_value: assert" o)
  (check-regexp-match #rx"info test-failed-more T1: \\(\\+2 more failing tests; add --full or --limit N\\)" o))
(let-values ([(c o) (steer "done" "T1" "--full")])
  (check-equal? (length (regexp-match* #rx"error test-failed" o)) 5 "--full lifts the cap"))
(let-values ([(c o) (steer "--json" "done" "T1")])
  (define j (string->jsexpr o))
  (check-equal? c 1)
  (check-true (>= (length (filter (λ (f) (equal? (hash-ref f 'kind) "test-failed")) (hash-ref j 'findings))) 1)))

;; a check whose output nothing recognises falls back to the tail, exactly as before this task
(call-with-output-file (build-path dir "plain.sh") #:exists 'truncate (λ (o) (void (write-string "#!/bin/sh
echo custom failure line
exit 1
" o))))
(file-or-directory-permissions (build-path dir "plain.sh") #o755)
(call-with-values (λ () (steer "add" "t2" "--check" "sh plain.sh")) void)
(call-with-values (λ () (steer "claim" "T2")) void)
(let-values ([(c o) (steer "done" "T2")])
  (check-equal? c 1 o)
  (check-regexp-match #rx"custom failure line" o)
  (check-false (regexp-match? #rx"test-failed" o) "no structured findings when nothing was recognised"))

(delete-directory/files dir)

;; ---------------------------------------------------------------------------------------------
;; run-check keeps the output, and the tail still works for anything the parsers do not recognise

(let ([r (run-check "echo one; echo two" (find-system-path 'temp-dir) 5)])
  (check-equal? (hash-ref r 'output) "one\ntwo\n")
  (check-equal? (hash-ref r 'tail) "one\ntwo"))
