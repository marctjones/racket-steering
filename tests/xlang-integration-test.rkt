#lang racket/base
;; T53 (milestone XL1): an agent's session on a Python file and a C# file, end to end, through the
;; real CLI. C# needs no dotnet SDK at all (steer's own lexer/scanner does the work); the one place a
;; real `dotnet test` would appear is simulated with a script that emits the exact output shape a real
;; run has (tests/fixtures/test-output/dotnet-build-fail.txt), so "readable check failures" is proven
;; without requiring the SDK in CI. Python's own steps use python3, which CI images carry; each
;; assertion that needs pytest specifically is skipped, loudly, if it is missing.
(require rackunit racket/list racket/string racket/file racket/port racket/system racket/runtime-path json)

(define-runtime-path main-rkt "../steer/main.rkt")
(define (have? exe) (and (find-executable-path exe) #t))
(define python? (have? "python3"))
(define pytest?
  (and python?
       (with-handlers ([exn:fail? (λ (e) #f)])
         (parameterize ([current-output-port (open-output-nowhere)] [current-error-port (open-output-nowhere)])
           (system* (find-executable-path "python3") "-c" "import pytest")))))

(define dir (make-temporary-directory "steer-xlang~a"))
(define (steer #:in [input ""] . args)
  (define-values (p out in err)
    (parameterize ([current-directory dir])
      (apply subprocess #f #f 'stdout (find-executable-path "racket") (path->string main-rkt) args)))
  (write-string input in) (close-output-port in)
  (define text (port->string out))
  (subprocess-wait p) (close-input-port out)
  (values (subprocess-status p) text))
(define (put! name text) (call-with-output-file (build-path dir name) #:exists 'truncate (λ (o) (void (write-string text o)))))
(define (hook file) (steer "hook" "post-edit" #:in (format "{\"tool_input\":{\"file_path\":~s}}" file)))

(let-values ([(c o) (steer "init")]) (check-equal? c 0 o))

;; ---------------------------------------------------------------------------------------------
;; Part A: Python — init, import with a Class.method anchor, body edit → stale, broken syntax → hook
;; exit 2 with a located finding, failing test → done shows the failing test with file:line

(put! "shapes.py" "class Calc:\n    def add(self, a, b):\n        return a + b\n\n    def sub(self, a, b):\n        return a - b\n")
(put! "test_calc.py" "from shapes import Calc\n\ndef test_add():\n    assert Calc().add(2, 3) == 5\n\ndef test_sub():\n    assert Calc().sub(5, 2) == 3\n")

(let-values ([(c o) (steer "import" "-" #:in "(task \"Calc.add\" #:anchor \"shapes.py#Calc.add\" #:check \"python3 -m pytest test_calc.py\")\n")])
  (check-equal? c 0 o)
  (check-false (regexp-match? #rx"anchor-unresolved|anchor-pending" o) "the anchor resolves: the class and method exist"))
(let-values ([(c o) (steer "show" "T1")])
  (check-regexp-match #rx"anchor: shapes.py#Calc.add L2-3 ok" o "an exact Python anchor, not a heuristic guess"))

;; body edit → stale
(put! "shapes.py" "class Calc:\n    def add(self, a, b):\n        return a + b + 1\n\n    def sub(self, a, b):\n        return a - b\n")
(let-values ([(c o) (steer "stale")])
  (check-equal? c 1 o)
  (check-regexp-match #rx"warning stale-anchor T1: shapes.py#Calc.add.*CHANGED since planned" o))
(let-values ([(c o) (steer "refresh" "T1")]) (check-equal? c 0 o))
(let-values ([(c o) (steer "stale")]) (check-equal? c 0 "refreshed, no longer stale"))

;; a comment/whitespace-only edit is not drift
(put! "shapes.py" "class Calc:\n    def add(self, a, b):\n        # sum\n        return a + b + 1\n\n    def sub(self, a, b):\n        return a - b\n")
(let-values ([(c o) (steer "stale")]) (check-equal? c 0 "a comment does not make the anchor stale"))

;; broken syntax → hook exit 2, located, and steer syntax --fix repairs it (the +1 logic bug stays,
;; so the "failing test" step below has something real to fail on)
(put! "shapes.py" "class Calc:\n    def add(self, a, b):\n        return (a + b + 1\n\n    def sub(self, a, b):\n        return a - b\n")
(let-values ([(c o) (hook "shapes.py")])
  (check-equal? c 2 o)
  (check-regexp-match #rx"steer syntax found structural problems.*shapes.py:3:" o))
(let-values ([(c o) (steer "syntax" "--fix" "shapes.py")]) (check-equal? c 0 o))
(let-values ([(c o) (hook "shapes.py")]) (check-equal? (list c o) '(0 "") "fixed: the hook is silent again"))

;; failing test → done shows the failing test with file:line
(cond
  [(not pytest?) (eprintf "xlang-integration-test: SKIPPED the pytest half of Part A: pytest not importable\n")]
  [else
   (let-values ([(c o) (steer "claim" "T1")]) (check-equal? c 0 o))
   (let-values ([(c o) (steer "done" "T1")])
     (check-equal? c 1 o)
     (check-regexp-match #rx"error check-failed T1: `python3 -m pytest test_calc.py` exited 1 after" o)
     (check-regexp-match #rx"error test-failed test_calc.py:4: test_calc.py::test_add: assert 6 == 5" o "the failing test, its file:line, and the assertion"))
   ;; fix it and it passes
   (put! "shapes.py" "class Calc:\n    def add(self, a, b):\n        return a + b\n\n    def sub(self, a, b):\n        return a - b\n")
   (let-values ([(c o) (steer "done" "T1")])
     (check-equal? c 0 o)
     (check-regexp-match #rx"T1 done \\(1 check passed\\)" o))])

;; ---------------------------------------------------------------------------------------------
;; Part B: C# — no dotnet SDK needed anywhere in this part. init/import with a Class.Method anchor,
;; body edit → stale, broken braces → hook exit 2 with a located, verified repair, and a simulated
;; `dotnet test` failure (the exact shape a real run has) parsed into a readable finding by `done`.

(put! "Calc.cs" "namespace Demo\n{\n    public class Calc\n    {\n        public int Add(int a, int b)\n        {\n            return a + b;\n        }\n\n        public int Sub(int a, int b)\n        {\n            return a - b;\n        }\n    }\n}\n")

(let-values ([(c o) (steer "add" "Calc.Add" "--anchor" "Calc.cs#Calc.Add" "--check" "true")])
  (check-equal? c 0 o)
  (check-false (regexp-match? #rx"anchor-unresolved|anchor-pending" o)))
(let-values ([(c o) (steer "show" "T2")])
  (check-regexp-match #rx"anchor: Calc.cs#Calc.Add L5-8 ok" o "an exact C# anchor, computed with no dotnet involved"))

;; body edit → stale
(put! "Calc.cs" "namespace Demo\n{\n    public class Calc\n    {\n        public int Add(int a, int b)\n        {\n            return a + b + 1;\n        }\n\n        public int Sub(int a, int b)\n        {\n            return a - b;\n        }\n    }\n}\n")
(let-values ([(c o) (steer "stale")])
  (check-equal? c 1 o)
  (check-regexp-match #rx"warning stale-anchor T2: Calc.cs#Calc.Add.*CHANGED since planned" o))
(let-values ([(c o) (steer "refresh" "T2")]) (check-equal? c 0 o))

;; broken braces → hook exit 2, located, verified repair; steer syntax --fix repairs it
(put! "Calc.cs" "namespace Demo\n{\n    public class Calc\n    {\n        public int Add(int a, int b)\n        {\n            return a + b + 1;\n\n        public int Sub(int a, int b)\n        {\n            return a - b;\n        }\n    }\n}\n")
(let-values ([(c o) (hook "Calc.cs")])
  (check-equal? c 2 o)
  (check-regexp-match #rx"steer syntax found structural problems.*Calc.cs:[0-9]+:" o))
(let-values ([(c o) (steer "syntax" "--fix" "Calc.cs")]) (check-equal? c 0 o))
(let-values ([(c o) (hook "Calc.cs")]) (check-equal? (list c o) '(0 "")))
(let-values ([(c o) (steer "stale")]) (check-equal? c 0 "still recognised as the same Calc.Add after the repair"))

;; a simulated dotnet test failure, in the real shape a run produces, parsed by `done`
(define-runtime-path dotnet-fixture "fixtures/test-output/dotnet-build-fail.txt")
(put! "run-dotnet-test.sh" (format "#!/bin/sh\ncat <<'SH_EOF'\n~aSH_EOF\nexit 1\n" (file->string dotnet-fixture)))
(file-or-directory-permissions (build-path dir "run-dotnet-test.sh") #o755)
(let-values ([(c o) (steer "edit" "T2" "--add-check" "sh run-dotnet-test.sh")]) (check-equal? c 0 o))
(let-values ([(c o) (steer "claim" "T2")]) (check-equal? c 0 o))
(let-values ([(c o) (steer "done" "T2")])
  (check-equal? c 1 o)
  (check-regexp-match #rx"error check-failed T2: `sh run-dotnet-test.sh` exited 1 after .*: 1 error, 0 warnings" o
                       "the MSBuild-style summary in the headline, from steer's own parser, no dotnet run")
  (check-regexp-match #rx"error diag-failed .*GuardAgainstEmptyOrWhiteSpaceExtensions.cs:26: CS1026: \\) expected" o
                       "the real compiler error, located and with its code, parsed into a finding"))

;; ---------------------------------------------------------------------------------------------
;; both languages: an unresolved anchor (existing file, wrong name) is distinguished from a pending one

(let-values ([(c o) (steer "add" "t" "--anchor" "shapes.py#Calc.mul")])
  (check-regexp-match #rx"warning anchor-unresolved.*no definition of Calc.mul" o))
(let-values ([(c o) (steer "add" "t" "--anchor" "Calc.cs#Calc.Mul")])
  (check-regexp-match #rx"warning anchor-unresolved.*no definition of Calc.Mul" o))
(let-values ([(c o) (steer "add" "t" "--anchor" "brand-new.py#x")])
  (check-regexp-match #rx"info anchor-pending" o))

;; and the docs an agent would actually read on this project state the coverage plainly
(let-values ([(c o) (steer "help" "syntax")])
  (for ([lang '("Racket" "Python" "C#")]) (check-true (string-contains? o lang))))

(delete-directory/files dir)
