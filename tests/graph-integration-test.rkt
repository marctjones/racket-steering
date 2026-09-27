#lang racket/base
;; T70 (XL3 milestone integration): the whole code-graph engine, end to end through the CLI, on a
;; Racket, a Python and a C# fixture project (no dotnet SDK anywhere in this file; python3 only) -
;; one generic engine (graph.rkt + the six-function lang contract) drives all three, not three
;; separate code paths. Each language's block runs the SAME sequence: `rules check` with layers,
;; `rules entries` (heuristic + an explicit marker), `rules dead` finds a planted dead symbol,
;; `# steer: entry` above it clears the finding, `rules reach` names the test that covers a symbol,
;; and `api snapshot --entries` -> a signature change -> `api diff --entries` reports the break AND
;; the mismatched caller.
(require rackunit racket/list racket/string racket/file racket/port racket/runtime-path json)

(define-runtime-path main-rkt "../steer/main.rkt")
(define (steer d . args)
  (define-values (p out in err)
    (parameterize ([current-directory d])
      (apply subprocess #f #f 'stdout (find-executable-path "racket") (path->string main-rkt) args)))
  (close-output-port in)
  (define text (port->string out))
  (subprocess-wait p) (close-input-port out)
  (values (subprocess-status p) text))
(define (write-file! d rel text)
  (define p (build-path d rel))
  (make-directory* (let-values ([(dd _n _x) (split-path p)]) dd))
  (call-with-output-file p #:exists 'truncate (λ (o) (void (write-string text o)))))

;; ===============================================================================================
;; Python

(let ()
  (define d (make-temporary-directory "steer-integ-py~a"))
  (write-file! d "app/core.py"
    (string-append "# steer: entry\ndef main():\n    return helper(1, 2)\n\n"
                   "def helper(x, y):\n    return x + y\n\n"
                   "def unused_dead(x):\n    return x\n"))
  (write-file! d "app/db.py" "def query():\n    return 1\n")
  (write-file! d "test_core.py" "from app.core import main\n\ndef test_main():\n    assert main() == 3\n")
  (write-file! d ".steer/rules.dl"
    (string-append "%layer app app/**\n%layer db app/db.py\n"
                   "violation(A, B, \"app must not reach db\") :- reach(A, B), layer(A, \"app\"), layer(B, \"db\").\n"))
  (let-values ([(c o) (steer d "init")]) (check-equal? c 0 o))

  ;; rules check with layers - no violation planted, so it passes
  (let-values ([(c o) (steer d "rules" "check")])
    (check-equal? c 0 o)
    (check-regexp-match #px"0 violation" o))

  ;; rules entries: the explicit marker AND a heuristic (test_main, via test_name+test_module)
  (let-values ([(c o) (steer d "rules" "entries")])
    (check-equal? c 0 o)
    (check-regexp-match #px"app/core.py#main\\s+\\([^)]*marker" o "the explicit marker is listed")
    (check-regexp-match #px"test_core.py#test_main" o "a heuristic (test_name+test_module) entry is listed too"))

  ;; rules dead: finds the planted dead symbol, and ONLY it (helper is called, main/test_main are entries)
  (let-values ([(c o) (steer d "rules" "dead")])
    (check-equal? c 1 o)
    (check-regexp-match #px"app/core.py#unused_dead" o)
    (check-false (regexp-match? #px"#helper:" o) "helper is called from main: not dead")
    (check-false (regexp-match? #px"#main:" o) "main is an entry: not dead"))

  ;; marking it an entry clears the finding
  (write-file! d "app/core.py"
    (string-append "# steer: entry\ndef main():\n    return helper(1, 2)\n\n"
                   "def helper(x, y):\n    return x + y\n\n"
                   "# steer: entry\ndef unused_dead(x):\n    return x\n"))
  (let-values ([(c o) (steer d "rules" "dead")])
    (check-equal? c 0 o)
    (check-false (regexp-match? #px"unused_dead" o) "the marker cleared the finding"))
  ;; restore for the next checks
  (write-file! d "app/core.py"
    (string-append "# steer: entry\ndef main():\n    return helper(1, 2)\n\n"
                   "def helper(x, y):\n    return x + y\n\n"
                   "def unused_dead(x):\n    return x\n"))

  ;; rules reach: the test that covers `helper`
  (let-values ([(c o) (steer d "rules" "reach" "app/core.py#helper")])
    (check-equal? c 0 o)
    (check-regexp-match #px"reached from test_core.py#test_main" o "the test covering this symbol is named")
    (check-regexp-match #px"reached from app/core.py#main" o))

  ;; api --entries: only ENTRY points get a tracked shape, so `helper` needs its own marker too
  ;; (a real design consequence, not a workaround: --entries snapshots entry points, not every
  ;; internal helper) - snapshot, break the signature, diff reports the break AND the caller.
  (write-file! d "app/core.py"
    (string-append "# steer: entry\ndef main():\n    return helper(1, 2)\n\n"
                   "# steer: entry\ndef helper(x, y):\n    return x + y\n\n"
                   "def unused_dead(x):\n    return x\n"))
  (let-values ([(c o) (steer d "api" "snapshot" "--entries")]) (check-equal? c 0 o))
  (write-file! d "app/core.py"
    (string-append "# steer: entry\ndef main():\n    return helper(1, 2)\n\n"
                   "# steer: entry\ndef helper(x):\n    return x\n\n"
                   "def unused_dead(x):\n    return x\n"))
  (let-values ([(c o) (steer d "api" "diff" "--entries")])
    (check-equal? c 1 o)
    (check-regexp-match #px"param-removed" o)
    (check-regexp-match #px"app/core.py#helper: called with 2 arguments from app/core.py#main" o
                         "the mismatched caller is named, not just the symbol whose shape changed"))
  (delete-directory/files d))

;; ===============================================================================================
;; Racket

(let ()
  (define d (make-temporary-directory "steer-integ-rkt~a"))
  (write-file! d "app/core.rkt"
    (string-append "#lang racket/base\n(provide main)\n"
                   ";; steer: entry\n(define (main) (helper 1 2))\n"
                   "(define (helper x y) (+ x y))\n"
                   "(define (unused-dead x) x)\n"))
  (write-file! d "app/db.rkt" "#lang racket/base\n(provide q)\n(define (q) 1)\n")
  (write-file! d "core-test.rkt"
    (string-append "#lang racket/base\n(require rackunit \"app/core.rkt\")\n"
                   "(check-equal? (main) 3)\n"))
  (write-file! d ".steer/rules.dl"
    (string-append "%layer app app/**\n%layer db app/db.rkt\n"
                   "violation(A, B, \"app must not reach db\") :- reach(A, B), layer(A, \"app\"), layer(B, \"db\").\n"))
  (let-values ([(c o) (steer d "init")]) (check-equal? c 0 o))

  (let-values ([(c o) (steer d "rules" "check")])
    (check-equal? c 0 o)
    (check-regexp-match #px"0 violation" o))

  (let-values ([(c o) (steer d "rules" "entries")])
    (check-equal? c 0 o)
    (check-regexp-match #px"app/core.rkt#main\\s+\\([^)]*marker" o))

  (let-values ([(c o) (steer d "rules" "dead")])
    (check-equal? c 1 o)
    (check-regexp-match #px"app/core.rkt#unused-dead" o)
    (check-false (regexp-match? #px"#helper:" o)))

  (write-file! d "app/core.rkt"
    (string-append "#lang racket/base\n(provide main)\n"
                   ";; steer: entry\n(define (main) (helper 1 2))\n"
                   "(define (helper x y) (+ x y))\n"
                   ";; steer: entry\n(define (unused-dead x) x)\n"))
  (let-values ([(c o) (steer d "rules" "dead")])
    (check-equal? c 0 o)
    (check-false (regexp-match? #px"unused-dead" o)))
  (write-file! d "app/core.rkt"
    (string-append "#lang racket/base\n(provide main)\n"
                   ";; steer: entry\n(define (main) (helper 1 2))\n"
                   "(define (helper x y) (+ x y))\n"
                   "(define (unused-dead x) x)\n"))

  (let-values ([(c o) (steer d "rules" "reach" "app/core.rkt#helper")])
    (check-equal? c 0 o)
    (check-regexp-match #px"reached from app/core.rkt#main" o))

  (write-file! d "app/core.rkt"
    (string-append "#lang racket/base\n(provide main)\n"
                   ";; steer: entry\n(define (main) (helper 1 2))\n"
                   ";; steer: entry\n(define (helper x y) (+ x y))\n"
                   "(define (unused-dead x) x)\n"))
  (let-values ([(c o) (steer d "api" "snapshot" "--entries")]) (check-equal? c 0 o))
  (write-file! d "app/core.rkt"
    (string-append "#lang racket/base\n(provide main)\n"
                   ";; steer: entry\n(define (main) (helper 1 2))\n"
                   ";; steer: entry\n(define (helper x) x)\n"
                   "(define (unused-dead x) x)\n"))
  (let-values ([(c o) (steer d "api" "diff" "--entries")])
    (check-equal? c 1 o)
    (check-regexp-match #px"param-removed" o)
    (check-regexp-match #px"app/core.rkt#helper: called with 2 arguments from app/core.rkt#main" o))
  (delete-directory/files d))

;; ===============================================================================================
;; C# (no dotnet SDK needed anywhere in this analysis)

(let ()
  (define d (make-temporary-directory "steer-integ-cs~a"))
  (write-file! d "src/App/Core.cs"
    (string-append "namespace App {\n  public class Core {\n"
                   "    // steer: entry\n    public static int Main() { return Helper(1, 2); }\n"
                   "    private static int Helper(int x, int y) { return x + y; }\n"
                   "    private static int UnusedDead(int x) { return x; }\n"
                   "  }\n}\n"))
  (write-file! d "src/Db/Store.cs" "namespace Db { public class Store { public static int Query() { return 1; } } }\n")
  (write-file! d ".steer/rules.dl"
    (string-append "%layer app src/App/**\n%layer db src/Db/**\n"
                   "violation(A, B, \"app must not reach db\") :- reach(A, B), layer(A, \"app\"), layer(B, \"db\").\n"))
  (let-values ([(c o) (steer d "init")]) (check-equal? c 0 o))

  (let-values ([(c o) (steer d "rules" "check")])
    (check-equal? c 0 o)
    (check-regexp-match #px"0 violation" o))

  (let-values ([(c o) (steer d "rules" "entries")])
    (check-equal? c 0 o)
    (check-regexp-match #px"src/App/Core.cs#Core.Main\\s+\\([^)]*marker" o))

  (let-values ([(c o) (steer d "rules" "dead")])
    (check-equal? c 1 o)
    (check-regexp-match #px"src/App/Core.cs#Core.UnusedDead" o)
    (check-false (regexp-match? #px"#Core.Helper:" o) "Helper is called from Main: not dead"))

  (write-file! d "src/App/Core.cs"
    (string-append "namespace App {\n  public class Core {\n"
                   "    // steer: entry\n    public static int Main() { return Helper(1, 2); }\n"
                   "    private static int Helper(int x, int y) { return x + y; }\n"
                   "    // steer: entry\n    private static int UnusedDead(int x) { return x; }\n"
                   "  }\n}\n"))
  (let-values ([(c o) (steer d "rules" "dead")])
    (check-equal? c 0 o)
    (check-false (regexp-match? #px"UnusedDead" o)))
  (write-file! d "src/App/Core.cs"
    (string-append "namespace App {\n  public class Core {\n"
                   "    // steer: entry\n    public static int Main() { return Helper(1, 2); }\n"
                   "    private static int Helper(int x, int y) { return x + y; }\n"
                   "    private static int UnusedDead(int x) { return x; }\n"
                   "  }\n}\n"))

  (let-values ([(c o) (steer d "rules" "reach" "src/App/Core.cs#Core.Helper")])
    (check-equal? c 0 o)
    (check-regexp-match #px"reached from src/App/Core.cs#Core.Main" o))

  (write-file! d "src/App/Core.cs"
    (string-append "namespace App {\n  public class Core {\n"
                   "    // steer: entry\n    public static int Main() { return Helper(1, 2); }\n"
                   "    // steer: entry\n    private static int Helper(int x, int y) { return x + y; }\n"
                   "    private static int UnusedDead(int x) { return x; }\n"
                   "  }\n}\n"))
  (let-values ([(c o) (steer d "api" "snapshot" "--entries")]) (check-equal? c 0 o))
  (write-file! d "src/App/Core.cs"
    (string-append "namespace App {\n  public class Core {\n"
                   "    // steer: entry\n    public static int Main() { return Helper(1, 2); }\n"
                   "    // steer: entry\n    private static int Helper(int x) { return x; }\n"
                   "    private static int UnusedDead(int x) { return x; }\n"
                   "  }\n}\n"))
  (let-values ([(c o) (steer d "api" "diff" "--entries")])
    (check-equal? c 1 o)
    (check-regexp-match #px"param-removed" o)
    (check-regexp-match #px"src/App/Core.cs#Core.Helper: called with 2 arguments from src/App/Core.cs#Core.Main" o))
  (delete-directory/files d))
