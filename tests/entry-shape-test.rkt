#lang racket/base
;; T67: entry shapes in the API lock, generic across languages. Static shapes for every entry symbol
;; read through the SAME graph-ir `shape` field every extractor already fills (real Racket formals,
;; py-extract's ast.unparse-style signature, cs-extract's real paramtypes) - lock v2, read-lock
;; accepts v1/v2 and refuses a cross-route diff, and api-diff reuses its existing kinds plus
;; entry-removed/entry-demoted/arity-mismatch. Verified on all three fixture languages with one
;; breaking and one compatible change each.
(require rackunit racket/list racket/string racket/file racket/port racket/runtime-path
         "../steer/api.rkt" "../steer/graph.rkt" "../steer/entries.rkt")

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

;; ---------------------------------------------------------------------------------------------
;; entry-shapes: direct - shape text comes from the graph, not a re-derived describe pass

(define dir (make-temporary-directory "steer-eshape~a"))
(write-file! dir "m.py" "# steer: entry\ndef area(shape, unit=\"cm\"):\n    return 1\n")
(define shapes1 (entry-shapes dir))
(check-equal? (hash-count shapes1) 1)
(define e1 (hash-ref shapes1 "m.py#area"))
(check-equal? (car e1) "python")
(check-true (and (member "marker" (cadr e1)) #t))
(check-equal? (caddr e1) "def area(shape, unit='cm')")
(check-equal? (cadddr e1) "function")
(delete-directory/files dir)

;; ---------------------------------------------------------------------------------------------
;; lock v2 round-trips, and read-lock/read-lock-v2 each refuse the OTHER's version cleanly

(define dir2 (make-temporary-directory "steer-eshape~a"))
(write-file! dir2 "m.py" "# steer: entry\ndef f(x):\n    return x\n")
(make-directory* (build-path dir2 ".steer"))
(write-lock-v2! dir2 (entry-shapes dir2))
(check-false (read-lock dir2) "a v2 lock is never misread as v1 data")
(define reread (read-lock-v2 dir2))
(check-true (hash? reread))
(check-equal? (caddr (hash-ref reread "m.py#f")) "def f(x)")
(delete-directory/files dir2)

(define dir3 (make-temporary-directory "steer-eshape~a"))
(write-file! dir3 "m.rkt" "#lang racket/base\n(provide f)\n(define (f x) x)\n")
(make-directory* (build-path dir3 ".steer"))
(write-lock! dir3 (hash "m.rkt" (list (list 'f 'procedure 1 '() #f))))
(check-false (read-lock-v2 dir3) "a v1 lock is never misread as v2 data")
(check-true (hash? (read-lock dir3)))
(delete-directory/files dir3)

;; ---------------------------------------------------------------------------------------------
;; one breaking and one compatible change, for each of the three fixture languages, via the real CLI

;; `expect-compatible-finding?`: C# has no per-language default-value tracking in its shape (T50's
;; own param-type-text deliberately drops a parameter's default, for anchor purposes; extending that
;; is out of scope here - recorded via `steer note T67`), so an added C# parameter can never be told
;; apart from a genuinely breaking one and IS classified breaking, correctly and conservatively; its
;; "compatible" case below is a body-only change (the signature does not move at all) instead.
(define (run-lang-case lang fixture-text breaking-text compatible-text entry-name breaking-kind #:expect-compatible-finding? [expect-compatible-finding? #t])
  (define d (make-temporary-directory "steer-eshape~a"))
  (write-file! d (format "m.~a" lang) fixture-text)
  (let-values ([(c o) (steer d "init")]) (check-equal? c 0 o))
  (let-values ([(c o) (steer d "api" "snapshot" "--entries")]) (check-equal? c 0 (format "~a: snapshot\n~a" lang o)))
  ;; compatible change first
  (write-file! d (format "m.~a" lang) compatible-text)
  (let-values ([(c o) (steer d "api" "diff" "--entries")])
    (check-equal? c 0 (format "~a: a compatible change does not fail the diff\n~a" lang o))
    (when expect-compatible-finding? (check-regexp-match #rx"compatible" o)))
  ;; restore, re-snapshot, then the breaking change
  (write-file! d (format "m.~a" lang) fixture-text)
  (let-values ([(c o) (steer d "api" "snapshot" "--entries")]) (check-equal? c 0 o))
  (write-file! d (format "m.~a" lang) breaking-text)
  (let-values ([(c o) (steer d "api" "diff" "--entries")])
    (check-equal? c 1 (format "~a: a breaking change fails the diff\n~a" lang o))
    (check-regexp-match (regexp (format "~a" breaking-kind)) o)
    (check-regexp-match (regexp (regexp-quote entry-name)) o))
  (delete-directory/files d))

;; Python
(run-lang-case "py"
  "# steer: entry\ndef area(x, y):\n    return x + y\n"
  "# steer: entry\ndef area(x):\n    return x\n"                     ; breaking: param removed
  "# steer: entry\ndef area(x, y, z=0):\n    return x + y + z\n"      ; compatible: optional param added
  "m.py#area" "param-removed")

;; C#: the "compatible" case is body-only (a Console.WriteLine added, signature untouched) - see the
;; #:expect-compatible-finding? note above for why this language can't demonstrate an ADDED optional
;; parameter the way Python/Racket do.
(run-lang-case "cs"
  "namespace N { public class Program { // steer: entry\npublic static int Area(int x, int y) { return x + y; } } }\n"
  "namespace N { public class Program { // steer: entry\npublic static int Area(int x) { return x; } } }\n"
  "using System;\nnamespace N { public class Program { // steer: entry\npublic static int Area(int x, int y) { Console.WriteLine(x); return x + y; } } }\n"
  "m.cs#Program.Area" "param-removed" #:expect-compatible-finding? #f)

;; Racket
(run-lang-case "rkt"
  "#lang racket/base\n;; steer: entry\n(define (area x y) (+ x y))\n"
  "#lang racket/base\n;; steer: entry\n(define (area x) x)\n"
  "#lang racket/base\n;; steer: entry\n(define (area x y [z 0]) (+ x y z))\n"
  "m.rkt#area" "param-removed")

;; ---------------------------------------------------------------------------------------------
;; entry-removed vs entry-demoted, and arity-mismatch at a real call site (Python, where the entry
;; heuristics are richest to set up cleanly - already exercised per-language above for shape-diff)

(define dir4 (make-temporary-directory "steer-eshape~a"))
(write-file! dir4 "app.py"
  (string-append "__all__ = [\"main\", \"other\"]\n\n"
                 "# steer: entry\ndef main():\n    return other(1, 2)\n\n"
                 "def other(x, y):\n    return x + y\n"))
(let-values ([(c o) (steer dir4 "init")]) (check-equal? c 0 o))
(let-values ([(c o) (steer dir4 "api" "snapshot" "--entries")]) (check-equal? c 0 o))

;; entry-demoted: `other` stays in source, just falls out of __all__
(write-file! dir4 "app.py"
  (string-append "__all__ = [\"main\"]\n\n"
                 "# steer: entry\ndef main():\n    return other(1, 2)\n\n"
                 "def other(x, y):\n    return x + y\n"))
(let-values ([(c o) (steer dir4 "api" "diff" "--entries")])
  (check-equal? c 0 "a demotion alone is a warning, not an error")
  (check-regexp-match #rx"entry-demoted app.py: app.py#other" o))

;; back to the original, re-snapshot, then genuinely remove `other` and its call - entry-removed
(write-file! dir4 "app.py"
  (string-append "__all__ = [\"main\", \"other\"]\n\n"
                 "# steer: entry\ndef main():\n    return other(1, 2)\n\n"
                 "def other(x, y):\n    return x + y\n"))
(let-values ([(c o) (steer dir4 "api" "snapshot" "--entries")]) (check-equal? c 0 o))
(write-file! dir4 "app.py" "__all__ = [\"main\"]\n\n# steer: entry\ndef main():\n    return 1\n")
(let-values ([(c o) (steer dir4 "api" "diff" "--entries")])
  (check-equal? c 1 o)
  (check-regexp-match #rx"entry-removed app.py: app.py#other" o))

;; arity-mismatch: `other`'s shape shrinks but its one call site is not updated
(write-file! dir4 "app.py"
  (string-append "__all__ = [\"main\", \"other\"]\n\n"
                 "# steer: entry\ndef main():\n    return other(1, 2)\n\n"
                 "def other(x, y):\n    return x + y\n"))
(let-values ([(c o) (steer dir4 "api" "snapshot" "--entries")]) (check-equal? c 0 o))
(write-file! dir4 "app.py"
  (string-append "__all__ = [\"main\", \"other\"]\n\n"
                 "# steer: entry\ndef main():\n    return other(1, 2)\n\n"
                 "def other(x):\n    return x\n"))
(let-values ([(c o) (steer dir4 "api" "diff" "--entries")])
  (check-equal? c 1 o)
  (check-regexp-match #rx"arity-mismatch app.py: app.py#other: called with 2 arguments from app.py#main" o))
(delete-directory/files dir4)
