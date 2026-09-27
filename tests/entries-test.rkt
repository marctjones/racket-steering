#lang racket/base
;; T64: entry points, one generic engine over the shared graph. Every language plugs in with its
;; lang.rkt gate's entry-prelude (Datalog rules) and implicit-names; `steer: entry` markers and
;; implicit-names are handled by ONE engine-level rule each, identically for every language.
(require rackunit racket/list racket/string racket/file racket/port racket/runtime-path json
         "../steer/entries.rkt" "../steer/rules.rkt")

;; ---------------------------------------------------------------------------------------------
;; the three fixture projects already built for T60/T62/T63: each finds its own explicit marker,
;; and does NOT cross-contaminate with another language's heuristics run over the same shared facts

(define-runtime-path rkt-dir "fixtures/rkt")
(define-runtime-path py-dir "fixtures/pyproj")
(define-runtime-path cs-dir "fixtures/csproj")

(let-values ([(ids by) (entry-facts rkt-dir)])
  (check-not-false (member "shapes.rkt#area" ids) "the explicit marker in the Racket fixture is found")
  (check-not-false (member "marker" (entry-admitted-by by "shapes.rkt#area")))
  (check-false (member "python" (entry-admitted-by by "shapes.rkt#area")) "a Racket symbol is never admitted by python's own prelude"))

(let-values ([(ids by) (entry-facts py-dir)])
  (check-not-false (member "shapes.py#area" ids))
  (check-not-false (member "marker" (entry-admitted-by by "shapes.py#area")))
  (check-not-false (member "python" (entry-admitted-by by "shapes.py#area")) "also admitted by python's own public_api+root_module rule")
  (check-false (member "racket" (entry-admitted-by by "shapes.py#area")) "a Python symbol is never admitted by racket's own prelude"))

(let-values ([(ids by) (entry-facts cs-dir)])
  (check-not-false (member "Shapes.cs#Program.Run" ids))
  (check-not-false (member "marker" (entry-admitted-by by "Shapes.cs#Program.Run"))))

;; ---------------------------------------------------------------------------------------------
;; implicit-names: a bare name the language calls without a visible ref (Python's __init__, C#'s
;; Main/Dispose/ToString, Racket's main) is always an entry, language-blind to the ENGINE - the
;; per-language table decides WHICH names, not new engine code (T64's own point)

(define dir2 (make-temporary-directory "steer-entries~a"))
(call-with-output-file (build-path dir2 "m.py") #:exists 'truncate
  (λ (o) (void (write-string "class A:\n    def __init__(self):\n        pass\n    def helper(self):\n        pass\n" o))))
(let-values ([(ids by) (entry-facts dir2)])
  (check-true (and (member "m.py#A.__init__" ids) #t) "__init__ is an implicit entry for Python")
  (check-not-false (member "implicit" (entry-admitted-by by "m.py#A.__init__")))
  (check-false (and (member "m.py#A.helper" ids) #t) "an ordinary method is not"))
(delete-directory/files dir2)

(define dir3 (make-temporary-directory "steer-entries~a"))
(call-with-output-file (build-path dir3 "P.cs") #:exists 'truncate
  (λ (o) (void (write-string "namespace N { public class Program { public static void Main() { } public static void Helper() { } } }\n" o))))
(let-values ([(ids by) (entry-facts dir3)])
  (check-true (and (member "P.cs#Program.Main" ids) #t) "Main is an implicit entry for C#")
  (check-not-false (member "implicit" (entry-admitted-by by "P.cs#Program.Main")))
  (check-false (and (member "P.cs#Program.Helper" ids) #t)))
(delete-directory/files dir3)

;; ---------------------------------------------------------------------------------------------
;; the generic helpers, checked directly (not just through a prelude's use of them)

(define dir4 (make-temporary-directory "steer-entries~a"))
(call-with-output-file (build-path dir4 "test_math.py") #:exists 'truncate
  (λ (o) (void (write-string "def test_add():\n    assert 1 + 1 == 2\n" o))))
(let-values ([(g fs) (build-project-graph dir4)])
  (define helpers (helper-facts g fs))
  (check-true (and (member (list 'test_module "test_math.py") helpers) #t))
  (check-true (and (member (list 'test_name "test_math.py#test_add") helpers) #t))
  (check-true (and (member (list 'root_module "test_math.py") helpers) #t)))
(let-values ([(ids by) (entry-facts dir4)])
  (check-true (and (member "test_math.py#test_add" ids) #t) "a test function is an entry via test_name+test_module")
  (check-not-false (member "python" (entry-admitted-by by "test_math.py#test_add"))))
(delete-directory/files dir4)

;; console_script: a bare `main` function is an entry in every language that has one
(define dir5 (make-temporary-directory "steer-entries~a"))
(call-with-output-file (build-path dir5 "app.py") #:exists 'truncate (λ (o) (void (write-string "def main():\n    pass\n" o))))
(let-values ([(ids by) (entry-facts dir5)])
  (check-true (and (member "app.py#main" ids) #t)))
(delete-directory/files dir5)

;; controller_base (C#): a method on a class whose name/base ends in "Controller" is an entry
(define dir6 (make-temporary-directory "steer-entries~a"))
(call-with-output-file (build-path dir6 "Home.cs") #:exists 'truncate
  (λ (o) (void (write-string "namespace N { public class HomeController { public void Index() { } } }\n" o))))
(let-values ([(ids by) (entry-facts dir6)])
  (check-true (and (member "Home.cs#HomeController.Index" ids) #t) "a controller's method is an entry")
  (check-not-false (member "csharp" (entry-admitted-by by "Home.cs#HomeController.Index"))))
(delete-directory/files dir6)

;; package_init (Python): __init__.py itself is an entry
(define dir7 (make-temporary-directory "steer-entries~a"))
(call-with-output-file (build-path dir7 "__init__.py") #:exists 'truncate (λ (o) (void (write-string "" o))))
(let-values ([(ids by) (entry-facts dir7)])
  (check-true (and (member "__init__.py" ids) #t))
  (check-not-false (member "python" (entry-admitted-by by "__init__.py"))))
(delete-directory/files dir7)

;; ---------------------------------------------------------------------------------------------
;; the SAME rules.dl this project already uses for architecture rules can add its own entry(...)
;; rules over the shared facts (a project-specific override), tagged "user"

(define dir8 (make-temporary-directory "steer-entries~a"))
(call-with-output-file (build-path dir8 "m.rkt") #:exists 'truncate
  (λ (o) (void (write-string "#lang racket/base\n(define (special-hook) 1)\n(define (ignored) 2)\n" o))))
(let-values ([(ids by) (entry-facts dir8 #:rules-text "entry(S) :- name(S, \"special-hook\").")])
  (check-true (and (member "m.rkt#special-hook" ids) #t) "a user rule in rules.dl can add its own entries")
  (check-not-false (member "user" (entry-admitted-by by "m.rkt#special-hook")))
  (check-false (and (member "m.rkt#ignored" ids) #t)))
(delete-directory/files dir8)

;; ---------------------------------------------------------------------------------------------
;; the CLI: `steer rules entries` (T64's own listing, with the admitting rule)

(define-runtime-path main-rkt "../steer/main.rkt")
(define (steer dir . args)
  (define-values (p out in err)
    (parameterize ([current-directory dir])
      (apply subprocess #f #f 'stdout (find-executable-path "racket") (path->string main-rkt) args)))
  (close-output-port in)
  (define text (port->string out))
  (subprocess-wait p) (close-input-port out)
  (values (subprocess-status p) text))
(define (write-file! dir rel text)
  (define p (build-path dir rel))
  (make-directory* (let-values ([(d _n _x) (split-path p)]) d))
  (call-with-output-file p #:exists 'truncate (λ (o) (void (write-string text o)))))

(define dir9 (make-temporary-directory "steer-entries~a"))
(write-file! dir9 "m.rkt" "#lang racket/base\n;; steer: entry\n(define (go) 1)\n(define (skip) 2)\n")
(let-values ([(c o) (steer dir9 "init")]) (check-equal? c 0 o))
(let-values ([(c o) (steer dir9 "rules" "entries")])
  (check-equal? c 0 o)
  (check-regexp-match #px"m.rkt#go\\s+\\(marker\\)" o)
  (check-false (regexp-match? #rx"m.rkt#skip" o)))
(let-values ([(c o) (steer dir9 "--json" "rules" "entries")])
  (check-equal? c 0)
  (define j (string->jsexpr o))
  (define data (hash-ref j 'data))
  (check-equal? (hash-ref data 'count) 1)
  (define e (car (hash-ref data 'entries)))
  (check-equal? (hash-ref e 'id) "m.rkt#go")
  (check-equal? (hash-ref e 'by) (list "marker")))
(delete-directory/files dir9)
