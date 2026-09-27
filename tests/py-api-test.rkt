#lang racket/base
;; T54: a static Python API lock via the ast (python.rkt's extractor, T62's py-extract) - no import,
;; no execution. Public names (respecting `__all__` when present, else the leading-underscore
;; convention), a class's own public methods too (an exported class's members are part of its API
;; surface, not just its existence), and a diff that classifies parameter changes the same way
;; Racket's arity/keyword diff does: removed = breaking, added-with-a-default = compatible,
;; added-without = breaking, changed = review.
(require rackunit racket/list racket/string racket/file racket/port racket/runtime-path
         "../steer/api.rkt" "../steer/python.rkt")

(define-runtime-path main-rkt "../steer/main.rkt")
(define (steer d . args)
  (define-values (p out in err)
    (parameterize ([current-directory d])
      (apply subprocess #f #f 'stdout (find-executable-path "racket") (path->string main-rkt) args)))
  (close-output-port in)
  (define text (port->string out))
  (subprocess-wait p) (close-input-port out)
  (values (subprocess-status p) text))

(cond
  [(not (python-available?))
   (eprintf "py-api-test: SKIPPED: python3 not found\n")]
  [else

(define dir (make-temporary-directory "steer-py-api~a"))
(define (write-file! rel text)
  (define p (build-path dir rel))
  (make-directory* (let-values ([(d _n _x) (split-path p)]) d))
  (call-with-output-file p #:exists 'truncate (λ (o) (void (write-string text o)))))

;; ---------------------------------------------------------------------------------------------
;; describe: public surface, __all__, class members

(write-file! "pkg.py"
  (string-append
   "def add(x, y):\n    return x + y\n\n"
   "def _hidden():\n    return 1\n\n"
   "class Greeter:\n"
   "    def __init__(self, name):\n        self.name = name\n"
   "    def greet(self):\n        return self.name\n"
   "    def _private(self):\n        pass\n"))
(define r1 (api-describe dir (list "pkg.py")))
(check-equal? (car (hash-ref r1 "pkg.py")) 'ok)
(define entries1 (cadr (hash-ref r1 "pkg.py")))
(define names1 (map car entries1))
(check-true (and (member 'add names1) #t))
(check-false (and (member '_hidden names1) #t) "a leading-underscore top-level name is not public")
(check-true (and (member 'Greeter names1) #t))
(check-true (and (member 'Greeter.greet names1) #t) "an exported class's own public method is part of its API")
(check-false (and (member 'Greeter.__init__ names1) #t) "__init__ is not shown as its own API entry (dunder)")
(check-false (and (member 'Greeter._private names1) #t))
(check-equal? (caddr (assq 'add entries1)) "def add(x, y)")

;; __all__ narrows the public surface even for a name that would otherwise qualify
(write-file! "narrowed.py" "__all__ = [\"add\"]\n\ndef add(x, y):\n    return x + y\n\ndef also_public_looking(x):\n    return x\n")
(define r2 (api-describe dir (list "narrowed.py")))
(define names2 (map car (cadr (hash-ref r2 "narrowed.py"))))
(check-equal? names2 '(add))

;; ---------------------------------------------------------------------------------------------
;; diff: removed/added exports, and parameter-level classification

(define (diff old-shape new-shape)
  (api-diff "m.py" (list (list 'f 'python-def old-shape)) (list (list 'f 'python-def new-shape))))
(check-equal? (diff "def f(x, y)" "def f(x, y)") '() "an unchanged shape: no findings")
(let ([fs (diff "def f(x, y)" "def f(x)")])
  (check-equal? (length fs) 1)
  (check-equal? (hash-ref (car fs) 'severity) 'error)
  (check-regexp-match #rx"y removed" (hash-ref (car fs) 'message)))
(let ([fs (diff "def f(x)" "def f(x, y=1)")])
  (check-equal? (length fs) 1)
  (check-equal? (hash-ref (car fs) 'severity) 'info)
  (check-regexp-match #rx"y=1 added \\(compatible\\)" (hash-ref (car fs) 'message)))
(let ([fs (diff "def f(x)" "def f(x, y)")])
  (check-equal? (length fs) 1)
  (check-equal? (hash-ref (car fs) 'severity) 'error)
  (check-regexp-match #rx"required parameter y added" (hash-ref (car fs) 'message)))
(let ([fs (diff "def f(x=1)" "def f(x=2)")])
  (check-equal? (length fs) 1)
  (check-equal? (hash-ref (car fs) 'severity) 'warning)
  (check-regexp-match #rx"review" (hash-ref (car fs) 'message)))
;; a default containing its own comma/parens must not be split in the wrong place
(check-equal? (diff "def f(x=(1, 2))" "def f(x=(1, 2))") '())

(define full-old (list (list 'add 'python-def "def add(x, y)") (list 'gone 'python-def "def gone(x)")))
(define full-new (list (list 'add 'python-def "def add(x, y)") (list 'brand_new 'python-def "def brand_new(x)")))
(define full-fs (api-diff "m.py" full-old full-new))
(check-true (ormap (λ (f) (and (eq? (hash-ref f 'kind) 'removed-export) (regexp-match? #rx"gone" (hash-ref f 'message)))) full-fs))
(check-true (ormap (λ (f) (and (eq? (hash-ref f 'kind) 'added-export) (regexp-match? #rx"brand_new" (hash-ref f 'message)))) full-fs))

;; ---------------------------------------------------------------------------------------------
;; the CLI: snapshot, show, diff on a real Python module, and a MIXED Racket+Python snapshot

(let-values ([(c o) (steer dir "init")]) (check-equal? c 0 o))
(let-values ([(c o) (steer dir "api" "snapshot" "pkg.py")])
  (check-equal? c 0 o)
  (check-regexp-match #rx"pkg.py: 3 export" o))
(let-values ([(c o) (steer dir "api" "show")])
  (check-equal? c 0)
  (check-regexp-match #rx"def add\\(x, y\\)" o)
  (check-regexp-match #rx"class Greeter" o))
(write-file! "pkg.py"
  (string-append
   "def add(x, y, z=0):\n    return x + y + z\n\n"
   "class Greeter:\n"
   "    def __init__(self, name):\n        self.name = name\n"
   "    def greet(self):\n        return self.name\n"))
(let-values ([(c o) (steer dir "api" "diff")])
  (check-equal? c 0 "an added optional parameter is compatible, not breaking")
  (check-regexp-match #rx"z=0 added \\(compatible\\)" o))

;; mixed: a Racket module and a Python module snapshotted together, each read by its own path
(write-file! "m.rkt" "#lang racket/base\n(provide f)\n(define (f x) x)\n")
(let-values ([(c o) (steer dir "api" "snapshot" "m.rkt" "pkg.py")])
  (check-equal? c 0 o))
(let-values ([(c o) (steer dir "api" "show")])
  (check-equal? c 0)
  (check-regexp-match #rx"def add" o "python entries render with their own shape text")
  (check-regexp-match #rx"m.rkt:" o "and the racket module is still there, in its own format"))

(delete-directory/files dir)
])
