#lang racket/base
;; T58: anchors.rkt's separate racket/python/csharp conds and lang.rkt's syntax gates become one
;; per-language record (`gate`, with `check`/`find-anchor`/`list-names` filled for every XL1 language
;; and `extract`/`resolve-import`/`entry-prelude`/`implicit-names`/`dynamic-calls?` reserved for T59-T64).
;; This test is the contract: every gate has the same slot names, a language missing an optional slot
;; is #f (never an error), and `resolve-anchor`/`anchor-names-in-file` now dispatch generically through
;; `gate-for-path` instead of by-name conds, while staying byte-identical to the pre-T58 behavior.
(require rackunit racket/list racket/runtime-path racket/file racket/path
         "../steer/lang.rkt" "../steer/anchors.rkt" "../steer/python.rkt" "../steer/csharp.rkt")

;; ---------------------------------------------------------------------------------------------
;; every gate has the full slot set; required slots are always functions, optional ones are #f
;; until a later task fills them in (never a wrong/crashing value)

(for ([g gates])
  (check-true (procedure? (gate-check g)) (format "~a: check is required" (gate-name g)))
  (check-true (procedure? (gate-find-anchor g)) (format "~a: find-anchor is required" (gate-name g)))
  (check-true (procedure? (gate-list-names g)) (format "~a: list-names is required" (gate-name g)))
  (for ([acc (list gate-extract gate-resolve-import gate-entry-prelude gate-implicit-names gate-dynamic-calls?)])
    (check-true (or (not (acc g)) (procedure? (acc g)) (boolean? (acc g)) (string? (acc g)) (list? (acc g)))
                (format "~a: optional slot is #f, a procedure, a boolean, a string (entry-prelude) or a list (implicit-names), never a wrong value" (gate-name g)))))

(check-equal? (sort (map gate-name gates) symbol<?) '(csharp python racket))

;; ---------------------------------------------------------------------------------------------
;; lang-find-anchor / lang-list-names: the generic dispatch anchors.rkt now uses

(check-false (lang-find-anchor "x.md" "whatever" "f") "no gate for .md: #f, not an error")
(check-false (lang-list-names "x.md" "whatever"))

(let-values ([(fs n lang) (let () (define g (gate-for-path "a.rkt")) ((gate-check g) "#lang racket/base\n(define x 1)\n(define y 2)\n" "a.rkt"))])
  (check-equal? n 2))

(define rkt-text "#lang racket/base\n(define (f x) (+ x 1))\n(define y 2)\n")
(let ([r (lang-find-anchor "a.rkt" rkt-text "f")])
  (check-true (hash-ref r 'found?))
  (check-equal? (hash-ref r 'line) 2))
(check-false (hash-ref (lang-find-anchor "a.rkt" rkt-text "nope") 'found? #f))
(check-equal? (sort (lang-list-names "a.rkt" rkt-text) string<?) '("f" "y"))

;; ---------------------------------------------------------------------------------------------
;; resolve-anchor's Racket branch is unchanged behavior (hash stability matters: baselines in
;; .steer/tasks/*.rktd store these hashes, and a consolidation that changes hashing would mark
;; every tracked anchor CHANGED)

(define-runtime-path tmp-dir ".")
(define dir (make-temporary-directory "steer-lang-registry~a"))
(void (call-with-output-file (build-path dir "m.rkt") #:exists 'truncate
  (λ (o) (write-string "#lang racket/base\n(define (f x)\n  (+ x 1))\n(define y 2)\n" o))))
(define r1 (resolve-anchor dir "m.rkt#f"))
(check-true (hash-ref r1 'found?))
(check-equal? (hash-ref r1 'method) 'racket)
(check-equal? (hash-ref r1 'line) 2)
(check-equal? (hash-ref r1 'end) 3)
(check-true (string? (hash-ref r1 'hash)))
(check-false (hash-ref (resolve-anchor dir "m.rkt#nope") 'found? #f))
(check-regexp-match #rx"closest: f, y" (hash-ref (resolve-anchor dir "m.rkt#g") 'problem))

;; a missing file still reports file-not-found with no gate dispatch attempted
(check-equal? (hash-ref (resolve-anchor dir "missing.rkt#f") 'problem) "file not found")

(delete-directory/files dir)

;; ---------------------------------------------------------------------------------------------
;; a language with no gate at all still uses the generic keyword/indentation heuristic (unchanged)

(define dir2 (make-temporary-directory "steer-lang-registry~a"))
(void (call-with-output-file (build-path dir2 "app.js") #:exists 'truncate
  (λ (o) (write-string "function greet(name) {\n  return name;\n}\n" o))))
(define r2 (resolve-anchor dir2 "app.js#greet"))
(check-true (hash-ref r2 'found?))
(check-equal? (hash-ref r2 'method) 'heuristic)
(delete-directory/files dir2)
