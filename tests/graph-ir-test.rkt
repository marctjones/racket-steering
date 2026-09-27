#lang racket/base
;; T59: the generic graph IR and linker, exercised entirely on hand-written file-facts (no extractor
;; exists yet - that is T60/T62/T63). This freezes the def/ref/import/file-facts and gnode/gedge/graph
;; contract those tasks build against, and proves the linker's scope/import/self+base-receiver/
;; name-match resolution chain and confidence tagging without any language-specific code.
(require rackunit racket/list racket/file "../steer/graph.rkt")

;; ---------------------------------------------------------------------------------------------
;; a small two-file "project": a.rkt defines helper + uses-helper; b.rkt imports a.rkt and calls
;; helper (declared); calls an undefined name (external); and a class hierarchy across the two
;; files to exercise inherits/overrides/constructors.

(define a-defs
  (list (def 'function "helper" "helper" #f 1 2 "(helper x)" "hh1" '() '() #f #t)
        (def 'function "uses-helper" "uses-helper" #f 3 4 "(uses-helper)" "hh2" '() '() #f #t)
        (def 'class "Animal" "Animal" #f 5 8 "(class Animal)" "hh3" '() '() #f #t)
        (def 'constructor "Animal.make" "Animal.make" "Animal" 6 6 "(make)" "hh4" '() '() #f #t)
        (def 'method "Animal.speak" "Animal.speak" "Animal" 7 7 "(speak)" "hh5" '() '() #f #t)))
(define a-refs
  (list (ref 'call "helper" #f "uses-helper" 0 3)))
(define a (file-facts "a.rkt" 'racket a-defs a-refs '() #f "sha-a"))

(define b-defs
  (list (def 'function "main" "main" #f 1 5 "(main)" "hh6" '() '() #t #t)
        (def 'class "Dog" "Dog" #f 6 9 "(class Dog Animal)" "hh7" '("Animal") '() #f #t)
        (def 'method "Dog.speak" "Dog.speak" "Dog" 7 7 "(speak)" "hh8" '() '() #f #t)))
(define b-refs
  (list (ref 'call "helper" #f "main" 0 2)         ; resolved via import -> declared
        (ref 'call "not-a-real-function" #f "main" 0 3) ; unresolved -> external
        (ref 'call "self.speak" 'self "Dog.speak" 0 8))) ; self on the overriding method itself (no-op target)
(define b-imports (list (import "a" #f 1)))
(define b (file-facts "b.rkt" 'racket b-defs b-refs b-imports #t "sha-b"))

(define g (link-facts (list a b) #:root "."))

;; ---------------------------------------------------------------------------------------------
;; nodes: one module node per file, one symbol node per def, ids are "path" / "path#qualname"

(check-equal? (length (graph-nodes g)) (+ 2 (length a-defs) (length b-defs)))
(check-true (gnode? (graph-node g "a.rkt")))
(check-equal? (gnode-kind (graph-node g "a.rkt")) 'module)
(check-true (gnode? (graph-node g "a.rkt#helper")))
(check-equal? (gnode-kind (graph-node g "a.rkt#helper")) 'function)
(check-equal? (gnode-hash (graph-node g "a.rkt#helper")) "hh1")

;; ---------------------------------------------------------------------------------------------
;; defines: module -> top-level defs, class -> members (covers "class reachable => constructors")

(check-true (and (member (gedge "a.rkt" "a.rkt#helper" 'defines 'exact #f #f) (graph-edges g)) #t))
(check-true (and (member (gedge "a.rkt#Animal" "a.rkt#Animal.make" 'defines 'exact #f #f) (graph-edges g)) #t)
            "class -> its own constructor, so 'class reachable => constructors' is a plain forward walk")
(check-true (and (member (gedge "b.rkt#Dog" "b.rkt#Dog.speak" 'defines 'exact #f #f) (graph-edges g)) #t))

;; ---------------------------------------------------------------------------------------------
;; imports: b.rkt's `(import "a" ...)` resolves to a.rkt via the default (generic) resolver

(check-true (and (member (gedge "b.rkt" "a.rkt" 'imports 'declared #f #f) (graph-edges g)) #t))

;; ---------------------------------------------------------------------------------------------
;; calls: same-file exact, cross-file via import = declared, unresolved = external (to=#f)

(check-true (and (member (gedge "a.rkt#uses-helper" "a.rkt#helper" 'calls 'exact 0 #f) (graph-edges g)) #t))
(check-true (and (member (gedge "b.rkt#main" "a.rkt#helper" 'calls 'declared 0 #f) (graph-edges g)) #t)
            "helper is resolved through b.rkt's import of a.rkt, not a project-wide name-match guess")
(check-true (and (member (gedge "b.rkt#main" #f 'calls 'name-match 0 #t) (graph-edges g)) #t)
            "an unresolved call becomes an external sink, not a dropped edge")

;; ---------------------------------------------------------------------------------------------
;; inherits + overrides: Dog(Animal) inherits (exact: same-project, resolved via project name-match
;; here since Animal is in a different file with no import from Dog's own bases lookup path — but a
;; class in an IMPORTED file should be 'declared) and Dog.speak overrides Animal.speak

(define dog-inherits (findf (λ (e) (and (equal? (gedge-from e) "b.rkt#Dog") (eq? (gedge-kind e) 'inherits))) (graph-edges g)))
(check-true (and dog-inherits #t) "Dog inherits Animal is resolved")
(check-equal? (gedge-to dog-inherits) "a.rkt#Animal")
(check-equal? (gedge-confidence dog-inherits) 'declared "Animal is reached through b.rkt's own import of a.rkt")

(check-true (and (member (gedge "b.rkt#Dog.speak" "a.rkt#Animal.speak" 'overrides 'declared #f #f) (graph-edges g)) #t)
            "Dog.speak overrides Animal.speak: a forward edge child -> parent, walked BACKWARDS by reachability (T65)")

;; ---------------------------------------------------------------------------------------------
;; name-match fallback: a third file with no import relationship to a.rkt, calling a bare `helper`

(define c-defs (list (def 'function "elsewhere" "elsewhere" #f 1 2 "(elsewhere)" "hh9" '() '() #f #t)))
(define c-refs (list (ref 'call "helper" #f "elsewhere" 0 2)))
(define c (file-facts "c.rkt" 'racket c-defs c-refs '() #f "sha-c"))
(define g2 (link-facts (list a b c) #:root "."))
(check-true (and (member (gedge "c.rkt#elsewhere" "a.rkt#helper" 'calls 'name-match 0 #f) (graph-edges g2)) #t)
            "no import connects c.rkt to a.rkt; the shared name still produces an edge, tagged name-match")

;; ---------------------------------------------------------------------------------------------
;; stats: per-language counts include exact/declared/name-match/external totals

(define stats (graph-stats-for g2 'racket))
(check-true (hash? stats))
(check-equal? (hash-ref stats 'files) 3)
(check-true (> (hash-ref stats 'exact) 0))
(check-true (> (hash-ref stats 'declared) 0))
(check-true (> (hash-ref stats 'name-match) 0))
(check-true (> (hash-ref stats 'external) 0))

;; ---------------------------------------------------------------------------------------------
;; read helpers

(check-equal? (length (graph-nodes-in g "a.rkt")) (add1 (length a-defs)))
(check-equal? (length (graph-edges-from g "a.rkt#uses-helper")) 1)
(check-true (pair? (graph-edges-to g "a.rkt#helper")))

;; ---------------------------------------------------------------------------------------------
;; .rktd round-trip: prefab structs read back byte-identical

(define tmp (make-temporary-file "steer-graph-~a.rktd"))
(write-graph g2 tmp)
(define g3 (read-graph tmp))
(check-equal? g3 g2 "a graph read back from .rktd equals the one written, field for field")
(delete-file tmp)
