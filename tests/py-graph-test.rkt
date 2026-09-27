#lang racket/base
;; T62: the Python extractor implementing lang.rkt's generic extract/resolve-import contract, over
;; the two-file fixtures/pyproj/{shapes,helper}.py project. Same five-list conformance pattern XL1's
;; tests/cs-anchors-test.rkt used (must-find-edges, known-invisible, must-find-entries,
;; must-not-be-dead, plus a hash-identity check against the existing python-find-anchor).
(require rackunit racket/list racket/string racket/file racket/runtime-path racket/path
         "../steer/graph.rkt" "../steer/python.rkt" "../steer/anchors.rkt" "../steer/lang.rkt")

(define-runtime-path shapes-py "fixtures/pyproj/shapes.py")
(define-runtime-path helper-py "fixtures/pyproj/helper.py")
(define dir (path-only shapes-py))

(cond
  [(not (python-available?))
   (eprintf "py-graph-test: SKIPPED: python3 not found\n")]
  [else

(define shapes-text (file->string shapes-py))
(define helper-text (file->string helper-py))
(define facts (py-extract-batch (list (cons "shapes.py" shapes-text) (cons "helper.py" helper-text))))
(define fs (car facts))
(define fh (cadr facts))
(define g (link-facts facts #:root dir #:resolve-import py-resolve-import))

(define (has-edge? from to kind conf [arity #f] [ext? #f]) (and (member (gedge from to kind conf arity ext?) (graph-edges g)) #t))

;; ---------------------------------------------------------------------------------------------
;; 1. must-find-edges: same-file exact, cross-file declared (via import), self/base receiver-hint
;; resolution (Dog.speak's `super().speak()` and `self.bark()`), inherits, and overrides

(define must-find-edges
  (list (list "shapes.py#area" "shapes.py#compute_area" 'calls 'exact 1)
        (list "shapes.py#compute_area" "helper.py#double" 'calls 'declared 1)
        (list "shapes.py#Dog" "shapes.py#Animal" 'inherits 'exact #f)
        (list "shapes.py#Dog.speak" "shapes.py#Animal.speak" 'overrides 'declared #f)
        (list "shapes.py#Dog.speak" "shapes.py#Animal.speak" 'calls 'declared 0)   ; super().speak()
        (list "shapes.py#Dog.speak" "shapes.py#Dog.bark" 'calls 'exact 0)          ; self.bark()
        (list "shapes.py#unused_helper" "helper.py#triple" 'calls 'declared 1)
        (list "shapes.py" "helper.py" 'imports 'declared #f)))
(for ([e must-find-edges]) (check-true (apply has-edge? e) (format "missing edge: ~a" e)))

;; ---------------------------------------------------------------------------------------------
;; 2. known-invisible: Racket's local-shadowing subtraction has no Python equivalent (LEGB scoping
;; is genuinely more involved - notes/12/16), so this extractor does NOT subtract locals; what IS
;; documented as invisible here is a NESTED (function-body) import statement, which this extractor
;; does not scan (only top-level Import/ImportFrom are read) - a real, intentional simplification.

(check-equal? (length (file-facts-imports fs)) 1 "one top-level import statement (from helper import double, triple)")
(check-true (andmap (λ (im) (equal? (import-spec im) "helper")) (file-facts-imports fs)))

;; ---------------------------------------------------------------------------------------------
;; 3. must-find-entries: the `# steer: entry` marker above `area`, and __all__-based exported?

(define area-def (findf (λ (d) (equal? (def-qualname d) "area")) (file-facts-defs fs)))
(check-true (def-entry? area-def) "area should be marked entry?")
(check-true (def-exported? area-def) "area is in __all__")
(define compute-def (findf (λ (d) (equal? (def-qualname d) "compute_area")) (file-facts-defs fs)))
(check-false (def-entry? compute-def))
(check-false (def-exported? compute-def) "compute_area is NOT in __all__, so it is not exported")

;; ---------------------------------------------------------------------------------------------
;; 4. must-not-be-dead: a forward walk from the one entry (`area`), over calls+defines+imports,
;; reaches every symbol a real call chain touches, and does not reach unused_helper or Dog/Animal
;; (nothing in this fixture calls them from the entry point).

(define (forward-reachable-from start)
  (let loop ([frontier (list start)] [seen (hash start #t)])
    (define next
      (for*/list ([id frontier] [e (graph-edges g)] #:when (and (equal? (gedge-from e) id) (gedge-to e)) #:unless (hash-ref seen (gedge-to e) #f))
        (gedge-to e)))
    (if (null? next) seen (loop next (for/fold ([s seen]) ([n next]) (hash-set s n #t))))))

(define reachable (forward-reachable-from "shapes.py#area"))
(for ([id '("shapes.py#area" "shapes.py#compute_area" "helper.py#double")])
  (check-true (hash-ref reachable id #f) (format "~a must be reachable from the entry" id)))
(for ([id '("shapes.py#unused_helper" "shapes.py#Dog" "shapes.py#Dog.speak" "shapes.py#Animal" "helper.py#triple")])
  (check-false (hash-ref reachable id #f) (format "~a is NOT reached from the one entry (a real dead-code candidate for T65)" id)))

;; ---------------------------------------------------------------------------------------------
;; 5. hash-identity: the graph's def-hash for a symbol equals resolve-anchor's hash for that symbol -
;; the extractor must call the SAME hashing the anchor resolver already uses, never a second one

(define area-anchor (resolve-anchor dir "shapes.py#area"))
(check-true (hash-ref area-anchor 'found?))
(check-equal? (def-hash area-def) (hash-ref area-anchor 'hash) "graph def-hash and resolve-anchor's hash must agree exactly")
(define dog-speak-def (findf (λ (d) (equal? (def-qualname d) "Dog.speak")) (file-facts-defs fs)))
(check-equal? (def-hash dog-speak-def) (hash-ref (resolve-anchor dir "shapes.py#Dog.speak") 'hash))

;; ---------------------------------------------------------------------------------------------
;; file-facts flags: the `if __name__ == "__main__":` block is real executable top-level code

(check-true (file-facts-has-statements? fs) "the __main__ guard makes this a script, not just a module of defs")
(check-false (file-facts-has-statements? fh) "helper.py has only def statements at top level")
(check-equal? (file-facts-lang fs) 'python)
(check-true (string? (file-facts-content-hash fs)))

;; ---------------------------------------------------------------------------------------------
;; kind mapping: __init__ is a constructor, a class member is a method, a top-level def is a function

(check-equal? (def-kind area-def) 'function)
(check-equal? (def-kind (findf (λ (d) (equal? (def-qualname d) "Animal")) (file-facts-defs fs))) 'class)
(check-equal? (def-kind dog-speak-def) 'method)

;; ---------------------------------------------------------------------------------------------
;; the six-function contract: the python gate now exposes `extract`/`resolve-import`/`dynamic-calls?`
;; (T58 reserved extract/resolve-import as #f; T62 fills them in - dynamic-calls?=#t stays for T64)

(define python-gate (gate-for-path "x.py"))
(check-true (procedure? (gate-extract python-gate)) "T58's reserved slot is now implemented")
(check-true (procedure? (gate-resolve-import python-gate)))
(check-true (gate-dynamic-calls? python-gate) "Python's attribute calls are inherently dynamic - resolved via name-match, never claimed exact")
(define fs2 ((gate-extract python-gate) shapes-text "shapes.py"))
(check-equal? fs2 fs "the gate's extract slot IS py-extract, not a second implementation")

])
