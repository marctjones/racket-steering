#lang racket/base
;; T60: the Racket extractor implementing lang.rkt's generic extract/resolve-import contract, over the
;; two-file fixtures/rkt/{shapes,helper}.rkt project. Same five-list conformance pattern XL1's
;; tests/cs-anchors-test.rkt used (must-find-edges, known-invisible, must-find-entries,
;; must-not-be-dead, plus a hash-identity check against the existing resolve-anchor) - not skipped
;; just because this is a different task.
(require rackunit racket/list racket/string racket/file racket/runtime-path racket/path
         "../steer/graph.rkt" "../steer/rkt-extract.rkt" "../steer/anchors.rkt" "../steer/lang.rkt")

(define-runtime-path shapes-rkt "fixtures/rkt/shapes.rkt")
(define-runtime-path helper-rkt "fixtures/rkt/helper.rkt")
(define dir (path-only shapes-rkt))

(define shapes-text (file->string shapes-rkt))
(define helper-text (file->string helper-rkt))
(define fs (rkt-extract shapes-text "shapes.rkt"))
(define fh (rkt-extract helper-text "helper.rkt"))
(define g (link-facts (list fs fh) #:root dir #:resolve-import rkt-resolve-import))

(define (edge from to kind conf [arity #f] [ext? #f]) (gedge from to kind conf arity ext?))
(define (has-edge? e) (and (member e (graph-edges g)) #t))

;; ---------------------------------------------------------------------------------------------
;; 1. must-find-edges: same-file exact, cross-file declared (via require), a generated struct
;; accessor as an exact call target, and an unresolved call as an external sink

(define must-find-edges
  (list (edge "shapes.rkt#area" "shapes.rkt#compute-area" 'calls 'exact 1)
        (edge "shapes.rkt#compute-area" "shapes.rkt#rect-w" 'calls 'exact 1)
        (edge "shapes.rkt#compute-area" "helper.rkt#double" 'calls 'declared 1)
        (edge "shapes.rkt#perimeter" "shapes.rkt#rect-w" 'calls 'exact 1)
        (edge "shapes.rkt#perimeter" "shapes.rkt#rect-h" 'calls 'exact 1)
        (edge "shapes.rkt#perimeter" "helper.rkt#double" 'calls 'declared 1)
        (edge "shapes.rkt#unused-helper" "helper.rkt#triple" 'calls 'declared 1)
        (edge "shapes.rkt" "helper.rkt" 'imports 'declared)
        (edge "shapes.rkt#shadow-triple" #f 'calls 'name-match 2 #t)))  ; the `(+ y 1)` inside the shadowing lambda, not `triple`
(for ([e must-find-edges]) (check-true (has-edge? e) (format "missing edge: ~a" e)))

;; ---------------------------------------------------------------------------------------------
;; 2. known-invisible: a local variable that shadows a real function must never appear as a call
;; to that function (dup.rkt's collect-binders subtracts it) - only the invisible `(+ y 1)` above
;; should show up for shadow-triple's body, never a call named "triple" in that scope

(define known-invisible
  (filter (λ (r) (and (equal? (ref-scope r) "shadow-triple") (equal? (ref-name r) "triple"))) (file-facts-refs fs)))
(check-equal? known-invisible '() "the local `triple` binding is invisible to the graph, not a call to helper.rkt#triple")

;; ---------------------------------------------------------------------------------------------
;; 3. must-find-entries: the `;; steer: entry` marker above `area`

(define must-find-entries '("area"))
(for ([n must-find-entries])
  (check-true (def-entry? (findf (λ (d) (equal? (def-qualname d) n)) (file-facts-defs fs))) (format "~a should be marked entry?" n)))
(check-false (def-entry? (findf (λ (d) (equal? (def-qualname d) "perimeter")) (file-facts-defs fs))) "perimeter has no entry marker")

;; ---------------------------------------------------------------------------------------------
;; 4. must-not-be-dead: a forward walk from the one entry (`area`), over calls+defines+imports,
;; reaches every symbol a real call chain touches. Full dead-code reporting is T65; this is the
;; connectivity T65 will walk, proved now so a later regression is caught at the source.

(define (forward-reachable-from start)
  (let loop ([frontier (list start)] [seen (hash start #t)])
    (define next
      (for*/list ([id frontier] [e (graph-edges g)] #:when (and (equal? (gedge-from e) id) (gedge-to e)) #:unless (hash-ref seen (gedge-to e) #f))
        (gedge-to e)))
    (if (null? next) seen (loop next (for/fold ([s seen]) ([n next]) (hash-set s n #t))))))

(define reachable (forward-reachable-from "shapes.rkt#area"))
(define must-not-be-dead '("shapes.rkt#area" "shapes.rkt#compute-area" "shapes.rkt#rect-w" "helper.rkt#double"))
(for ([id must-not-be-dead]) (check-true (hash-ref reachable id #f) (format "~a must be reachable from the entry" id)))
;; and the negative: nothing calls perimeter/shadow-triple/unused-helper from the one entry point
(for ([id '("shapes.rkt#perimeter" "shapes.rkt#shadow-triple" "shapes.rkt#unused-helper" "helper.rkt#triple")])
  (check-false (hash-ref reachable id #f) (format "~a is NOT reached from the one entry (a real dead-code candidate for T65)" id)))

;; ---------------------------------------------------------------------------------------------
;; 5. hash-identity: the graph's def-hash for a symbol equals resolve-anchor's hash for that same
;; symbol - the extractor must call the SAME hashing (datum-hash), never a second implementation

(define area-def (findf (λ (d) (equal? (def-qualname d) "area")) (file-facts-defs fs)))
(define area-anchor (resolve-anchor dir "shapes.rkt#area"))
(check-true (hash-ref area-anchor 'found?))
(check-equal? (def-hash area-def) (hash-ref area-anchor 'hash) "graph def-hash and resolve-anchor's hash must agree exactly")

(define perimeter-def (findf (λ (d) (equal? (def-qualname d) "perimeter")) (file-facts-defs fs)))
(check-equal? (def-hash perimeter-def) (hash-ref (resolve-anchor dir "shapes.rkt#perimeter") 'hash))

;; ---------------------------------------------------------------------------------------------
;; file-facts flags

(check-false (file-facts-has-statements? fs) "shapes.rkt has only defines/requires/provides at top level")
(check-equal? (file-facts-lang fs) 'racket)
(check-true (string? (file-facts-content-hash fs)))

;; ---------------------------------------------------------------------------------------------
;; the six-function contract: the racket gate now exposes `extract`/`resolve-import` (T58 reserved
;; them as #f; T60 fills them in)

(define racket-gate (gate-for-path "x.rkt"))
(check-true (procedure? (gate-extract racket-gate)) "T58's reserved slot is now implemented")
(check-true (procedure? (gate-resolve-import racket-gate)))
(define fs2 ((gate-extract racket-gate) shapes-text "shapes.rkt"))
(check-equal? fs2 fs "the gate's extract slot IS rkt-extract, not a second implementation")
