#lang racket/base
;; Reachability (T65): one generic worklist BFS over the shared graph, language-blind - it only walks
;; gedge structs, never asks what language a node came from. Forward over calls/references/inherits/
;; implements/imports/decorates/defines; a reachable symbol makes its own MODULE reachable too (so the
;; module's own imports, and the fact that it's a module at all, count - "<toplevel>" in the tracked
;; goal's words); `overrides` is walked BACKWARDS from a reachable base method to every method that
;; overrides it (a call on the base type may dynamically dispatch to any of them). "class reachable =>
;; its constructors" needs no special case: graph.rkt already emits a `defines` edge from a class to
;; every one of its own members, constructors included, so a plain forward walk over `defines` covers it.
(require racket/list racket/string "graph.rkt" "entries.rkt")
(provide forward-edge-kinds reach-info path-to reachable-ids dead-nodes project-reachability)

(define forward-edge-kinds '(calls references inherits implements imports decorates defines))

;; A single multi-source BFS from every entry id, over three neighbor relations: forward-kind edges,
;; `overrides` edges walked backwards (to -> from), and "symbol -> its own module". → hash id -> (cons
;; parent-id edge-tag) for every reachable id; an entry's own parent is (cons #f 'entry).
(define (reach-info g entry-ids)
  (define by-from (make-hash))
  (define overrides-to (make-hash))
  ;; `defines` (module -> its top-level defs, and class -> its own members) is walked forward ONLY
  ;; when the TARGET is a constructor - that is precisely, and only, "class reachable => its
  ;; constructors". A module becoming reachable must not make every OTHER top-level def in that file
  ;; reachable (dead-code detection would be useless), and a class becoming reachable (e.g. `new
  ;; Widget()`) must not make every OTHER method on it reachable either - only an actual call/ref
  ;; edge, or an explicit override relationship, should do that (found by testing exactly this shape:
  ;; a class instantiated once, with one live and one genuinely-never-called method).
  (define (defines-edge-to-walk? e)
    (or (not (eq? (gedge-kind e) 'defines))
        (let ([to (graph-node g (gedge-to e))]) (and to (eq? (gnode-kind to) 'constructor)))))
  (for ([e (graph-edges g)])
    (when (and (memq (gedge-kind e) forward-edge-kinds) (gedge-to e) (defines-edge-to-walk? e))
      (hash-update! by-from (gedge-from e) (λ (l) (cons (gedge-to e) l)) '()))
    (when (eq? (gedge-kind e) 'overrides)
      (hash-update! overrides-to (gedge-to e) (λ (l) (cons (gedge-from e) l)) '())))
  (define (module-of id) (let ([n (graph-node g id)]) (and n (not (eq? (gnode-kind n) 'module)) (gnode-path n))))
  (define parent (make-hash))
  (for ([id entry-ids] #:when (graph-node g id)) (hash-set! parent id (cons #f 'entry)))
  (let loop ([frontier (filter (λ (id) (graph-node g id)) entry-ids)])
    (define next
      (remove-duplicates
       (for*/list ([id frontier]
                   [nb+kind (append (map (λ (t) (cons t 'fwd)) (hash-ref by-from id '()))
                                    (map (λ (f) (cons f 'overrides)) (hash-ref overrides-to id '()))
                                    (let ([m (module-of id)]) (if m (list (cons m 'module)) '())))]
                   #:unless (hash-ref parent (car nb+kind) #f))
         (hash-set! parent (car nb+kind) (cons id (cdr nb+kind)))
         (car nb+kind))))
    (when (pair? next) (loop next)))
  parent)

;; The path from whichever entry reached `target` first (BFS order = shortest), as a list of ids
;; starting at that entry and ending at target, or #f if target is unreached. Each hop after the
;; first also names its edge-tag ('fwd | 'overrides | 'module), so a caller can explain WHY.
(define (path-to parent target)
  (and (hash-ref parent target #f)
       (let loop ([id target] [acc '()])
         (define p (hash-ref parent id #f))
         (cond [(not p) acc]
               [(not (car p)) (cons (cons id 'entry) acc)]
               [else (loop (car p) (cons (cons id (cdr p)) acc))]))))

(define (reachable-ids parent) (hash-keys parent))

;; every SYMBOL gnode (never a module - "dead" is about code, and a module with zero live symbols is
;; reported as its own, folded finding by the caller) not in the reachable set.
(define (dead-nodes g parent)
  (for/list ([n (graph-nodes g)] #:when (and (not (eq? (gnode-kind n) 'module)) (not (hash-ref parent (gnode-id n) #f))))
    n))

;; → (values graph parent entry-ids admitted-by). The one call site everything else (steer rules dead/
;; reach, and check-rules' optional dead(S)/reachable(S) facts) goes through - the project is extracted
;; and linked exactly once (entries.rkt's entries-from-graph reuses this same graph, not a second one).
(define (project-reachability root #:rules-text [rules-text ""])
  (define-values (g fs-list) (build-project-graph root))
  (define-values (ids admitted-by) (entries-from-graph g fs-list #:rules-text rules-text))
  (values g (reach-info g ids) ids admitted-by))
