#lang racket/base
;; T68: measure the graph on a real project. One script, run per language (the project's own file
;; extensions decide which gates fire) - no language gets its own measurement methodology.
;;   racket scripts/graph-measure.rkt PROJECT_ROOT [--rules PATH] [--dead-sample N]
;; Prints: per-language ref/edge counts by confidence and external (unresolved), entries by admitting
;; source, dead-symbol count, and a sample of dead symbols (id + line, for hand inspection) capped at
;; --dead-sample (default 20) per language.
(require racket/list racket/string racket/file racket/path racket/cmdline
         "../steer/graph.rkt" "../steer/entries.rkt" "../steer/reach.rkt")

(define dead-sample-n (make-parameter 20))
(define rules-text (make-parameter ""))
(define (~1 x) (/ (round (* x 10)) 10.0))

(define root
  (command-line
   #:program "graph-measure"
   #:once-each
   [("--rules") p "rules.dl to load for entry(...) overrides" (rules-text (file->string p))]
   [("--dead-sample") n "dead symbols to sample per language" (dead-sample-n (string->number n))]
   #:args (project-root) project-root))

(define t0 (current-inexact-milliseconds))
(define-values (g parent ids admitted-by) (project-reachability root #:rules-text (rules-text)))
(define elapsed (- (current-inexact-milliseconds) t0))
(define dead (dead-nodes g parent))
(define langs (remove-duplicates (map gnode-lang (graph-nodes g))))

(printf "=== ~a ===\n" root)
(printf "extracted+linked+reached in ~a ms\n\n" (round elapsed))

(for ([l langs])
  (define stats (graph-stats-for g l))
  (printf "-- ~a --\n" l)
  (when stats
    (printf "  files: ~a  defs: ~a  refs: ~a\n" (hash-ref stats 'files) (hash-ref stats 'defs) (hash-ref stats 'refs))
    (define edges (hash-ref stats 'edges))
    (define (pct k) (if (zero? edges) 0.0 (/ (* 100.0 (hash-ref stats k)) edges)))
    (printf "  edges: ~a total - exact ~a (~a%), declared ~a (~a%), name-match ~a (~a%), external ~a (~a%)\n"
            edges (hash-ref stats 'exact) (~1 (pct 'exact)) (hash-ref stats 'declared) (~1 (pct 'declared))
            (hash-ref stats 'name-match) (~1 (pct 'name-match)) (hash-ref stats 'external) (~1 (pct 'external)))
    (printf "  resolution ratio (exact+declared / edges): ~a%\n" (~1 (+ (pct 'exact) (pct 'declared))))))

(printf "\n-- entries: ~a total --\n" (length ids))
(define by-source (make-hash))
(for ([id ids]) (for ([src (entry-admitted-by admitted-by id)]) (hash-update! by-source src add1 0)))
(for ([k (sort (hash-keys by-source) string<?)]) (printf "  ~a: ~a\n" k (hash-ref by-source k)))

(printf "\n-- dead symbols: ~a total --\n" (length dead))
(for ([l langs])
  (define dl (filter (λ (n) (eq? (gnode-lang n) l)) dead))
  (printf "  ~a: ~a dead\n" l (length dl))
  (define sample (take dl (min (dead-sample-n) (length dl))))
  (for ([n sample]) (printf "    ~a  (~a:~a)\n" (gnode-id n) (gnode-path n) (gnode-line n))))
