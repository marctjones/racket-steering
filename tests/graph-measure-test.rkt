#lang racket/base
;; T68: the graph measured for real on note 12's own fixture projects (see notes/16-code-graph.md for
;; the full numbers and the hand-checked dead-finding samples). This test's own job is narrow but
;; concrete: assert the resolution ratio measured on jackfirth/rebellion (a real, ~1900-symbol Racket
;; library at the commit pinned in scripts/samples-sources.rktd) does not regress below the floor
;; notes/16 records. `make samples` fetches it; without it, this test skips rather than failing (the
;; other three fixtures - tomli, aiofiles, GuardClauses - are cloned ad hoc for the note and are not
;; wired into `make samples`, so they are not re-checked here on every run).
(require rackunit racket/runtime-path racket/path
         "../steer/reach.rkt" "../steer/graph.rkt")

(define-runtime-path corpus-dir "../samples/corpus/github")
(define rebellion-dir (build-path corpus-dir "jackfirth-rebellion"))

(cond
  [(not (directory-exists? rebellion-dir))
   (eprintf "graph-measure-test: SKIPPED: no samples/corpus/github/jackfirth-rebellion (run `make samples`)\n")]
  [else
   (define-values (g parent ids by) (project-reachability rebellion-dir))
   (define stats (graph-stats-for g 'racket))
   (check-true (hash? stats) "rebellion is a Racket project: per-language stats exist")
   (define edges (hash-ref stats 'edges))
   (define resolved (+ (hash-ref stats 'exact) (hash-ref stats 'declared)))
   (define ratio (if (zero? edges) 0.0 (/ (* 100.0 resolved) edges)))
   (printf "graph-measure-test: rebellion resolution ratio: ~a% (~a/~a edges exact+declared)\n"
           (/ (round (* ratio 10)) 10.0) resolved edges)
   ;; the recorded floor in notes/16-code-graph.md is 51.2%, measured after the collection-style
   ;; require fix (T68 itself found and fixed this): a regression below 45% would mean either that
   ;; fix broke, or a real new resolution gap opened up - either way, worth a human's attention.
   (check-true (>= ratio 45.0) (format "rebellion's resolution ratio dropped to ~a%, below the recorded floor (45%, notes/16)" ratio))
   ;; a sanity bound the OTHER way too: 100% would mean nothing is genuinely ambiguous any more,
   ;; which is not a claim this system makes about any real, non-trivial project.
   (check-true (< ratio 90.0) "a suspiciously high ratio is itself worth a second look, not just a low one")])
