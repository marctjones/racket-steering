#lang racket/base
;; Robustness gate over real code (task T22 and general hardening). Every installation file already
;; compiled, and the pinned clones are released code, so:
;;   - `steer syntax` must report no error on any of them (an error is a false positive),
;;   - every top-level definition must resolve as an anchor with a stable hash,
;;   - clone detection must finish on each repository,
;;   - nothing may raise an internal exception.
;;   racket scripts/corpus-gate.rkt [--details]
(require racket/list racket/string racket/file racket/format
         "corpus.rkt" "../steer/srcread.rkt" "../steer/syntax-check.rkt" "../steer/anchors.rkt"
         "../steer/dup.rkt")

(define details? (member "--details" (vector->list (current-command-line-arguments))))
(define files (corpus-files))

(define outcomes (make-hash))                  ; outcome → list of paths
(define (note! k p) (hash-update! outcomes k (λ (l) (cons p l)) '()))
(define anchor-total 0)
(define anchor-miss '())
(define crashes '())

(define start (current-inexact-milliseconds))
(for ([e files])
  (define p (cdr e))
  (with-handlers ([exn:fail? (λ (x) (set! crashes (cons (cons p (exn-message x)) crashes)))])
    (define text (file->text p))
    (define-values (fs n lang) (check-source text p))
    (define errors (filter (λ (f) (eq? (hash-ref f 'severity) 'error)) fs))
    (cond
      [(pair? errors) (note! 'false-positive (cons p (map (λ (f) (hash-ref f 'kind)) errors)))]
      [(for/or ([f fs]) (eq? (hash-ref f 'kind) 'not-sexp)) (note! 'skipped-not-sexp p)]
      [else
       (note! 'ok p)
       ;; anchors: every definition must resolve, and resolve to the hash of its own form
       (define-values (forms _l _t) (read-racket-source text))
       (define-values (dir name _d) (split-path p))
       (for ([d (find-definitions forms)])
         (set! anchor-total (add1 anchor-total))
         (define r (resolve-anchor dir (string-append (path->string name) "#" (symbol->anchor-name (car d)))))
         (unless (and (hash-ref r 'found? #f) (equal? (hash-ref r 'hash) (datum-hash (cdr d))))
           (set! anchor-miss (cons (format "~a#~a" p (car d)) anchor-miss))))])))
(define syntax-ms (- (current-inexact-milliseconds) start))

;; clone detection per clone repository (and one large installation package) must finish
(define dup-rows
  (for/list ([src (remove-duplicates (map car files))])
    (define fs (filter (λ (e) (equal? (car e) src)) files))
    (define fs* (if (equal? src "installation") (take fs (min 800 (length fs))) fs))
    (define t0 (current-inexact-milliseconds))
    (with-handlers ([exn:fail? (λ (x) (set! crashes (cons (cons (format "dup ~a" src) (exn-message x)) crashes)) (list src (length fs*) 'crash 0))])
      (define-values (groups skipped)
        (find-clones (for/list ([e fs*]) (cons (cdr e) (file->text (cdr e)))) #:min-size 40))
      (list src (length fs*) (length groups) (round (- (current-inexact-milliseconds) t0))))))

(define (count k) (length (hash-ref outcomes k '())))
(printf "corpus gate over ~a files (~as syntax+anchors)\n" (length files) (~r (/ syntax-ms 1000.0) #:precision 1))
(printf "  syntax ok: ~a · skipped (not s-expr #lang): ~a · false positives: ~a · crashes: ~a\n"
        (count 'ok) (count 'skipped-not-sexp) (count 'false-positive) (length crashes))
(printf "  anchors: ~a definitions, ~a failed to resolve to their own hash\n" anchor-total (length anchor-miss))
(printf "  dup (≥40 tokens): ~a\n"
        (string-join (for/list ([r dup-rows]) (format "~a ~a files → ~a groups in ~ams" (first r) (second r) (third r) (fourth r))) "; "))
(define (show title items)
  (unless (null? items)
    (printf "~a (~a):\n" title (length items))
    (for ([i (if details? items (take items (min 8 (length items))))]) (printf "  ~a\n" i))))
(show "false positives" (reverse (hash-ref outcomes 'false-positive '())))
(show "anchor failures" (reverse anchor-miss))
(show "crashes" (reverse crashes))
(when (or (> (count 'false-positive) 0) (pair? crashes) (pair? anchor-miss)) (exit 1))
