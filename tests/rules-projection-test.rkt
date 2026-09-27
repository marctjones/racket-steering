#lang racket/base
;; T61: module-facts now builds the ONE shared graph (T59's link-facts over T60's rkt-extract) and
;; projects module/requires/uses/layer from it, instead of re-implementing require resolution. This
;; checks the projection directly (not just through the CLI, which tests/rules-test.rkt already
;; covers byte-for-byte), the "only materialise what's referenced" optimization, and a wall-time bound
;; on this repo's own self-check (note 15: this file is rules.rkt's own regression test).
(require rackunit racket/list racket/string racket/file racket/runtime-path
         "../steer/rules.rkt" "../steer/graph.rkt")

(define-runtime-path repo-root "..")

;; ---------------------------------------------------------------------------------------------
;; a small synthetic project: module-facts' output, checked directly against graph semantics

(define dir (make-temporary-directory "steer-proj~a"))
(define (write-file! rel text)
  (define p (build-path dir rel))
  (make-directory* (let-values ([(d _n _x) (split-path p)]) d))
  (call-with-output-file p #:exists 'truncate (λ (o) (void (write-string text o)))))
(write-file! "db/conn.rkt" "#lang racket/base\n(provide connect)\n(define (connect) 1)\n")
(write-file! "util/helper.rkt" "#lang racket/base\n(require \"../db/conn.rkt\" racket/list)\n(provide help)\n(define (help) (connect))\n")
(write-file! "ui/view.rkt" "#lang racket/base\n(require \"../db/conn.rkt\")\n(define v (connect))\n")

(define-values (facts edge-lines) (module-facts dir '()))
(check-true (and (member (list 'module "db/conn.rkt") facts) #t) "module facts for every file")
(check-true (and (member (list 'requires "util/helper.rkt" "db/conn.rkt") facts) #t) "a resolved require is a `requires` fact")
(check-true (and (member (list 'requires "ui/view.rkt" "db/conn.rkt") facts) #t))
(check-true (and (member (list 'uses "util/helper.rkt" "racket/list") facts) #t) "an unresolved (library) spec is a `uses` fact")
(check-equal? (hash-ref edge-lines (cons "util/helper.rkt" "db/conn.rkt")) 2 "edge-lines still gives the require's own line")

;; ---------------------------------------------------------------------------------------------
;; needed-predicate projection: `uses` is skipped when no loaded rule/prelude references it, and
;; included whenever it is (or when `needed` is #f, i.e. "everything", as `steer rules facts` wants)

(check-equal? (referenced-predicates '() '()) '(module requires layer) "no user rules: just the builtins")
(define-values (facts-no-uses _el1) (module-facts dir '() '(module requires layer)))
(check-false (for/or ([f facts-no-uses]) (eq? (car f) 'uses)) "uses is not materialised when nothing needs it")
(define-values (facts-with-uses _el2) (module-facts dir '() '(module requires layer uses)))
(check-true (for/or ([f facts-with-uses]) (eq? (car f) 'uses)) "uses IS materialised once something references it")
(define-values (facts-default _el3) (module-facts dir '() #f))
(check-true (for/or ([f facts-default]) (eq? (car f) 'uses)) "the default (#f = everything) always includes uses, for `rules facts`")

;; requires/module/layer are identical whether or not `uses` was skipped - projecting less never
;; changes what IS projected
(check-equal? (sort (filter (λ (f) (eq? (car f) 'requires)) facts-no-uses) string<? #:key (λ (f) (format "~a" f)))
              (sort (filter (λ (f) (eq? (car f) 'requires)) facts-with-uses) string<? #:key (λ (f) (format "~a" f))))

(delete-directory/files dir)

;; ---------------------------------------------------------------------------------------------
;; this repo's own architecture check: a wall-time bound (note 15's regression test for rules.rkt -
;; module-facts now builds a full project graph, not just a require scan, so this guards against that
;; becoming slow enough to matter)

(define t0 (current-inexact-milliseconds))
(define-values (findings info) (check-rules repo-root (file->string (build-path repo-root ".steer" "rules.dl")) ".steer/rules.dl"))
(define elapsed (- (current-inexact-milliseconds) t0))
(check-equal? (hash-ref info 'violations) 0 "steer still satisfies its own architecture rules")
(check-true (< elapsed 10000) (format "steer rules check on this repo took ~a ms, expected well under 10s" elapsed))
(printf "rules-projection-test: this repo's own rules check (graph-based module-facts) took ~a ms\n" (round elapsed))
