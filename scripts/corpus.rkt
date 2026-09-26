#lang racket/base
;; The measurement corpus: .rkt files from the local installation (read in place) and the pinned
;; GitHub clones under samples/corpus/github. Fetch it first with `make samples`.
(require racket/list racket/string racket/file racket/runtime-path "../steer/srcread.rkt")
(provide corpus-files corpus-source seeded-sample)

(define-runtime-path corpus-dir "../samples/corpus")

;; → list of (source . path-string); source is "installation" or the clone's directory name
(define (corpus-files)
  (unless (directory-exists? corpus-dir)
    (error 'corpus "no corpus at ~a; run `make samples`" corpus-dir))
  (define installed
    (for*/list ([f (directory-list corpus-dir #:build? #t)]
                #:when (regexp-match? #rx"installation-.*\\.txt$" (path->string f))
                [line (file->lines f)] #:unless (string=? line ""))
      (cons "installation" line)))
  (define gh (build-path corpus-dir "github"))
  (define cloned
    (for*/list ([repo (if (directory-exists? gh) (directory-list gh) '())]
                [f (in-directory (build-path gh repo) (λ (d) (not (regexp-match? #rx"/(compiled|\\.git)$" (path->string d)))))]
                #:when (and (file-exists? f) (racket-file? f)))
      (cons (path->string repo) (path->string f))))
  (append installed cloned))

(define (corpus-source entry) (car entry))

;; Deterministic sample of n items: order by sha1(seed/item) and take the first n.
(define (seeded-sample items n seed #:key [key values])
  (define (h x) (hex (sha1-bytes (open-input-bytes (string->bytes/utf-8 (format "~a/~a" seed (key x)))))))
  (define sorted (sort items string<? #:key h #:cache-keys? #t))
  (take sorted (min n (length sorted))))
