#lang racket/base
;; A per-file cache for extracted file-facts (T66), keyed by the file's own raw-content sha1 - one
;; cache format shared by every language, since file-facts is already the one shared IR every
;; extractor produces (T59). A file whose content hash matches a cached entry is never re-extracted:
;; no worker process (Python), no scanner pass (Racket/C#) runs for it on a second, unchanged run.
(require racket/file racket/port "graph.rkt")
(provide content-sha1 cache-path cache-read cache-write! reset-extract-launches! extract-launches record-extract-launch!)

(define (content-sha1 bs) (bytes->hex-string (sha1-bytes (open-input-bytes bs))))
(define (bytes->hex-string bs) (apply string-append (for/list ([b bs]) (string-append (if (< b 16) "0" "") (number->string b 16)))))

;; .steer/cache/graph/<lang>/<sha1>.rktd
(define (cache-path root lang sha)
  (build-path root ".steer" "cache" "graph" (symbol->string lang) (string-append sha ".rktd")))

;; → file-facts, or #f on a miss or a corrupt/foreign cache entry (never trusted blindly: a hand-edited
;; or truncated cache file just falls back to a fresh extraction, exactly like a miss).
(define (cache-read path)
  (and (file-exists? path)
       (with-handlers ([exn:fail? (λ (e) #f)])
         (define v (call-with-input-file path read))
         (and (file-facts? v) v))))

(define (cache-write! path ff)
  (make-directory* (let-values ([(d _n _x) (split-path path)]) d))
  (with-handlers ([exn:fail? (λ (e) (void))])   ; a cache WRITE failure (disk full, permissions) is never fatal
    (call-with-output-file path #:exists 'truncate (λ (o) (write ff o)))))

;; instrumentation: how many times an actual extractor (a worker process or a scanner pass) launched,
;; for T66's own test ("a second run launches no worker and no scanner") and wall-time measurement.
(define launches (box 0))
(define (reset-extract-launches!) (set-box! launches 0))
(define (extract-launches) (unbox launches))
(define (record-extract-launch! [n 1]) (set-box! launches (+ (unbox launches) n)))
