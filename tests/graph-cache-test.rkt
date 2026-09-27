#lang racket/base
;; T66: a per-file graph cache keyed by content hash, .steer/cache/graph/<lang>/<sha1>.rktd, one
;; format shared by every language. A second run on an unchanged tree launches no worker and no
;; scanner for any language; `steer init` ignores .steer/cache/; `steer doctor` warns when it is
;; tracked; cold <= 5s and warm <= 0.5s on a 500-file synthetic project.
(require rackunit racket/list racket/string racket/file racket/port racket/system racket/runtime-path
         "../steer/entries.rkt" "../steer/graph-cache.rkt" "../steer/graph.rkt" "../steer/store.rkt")

;; ---------------------------------------------------------------------------------------------
;; a cache hit is byte-identical to a fresh extraction, and launches zero extractors

(define dir (make-temporary-directory "steer-cache~a"))
(define (write-file! rel text)
  (define p (build-path dir rel))
  (make-directory* (let-values ([(d _n _x) (split-path p)]) d))
  (call-with-output-file p #:exists 'truncate (λ (o) (void (write-string text o)))))

(write-file! "m.py" "def f():\n    return 1\n")
(write-file! "m.rkt" "#lang racket/base\n(define (g) 2)\n")
(reset-extract-launches!)
(define-values (g1 fs1) (build-project-graph dir))
(check-equal? (extract-launches) 2 "cold: one launch per language present (python batched as ONE call, racket per-file)")

(check-true (file-exists? (cache-path dir 'python (content-sha1 (file->bytes (build-path dir "m.py"))))) "the cache file exists at the documented path")
(check-true (file-exists? (cache-path dir 'racket (content-sha1 (file->bytes (build-path dir "m.rkt"))))))

(reset-extract-launches!)
(define-values (g2 fs2) (build-project-graph dir))
(check-equal? (extract-launches) 0 "warm: no worker, no scanner")
(check-equal? fs1 fs2 "a cache hit reproduces the exact same file-facts a fresh extraction would")

;; a real content change invalidates the cache (a new sha, so a fresh extraction happens; the OLD
;; cache entry is simply an orphan, never consulted again for this content)
(write-file! "m.py" "def f():\n    return 2\n")
(reset-extract-launches!)
(define-values (g3 fs3) (build-project-graph dir))
(check-equal? (extract-launches) 1 "only the changed file's language re-launches")
(check-not-equal? fs1 fs3 "the new content produces a different def-hash")
(delete-directory/files dir)

;; ---------------------------------------------------------------------------------------------
;; a cache entry surviving a lex/parse error is never trusted blindly: cache-read returns #f for
;; anything that is not a real file-facts struct (hand-edited, truncated, or from a stale format)

(define dir2 (make-temporary-directory "steer-cache~a"))
(define bogus-path (cache-path dir2 'python "deadbeef"))
(make-directory* (let-values ([(d _n _x) (split-path bogus-path)]) d))
(call-with-output-file bogus-path (λ (o) (void (write-string "not a file-facts struct" o))))
(check-false (cache-read bogus-path))
(delete-directory/files dir2)

;; ---------------------------------------------------------------------------------------------
;; `steer init` ignores .steer/cache/ (the .gitignore it writes says so)

(define dir3 (make-temporary-directory "steer-cache~a"))
(init-store! dir3)
(define gi (file->string (build-path dir3 ".steer" ".gitignore")))
(check-regexp-match #rx"cache/" gi)
(delete-directory/files dir3)

;; ---------------------------------------------------------------------------------------------
;; `steer doctor` warns when .steer/cache/ is tracked by git

(define-runtime-path main-rkt "../steer/main.rkt")
(define (steer d . args)
  (define-values (p out in err)
    (parameterize ([current-directory d])
      (apply subprocess #f #f 'stdout (find-executable-path "racket") (path->string main-rkt) args)))
  (close-output-port in)
  (define text (port->string out))
  (subprocess-wait p) (close-input-port out)
  (values (subprocess-status p) text))
(define (sh d . args) (parameterize ([current-directory d]) (apply system*/exit-code (find-executable-path "git") args)))

(cond
  [(not (find-executable-path "git")) (eprintf "graph-cache-test: SKIPPED the doctor/git check: no git on PATH\n")]
  [else
   (define dir4 (make-temporary-directory "steer-cache~a"))
   (void (sh dir4 "init" "-q"))
   (void (sh dir4 "config" "user.email" "t@example.com"))
   (void (sh dir4 "config" "user.name" "t"))
   (let-values ([(c o) (steer dir4 "init")]) (check-equal? c 0 o))
   (call-with-output-file (build-path dir4 "app.py") #:exists 'truncate (λ (o) (void (write-string "def f():\n    return 1\n" o))))
   (reset-extract-launches!)
   (let-values ([(_g4 _fs4) (build-project-graph dir4)]) (void))
   (void (sh dir4 "add" "-A" "-f" ".steer"))   ; -f: force-add past .gitignore, simulating a stray old commit
   (void (sh dir4 "commit" "-q" "-m" "x"))
   (let-values ([(c o) (steer dir4 "doctor")])
     (check-equal? c 0 "a warning, not an error - doctor still exits 0")
     (check-regexp-match #rx"tracked-cache" o))
   (delete-directory/files dir4)])

;; ---------------------------------------------------------------------------------------------
;; performance: cold <= 5s, warm <= 0.5s on a 500-file synthetic project

(define big (make-temporary-directory "steer-cache-big~a"))
(for ([i (in-range 500)])
  (call-with-output-file (build-path big (format "m~a.rkt" i)) #:exists 'truncate
    (λ (o) (void (write-string (format "#lang racket/base\n(define (f~a x) (+ x ~a))\n(define (g~a) (f~a 1))\n" i i i i) o)))))
(reset-extract-launches!)
(define t0 (current-inexact-milliseconds))
(let-values ([(_gbig _fsbig) (build-project-graph big)]) (void))
(define cold-ms (- (current-inexact-milliseconds) t0))
(printf "graph-cache-test: cold pass over 500 files: ~a ms\n" (round cold-ms))
(check-true (< cold-ms 5000) (format "cold pass took ~a ms, expected <= 5000" cold-ms))

(reset-extract-launches!)
(define t1 (current-inexact-milliseconds))
(let-values ([(_gbig _fsbig) (build-project-graph big)]) (void))
(define warm-ms (- (current-inexact-milliseconds) t1))
(printf "graph-cache-test: warm pass over 500 files: ~a ms\n" (round warm-ms))
(check-equal? (extract-launches) 0)
(check-true (< warm-ms 500) (format "warm pass took ~a ms, expected <= 500" warm-ms))
(delete-directory/files big)
