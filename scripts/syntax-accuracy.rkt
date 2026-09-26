#lang racket/base
;; T22: how often does `steer syntax` point at the right place, and does its machine-applicable
;; edit restore the program? Seeded mutations over the real corpus (make samples):
;;   delete-closer  remove one ) or ] somewhere in the file
;;   extra-closer   duplicate one closer
;;   swap-closer    turn a ) into ] or the reverse (mismatch)
;; For each trial: did steer report an error, did it offer an edit, does applying that single edit
;; give back exactly the original datum ("exact repair"), and how far is the reported line from the
;; mutated line, compared with the raw reader error's line (the baseline an agent sees today).
;;   racket scripts/syntax-accuracy.rkt [N-files] [seed]
(require racket/list racket/string racket/file racket/format racket/math
         "corpus.rkt" "../steer/srcread.rkt" "../steer/syntax-check.rkt")

(define args (vector->list (current-command-line-arguments)))
(define n-files (if (pair? args) (string->number (car args)) 400))
(define seed (if (> (length args) 1) (cadr args) "t22"))

(define (datum-of text)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (let-values ([(fs _l _t) (read-racket-source text)]) (map syntax->datum fs))))

;; closer positions of every list form (0-based), with the opener char
(define (closers text)
  (define-values (fs _l _t) (read-racket-source text))
  (define acc '())
  (let walk ([x fs])
    (cond [(list? x) (for-each walk x)]
          [(syntax? x)
           (define l (syntax->list x))
           (when (and l (syntax-position x) (> (or (syntax-span x) 0) 1))
             (define i (+ (sub1 (syntax-position x)) (sub1 (syntax-span x))))
             (when (memv (string-ref text i) '(#\) #\]))
               (set! acc (cons i acc))))
           (define e (syntax-e x))
           (when (pair? e) (let loop ([e e]) (cond [(pair? e) (walk (car e)) (loop (cdr e))] [(syntax? e) (walk e)])))]))
  (sort acc <))

(define (pick key lst) (and (pair? lst) (list-ref lst (modulo (string->number (substring (hex (sha1-bytes (open-input-bytes (string->bytes/utf-8 key)))) 0 8) 16) (length lst)))))

(define (mutate kind text i)
  (case kind
    [(delete-closer) (string-append (substring text 0 i) (substring text (add1 i)))]
    [(extra-closer) (string-append (substring text 0 (add1 i)) (string (string-ref text i)) (substring text (add1 i)))]
    [(swap-closer) (string-append (substring text 0 i) (if (char=? (string-ref text i) #\)) "]" ")") (substring text (add1 i)))]))

(define kinds '(delete-closer extra-closer swap-closer))

(define candidates
  (for*/list ([e (corpus-files)]
              #:when (regexp-match? #rx"\\.rkt$" (cdr e)))
    (cdr e)))

(define trials
  (for*/fold ([acc '()]) ([p (seeded-sample candidates (* 3 n-files) seed)]
                          #:break (>= (length acc) (* 3 n-files)))
    (define text (file->text p))
    (define orig (datum-of text))
    (cond
      [(or (not orig) (not (sexp-lang? (header-lang text))) (> (string-length text) 150000)) acc]
      [else
       (define cs (closers text))
       (append acc
               (for/list ([k kinds] #:when (pair? cs))
                 (define i (pick (format "~a/~a/~a" seed p k) cs))
                 (define bad (mutate k text i))
                 (define truth-line (position->line text (add1 i)))
                 (define-values (fs _n _l) (check-source bad p))
                 (define errs (filter (λ (f) (eq? (hash-ref f 'severity) 'error)) fs))
                 (define reader (findf (λ (f) (eq? (hash-ref f 'kind) 'read-error)) errs))
                 (define edit (for/first ([f errs] #:when (hash-ref f 'edit #f)) (hash-ref f 'edit)))
                 (define fixed (and edit (with-handlers ([exn:fail? (λ (e) #f)]) (apply-edit bad edit))))
                 (define fixed-datum (and fixed (datum-of fixed)))
                 (hasheq 'kind k 'file p
                         'detected (pair? errs)
                         'edit (and edit #t)
                         'exact (and fixed-datum (equal? fixed-datum orig) #t)
                         'readable (and fixed-datum #t)
                         'steer-dist (and edit (abs (- (hash-ref edit 'line) truth-line)))
                         'reader-dist (and reader (hash-ref reader 'line #f) (abs (- (hash-ref reader 'line) truth-line))))))])))

(define (pct n d) (if (zero? d) "–" (format "~a%" (~r (* 100.0 (/ n d)) #:precision 0))))
(define (median l) (if (null? l) "–" (let ([s (sort l <)]) (list-ref s (quotient (length s) 2)))))
(define (within l k) (length (filter (λ (d) (<= d k)) l)))

(printf "T22 syntax accuracy · seed ~s · ~a trials over ~a files\n\n" seed (length trials) (length (remove-duplicates (map (λ (t) (hash-ref t 'file)) trials))))
(printf "| mutation | n | detected | edit offered | exact repair | readable after edit | steer line exact | steer ±2 | reader line exact | reader ±2 | median dist steer / reader |\n")
(printf "|---|---|---|---|---|---|---|---|---|---|---|\n")
(for ([k (append kinds '(all))])
  (define ts (if (eq? k 'all) trials (filter (λ (t) (eq? (hash-ref t 'kind) k)) trials)))
  (define n (length ts))
  (define (cnt key) (length (filter (λ (t) (hash-ref t key)) ts)))
  (define sd (filter values (map (λ (t) (hash-ref t 'steer-dist)) ts)))
  (define rd (filter values (map (λ (t) (hash-ref t 'reader-dist)) ts)))
  (printf "| ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a / ~a |\n"
          k n (pct (cnt 'detected) n) (pct (cnt 'edit) n) (pct (cnt 'exact) n) (pct (cnt 'readable) n)
          (pct (within sd 0) n) (pct (within sd 2) n) (pct (within rd 0) n) (pct (within rd 2) n)
          (median sd) (median rd)))
