#lang racket/base
;; Reading Racket source as plain s-expressions.
;; A standalone `raco exe` binary cannot load #lang readers (tested: "collection not found"), and we
;; do not want reading to run reader code anyway. So the #lang line is blanked with spaces, which
;; keeps every later character at the same line and column, and the rest is read with #lang and
;; #reader disabled.
(require racket/list racket/string racket/port racket/path)
(provide read-racket-source racket-file? sexp-lang? file->text
         find-definitions def-name-strings datum-hash text-hash hex
         position->line)

(define racket-exts '(#".rkt" #".rktl" #".ss" #".scm" #".rkts"))

(define (racket-file? p)
  (define e (path-get-extension p))
  (and e (member (string-downcase (bytes->string/utf-8 e #\?)) (map bytes->string/utf-8 racket-exts)) #t))

(define (file->text p) (call-with-input-file p port->string))

(define lang-rx #px"^(?:[ \t\r\n]|;[^\n]*\n|#\\|.*?\\|#)*(#lang[ \t]+[^\n]*|#![^\n]*)")

;; Returns (values text-with-#lang-blanked lang-name-or-#f)
(define (blank-lang text)
  (define m (regexp-match-positions lang-rx text))
  (cond
    [(and m (cadr m))
     (define s (caadr m))
     (define e (cdadr m))
     (define line (substring text s e))
     (define name (let ([mm (regexp-match #px"^#(?:lang|!)[ \t]*([^ \t\r\n]+(?:[ \t]+[^ \t\r\n]+)*)" line)])
                    (and mm (string-trim (cadr mm)))))
     (values (string-append (substring text 0 s) (make-string (- e s) #\space) (substring text e)) name)]
    [else (values text #f)]))

;; #langs whose surface syntax is not plain s-expressions: reading them as s-exps would mislead.
(define (sexp-lang? name)
  (not (and name (regexp-match? #px"^(scribble|at-exp|datalog|pollen|reader|video|2d|brag|ragg)" name))))

;; Read all forms. Returns (values forms lang text). Raises exn:fail:read on malformed input.
(define (read-racket-source text #:source [source 'input])
  (define-values (blanked lang) (blank-lang text))
  (define in (open-input-string blanked))
  (port-count-lines! in)
  (define forms
    (parameterize ([read-accept-reader #f] [read-accept-lang #f] [read-on-demand-source #f])
      (let loop ([acc '()])
        (define s (read-syntax source in))
        (if (eof-object? s) (reverse acc) (loop (cons s acc))))))
  (values forms lang text))

;; ---------------------------------------------------------------------------------------------
;; Definitions: (name-symbol . form-syntax) for top-level definitions, looking inside module,
;; module*, module+ and begin. Handles (define x ..), (define (f . a) ..), curried defines,
;; define-values/define-syntaxes, struct and define-struct. First occurrence wins.

(define (find-definitions forms)
  (define seen (make-hasheq))
  (define out '())
  (define (add! name form)
    (unless (hash-ref seen name #f)
      (hash-set! seen name #t)
      (set! out (cons (cons name form) out))))
  (define (walk f)
    (define l (syntax->list f))
    (when (and l (pair? l) (symbol? (syntax-e (car l))))
      (define head (symbol->string (syntax-e (car l))))
      (cond
        [(member head '("module" "module*")) (when (>= (length l) 3) (for-each walk (cdddr l)))]
        [(equal? head "module+") (when (>= (length l) 2) (for-each walk (cddr l)))]
        [(member head '("begin" "begin-for-syntax")) (for-each walk (cdr l))]
        [(and (or (string-prefix? head "define") (member head '("struct" "define-struct")))
              (>= (length l) 2))
         (for ([n (def-names head (cadr l))]) (add! n f))])))
  (for-each walk forms)
  (reverse out))

(define (def-names head target)
  (define e (syntax-e target))
  (cond
    [(symbol? e) (list e)]
    [(and (pair? e) (regexp-match? #rx"values|syntaxes" head))
     (filter symbol? (map syntax-e (or (syntax->list target) '())))]
    [(pair? e)                                  ; (f . args), ((f . a) . b), or (name super)
     (let loop ([x (car e)])
       (define v (if (syntax? x) (syntax-e x) x))
       (cond [(symbol? v) (list v)]
             [(pair? v) (loop (car v))]
             [else '()]))]
    [else '()]))

(define (def-name-strings forms) (map (λ (p) (symbol->string (car p))) (find-definitions forms)))

;; ---------------------------------------------------------------------------------------------
;; Hashing. Datum hashes ignore whitespace, comments and formatting; they change only when the
;; code changes.

(define (hex bs)
  (apply string-append (for/list ([b bs]) (string-append (if (< b 16) "0" "") (number->string b 16)))))

(define (short-sha1 s) (substring (hex (sha1-bytes (open-input-bytes (string->bytes/utf-8 s)))) 0 12))

(define (datum-hash stx) (short-sha1 (format "~s" (syntax->datum stx))))

(define (text-hash s) (short-sha1 (string-normalize-spaces s)))

;; 1-based char position → 1-based line in text.
(define (position->line text pos)
  (add1 (for/sum ([c (in-string text 0 (min (string-length text) (max 0 (sub1 pos))))])
          (if (char=? c #\newline) 1 0))))
