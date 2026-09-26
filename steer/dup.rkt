#lang racket/base
;; Clone detection for Racket source (catalog F5). Every list subtree above a size threshold is
;; normalised (locally bound names renamed by first occurrence, so `(λ (x) (+ x 1))` equals
;; `(λ (y) (+ y 1))`; free names like `map` kept, so map vs filter still differ), hashed and grouped.
;; Only maximal clones are reported: a group whose members all sit inside an already reported
;; clone adds nothing. Works on surface syntax; seeing through macros needs expansion (later).
(require racket/list racket/string "srcread.rkt")
(provide find-clones collect-binders normalize tokens)

(define skip-heads '(require provide only-in except-in rename-in prefix-in for-syntax for-label
                     contract-out struct-out all-from-out #%require #%provide))

;; ---------------------------------------------------------------------------------------------
;; Binders: symbols introduced by binding forms anywhere inside a datum (by name; shadowing is
;; ignored, which only makes the renaming slightly coarser).

(define (collect-binders d)
  (define b (make-hasheq))
  (define (bind! x) (when (symbol? x) (hash-set! b x #t)))
  (define (bind-formals! f)                       ; x | (a [b 1] #:k k . rest)
    (let loop ([f f])
      (cond [(symbol? f) (bind! f)]
            [(pair? f)
             (define x (car f))
             (cond [(symbol? x) (bind! x)]
                   [(and (pair? x) (symbol? (car x))) (bind! (car x))])
             (loop (cdr f))]
            [else (void)])))
  (define (bind-clauses! cs)                      ; ([x e] ... #:when guard ...) | ([(a b) e] ...)
    (when (list? cs)
      (let loop ([cs cs])
        (cond [(null? cs) (void)]
              [(keyword? (car cs)) (loop (if (pair? (cdr cs)) (cddr cs) '()))]   ; #:when e is not a clause
              [(pair? (car cs))
               (define lhs (car (car cs)))
               (cond [(symbol? lhs) (bind! lhs)]
                     [(list? lhs) (for-each bind! lhs)])
               (loop (cdr cs))]
              [else (loop (cdr cs))]))))
  (define (bind-pattern! p)                       ; match patterns, approximately
    (cond [(symbol? p) (unless (memq p '(_ ... ..1 else)) (bind! p))]
          [(pair? p)
           (define h (car p))
           (cond [(memq h '(quote quasiquote)) (void)]
                 [(memq h '(? app)) (when (and (pair? (cdr p)) (pair? (cddr p))) (for-each bind-pattern! (cddr p)))]
                 [(symbol? h) (when (list? (cdr p)) (for-each bind-pattern! (cdr p)))]   ; constructor head
                 [(list? p) (for-each bind-pattern! p)])]
          [else (void)]))
  (let walk ([d d])
    (when (pair? d)
      (define h (car d))
      (when (and (symbol? h) (list? d))
        (define hs (symbol->string h))
        (cond
          [(memq h '(lambda λ)) (when (pair? (cdr d)) (bind-formals! (cadr d)))]
          [(eq? h 'case-lambda) (for ([c (cdr d)] #:when (pair? c)) (bind-formals! (car c)))]
          [(regexp-match? #rx"^define" hs)
           (when (pair? (cdr d))
             (define t (cadr d))
             (cond [(symbol? t) (bind! t)]
                   [(and (pair? t) (regexp-match? #rx"values|syntaxes" hs)) (for-each bind! t)]
                   [(pair? t) (let loop ([t t]) (cond [(symbol? t) (bind! t)]
                                                      [(pair? t) (bind-formals! (cdr t)) (loop (car t))]))]))]
          [(memq h '(let let* letrec letrec* let-values let*-values letrec-values))
           (cond [(and (pair? (cdr d)) (symbol? (cadr d)))           ; named let
                  (bind! (cadr d))
                  (when (pair? (cddr d)) (bind-clauses! (caddr d)))]
                 [(pair? (cdr d)) (bind-clauses! (cadr d))])]
          [(regexp-match? #px"^for\\*?(/.*)?$" hs)
           (when (pair? (cdr d))
             (bind-clauses! (cadr d))                                 ; for/fold accumulators or clauses
             (when (and (regexp-match? #rx"fold" hs) (pair? (cddr d))) (bind-clauses! (caddr d))))]
          [(memq h '(match match*)) (when (pair? (cdr d)) (for ([c (cddr d)] #:when (pair? c)) (bind-pattern! (car c))))]
          [(memq h '(match-lambda match-lambda*)) (for ([c (cdr d)] #:when (pair? c)) (bind-pattern! (car c)))]
          [(eq? h 'match-define) (when (pair? (cdr d)) (bind-pattern! (cadr d)))]))
      (let loop ([x d]) (when (pair? x) (walk (car x)) (loop (cdr x))))))
  b)

;; ---------------------------------------------------------------------------------------------
;; Normalisation and size

(define (normalize d loose?)
  (define binders (collect-binders d))
  (define names (make-hasheq))
  (let walk ([d d])
    (cond [(symbol? d) (if (hash-ref binders d #f)
                           (hash-ref! names d (λ () (string->symbol (format "§~a" (add1 (hash-count names))))))
                           d)]
          [(pair? d) (cons (walk (car d)) (walk (cdr d)))]
          [(vector? d) (list->vector (map walk (vector->list d)))]
          [(and loose? (or (string? d) (number? d) (char? d) (bytes? d))) '§lit]
          [else d])))

(define (tokens d)
  (cond [(pair? d) (+ (tokens (car d)) (tokens (cdr d)))]
        [(null? d) 0]
        [(vector? d) (for/sum ([x d]) (tokens x))]
        [else 1]))

;; ---------------------------------------------------------------------------------------------
;; Detection. files: (list (rel-path . text)). → (values groups skipped)
;; group: hasheq 'tokens 'members, member: hasheq 'file 'line 'end 'in 'preview

(define (find-clones files #:min-size [min-size 30] #:loose? [loose? #f])
  (define occs (make-hash))                       ; key → list of occurrences
  (define skipped '())
  (for ([f files])
    (define rel (car f))
    (define text (cdr f))
    (with-handlers ([exn:fail:read? (λ (e) (set! skipped (cons (cons rel (car (string-split (exn-message e) "\n"))) skipped)))])
      (define-values (forms lang _t) (read-racket-source text #:source rel))
      (when (sexp-lang? lang)
        (define defs (find-definitions forms))
        (define (enclosing pos)
          (for/first ([d defs]
                      #:when (let ([s (cdr d)]) (and (syntax-position s) (<= (syntax-position s) pos)
                                                     (< pos (+ (syntax-position s) (syntax-span s))))))
            (car d)))
        (let walk ([stx forms])
          (cond
            [(list? stx) (for-each walk stx)]
            [(syntax? stx)
             (define l (syntax->list stx))
             (when l
               (define head (and (pair? l) (syntax-e (car l))))
               (unless (memq head skip-heads)
                 (define d (syntax->datum stx))
                 (when (and (>= (tokens d) min-size) (syntax-position stx))
                   (define key (format "~s" (normalize d loose?)))
                   (define pos (syntax-position stx))
                   (define span (syntax-span stx))
                   (hash-update! occs key
                                 (λ (l) (cons (hasheq 'file rel 'pos pos 'span span 'tokens (tokens d)
                                                      'line (syntax-line stx) 'end (position->line text (+ pos (max 0 (sub1 span))))
                                                      'in (enclosing pos)
                                                      'preview (clip-line (substring text (sub1 pos) (min (string-length text) (+ (sub1 pos) 200)))))
                                              l))
                                 '()))
                 (for-each walk l)))]
            [else (void)])))))
  (define groups
    (sort (for/list ([(k l) (in-hash occs)] #:when (>= (length l) 2))
            (sort l (λ (a b) (or (string<? (hash-ref a 'file) (hash-ref b 'file))
                                 (and (string=? (hash-ref a 'file) (hash-ref b 'file)) (< (hash-ref a 'pos) (hash-ref b 'pos)))))))
          > #:key (λ (l) (hash-ref (car l) 'tokens))))
  ;; keep maximal groups: at least one member not inside an already-reported member
  (define covered (make-hash))                    ; file → list of (start . end)
  (define (inside? o)
    (for/or ([iv (hash-ref covered (hash-ref o 'file) '())])
      (and (<= (car iv) (hash-ref o 'pos)) (<= (+ (hash-ref o 'pos) (hash-ref o 'span)) (cdr iv)))))
  (define kept
    (for/list ([g groups]
               #:when (for/or ([o g]) (not (inside? o))))
      (for ([o g])
        (hash-update! covered (hash-ref o 'file) (λ (l) (cons (cons (hash-ref o 'pos) (+ (hash-ref o 'pos) (hash-ref o 'span))) l)) '()))
      (hasheq 'tokens (hash-ref (car g) 'tokens) 'members g)))
  (values kept (reverse skipped)))

(define (clip-line s)
  (define first (car (append (string-split s "\n") '(""))))
  (if (> (string-length first) 100) (string-append (substring first 0 99) "…") first))
