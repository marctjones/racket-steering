#lang racket/base
;; The Datalog evaluator (positive rules, naive bottom-up with a first-column index), extracted from
;; rules.rkt (T64) so it has no dependency on anything project-specific: rules.rkt (architecture rules)
;; and entries.rkt (T64's entry-resolution engine) both build their own fact bases and rule sets over
;; this one evaluator, without either module requiring the other.
;; The datalog package's *parser* is used (it works in a raco exe binary); evaluation is ours: the
;; package's runtime returned no answers for any query in this environment (probed), and bottom-up
;; evaluation of positive rules is small.
(require racket/list racket/string
         datalog/parse datalog/ast
         "common.rkt")
(provide (struct-out rule) parse-rules eval-datalog run-datalog tuples-of var?)

(struct rule (head body line))                    ; head/body literals as (pred . terms); term: (cons 'var sym) | value

(define (term-of t) (if (variable? t) (cons 'var (variable-sym t)) (constant-value t)))
(define (lit->list l) (cons (predicate-sym-sym (literal-predicate l)) (map term-of (literal-terms l))))
(define (var? t) (and (pair? t) (eq? (car t) 'var)))

;; → (values facts rules queries); facts: list of (pred . values). Raises exn:steer on unsafe rules.
(define (parse-rules text source)
  (define stmts
    (with-handlers ([exn:fail? (λ (e) (fail! 'rules-syntax (format "~a: ~a" source (car (string-split (exn-message e) "\n")))
                                             #:hint "rules are positive Datalog: `head(X, \"s\") :- body1(X), body2(X, Y).`"))])
      (parameterize ([current-source-name source]) (parse-program (open-input-string text)))))
  (for/fold ([facts '()] [rules '()] [queries '()] #:result (values (reverse facts) (reverse rules) (reverse queries)))
            ([s stmts])
    (cond
      [(assertion? s)
       (define c (assertion-clause s))
       (define head (lit->list (clause-head c)))
       (define body (map lit->list (clause-body c)))
       (define line (let ([sl (assertion-srcloc s)]) (and (list? sl) (>= (length sl) 2) (cadr sl))))
       (cond
         [(null? body)
          (when (ormap var? (cdr head))
            (fail! 'rules-unsafe (format "~a: fact ~a has a variable" source (car head)) #:hint "facts must be ground"))
          (values (cons head facts) rules queries)]
         [else
          (define bound (for*/list ([b body] [t (cdr b)] #:when (var? t)) t))
          (for ([t (cdr head)] #:when (and (var? t) (not (member t bound))))
            (fail! 'rules-unsafe (format "~a: in the rule for `~a`, variable ~a appears only in the head" source (car head) (cdr t))
                   #:hint "every head variable must also appear in a body literal"))
          (values facts (cons (rule head body line) rules) queries)])]
      [(query? s) (values facts rules (cons (lit->list (query-question s)) queries))]
      [else (values facts rules queries)])))

;; relations: pred → (mutable set of tuples as lists), plus a first-column index
(struct rel (tuples index) #:mutable)
(define (make-rel) (rel (make-hash) (make-hash)))
(define (rel-add! r tuple)
  (and (not (hash-ref (rel-tuples r) tuple #f))
       (begin (hash-set! (rel-tuples r) tuple #t)
              (hash-update! (rel-index r) (if (pair? tuple) (car tuple) '()) (λ (l) (cons tuple l)) '())
              #t)))

;; unify body literal terms with a tuple under substitution `s` (immutable hash var → value)
(define (unify terms tuple s)
  (let loop ([ts terms] [vs tuple] [s s])
    (cond [(and (null? ts) (null? vs)) s]
          [(or (null? ts) (null? vs)) #f]
          [(var? (car ts))
           (define b (hash-ref s (cdr (car ts)) 'unbound))
           (cond [(eq? b 'unbound) (loop (cdr ts) (cdr vs) (hash-set s (cdr (car ts)) (car vs)))]
                 [(equal? b (car vs)) (loop (cdr ts) (cdr vs) s)]
                 [else #f])]
          [(equal? (car ts) (car vs)) (loop (cdr ts) (cdr vs) s)]
          [else #f])))

(define (candidates r terms s)
  (define t0 (and (pair? terms) (car terms)))
  (define key (cond [(not t0) #f]
                    [(var? t0) (let ([b (hash-ref s (cdr t0) 'unbound)]) (if (eq? b 'unbound) #f (list b)))]
                    [else (list t0)]))
  (if key (hash-ref (rel-index r) (car key) '()) (hash-keys (rel-tuples r))))

;; → hash pred → rel. `facts`: list of (pred . values).
(define (eval-datalog facts rules #:max-rounds [max-rounds 500])
  (define rels (make-hasheq))
  (define (rel-of p) (hash-ref! rels p make-rel))
  (for ([f facts]) (rel-add! (rel-of (car f)) (cdr f)))
  (let round ([n 0])
    (when (> n max-rounds) (fail! 'rules-diverge "rules did not reach a fixpoint" #:code 3))
    (define changed? #f)
    (for ([r rules])
      (define subs
        (let loop ([body (rule-body r)] [subs (list (hasheq))])
          (if (null? body) subs
              (let* ([lit (car body)] [rl (rel-of (car lit))])
                (loop (cdr body)
                      (for*/list ([s subs] [tuple (candidates rl (cdr lit) s)] [s2 (in-value (unify (cdr lit) tuple s))] #:when s2) s2))))))
      (for ([s subs])
        (define tuple (for/list ([t (cdr (rule-head r))]) (if (var? t) (hash-ref s (cdr t)) t)))
        (when (rel-add! (rel-of (car (rule-head r))) tuple) (set! changed? #t))))
    (when changed? (round (add1 n))))
  rels)

;; Whole program from text → hasheq pred → list of tuples (each a list of values), sorted. For tests and tools.
(define (run-datalog text [source "program"])
  (define-values (facts rules _q) (parse-rules text source))
  (define rels (eval-datalog facts rules))
  (for/hasheq ([(p r) (in-hash rels)])
    (values p (sort (hash-keys (rel-tuples r)) string<? #:key (λ (t) (format "~s" t))))))

(define (tuples-of rels pred) (let ([r (hash-ref rels pred #f)]) (if r (hash-keys (rel-tuples r)) '())))
