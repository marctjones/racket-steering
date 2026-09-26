#lang racket/base
;; Architecture rules (catalog F4): "the UI layer must not reach the database layer", checked over the
;; project's require graph with Datalog rules the user writes and can extend.
;;   steer rules check [--rules FILE]      exit 1 on violations; each shows the require path that causes it
;;   steer rules facts [--rules FILE]      the extracted facts, counts per predicate (for writing rules)
;;   steer rules init                      write an example .steer/rules.dl
;; Rules file = positive Datalog (no negation) plus `%layer NAME GLOB` lines, which are Datalog comments,
;; so the file stays valid Datalog:
;;     %layer ui   ui/**
;;     %layer db   db/**
;;     violation(A, B, "ui must not reach db") :- reach(A, B), layer(A, "ui"), layer(B, "db").
;; Facts steer supplies (paths are project-relative strings):
;;     module(M)   requires(A,B) direct project require   uses(A,Lib) library require   layer(M,L)
;;     reach(A,B)  transitive closure of requires (defined by steer)
;; A `violation(A,B)` or `violation(A,B,Message)` tuple is reported as a finding on file A.
;; The datalog package's *parser* is used (it works in a raco exe binary); evaluation is ours: the
;; package's runtime returned no answers for any query in this environment (probed), and bottom-up
;; evaluation of positive rules is small.
(require racket/list racket/string racket/file racket/path racket/port
         datalog/parse datalog/ast
         "common.rkt" "store.rkt" "srcread.rkt")
(provide cmd-rules extract-requires glob->regexp run-datalog check-rules)

;; ---------------------------------------------------------------------------------------------
;; Datalog evaluation over the parsed AST (positive rules, naive bottom-up with a first-column index)

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

;; ---------------------------------------------------------------------------------------------
;; Facts from Racket sources

;; `%layer NAME GLOB` lines → list of (name . regexp)
(define (parse-layer-directives text)
  (for*/list ([line (string-split text "\n")]
              [m (in-value (regexp-match #px"^\\s*%\\s*layer\\s+(\\S+)\\s+(\\S+)\\s*$" line))]
              #:when m)
    (cons (cadr m) (glob->regexp (caddr m)))))

(define (glob->regexp g)
  (define body
    (let loop ([cs (string->list g)] [acc '()])
      (cond [(null? cs) (apply string-append (reverse acc))]
            [(and (char=? (car cs) #\*) (pair? (cdr cs)) (char=? (cadr cs) #\*))
             (if (and (pair? (cddr cs)) (char=? (caddr cs) #\/))
                 (loop (cdddr cs) (cons "(?:.*/)?" acc))             ; **/ = zero or more directories
                 (loop (cddr cs) (cons ".*" acc)))]
            [(char=? (car cs) #\*) (loop (cdr cs) (cons "[^/]*" acc))]
            [(char=? (car cs) #\?) (loop (cdr cs) (cons "[^/]" acc))]
            [else (loop (cdr cs) (cons (regexp-quote (string (car cs))) acc))])))
  (pregexp (string-append "^" body "$")))

;; Static requires of one file (no expansion: requires produced by macros are invisible).
;; → list of (spec-string-or-symbol . line), spec is a relative path string, or a collection symbol/list
(define (extract-requires text source)
  (define-values (forms lang _t) (read-racket-source text #:source source))
  (define out '())
  (define (add! spec line) (set! out (cons (cons spec line) out)))
  (define (handle-spec s)                          ; a syntax object
    (define d (syntax-e s))
    (define line (syntax-line s))
    (cond
      [(string? d) (add! d line)]
      [(symbol? d) (add! d line)]
      [(and (pair? d) (symbol? (syntax-e (car d))))
       (define head (syntax-e (car d)))
       (define args (or (syntax->list s) '()))
       (case head
         [(only-in except-in rename-in prefix-in all-except-out for-syntax for-template for-meta only-meta-in)
          (define target (if (eq? head 'prefix-in) (and (>= (length args) 3) (caddr args)) (and (>= (length args) 2) (cadr args))))
          (when (and (memq head '(for-syntax for-template for-meta)) (pair? (cdr args)))
            (for-each handle-spec (if (eq? head 'for-meta) (cddr args) (cdr args))))
          (when (and target (not (memq head '(for-syntax for-template for-meta)))) (handle-spec target))]
         [(file) (when (and (= (length args) 2) (string? (syntax-e (cadr args)))) (add! (syntax-e (cadr args)) line))]
         [(submod) (when (and (>= (length args) 2))
                     (define base (syntax-e (cadr args)))
                     ;; (submod "." x) and (submod ".." x) name a submodule of this file or its parent: no new file
                     (cond [(string? base) (unless (member base '("." "..")) (add! base line))]
                           [(symbol? base) (add! base line)]))]
         [(lib) (when (and (= (length args) 2) (string? (syntax-e (cadr args)))) (add! (string->symbol (syntax-e (cadr args))) line))]
         [(for-label) (void)]
         [else (void)])]
      [else (void)]))
  (let walk ([fs forms])
    (for ([f fs])
      (define l (syntax->list f))
      (when (and l (pair? l) (symbol? (syntax-e (car l))))
        (case (syntax-e (car l))
          [(require) (for-each handle-spec (cdr l))]
          [(module module* module+) (walk (if (eq? (syntax-e (car l)) 'module+) (cddr l) (if (>= (length l) 3) (cdddr l) '())))]
          [(begin) (walk (cdr l))]))))
  (reverse out))

(define ignored-dirs '("compiled" ".git" ".steer" "node_modules" "dist" "build" ".claude" "samples"))

(define (project-files root)
  (sort (for/list ([f (in-directory root (λ (d) (not (member (let-values ([(_b n _d) (split-path d)]) (path->string n)) ignored-dirs))))]
                   #:when (and (file-exists? f) (regexp-match? #rx"[.]rkt$" (path->string f))))
          (path->string (find-relative-path (simplify-path root) (simplify-path f))))
        string<?))

;; → (values facts edge-lines) ; edge-lines: hash (A . B) → line of the require in A
(define (module-facts root layers)
  (define files (project-files root))
  (define file-set (for/hash ([f files]) (values f #t)))
  (define edge-lines (make-hash))
  (define facts
    (append
     (for/list ([f files]) (list 'module f))
     (for*/list ([f files] [l layers] #:when (regexp-match? (cdr l) f)) (list 'layer f (car l)))
     (append*
      (for/list ([f files])
        (define reqs (with-handlers ([exn:fail? (λ (e) '())]) (extract-requires (file->string (build-path root f)) f)))
        (define dir (let-values ([(d _n _x) (split-path (build-path root f))]) d))
        (for/list ([r reqs])
          (define spec (car r))
          (cond
            [(string? spec)
             (define target (path->string (find-relative-path (simplify-path root) (simplify-path (build-path dir spec)))))
             (cond [(hash-ref file-set target #f)
                    (hash-ref! edge-lines (cons f target) (cdr r))
                    (list 'requires f target)]
                   [else (list 'uses f spec)])]
            [else (list 'uses f (format "~a" spec))]))))))
  (values (remove-duplicates facts) edge-lines))

;; ---------------------------------------------------------------------------------------------
;; Checking

(define prelude-rules
  (let-values ([(f r q) (parse-rules "reach(A, B) :- requires(A, B).\nreach(A, C) :- requires(A, B), reach(B, C).\n" "prelude")]) r))

(define builtin-preds '(module requires uses layer reach violation))

;; shortest require path from a to b over `requires` edges, as a list of files (BFS), or #f
(define (shortest-path edges a b)
  (define adj (for/fold ([h (hash)]) ([e edges]) (hash-update h (car e) (λ (l) (cons (cdr e) l)) '())))
  (let loop ([frontier (list (list a))] [seen (hash a #t)])
    (cond [(null? frontier) #f]
          [else
           (define next
             (for*/list ([p frontier] [n (sort (hash-ref adj (car p) '()) string<?)] #:unless (hash-ref seen n #f)) (cons n p)))
           (define hit (findf (λ (p) (equal? (car p) b)) next))
           (if hit (reverse hit)
               (loop next (for/fold ([s seen]) ([p next]) (hash-set s (car p) #t))))])))

;; → (values findings info); info: hasheq counts
(define (check-rules root rules-text source)
  (define layers (parse-layer-directives rules-text))
  (define-values (user-facts user-rules queries) (parse-rules rules-text source))
  (define-values (code-facts edge-lines) (module-facts root layers))
  (define rels (eval-datalog (append code-facts user-facts) (append prelude-rules user-rules)))
  (define edges (for/list ([t (tuples-of rels 'requires)]) (cons (car t) (cadr t))))
  (define known (remove-duplicates (append builtin-preds (map car user-facts) (map (λ (r) (car (rule-head r))) user-rules))))
  (define unknown
    (for*/list ([r user-rules] [b (rule-body r)] #:unless (memq (car b) known)) (list (car b) (rule-line r))))
  (define violations (sort (tuples-of rels 'violation) string<? #:key (λ (t) (format "~a" t))))
  (define findings
    (append
     (for/list ([u (remove-duplicates unknown)])
       (finding 'warning 'unknown-predicate
                (format "rule uses `~a`, which no fact or rule defines (a typo? built-ins: ~a)" (car u) (string-join (map symbol->string builtin-preds) ", "))
                #:file source #:line (cadr u)))
     (for/list ([t violations])
       (define a (car t)) (define b (cadr t))
       (define msg (if (>= (length t) 3) (format "~a" (caddr t)) "violates an architecture rule"))
       (define path (and (member (cons a b) edges) (list a b)))
       (define via (or path (shortest-path edges a b)))
       (finding 'error 'architecture-violation
                (format "~a: ~a → ~a~a" msg a b
                        (cond [(and via (> (length via) 2)) (format " (via ~a)" (string-join (cdr (drop-right via 1)) " → "))]
                              [else ""]))
                #:file a #:line (hash-ref edge-lines (cons a (if (and via (pair? (cdr via))) (cadr via) b)) #f)
                #:fix (if (and via (> (length via) 2))
                          (format "cut the chain, usually at its last link: remove the require of ~a from ~a (or redirect an earlier link)"
                                  b (list-ref via (- (length via) 2)))
                          (format "remove the require of ~a from ~a, or move the code to a layer that may use it" b a))))))
  (values findings
          (hasheq 'modules (length (tuples-of rels 'module)) 'requires (length edges)
                  'layers (length layers) 'rules (length user-rules) 'violations (length violations))))

;; ---------------------------------------------------------------------------------------------
;; Command

(define example-rules #<<EX
% Architecture rules for steer (`steer rules check`). Positive Datalog; `%layer NAME GLOB` assigns files.
% Facts: module(M). requires(A,B). uses(A,Lib). layer(M,L). and reach(A,B), the transitive closure.
%layer ui   ui/**
%layer db   db/**
%layer core core/**

violation(A, B, "the UI must not reach the database") :- reach(A, B), layer(A, "ui"), layer(B, "db").
violation(A, B, "core must not depend on the UI")     :- reach(A, B), layer(A, "core"), layer(B, "ui").
EX
  )

(define (default-rules-path root) (build-path root ".steer" "rules.dl"))

(define (cmd-rules argv)
  (define-values (pos o) (parse-args "rules" argv '(("--rules" one)) #:min 1 #:max 1
                                     #:usage "steer rules check|facts|init [--rules FILE]"))
  (define root (find-root))
  (define rules-path (if (opt-ref o 'rules) (path->complete-path (opt-ref o 'rules)) (default-rules-path root)))
  (define (load-rules)
    (unless (file-exists? rules-path)
      (fail! 'no-rules (format "no rules file at ~a" rules-path) #:hint "`steer rules init` writes an example"))
    (file->string rules-path))
  (define shown (let ([r (find-relative-path (simplify-path root) (simplify-path rules-path))]) (path->string r)))
  (case (car pos)
    [("init")
     (when (file-exists? rules-path) (fail! 'exists (format "~a already exists" shown) #:hint "edit it, or pass --rules to write elsewhere"))
     (call-with-output-file rules-path (λ (out) (void (write-string example-rules out)) (newline out)))
     (make-reply "rules" (format "wrote ~a: edit the %layer globs and rules, then `steer rules check`" shown))]
    [("check")
     (define-values (findings info) (check-rules root (load-rules) shown))
     (define errors (filter (λ (f) (eq? (hash-ref f 'severity) 'error)) findings))
     (make-reply "rules"
                 (format "~a module~a, ~a require edge~a, ~a layer~a, ~a rule~a: ~a violation~a"
                         (hash-ref info 'modules) (plural (hash-ref info 'modules)) (hash-ref info 'requires) (plural (hash-ref info 'requires))
                         (hash-ref info 'layers) (plural (hash-ref info 'layers)) (hash-ref info 'rules) (plural (hash-ref info 'rules))
                         (hash-ref info 'violations) (plural (hash-ref info 'violations)))
                 info #:ok? (null? errors) #:findings findings)]
    [("facts")
     (define layers (parse-layer-directives (if (file-exists? rules-path) (file->string rules-path) "")))
     (define-values (facts _e) (module-facts root layers))
     (define counts (for/fold ([h (hasheq)]) ([f facts]) (hash-update h (car f) add1 0)))
     (make-reply "rules"
                 (string-join (for/list ([(k v) (in-hash counts)]) (format "~a: ~a" k v)) "\n")
                 (for/hasheq ([(k v) (in-hash counts)]) (values k v)))]
    [else (fail! 'usage (format "unknown rules action ~a" (car pos)) #:hint "check | facts | init")]))
