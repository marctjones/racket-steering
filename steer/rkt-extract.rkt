#lang racket/base
;; The Racket implementation of lang.rkt's `extract`/`resolve-import` graph-ir contract (T60). Table-
;; driven over srcread's plain s-expression forms: define heads, require/provide heads, module heads.
;; Nothing generic lives here — this is exactly the six-function contract every language implements
;; (rkt-extract/py-extract/cs-extract), and nothing outside it ever reads Racket syntax again.
;;
;; `extract-requires` (Racket's static require scanner) moves here UNCHANGED from rules.rkt, because
;; after T61 rules.rkt queries the graph built from this module — rules.rkt requiring THIS module,
;; not the other way, avoids the require cycle rules.rkt -> graph.rkt -> lang.rkt -> rkt-extract.
(require racket/list racket/string racket/path
         "srcread.rkt" "dup.rkt" "graph.rkt")
(provide rkt-extract rkt-resolve-import extract-requires)

;; ===============================================================================================
;; extract-requires: moved verbatim from rules.rkt (T60). Static requires of one file (no expansion:
;; requires produced by macros are invisible). → list of (spec-string-or-symbol . line).
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

;; ===============================================================================================
;; extract: file-facts per the graph-ir contract

;; `;; steer: entry` immediately above a top-level form marks that form's def(s) as an entry point.
(define entry-comment-rx #px";;\\s*steer:\\s*entry\\s*$")
(define (entry-lines text)
  (define lines (string-split text "\n" #:trim? #f))
  (for/list ([l lines] [i (in-naturals 1)] #:when (regexp-match? entry-comment-rx l)) (add1 i)))

;; every name a top-level `provide` actually makes public, plus a "struct-out:NAME" marker per
;; `(struct-out NAME)` (its generated constructor/predicate/accessors/mutators are ONLY exported
;; through struct-out, never through providing the struct's own name alone). → (values names-hash
;; all-defined-out?). Common forms only (bare id, contract-out incl. rename, rename-out, struct-out,
;; all-defined-out, except-out); an unrecognised provide sub-form is silently skipped, same spirit as
;; the rest of this extractor never claiming precision it cannot back.
(define (provided-names forms)
  (define names (make-hash))
  (define all-out? #f)
  (define (add! s) (hash-set! names (symbol->anchor-name s) #t))
  (define (spec d)
    (cond
      [(symbol? d) (add! d)]
      [(pair? d)
       (case (car d)
         [(contract-out)
          (for ([item (cdr d)])
            (cond [(and (pair? item) (eq? (car item) 'rename) (>= (length item) 3)) (add! (caddr item))]
                  [(pair? item) (add! (car item))]
                  [(symbol? item) (add! item)]))]
         [(rename-out) (for ([item (cdr d)]) (when (and (pair? item) (>= (length item) 2)) (add! (cadr item))))]
         [(all-defined-out) (set! all-out? #t)]
         [(except-out prefix-out) (for-each spec (cdr d))]
         [(struct-out) (for ([nm (cdr d)] #:when (symbol? nm)) (hash-set! names (format "struct-out:~a" nm) #t))]
         [else (void)])]
      [else (void)]))
  (let walk ([fs forms])
    (for ([f fs])
      (define l (syntax->list f))
      (when (and l (pair? l) (symbol? (syntax-e (car l))))
        (define head (syntax-e (car l)))
        (case head
          [(provide) (for ([s (cdr l)]) (spec (syntax->datum s)))]
          [(module module*) (when (>= (length l) 3) (walk (cdddr l)))]
          [(module+) (when (>= (length l) 2) (walk (cddr l)))]
          [(begin) (walk (cdr l))]
          [else (void)]))))
  (values names all-out?))

;; Special forms that are never themselves "a call to a project symbol" and never contribute a def.
(define call-skip-heads
  '(quote quasiquote unquote unquote-splicing lambda λ case-lambda if cond when unless and or
    let let* letrec letrec* let-values let*-values letrec-values begin begin0 begin-for-syntax
    set! define define-values define-syntax define-syntaxes require provide module module* module+
    #%require #%provide #%module-begin #%app #%datum #%top parameterize with-handlers
    for for* for/list for/vector for/hash for/hasheq for/sum for/fold for/and for/or
    for/first for/last in-range in-list in-naturals in-string in-vector do while
    struct define-struct match match* match-lambda match-lambda* match-define
    delay lazy syntax-rules syntax-case define-syntax-rule quote-syntax))

;; The kind for a top-level (define ...) or (struct ...): 'function for (define (f . a) ...) and
;; struct's own type name, 'variable for a plain value binding.
(define (rkt-extract text path)
  (define-values (forms _lang _t)
    (with-handlers ([exn:fail:read? (λ (e) (values '() #f text))]) (read-racket-source text #:source path)))
  (define entries (entry-lines text))
  (define-values (provided all-defined-out?) (provided-names forms))
  (define defs '())
  (define refs '())
  (define has-stmt? #f)
  (define (add-def! d) (set! defs (cons d defs)))
  (define (add-ref! r) (set! refs (cons r refs)))
  (define (entry-here? line) (and (memq line entries) #t))
  ;; T68 (measured on a real library, rebellion): exported? was hardcoded #t for every define, so
  ;; the entry engine's public_api rule (mirroring the one Python/C# already needed) had no way to
  ;; tell a genuinely `provide`d name from an internal helper - 1017 of 1881 symbols showed up "dead"
  ;; before this, entirely because rebellion is a well-designed library that hides its internals and
  ;; exposes a curated public surface via `contract-out`, which nothing in-project ever calls itself.
  (define (exported? name) (or all-defined-out? (hash-ref provided name #f)))

  ;; scan a def's body for calls, subtracting locally-bound names (dup.rkt's collect-binders). Uses
  ;; syntax->list (not raw car/cdr) so the line of the CALL form itself is available before any
  ;; unwrapping — a raw pair has no source location once syntax-e has been peeled off.
  (define (scan-calls! stx scope)
    (define locals (collect-binders (syntax->datum stx)))
    (let walk ([x stx])
      (when (syntax? x)
        (define l (syntax->list x))
        (when (pair? l)
          (define h (car l))
          (define hv (and (syntax? h) (syntax-e h)))
          (cond
            [(and (symbol? hv) (memq hv call-skip-heads)) (for-each walk (cdr l))]
            [(symbol? hv)
             (unless (hash-ref locals hv #f)
               (add-ref! (ref 'call (symbol->anchor-name hv) #f scope (max 0 (sub1 (length l))) (or (syntax-line x) 0))))
             (for-each walk (cdr l))]
            [else (for-each walk l)])))))

  ;; struct forms synthesize generated names: constructor, predicate, accessors, (mutable) mutators
  (define (handle-struct! l line-of-form)
    (define args (or (syntax->list l) '()))
    (when (>= (length args) 3)
      (define name-form (list-ref args 1))
      (define name-e (syntax-e name-form))
      (define-values (name super)
        (if (pair? name-e) (values (syntax-e (car name-e)) (syntax-e (cadr name-e))) (values name-e #f)))
      (define fields-stx (list-ref args 2))
      (define fields (filter symbol? (map (λ (f) (let ([e (syntax-e f)]) (if (pair? e) (syntax-e (car e)) e)))
                                           (or (syntax->list fields-stx) '()))))
      (define opts (map (λ (s) (if (syntax? s) (syntax-e s) s)) (cdddr-safe args)))
      (define mutable? (memq '#:mutable opts))
      (define h (datum-hash l))
      (define entry? (entry-here? line-of-form))
      ;; `(struct-out name)` provides the name AND every generated identifier at once; a plain
      ;; `provide`d struct NAME on its own (rare) still only covers the type itself, not its
      ;; generated accessors - matching what `provide` actually promises either way.
      (define struct-out? (hash-ref provided (string-append "struct-out:" (symbol->string name)) #f))
      (define struct-exported? (or (exported? (symbol->anchor-name name)) struct-out?))
      (add-def! (def 'struct (symbol->anchor-name name) (symbol->anchor-name name) #f line-of-form line-of-form
                     (format "(struct ~a (~a))" name (string-join (map symbol->string fields) " ")) h
                     (if super (list (symbol->anchor-name super)) '()) '() entry? struct-exported?))
      (add-def! (def 'constructor (symbol->anchor-name name) (symbol->anchor-name name) #f line-of-form line-of-form
                     "constructor" (string-append h "-make") '() '() #f struct-out?))
      (add-def! (def 'function (format "~a?" name) (format "~a?" name) #f line-of-form line-of-form
                     "predicate" (string-append h "-pred") '() '() #f struct-out?))
      (for ([fl fields])
        (define acc (format "~a-~a" name fl))
        (add-def! (def 'function acc acc #f line-of-form line-of-form "accessor" (string-append h "-" acc) '() '() #f struct-out?))
        (when mutable?
          (define mut (format "set-~a-~a!" name fl))
          (add-def! (def 'function mut mut #f line-of-form line-of-form "mutator" (string-append h "-" mut) '() '() #f struct-out?))))))
  (define (cdddr-safe l) (if (and (pair? l) (pair? (cdr l)) (pair? (cddr l))) (cdddr l) '()))

  ;; a top-level (define (f args) body...) / (define x val): one def, then scan its body for calls
  (define (handle-define! l)
    (define args (or (syntax->list l) '()))
    (when (>= (length args) 2)
      (define target (list-ref args 1))
      (define e (syntax-e target))
      (define line (syntax-line l))
      (cond
        [(symbol? e)
         (define nm (symbol->anchor-name e))
         (add-def! (def 'variable nm nm #f line (end-line l) (format "~a" nm) (datum-hash l) '() '() (entry-here? line) (exported? nm)))
         (when (>= (length args) 3) (scan-calls! (list-ref args 2) nm))]
        [(pair? e)
         (let loop ([t e])
           (define hv (if (syntax? (car t)) (syntax-e (car t)) (car t)))
           (cond
             [(symbol? hv)
              (define nm (symbol->anchor-name hv))
              ;; T67: the real formals text ("f(x, y=1)"), not a placeholder - read via syntax->datum
              ;; off `target` (the whole `(f x [y 1])` head), comma-joined so the SAME generic
              ;; param-by-name shape-diff T54 built for Python's flat signature strings also works
              ;; here (and for C#'s, which is already comma-separated). A curried define's shape
              ;; reflects only its outermost formals - a known, minor simplification.
              (define formals (with-handlers ([exn:fail? (λ (e) '())]) (cdr (syntax->datum target))))
              (add-def! (def 'function nm nm #f line (end-line l)
                            (format "~a(~a)" nm (formals->shape-args formals)) (datum-hash l) '() '() (entry-here? line) (exported? nm)))
              (for ([body (cddr-safe args)]) (scan-calls! body nm))]
             [(pair? hv) (loop hv)]
             [else (void)]))])))

  ;; a formals list/rest-arg (dotted or a bare symbol), keyword args (`#:k k` / `#:k [k 1]` - TWO
  ;; consecutive list elements, the keyword then its name, never one pair), and [name default]
  ;; positional optionals → "x, y=1, k, *rest" - comma-joined, matching Python's/C#'s own flat shape.
  (define (one x)
    (cond [(symbol? x) (symbol->string x)]
          [(and (pair? x) (pair? (cdr x))) (format "~a=~a" (car x) (cadr x))]
          [else (format "~a" x)]))
  (define (formals->shape-args formals)
    (let loop ([f formals] [acc '()])
      (cond [(null? f) (string-join (reverse acc) ", ")]
            [(and (pair? f) (keyword? (car f)) (pair? (cdr f))) (loop (cddr f) (cons (one (cadr f)) acc))]
            [(pair? f) (loop (cdr f) (cons (one (car f)) acc))]
            [else (string-join (reverse (cons (format "*~a" f) acc)) ", ")])))

  (define (cddr-safe l) (if (and (pair? l) (pair? (cdr l))) (cddr l) '()))
  (define (end-line l) (syntax-line l))  ; forms don't carry a reliable multi-line end without a full span walk; T67 refines if needed

  (let walk ([fs forms] [top? #t])
    (for ([f fs])
      (define l (syntax->list f))
      (cond
        [(and l (pair? l) (symbol? (syntax-e (car l))))
         (define head (syntax-e (car l)))
         (define hs (symbol->string head))
         (cond
           [(eq? head 'struct) (handle-struct! f (syntax-line f))]
           [(regexp-match? #rx"^define" hs) (handle-define! f)]
           [(member hs '("module" "module*")) (when (>= (length l) 3) (walk (cdddr l) top?))]
           [(equal? hs "module+") (when (>= (length l) 2) (walk (cddr l) top?))]
           [(member hs '("begin" "begin-for-syntax")) (walk (cdr l) top?)]
           [(member hs '("require" "provide")) (void)]
           [else (set! has-stmt? #t) (scan-calls! f #f)])]
        [else (set! has-stmt? #t)])))

  (define reqs (with-handlers ([exn:fail? (λ (e) '())]) (extract-requires text path)))
  (define imports (for/list ([r reqs]) (import (format "~a" (car r)) #f (cdr r))))
  (file-facts path 'racket (reverse defs) (reverse refs) imports has-stmt? (text-hash text)))

;; resolve-import: a require's spec resolves to a project file when it names a relative path (any
;; extension-bearing spec, e.g. "helper.rkt") that lands on one of `all-paths`, OR - found measuring
;; this on a real multi-file library (T68, rebellion): a COLLECTION-style spec, e.g.
;; `(require rebellion/type/tuple)`, which is not a relative path at all. Racket resolves that
;; against an installed collection named "rebellion"; when the collection root simply IS this
;; project's own root (a package cloned/checked out on its own, not installed), the same spec maps to
;; a project file once its OWN leading package-name segment is dropped: "type/tuple.rkt" here.
;; A genuine collection/library spec that is neither of these stays external, same as ever (the
;; `uses` vs `requires` distinction check-rules already made before the graph existed).
(define (rkt-resolve-import lang spec importing-path root all-paths)
  (cond
    [(not (string? spec)) '()]
    [else
     (define dir (let-values ([(d _n _x) (split-path (build-path root importing-path))]) d))
     (define (resolve-rel base-dir rel)
       (with-handlers ([exn:fail? (λ (e) #f)])
         (path->string (find-relative-path (simplify-path root) (simplify-path (build-path base-dir rel))))))
     (define direct (resolve-rel dir spec))
     (cond
       [(and direct (member direct all-paths)) (list direct)]
       [else
        (define segs (string-split spec "/"))
        (define stripped (and (> (length segs) 1) (string-join (cdr segs) "/")))
        (define candidates (filter values (list (and stripped (resolve-rel root (string-append stripped ".rkt")))
                                                 (and stripped (resolve-rel root stripped)))))
        (filter (λ (p) (member p all-paths)) candidates)])]))
