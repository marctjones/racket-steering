#lang racket/base
;; Public API lock for Racket modules (catalog F3). A worker process (the installed `racket`, not
;; this binary: it needs every collection, and a separate process can be killed on timeout)
;; instantiates each module and records its phase-0 exports: kind, arity, keywords, contract.
;; Contracts are read inside the module's own namespace: `value-contract` only recognises wrappers
;; made by the same instance of racket/contract (tested; from outside it returns #f).
(require racket/list racket/string racket/file racket/pretty racket/port
         "common.rkt" "checks.rkt" "graph.rkt" "python.rkt")
(provide api-describe api-diff read-lock write-lock! arity-text)

(define worker-source #<<EOF
#lang racket/base
(require racket/list racket/vector)
(define out-file (vector-ref (current-command-line-arguments) 0))
(define (arity->datum a)
  (cond [(exact-nonnegative-integer? a) a]
        [(arity-at-least? a) (list '>= (arity-at-least-value a))]
        [(list? a) (map arity->datum a)]
        [else #f]))
(define (describe path)
  (with-handlers ([(λ (e) #t) (λ (e) (list 'error (if (exn? e) (exn-message e) (format "~s" e))))])
    (define mp (path->complete-path path))
    (define ns (make-base-namespace))
    (parameterize ([current-namespace ns])
      (dynamic-require mp #f)
      (define-values (vars stxs) (module->exports mp))
      (define (phase0 l) (let ([p (assv 0 l)]) (if p (map car (cdr p)) '())))
      (namespace-require 'racket/contract/base)
      (namespace-require `(prefix steer-api: (file ,(path->string mp))))
      (list 'ok
            (for/list ([n (sort (remove-duplicates (append (phase0 vars) (phase0 stxs))) symbol<?)])
              (define id (string->symbol (format "steer-api:~a" n)))
              (define info
                (with-handlers ([exn:fail? (λ (e) #f)])
                  (eval `(let ([v ,id])
                           (list (if (procedure? v) 'procedure 'value)
                                 (and (procedure? v) (procedure-arity v))
                                 (and (procedure? v) (call-with-values (λ () (procedure-keywords v)) list))
                                 (let ([c (value-contract v)]) (and c (format "~s" (contract-name c)))))))))
              (if info
                  (list n (car info) (arity->datum (cadr info)) (caddr info) (cadddr info))
                  (list n 'macro #f #f #f)))))))
(define results (for/list ([p (vector-drop (current-command-line-arguments) 1)]) (cons p (describe p))))
(call-with-output-file out-file #:exists 'truncate (λ (o) (write results o)))
EOF
  )

(define (shell-quote s) (string-append "'" (string-replace s "'" "'\\''") "'"))

;; T54: a Python module's public API, static (via python.rkt's ast-based extractor - the module is
;; never imported/run, unlike the Racket path above, which genuinely instantiates each module).
;; Entries: (name 'python-def shape) for an exported top-level function/class, and one more per
;; exported CLASS for each of its own public (non-`_`-prefixed) methods, named "Class.method" - the
;; class's own members are part of its API surface too, not just its own existence. `shape` is the
;; extractor's own signature text ("def f(x, y=1)" / "class C(Base)"), read literally, never re-derived.
(define (python-describe root modules)
  (define abs (for/list ([m modules]) (build-path root m)))
  (define texts (for/list ([p abs]) (with-handlers ([exn:fail? (λ (e) #f)]) (file->string p))))
  (define batch (for/list ([m modules] [t texts] #:when t) (cons m t)))
  (define facts (py-extract-batch batch))
  (define by-mod (for/hash ([m modules] [t texts]) (values m t)))
  (for/hash ([m modules])
    (cond
      [(not (hash-ref by-mod m #f)) (values m (list 'error (format "could not read ~a" m)))]
      [else
       (define ff (findf (λ (f) (equal? (file-facts-path f) m)) facts))
       (define top (filter (λ (d) (and (not (def-scope d)) (def-exported? d))) (file-facts-defs ff)))
       (define entries
         (append*
          (for/list ([d top])
            (cons (list (string->symbol (def-name d)) 'python-def (def-shape d))
                  (if (eq? (def-kind d) 'class)
                      (for/list ([md (file-facts-defs ff)]
                                 #:when (and (equal? (def-scope md) (def-qualname d)) (not (string-prefix? (def-name md) "_"))))
                        (list (string->symbol (def-qualname md)) 'python-def (def-shape md)))
                      '())))))
       (values m (list 'ok (sort entries symbol<? #:key car)))]))
  )

;; → hash: module-path → (list 'ok exports) | (list 'error msg)
(define (api-describe root modules #:timeout [timeout 120])
  (define-values (py-mods rkt-mods) (partition (λ (m) (regexp-match? #rx"[.]pyi?$" m)) modules))
  (define py-result (if (pair? py-mods) (python-describe root py-mods) (hash)))
  (define rkt-result
    (if (null? rkt-mods)
        (hash)
        (let ()
          (define racket (or (getenv "STEER_RACKET") (let ([p (find-executable-path "racket")]) (and p (path->string p)))))
          (unless racket
            (fail! 'no-racket "the API tool needs an installed `racket` on PATH to load modules" #:hint "install Racket or set STEER_RACKET" #:code 3))
          (define dir (make-temporary-directory "steer-api~a"))
          (define worker (build-path dir "worker.rkt"))
          (define out (build-path dir "out.rktd"))
          (call-with-output-file worker (λ (o) (write-string worker-source o)))
          (define r (run-check (string-join (append (list (shell-quote racket) (shell-quote (path->string worker)) (shell-quote (path->string out)))
                                                     (map shell-quote rkt-mods))
                                             " ")
                                root timeout))
          (define result
            (cond
              [(file-exists? out)
               (for/hash ([e (call-with-input-file out (λ (in) (parameterize ([read-accept-reader #f]) (read in))))])
                 (values (car e) (cdr e)))]
              [else (fail! 'api-worker (format "API worker failed (~a): ~a" (hash-ref r 'exit) (hash-ref r 'tail))
                           #:hint "run the module directly with `racket FILE` to see the error" #:code 3)]))
          (delete-directory/files dir)
          result)))
  (for/fold ([h py-result]) ([(k v) (in-hash rkt-result)]) (hash-set h k v)))

;; ---------------------------------------------------------------------------------------------
;; Lock file: ((version 1) (modules (("path" ((name kind arity keywords contract) ...)) ...)))

(define (lock-path root) (build-path root ".steer" "api.lock"))

(define (read-lock root)
  (define p (lock-path root))
  (and (file-exists? p)
       (let ([d (call-with-input-file p (λ (in) (parameterize ([read-accept-reader #f]) (read in))))])
         (for/hash ([m (cadr (assq 'modules d))]) (values (car m) (cadr m))))))

(define (write-lock! root mods)
  (define p (lock-path root))
  (call-with-output-file p #:exists 'truncate
    (λ (o) (parameterize ([pretty-print-columns 100])
             (pretty-write `((version 1) (modules ,(for/list ([k (sort (hash-keys mods) string<?)]) (list k (hash-ref mods k))))) o)))))

;; ---------------------------------------------------------------------------------------------
;; Diff and classification

(define (arity-text a)
  (cond [(not a) "-"]
        [(exact-integer? a) (number->string a)]
        [(and (pair? a) (eq? (car a) '>=)) (format "~a+" (cadr a))]
        [(list? a) (string-join (map arity-text a) "|")]
        [else (format "~a" a)]))

(define (accepts? a n)
  (cond [(not a) #f]
        [(exact-integer? a) (= a n)]
        [(and (pair? a) (eq? (car a) '>=)) (>= n (cadr a))]
        [(list? a) (for/or ([x a]) (accepts? x n))]
        [else #f]))

(define (unbounded? a)
  (cond [(and (pair? a) (eq? (car a) '>=)) #t]
        [(list? a) (ormap unbounded? a)]
        [else #f]))

;; Every call the old arity allowed must still be allowed.
(define (arity-narrowed? old new)
  (or (for/or ([n (in-range 0 64)]) (and (accepts? old n) (not (accepts? new n))))
      (and (unbounded? old) (not (unbounded? new)))))

(define (arity-widened? old new) (arity-narrowed? new old))

;; kws: (required allowed) where allowed #f = any keyword
(define (keywords-narrowed? old new)
  (and old new
       (or (for/or ([k (car new)]) (not (member k (car old))))                 ; new required keyword
           (and (cadr new) (or (not (cadr old)) (for/or ([k (cadr old)]) (not (member k (cadr new)))))))))

;; T54: a Python signature is one string ("def f(x, y=1)"), not a structured arity/keyword tuple, so
;; a changed one gets its own comparison: split the parameter list on top-level commas (depth-aware,
;; so a default value's own commas/parens/brackets - `x=(1, 2)` - never split it in the wrong place),
;; classify by NAME (removed = breaking, added-with-a-default = compatible, added-without = breaking),
;; and flag a same-named parameter whose own text (annotation or default) changed for review.
(define (split-params-text shape)
  (define m (regexp-match #px"\\((.*)\\)" shape))
  (if (not m) '()
      (let loop ([cs (string->list (cadr m))] [cur '()] [depth 0] [acc '()])
        (cond
          [(null? cs) (reverse (if (pair? cur) (cons (list->string (reverse cur)) acc) acc))]
          [(memv (car cs) '(#\( #\[ #\{)) (loop (cdr cs) (cons (car cs) cur) (add1 depth) acc)]
          [(memv (car cs) '(#\) #\] #\})) (loop (cdr cs) (cons (car cs) cur) (max 0 (sub1 depth)) acc)]
          [(and (zero? depth) (char=? (car cs) #\,)) (loop (cdr cs) '() 0 (cons (list->string (reverse cur)) acc))]
          [else (loop (cdr cs) (cons (car cs) cur) depth acc)]))))

(define (param-name text) (car (string-split (string-trim text) #px"[:=]")))

(define (python-shape-diff f name old-shape new-shape)
  (cond
    [(equal? old-shape new-shape) '()]
    [else
     (define ops (map string-trim (split-params-text old-shape)))
     (define nps (map string-trim (split-params-text new-shape)))
     (define (by-pname l) (for/hash ([p l]) (values (param-name p) p)))
     (define op (by-pname ops))
     (define np (by-pname nps))
     (append
      (for/list ([pn (sort (hash-keys op) string<?)] #:unless (hash-ref np pn #f))
        (f 'error 'param-removed name (format "parameter ~a removed (breaking)" pn)))
      (for/list ([pn (sort (hash-keys np) string<?)] #:unless (hash-ref op pn #f))
        (if (regexp-match? #rx"=" (hash-ref np pn))
            (f 'info 'param-added name (format "optional parameter ~a added (compatible)" pn))
            (f 'error 'param-added name (format "required parameter ~a added (breaking)" pn))))
      (for/list ([pn (sort (hash-keys op) string<?)] #:when (hash-ref np pn #f) #:unless (equal? (hash-ref op pn) (hash-ref np pn #f)))
        (f 'warning 'param-changed name (format "parameter ~a: ~a → ~a (review annotation/default)" pn (hash-ref op pn) (hash-ref np pn))))
      ;; same parameters, same defaults, but the shape text still differs (return annotation, a
      ;; reordered parameter list): report it once, generically, rather than claim nothing changed
      (if (and (null? (for/list ([pn (hash-keys op)] #:unless (hash-ref np pn #f)) pn))
               (null? (for/list ([pn (hash-keys np)] #:unless (hash-ref op pn #f)) pn))
               (for/and ([pn (hash-keys op)]) (equal? (hash-ref op pn) (hash-ref np pn #f))))
          (list (f 'warning 'signature-changed name (format "~a → ~a (review)" old-shape new-shape)))
          '()))]))

;; old, new: lists of (name kind ...) - Racket's (kind arity keywords contract), or Python's (shape).
;; → list of findings
(define (api-diff module old new)
  (define (by-name l) (for/hash ([e l]) (values (car e) e)))
  (define o (by-name old))
  (define n (by-name new))
  (define (f sev kind name msg [fix #f]) (finding sev kind (format "~a: ~a" name msg) #:file module #:fix fix))
  (append
   (for/list ([name (sort (hash-keys o) symbol<?)] #:unless (hash-ref n name #f))
     (f 'error 'removed-export name "export removed (breaking)" "restore it, or re-snapshot if the break is intended"))
   (append*
    (for/list ([name (sort (hash-keys o) symbol<?)]
               #:when (hash-ref n name #f))
      (define oe (hash-ref o name))
      (define ne (hash-ref n name))
      (if (eq? (cadr oe) 'python-def)
          (python-shape-diff f name (caddr oe) (caddr ne))
          (let-values ([(ka aa kwa ca) (apply values (cdr oe))]
                       [(kb ab kwb cb) (apply values (cdr ne))])
            (append
             (if (not (eq? ka kb)) (list (f 'error 'kind-changed name (format "was a ~a, now a ~a (breaking)" ka kb))) '())
             (cond [(and aa ab (arity-narrowed? aa ab))
                    (list (f 'error 'arity-narrowed name (format "arity ~a → ~a: some old calls now fail (breaking)" (arity-text aa) (arity-text ab))))]
                   [(and aa ab (arity-widened? aa ab))
                    (list (f 'info 'arity-widened name (format "arity ~a → ~a (compatible)" (arity-text aa) (arity-text ab))))]
                   [else '()])
             (if (keywords-narrowed? kwa kwb)
                 (list (f 'error 'keywords-changed name (format "keywords ~s → ~s (breaking)" kwa kwb))) '())
             (if (not (equal? ca cb))
                 (list (f 'warning 'contract-changed name (format "contract ~a → ~a (review compatibility)" (or ca "none") (or cb "none")))) '()))))))
   (for/list ([name (sort (hash-keys n) symbol<?)] #:unless (hash-ref o name #f))
     (f 'info 'added-export name "new export (compatible)"))))
