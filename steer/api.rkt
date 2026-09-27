#lang racket/base
;; Public API lock for Racket modules (catalog F3). A worker process (the installed `racket`, not
;; this binary: it needs every collection, and a separate process can be killed on timeout)
;; instantiates each module and records its phase-0 exports: kind, arity, keywords, contract.
;; Contracts are read inside the module's own namespace: `value-contract` only recognises wrappers
;; made by the same instance of racket/contract (tested; from outside it returns #f).
(require racket/list racket/string racket/file racket/pretty racket/port
         "common.rkt" "checks.rkt" "graph.rkt" "python.rkt" "entries.rkt" "reach.rkt")
(provide api-describe api-diff read-lock write-lock! arity-text
         entry-shapes read-lock-v2 write-lock-v2! entry-shape-diff)

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
;; Lock file v1: ((version 1) (modules (("path" ((name kind arity keywords contract) ...)) ...)))
;; Lock v2 (T67): ((version 2) (entries (("path#qualname" (lang route shape kind)) ...))) - the SAME
;; path, a DIFFERENT route (every entry point across the whole shared graph, any language, rather
;; than a list of modules the caller names) - so read-lock refuses to read a v2 file as v1 data (a
;; clean "no lock" rather than a crash on a shape it does not understand), and vice versa read-lock-v2
;; refuses a v1 file. The two routes are never mixed in one diff: `steer api diff` needs a v1 lock,
;; `steer api diff --entries` needs a v2 one, and each says so plainly when it finds the other.

(define (lock-path root) (build-path root ".steer" "api.lock"))

(define (read-lock-data root)
  (define p (lock-path root))
  (and (file-exists? p) (call-with-input-file p (λ (in) (parameterize ([read-accept-reader #f]) (read in))))))

(define (lock-version d) (and d (let ([v (assq 'version d)]) (and v (cadr v)))))

(define (read-lock root)
  (define d (read-lock-data root))
  (and d (eqv? (lock-version d) 1)
       (for/hash ([m (cadr (assq 'modules d))]) (values (car m) (cadr m)))))

(define (write-lock! root mods)
  (define p (lock-path root))
  (call-with-output-file p #:exists 'truncate
    (λ (o) (parameterize ([pretty-print-columns 100])
             (pretty-write `((version 1) (modules ,(for/list ([k (sort (hash-keys mods) string<?)]) (list k (hash-ref mods k))))) o)))))

;; entries: hash id -> (list lang route shape kind), route = (listof string), the admitting rule(s).
(define (read-lock-v2 root)
  (define d (read-lock-data root))
  (and d (eqv? (lock-version d) 2)
       (for/hash ([e (cadr (assq 'entries d))]) (values (car e) (cadr e)))))

(define (write-lock-v2! root entries)
  (define p (lock-path root))
  (call-with-output-file p #:exists 'truncate
    (λ (o) (parameterize ([pretty-print-columns 100])
             (pretty-write `((version 2) (entries ,(for/list ([k (sort (hash-keys entries) string<?)]) (list k (hash-ref entries k))))) o)))))

;; ---------------------------------------------------------------------------------------------
;; T67: entry shapes, generic across languages - read through the SAME graph-ir `shape` field
;; every extractor already fills (rkt-extract's real formals, py-extract's ast.unparse signature,
;; cs-extract's paramtypes), not three separate lock formats or a second describe pass per language.

;; → hash id -> (list lang route shape kind). `route` is entries.rkt's own admitted-by list for that
;; id (the rule(s) that made it an entry), sorted, so a later diff can tell "still an entry, just a
;; different reason" apart from "not an entry at all any more" if that distinction ever matters.
(define (entry-shapes root #:rules-text [rules-text ""])
  (define-values (g fs-list) (build-project-graph root))
  (define-values (ids admitted-by) (entries-from-graph g fs-list #:rules-text rules-text))
  (for/hash ([id ids])
    (define n (graph-node g id))
    (values id (list (symbol->string (gnode-lang n)) (sort (entry-admitted-by admitted-by id) string<?)
                      (gnode-shape n) (symbol->string (gnode-kind n))))))

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
     ;; keyed by name PLUS its occurrence index among same-named params: C#'s shape has no parameter
     ;; names at all, only types ("int, int"), so every param of the same type collides on a plain
     ;; name key - keying by occurrence too means "the 2nd `int` was removed" still reads as removed,
     ;; not silently absorbed into "nothing changed" because some OTHER same-named param still exists.
     (define (by-pname l)
       (define seen (make-hash))
       (for/hash ([p l])
         (define nm (param-name p))
         (define idx (hash-ref seen nm 0))
         (hash-set! seen nm (add1 idx))
         (values (format "~a#~a" nm idx) p)))
     (define op (by-pname ops))
     (define np (by-pname nps))
     (append
      (for/list ([pn (sort (hash-keys op) string<?)] #:unless (hash-ref np pn #f))
        (f 'error 'param-removed name (format "parameter ~a removed (breaking)" (hash-ref op pn))))
      (for/list ([pn (sort (hash-keys np) string<?)] #:unless (hash-ref op pn #f))
        (if (regexp-match? #rx"=" (hash-ref np pn))
            (f 'info 'param-added name (format "optional parameter ~a added (compatible)" (hash-ref np pn)))
            (f 'error 'param-added name (format "required parameter ~a added (breaking)" (hash-ref np pn)))))
      (for/list ([pn (sort (hash-keys op) string<?)] #:when (hash-ref np pn #f) #:unless (equal? (hash-ref op pn) (hash-ref np pn #f)))
        (f 'warning 'param-changed name (format "parameter ~a → ~a (review annotation/default)" (hash-ref op pn) (hash-ref np pn))))
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

;; ---------------------------------------------------------------------------------------------
;; T67: entry-shape diff, one generic classifier for every language's entries. Reuses api-diff's
;; existing kinds (added-export/param-*) plus two new ones this route makes possible - entry-removed
;; (the symbol is gone from the graph entirely: breaking) vs entry-demoted (the symbol still exists,
;; it just is not admitted as an entry any more - a review, not an automatic breaking claim: it may
;; simply have become truly private, or the heuristic just changed its mind) - and arity-mismatch,
;; located at every in-project call site whose own positional arity no longer fits the new shape.

;; a shape's required-positional count and its max (+inf.0 if it has a *rest/varargs parameter).
(define (shape-arity-range shape)
  (define ps (map string-trim (split-params-text shape)))
  (define required (for/list ([p ps] #:unless (or (regexp-match? #rx"=" p) (regexp-match? #rx"^[*]" p))) p))
  (define has-rest? (ormap (λ (p) (regexp-match? #rx"^[*]" p)) ps))
  (values (length required) (if has-rest? +inf.0 (length ps))))

;; every in-project call site into `id` (an exact or declared edge - name-match is too uncertain to
;; blame a specific caller for) whose own recorded arity no longer fits the new shape's range.
(define (arity-mismatch-findings f id shape g)
  (define-values (lo hi) (shape-arity-range shape))
  (for/list ([e (graph-edges-to g id)]
             #:when (and (memq (gedge-kind e) '(calls references)) (memq (gedge-confidence e) '(exact declared))
                         (gedge-arity e) (or (< (gedge-arity e) lo) (> (gedge-arity e) hi))))
    (f 'error 'arity-mismatch id
       (format "called with ~a argument~a from ~a, but the new shape ~a takes ~a (breaking)"
               (gedge-arity e) (plural (gedge-arity e)) (gedge-from e) shape
               (if (= lo hi) lo (format "~a..~a" lo (if (= hi +inf.0) "*" hi)))))))

;; old, new: hash id -> (list lang route shape kind), from entry-shapes. `graph` is the NEW project's
;; graph (for entry-removed/entry-demoted and arity-mismatch's call-site lookup).
(define (entry-shape-diff graph old new)
  (define (f sev kind id msg [fix #f]) (finding sev kind (format "~a: ~a" id msg) #:file (car (string-split id "#")) #:fix fix))
  (append
   (for/list ([id (sort (hash-keys old) string<?)] #:unless (hash-ref new id #f))
     (if (graph-node graph id)
         (f 'warning 'entry-demoted id "no longer admitted as an entry point (review - it may just be truly private now)")
         (f 'error 'entry-removed id "entry removed from source entirely (breaking)")))
   (append*
    (for/list ([id (sort (hash-keys old) string<?)] #:when (hash-ref new id #f))
      (define oe (hash-ref old id))
      (define ne (hash-ref new id))
      (define old-shape (caddr oe))
      (define new-shape (caddr ne))
      (append (python-shape-diff f id old-shape new-shape)
              (if (equal? old-shape new-shape) '() (arity-mismatch-findings f id new-shape graph)))))
   (for/list ([id (sort (hash-keys new) symbol<? #:key string->symbol)] #:unless (hash-ref old id #f))
     (f 'info 'added-export id "new entry point (compatible)"))))
