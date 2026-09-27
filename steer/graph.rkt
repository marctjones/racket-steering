#lang racket/base
;; The generic code graph (milestone XL3). Two layers:
;;
;;   file-facts (per file, UNRESOLVED): what one file's extractor saw, in its own vocabulary — defs,
;;   refs (calls/inherits/implements/decorates), imports, and a couple of file-level flags. Every
;;   language's extractor (T60 Racket, T62 Python, T63 C#) emits exactly this shape; nothing else in
;;   this module, or in rules.rkt/reach.rkt above it, ever special-cases a language by name again.
;;
;;   graph (project-level, RESOLVED): gnode/gedge, built by `link-facts` from a (listof file-facts).
;;   Every edge carries a confidence tag - `exact` (unambiguous, same-file or type-declared target),
;;   `declared` (resolved through an import or a base-class chain), or `name-match` (no import or
;;   scope connects caller to callee; they just share a name, so it is one edge per name collision -
;;   an intentional may-call over-approximation, never a claim of precision no static analysis
;;   without a real compiler can back: notes/12, notes/16). A ref that matches nothing becomes an
;;   `external` edge (to = #f) rather than being dropped, so `dead` code (T65) never flags something
;;   we simply failed to resolve as unreachable - unresolved is not the same claim as dead.
;;
;; This module never reads a file or shells out; it only links facts someone else produced.
(require racket/list racket/string racket/match racket/file)
(provide (struct-out def) (struct-out ref) (struct-out import) (struct-out file-facts)
         (struct-out gnode) (struct-out gedge) (struct-out graph)
         module-id link-facts
         graph-node graph-nodes-in graph-edges-from graph-edges-to graph-stats-for
         write-graph read-graph)

;; ===============================================================================================
;; file-facts: the per-language extractor contract (T59-T63)

;; kind: 'module | 'class | 'interface | 'function | 'method | 'constructor | 'property | 'field
;;       | 'variable | 'const | 'macro | 'struct | 'enum  (each extractor uses the subset that fits
;;       its language; nothing here requires every language to produce every kind)
;; name: the simple name as written · qualname: fully qualified within the file (dotted; equal to
;; `name` for a top-level def) · scope: the enclosing def's qualname, or #f at file top level
;; shape: the language's own signature text, read through by T67's entry-shape lock
;; hash: a formatting-insensitive hash over the definition (same algorithm resolve-anchor already
;; uses for that language, so a graph def and an anchor baseline for the same symbol always agree)
;; bases: base classes / interfaces this def extends or implements, as written (unresolved names)
;; decorators: decorator/attribute names on this def, as written
;; entry?: an explicit `steer: entry` marker was found immediately above this def
;; exported?: this language's own idea of "publicly visible" (provide, __all__, public modifier, ...)
(struct def (kind name qualname scope line end shape hash bases decorators entry? exported?) #:prefab)

;; kind: 'call | 'reference | 'inherits | 'implements | 'decorates
;; name: the name as written, possibly dotted (e.g. "self.foo", "Base.M", "pkg.mod.f")
;; receiver-hint: 'self | 'base | 'static | #f — disambiguates self.foo()/base.foo()/Type.M() from a
;; plain call; extractors that have no receiver concept (Racket) always pass #f
;; scope: the enclosing def's qualname this reference occurs in, or #f at file top level
;; arity: the call site's positional argument count, or #f when not a call or not countable
(struct ref (kind name receiver-hint scope arity line) #:prefab)

;; spec: the raw specifier as written (dotted module path / namespace / relative require path)
;; alias: the local alias/binding this import introduces, or #f
(struct import (spec alias line) #:prefab)

;; path: project-relative, forward-slashed · lang: 'racket | 'python | 'csharp | ...
;; has-statements?: the file has executable top-level code outside any def (Python/Racket "this file
;; is also a script" signal that T64's entry-prelude reads) · content-hash: sha1 of the raw file text
;; (T66's cache key; independent of `def`'s per-symbol hash, which ignores formatting)
(struct file-facts (path lang defs refs imports has-statements? content-hash) #:prefab)

;; ===============================================================================================
;; graph: the resolved, project-level IR

;; id: "path" for a module node, "path#qualname" for a symbol node
;; kind/name/qualname/line/end/shape/hash/bases/decorators/entry?/exported?/lang: carried over from
;; the originating `def` (kind='module for the file node itself, using file-facts' own fields)
(struct gnode (id kind path name qualname line end shape hash bases decorators entry? exported? lang) #:prefab)

;; from/to: gnode ids; `to` is #f exactly when external? is #t
;; kind: 'calls | 'references | 'inherits | 'implements | 'decorates | 'imports | 'defines | 'overrides
;; confidence: 'exact | 'declared | 'name-match  (meaningless, but present as 'exact, for 'defines/'imports)
(struct gedge (from to kind confidence arity external?) #:prefab)

(struct graph (nodes edges stats) #:prefab)

(define (module-id path) (path->fwd path))
(define (symbol-id path qualname) (string-append (path->fwd path) "#" qualname))
(define (path->fwd p) (string-replace (if (path? p) (path->string p) p) "\\" "/"))

;; ===============================================================================================
;; Linking

;; link-facts : (listof file-facts) #:root path-or-string
;;              #:resolve-import (lang import-spec importing-path root all-paths -> (listof path) or #f)
;;              -> graph
;; `resolve-import` defaults to a generic (language-blind) heuristic: match the spec's last dotted/
;; slashed segment against project file stems. A real per-language resolver (a lang.rkt gate's
;; `resolve-import` slot, wired in by T61) is strictly more precise and should be passed in by the
;; caller; this default exists so the linker is testable, and usable, with zero per-language code.
(define (link-facts facts #:root [root "."] #:resolve-import [resolve-import default-resolve-import])
  (define all-paths (map file-facts-path facts))
  ;; ---- pass 1: nodes ---------------------------------------------------------------------------
  (define module-nodes
    (for/list ([f facts])
      (gnode (module-id (file-facts-path f)) 'module (file-facts-path f)
             (file-facts-path f) (file-facts-path f) 1 1 #f #f '() '() #f #t (file-facts-lang f))))
  (define (def->gnode f d)
    (gnode (symbol-id (file-facts-path f) (def-qualname d)) (def-kind d) (file-facts-path f)
           (def-name d) (def-qualname d) (def-line d) (def-end d) (def-shape d) (def-hash d)
           (def-bases d) (def-decorators d) (def-entry? d) (def-exported? d) (file-facts-lang f)))
  (define def-nodes (for*/list ([f facts] [d (file-facts-defs f)]) (def->gnode f d)))
  (define nodes (append module-nodes def-nodes))
  ;; ---- indexes -----------------------------------------------------------------------------------
  (define by-id (for/hasheq ([n nodes]) (values (string->symbol (gnode-id n)) n)))
  (define (node-by-id id) (hash-ref by-id (string->symbol id) #f))
  ;; defs local to one file, by qualname and by bare (rightmost segment) name
  ;; string-keyed: for/hash (equal?-based), not for/hasheq — paths are not guaranteed eq? across
  ;; construction (file-facts literals) and lookup (freshly-built strings from resolve-import et al).
  (define file->defs (for/hash ([f facts]) (values (file-facts-path f) (file-facts-defs f))))
  (define (defs-of path) (hash-ref file->defs path '()))
  (define (find-in-file path qualname)
    (findf (λ (d) (equal? (def-qualname d) qualname)) (defs-of path)))
  (define (bare name) (last (string-split name ".")))
  (define (find-by-bare-in-file path name)
    (filter (λ (d) (equal? (bare (def-qualname d)) (bare name))) (defs-of path)))
  ;; every def project-wide, by bare name (name-match fallback) and by qualname (cross-file exact/base lookups)
  (define all-defs-flat (for*/list ([f facts] [d (file-facts-defs f)]) (cons f d)))
  (define by-bare (make-hash))
  (for ([fd all-defs-flat]) (hash-update! by-bare (bare (def-qualname (cdr fd))) (λ (l) (cons fd l)) '()))
  (define (project-name-matches name) (hash-ref by-bare (bare name) '()))
  ;; classes, so bases/self/base lookups can walk an inheritance chain
  (define classes (filter (λ (fd) (memq (def-kind (cdr fd)) '(class struct interface))) all-defs-flat))
  (define (class-fd-named path name)
    (or (findf (λ (fd) (and (equal? (file-facts-path (car fd)) path) (equal? (def-qualname (cdr fd)) name))) classes)
        (findf (λ (fd) (equal? (bare (def-qualname (cdr fd))) (bare name))) classes)))
  (define (enclosing-class f scope)
    ;; scope is the qualname of the def a ref/member sits in. That IS the class itself when the def
    ;; is a direct member (a method's own `scope` field, e.g. "Dog"); it is deeper, with the class's
    ;; qualname as a strict dotted prefix, when scope is a call site nested inside a method's own body
    ;; (e.g. a ref with scope "Dog.speak"). Either way, the deepest matching class wins (nested classes).
    (and scope
         (for/fold ([best #f]) ([fd classes] #:when (equal? (file-facts-path (car fd)) (file-facts-path f)))
           (define cq (def-qualname (cdr fd)))
           (if (and (or (equal? scope cq) (string-prefix? scope (string-append cq ".")))
                    (or (not best) (> (string-length cq) (string-length (def-qualname (cdr best))))))
               fd best))))
  ;; imports: resolved once per file to a list of target paths
  (define (imports-of f)
    (for/list ([im (file-facts-imports f)])
      (cons im (or (resolve-import (file-facts-lang f) (import-spec im) (file-facts-path f) root all-paths) '()))))
  (define file->import-targets (for/hash ([f facts]) (values (file-facts-path f) (imports-of f))))
  (define (imported-paths-of path) (remove-duplicates (append-map cdr (hash-ref file->import-targets path '()))))
  (define (find-in-imports path qualname-or-bare)
    (for*/first ([ip (imported-paths-of path)]
                 [d (defs-of ip)]
                 #:when (or (equal? (def-qualname d) qualname-or-bare) (equal? (bare (def-qualname d)) (bare qualname-or-bare))))
      (cons ip d)))
  ;; ---- pass 2: edges ------------------------------------------------------------------------------
  (define edges '())
  (define (emit! e) (set! edges (cons e edges)))
  ;; module -> its own top-level defs, and class -> its own members (covers "class reachable => its
  ;; constructors" as a special case of forward `defines`)
  (for ([f facts])
    (for ([d (file-facts-defs f)])
      (define from (if (def-scope d) (symbol-id (file-facts-path f) (def-scope d)) (module-id (file-facts-path f))))
      (when (node-by-id from)
        (emit! (gedge from (symbol-id (file-facts-path f) (def-qualname d)) 'defines 'exact #f #f)))))
  ;; imports: module -> module
  (for ([f facts])
    (for ([im+targets (hash-ref file->import-targets (file-facts-path f) '())])
      (for ([tgt (cdr im+targets)])
        (when (member tgt all-paths)
          (emit! (gedge (module-id (file-facts-path f)) (module-id tgt) 'imports 'declared #f #f))))))
  ;; inherits/implements, from each def's `bases`
  (for ([f facts])
    (for ([d (file-facts-defs f)] #:when (pair? (def-bases d)))
      (define from-id (symbol-id (file-facts-path f) (def-qualname d)))
      (for ([base-name (def-bases d)])
        (define local (find-in-file (file-facts-path f) base-name))
        (define imported (and (not local) (find-in-imports (file-facts-path f) base-name)))
        (define global (and (not local) (not imported) (project-name-matches base-name)))
        (define ekind (if (eq? (def-kind d) 'interface) 'implements 'inherits))
        (cond
          [local (emit! (gedge from-id (symbol-id (file-facts-path f) (def-qualname local)) ekind 'exact #f #f))]
          [imported (emit! (gedge from-id (symbol-id (car imported) (def-qualname (cdr imported))) ekind 'declared #f #f))]
          [(pair? global)
           (for ([fd global]) (emit! (gedge from-id (symbol-id (file-facts-path (car fd)) (def-qualname (cdr fd))) ekind 'name-match #f #f)))]
          [else (emit! (gedge from-id #f ekind 'name-match #f #t))]))))
  ;; overrides: a method overrides an ancestor's same-bare-name method, forward edge child -> parent
  ;; (T65 walks this BACKWARDS: a reachable base method's overriders are reachable too, since a call
  ;; on the base type may dynamically dispatch to any of them)
  (for ([f facts])
    (for ([d (file-facts-defs f)] #:when (memq (def-kind d) '(method constructor property)))
      (define cls (enclosing-class f (def-scope d)))
      (when cls
        (for ([base-name (def-bases (cdr cls))])
          (define base-cls (or (find-in-file (file-facts-path f) base-name) (find-in-imports (file-facts-path f) base-name)))
          (define base-cls-path+qn
            (cond [(and base-cls (def? base-cls)) (cons (file-facts-path f) base-cls)]
                  [(pair? base-cls) base-cls]
                  [else #f]))
          (when base-cls-path+qn
            (define bp (car base-cls-path+qn)) (define bq (def-qualname (cdr base-cls-path+qn)))
            (define base-member (findf (λ (bd) (equal? (bare (def-qualname bd)) (bare (def-qualname d)))) (defs-of bp)))
            (when (and base-member (not (equal? (def-qualname base-member) (def-qualname d))))
              (emit! (gedge (symbol-id (file-facts-path f) (def-qualname d)) (symbol-id bp (def-qualname base-member)) 'overrides 'declared #f #f))))))))
  ;; calls/references/decorates: resolved by scope -> self/base receiver -> same-file bare match ->
  ;; imports -> project-wide name-match -> external
  (for ([f facts])
    (for ([r (file-facts-refs f)])
      (define from-id (if (ref-scope r) (symbol-id (file-facts-path f) (ref-scope r)) (module-id (file-facts-path f))))
      (define ekind (case (ref-kind r) [(inherits) 'inherits] [(implements) 'implements] [(decorates) 'decorates] [(reference) 'references] [else 'calls]))
      (cond
        [(not (node-by-id from-id)) (void)] ; a ref whose own scope def didn't survive extraction; nothing to hang it on
        [(memq (ref-receiver-hint r) '(self base))
         (define cls (enclosing-class f (ref-scope r)))
         (define target
           (and cls
                (if (eq? (ref-receiver-hint r) 'self)
                    (or (find-in-file (file-facts-path f) (string-append (def-qualname (cdr cls)) "." (bare (ref-name r))))
                        (findf (λ (d) (and (equal? (bare (def-qualname d)) (bare (ref-name r))) (string-prefix? (def-qualname d) (string-append (def-qualname (cdr cls)) ".")))) (defs-of (file-facts-path f))))
                    #f)))
         (cond
           [target (emit! (gedge from-id (symbol-id (file-facts-path f) (def-qualname target)) ekind 'exact (ref-arity r) #f))]
           [cls
            ;; base.foo(), or self.foo() not found on this class: walk the base-class chain (declared)
            (define found
              (for/first ([base-name (def-bases (cdr cls))]
                          #:when (let* ([bp+d (or (find-in-file (file-facts-path f) base-name) (find-in-imports (file-facts-path f) base-name))])
                                   (and bp+d #t)))
                (define local (find-in-file (file-facts-path f) base-name))
                (define bp+d (or (and local (cons (file-facts-path f) local)) (find-in-imports (file-facts-path f) base-name)))
                (and bp+d (findf (λ (bd) (equal? (bare (def-qualname bd)) (bare (ref-name r)))) (defs-of (car bp+d)))
                     (cons (car bp+d) (findf (λ (bd) (equal? (bare (def-qualname bd)) (bare (ref-name r)))) (defs-of (car bp+d)))))))
            (if found
                (emit! (gedge from-id (symbol-id (car found) (def-qualname (cdr found))) ekind 'declared (ref-arity r) #f))
                (emit! (gedge from-id #f ekind 'name-match (ref-arity r) #t)))]
           [else (emit! (gedge from-id #f ekind 'name-match (ref-arity r) #t))])]
        [else
         (define local (or (find-in-file (file-facts-path f) (ref-name r)) (let ([m (find-by-bare-in-file (file-facts-path f) (ref-name r))]) (and (= (length m) 1) (car m))))
                    )
         (define imported (and (not local) (find-in-imports (file-facts-path f) (ref-name r))))
         (define global (and (not local) (not imported) (project-name-matches (ref-name r))))
         (cond
           [local (emit! (gedge from-id (symbol-id (file-facts-path f) (def-qualname local)) ekind 'exact (ref-arity r) #f))]
           [imported (emit! (gedge from-id (symbol-id (car imported) (def-qualname (cdr imported))) ekind 'declared (ref-arity r) #f))]
           [(pair? global)
            (for ([fd global]) (emit! (gedge from-id (symbol-id (file-facts-path (car fd)) (def-qualname (cdr fd))) ekind 'name-match (ref-arity r) #f)))]
           [else (emit! (gedge from-id #f ekind 'name-match (ref-arity r) #t))])])))
  (define final-edges (reverse edges))
  (graph nodes final-edges (compute-stats facts final-edges)))

;; A generic (language-blind) default: match an import spec's last segment against project files'
;; basenames (without extension). Good enough for same-directory Racket/test fixtures; a real
;; resolver (a gate's `resolve-import`) should be preferred once one exists (T60/T62/T63).
(define (default-resolve-import lang spec importing-path root all-paths)
  (define seg (last (string-split (string-replace spec "\\" "/") "/")))
  (define seg2 (last (string-split seg ".")))
  (filter (λ (p) (let ([stem (car (string-split (last (string-split (path->fwd p) "/")) "."))])
                   (or (equal? stem seg) (equal? stem seg2))))
          all-paths))

(define (compute-stats facts edges)
  (define langs (remove-duplicates (map file-facts-lang facts)))
  (for/hasheq ([l langs])
    (define fs (filter (λ (f) (eq? (file-facts-lang f) l)) facts))
    (define paths (map file-facts-path fs))
    (define es (filter (λ (e) (member (car (string-split (gedge-from e) "#")) paths)) edges))
    (values l (hasheq 'files (length fs)
                       'defs (for/sum ([f fs]) (length (file-facts-defs f)))
                       'refs (for/sum ([f fs]) (length (file-facts-refs f)))
                       'edges (length es)
                       'exact (count (λ (e) (eq? (gedge-confidence e) 'exact)) es)
                       'declared (count (λ (e) (eq? (gedge-confidence e) 'declared)) es)
                       'name-match (count (λ (e) (eq? (gedge-confidence e) 'name-match)) es)
                       'external (count gedge-external? es)))))

;; ===============================================================================================
;; Read-side helpers

(define (graph-node g id) (findf (λ (n) (equal? (gnode-id n) id)) (graph-nodes g)))
(define (graph-nodes-in g path) (filter (λ (n) (equal? (gnode-path n) path)) (graph-nodes g)))
(define (graph-edges-from g id) (filter (λ (e) (equal? (gedge-from e) id)) (graph-edges g)))
(define (graph-edges-to g id) (filter (λ (e) (equal? (gedge-to e) id)) (graph-edges g)))
(define (graph-stats-for g lang) (hash-ref (graph-stats g) lang #f))

;; ===============================================================================================
;; .rktd round-trip: prefab structs print and read back as themselves, so this is just write/read.

(define (write-graph g path) (call-with-output-file path #:exists 'truncate (λ (o) (write g o))))
(define (read-graph path) (call-with-input-file path read))
