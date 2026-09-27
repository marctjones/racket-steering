#lang racket/base
;; Entry points (T64): one generic engine over the shared graph. Every language plugs in with two
;; things from its lang.rkt gate - entry-prelude (Datalog rules over the base/helper facts below) and
;; implicit-names (bare names the language itself calls without a visible ref anywhere in source) -
;; and nothing else here ever special-cases a language by name.
;;
;; Base facts (module/symbol/in_module/name/kind/decorated/member_of/base_of/exported/requires/uses/
;; layer) come straight from the graph and file-facts, language-blind. The 9 Racket-computed helpers
;; (test_module/test_name/console_script/package_init/controller_base/public_api/api_module/
;; root_module/has_statements) exist because pattern-matching a name or a path is not something
;; Datalog itself can do; a language's prelude combines them with plain Datalog rules. Two engine-
;; level rules apply to every language identically: `steer: entry` markers (explicit_entry) and
;; implicit-names (implicit_name) - the goal T64 is named for: adding a language's entry heuristics
;; is adding its prelude table, not new engine code.
(require racket/list racket/string racket/file racket/path
         "common.rkt" "store.rkt" "graph.rkt" "lang.rkt" "datalog.rkt" "graph-cache.rkt"
         (only-in "python.rkt" py-extract-batch))
(provide project-files-multi build-project-graph base-facts helper-facts
         entry-facts entries-from-graph entry-admitted-by entry-id->display)

;; ---------------------------------------------------------------------------------------------
;; gathering: every file any gate can extract, across every language present in the project

(define ignored-dirs '("compiled" ".git" ".steer" "node_modules" "dist" "build" ".claude" "samples"))

(define (project-files-multi root)
  (sort (for/list ([f (in-directory root (λ (d) (not (member (let-values ([(_b n _d) (split-path d)]) (path->string n)) ignored-dirs))))]
                   #:when (and (file-exists? f) (gate-for-path f) (gate-extract (gate-for-path f))))
          (path->string (find-relative-path (simplify-path root) (simplify-path f))))
        string<?))

;; → (values graph file-facts-list). A file whose gate fails to extract it gets an empty file-facts
;; (never dropped, per file-facts' own contract), same as module-facts already does for Racket alone.
;; T66: a per-file cache, keyed by the file's own raw-content sha1, one shared .rktd format for every
;; language - a cache HIT costs a hash and a read, never a worker process or a scanner pass. Python's
;; own extractor batches every cache MISS into one process call (py-extract-batch, T62), rather than
;; one process per file - the one place this module knows a language's name, as a pure performance
;; path: correctness and the cache format are identical either way, and every other language's cache
;; misses still go through the plain per-file `gate-extract` the six-function contract already gives.
(define (bytes->string/utf-8-safe bs) (bytes->string/utf-8 bs #\?))

(define (build-project-graph root)
  (define files (project-files-multi root))
  (define bytes-of (for/hash ([f files]) (values f (with-handlers ([exn:fail? (λ (e) #"")]) (file->bytes (build-path root f))))))
  (define (bytes-for f) (hash-ref bytes-of f #""))
  (define lang-of (for/hash ([f files]) (values f (gate-name (gate-for-path f)))))
  (define sha-of (for/hash ([f files]) (values f (content-sha1 (bytes-for f)))))
  (define (cache-p f) (cache-path root (hash-ref lang-of f) (hash-ref sha-of f)))
  (define cache-hits (for/hash ([f files]) (values f (cache-read (cache-p f)))))
  (define cached (for/hash ([f files] #:when (hash-ref cache-hits f)) (values f (hash-ref cache-hits f))))
  (define misses (filter (λ (f) (not (hash-ref cached f #f))) files))
  (define python-misses (filter (λ (f) (eq? (hash-ref lang-of f) 'python)) misses))
  (define other-misses (filter (λ (f) (not (eq? (hash-ref lang-of f) 'python))) misses))
  (define python-extracted
    (if (null? python-misses)
        (hash)
        (let ()
          (record-extract-launch!)     ; ONE launch for the whole batch, however many files
          (for/hash ([f python-misses] [ff (py-extract-batch (for/list ([f python-misses]) (cons f (bytes->string/utf-8-safe (bytes-for f)))))])
            (values f ff)))))
  (define other-extracted
    (for/hash ([f other-misses])
      (define g (gate-for-path f))
      (record-extract-launch!)
      (values f (with-handlers ([exn:fail? (λ (e) (file-facts f (gate-name g) '() '() '() #f ""))])
                  ((gate-extract g) (bytes->string/utf-8-safe (bytes-for f)) f)))))
  (for ([f misses])
    (cache-write! (cache-p f) (hash-ref (if (eq? (hash-ref lang-of f) 'python) python-extracted other-extracted) f)))
  (define fs-list (for/list ([f files]) (or (hash-ref cached f #f) (hash-ref python-extracted f #f) (hash-ref other-extracted f #f))))
  (define (dispatch-resolve-import lang spec importing-path root2 all-paths)
    (define g (gate-for-path importing-path))
    (define ri (and g (gate-resolve-import g)))
    ((or ri default-resolve-import) lang spec importing-path root2 all-paths))
  (values (link-facts fs-list #:root root #:resolve-import dispatch-resolve-import) fs-list))

;; ---------------------------------------------------------------------------------------------
;; base facts: language-blind, straight off the graph/file-facts

(define (bare n) (last (string-split n ".")))

(define (base-facts g fs-list layers)
  (define syms (filter (λ (n) (not (eq? (gnode-kind n) 'module))) (graph-nodes g)))
  (define mods (filter (λ (n) (eq? (gnode-kind n) 'module)) (graph-nodes g)))
  (append
   (for/list ([m mods]) (list 'module (gnode-path m)))
   ;; lang(ID, L): every node (module or symbol) tagged with its own language - so a language's own
   ;; entry-prelude rules can constrain themselves to their own files and never match another
   ;; language's symbols just because a generic helper (public_api, root_module, ...) is language-blind.
   (for/list ([n (graph-nodes g)]) (list 'lang (gnode-id n) (symbol->string (gnode-lang n))))
   (for/list ([s syms]) (list 'symbol (gnode-id s)))
   (for/list ([s syms]) (list 'in_module (gnode-id s) (gnode-path s)))
   (for/list ([s syms]) (list 'name (gnode-id s) (bare (gnode-name s))))
   (for/list ([s syms]) (list 'kind (gnode-id s) (symbol->string (gnode-kind s))))
   (append* (for/list ([s syms]) (for/list ([d (gnode-decorators s)]) (list 'decorated (gnode-id s) d))))
   (append* (for/list ([s syms]) (for/list ([b (gnode-bases s)]) (list 'base_of (gnode-id s) b))))
   (append* (for/list ([s syms] #:when (enclosing-id s)) (list (list 'member_of (gnode-id s) (enclosing-id s)))))
   (for/list ([s syms] #:when (gnode-entry? s)) (list 'explicit_entry (gnode-id s)))
   (for/list ([s syms] #:when (gnode-exported? s)) (list 'exported (gnode-id s)))
   (for*/list ([e (graph-edges g)] #:when (eq? (gedge-kind e) 'imports)) (list 'requires (gedge-from e) (gedge-to e)))
   (for*/list ([f fs-list] [im (file-facts-imports f)]
               #:when (null? ((or (gate-resolve-import (gate-for-path (file-facts-path f))) default-resolve-import)
                              (file-facts-lang f) (import-spec im) (file-facts-path f) "." (map file-facts-path fs-list))))
     (list 'uses (file-facts-path f) (import-spec im)))
   (for*/list ([f fs-list] [l layers] #:when (regexp-match? (cdr l) (file-facts-path f))) (list 'layer (file-facts-path f) (car l)))))

;; the enclosing type's gnode-id, derived from a symbol's own dotted qualname (gnode itself carries no
;; separate `scope` field - that lives on the per-file `def` this node was built from, before linking).
;; "Dog.Speak" in "Shapes.cs" -> "Shapes.cs#Dog"; a top-level symbol ("area") has no dotted prefix -> #f.
(define (enclosing-id s)
  (define segs (string-split (gnode-qualname s) "."))
  (and (> (length segs) 1) (symbol-id (gnode-path s) (string-join (drop-right segs 1) "."))))

;; ---------------------------------------------------------------------------------------------
;; the 9 Racket-computed helpers

(define test-path-rx #px"(?i:(^|[/_-])tests?([/_-]|\\.[a-z]+$)|_test\\.[a-z]+$|test_[^/]+\\.[a-z]+$)")
(define test-name-rx #px"(?i:^test[_A-Z]|^Test[A-Za-z]|Test$|Tests$)")

(define (test_module? path) (regexp-match? test-path-rx path))
(define (test_name? bare-name) (regexp-match? test-name-rx bare-name))
(define (console_script? bare-name) (string-ci=? bare-name "main"))
(define (package_init? path) (equal? (last (string-split (path->fwd path) "/")) "__init__.py"))
(define (path->fwd p) (string-replace (if (path? p) (path->string p) p) "\\" "/"))
(define controller-rx #px"(?i:Controller$)")

(define (helper-facts g fs-list)
  (define syms (filter (λ (n) (not (eq? (gnode-kind n) 'module))) (graph-nodes g)))
  (define mods (filter (λ (n) (eq? (gnode-kind n) 'module)) (graph-nodes g)))
  (define has-stmt-paths (for/list ([f fs-list] #:when (file-facts-has-statements? f)) (file-facts-path f)))
  (define imported-paths (remove-duplicates (for/list ([e (graph-edges g)] #:when (eq? (gedge-kind e) 'imports)) (gedge-to e))))
  ;; a top-level exported symbol, OR an exported member of an exported class - C# has no "top-level"
  ;; functions at all (every def nests inside a class), so requiring "no dot in qualname" alone (true
  ;; for Python/Racket) would mean public_api NEVER matches a single C# symbol. Found measuring T68
  ;; on a real C# library (GuardClauses, whose whole purpose IS its public static API): 838 of 841
  ;; symbols showed up "dead" before this fix, because nothing ever admitted them as entries at all.
  (define (id->node id) (findf (λ (n) (equal? (gnode-id n) id)) syms))
  (define public-syms
    (for/list ([s syms]
               #:when (and (gnode-exported? s)
                           (let ([enc (enclosing-id s)]) (or (not enc) (let ([c (id->node enc)]) (and c (gnode-exported? c)))))))
      s))
  (define api-mod-paths (remove-duplicates (map gnode-path public-syms)))
  (define controller-classes
    (for/list ([s syms] #:when (and (memq (gnode-kind s) '(class struct))
                                     (or (regexp-match? controller-rx (bare (gnode-name s)))
                                         (ormap (λ (b) (regexp-match? controller-rx b)) (gnode-bases s)))))
      s))
  (append
   (for/list ([m mods] #:when (test_module? (gnode-path m))) (list 'test_module (gnode-path m)))
   (for/list ([s syms] #:when (test_name? (bare (gnode-name s)))) (list 'test_name (gnode-id s)))
   (for/list ([s syms] #:when (console_script? (bare (gnode-name s)))) (list 'console_script (gnode-id s)))
   (for/list ([m mods] #:when (package_init? (gnode-path m))) (list 'package_init (gnode-path m)))
   (for/list ([s controller-classes]) (list 'controller_base (gnode-id s)))
   (for/list ([s public-syms]) (list 'public_api (gnode-id s)))
   (for/list ([p api-mod-paths]) (list 'api_module p))
   (for/list ([m mods] #:unless (member (gnode-path m) imported-paths)) (list 'root_module (gnode-path m)))
   (for/list ([p has-stmt-paths]) (list 'has_statements p))))

;; ---------------------------------------------------------------------------------------------
;; the engine: explicit markers + implicit-names apply to every language identically; each active
;; gate's own entry-prelude runs alongside them, all over the SAME base+helper fact base.

(define engine-rules-text #<<DL
entry(S) :- explicit_entry(S).
entry(S) :- name(S, N), implicit_name(N).
DL
  )

(define (active-gates fs-list) (remove-duplicates (filter values (map (λ (f) (gate-for-path (file-facts-path f))) fs-list)) #:key gate-name))

;; → (values entry-ids admitted-by) ; admitted-by: hash entry-id -> (listof source-tag), source-tag one
;; of "marker" | "implicit" | a language name (symbol) | "user". Each source is evaluated on its own
;; (against the SAME shared fact base) so a per-entry admitting label is available - the simplification
;; this costs is that two DIFFERENT sources' rules cannot chain off EACH OTHER's derived entry(...)
;; tuples (a prelude referencing only the base/helper facts, never another source's `entry`, is
;; unaffected; recorded as a design note, not silently assumed away).
(define (entry-facts root #:rules-text [user-rules-text ""])
  (define-values (g fs-list) (build-project-graph root))
  (entries-from-graph g fs-list #:rules-text user-rules-text))

;; Split out so a caller that ALSO needs the graph for something else (T65's reachability) builds it
;; once and reuses it here, rather than extracting the whole project twice.
(define (entries-from-graph g fs-list #:rules-text [user-rules-text ""])
  (define layers (with-handlers ([exn:fail? (λ (e) '())]) (parse-layer-directives-of user-rules-text)))
  (define base (append (base-facts g fs-list layers) (helper-facts g fs-list)))
  (define gates (active-gates fs-list))
  (define implicit-facts (append* (for/list ([gt gates] #:when (gate-implicit-names gt)) (for/list ([n (gate-implicit-names gt)]) (list 'implicit_name n)))))
  (define all-facts (append base implicit-facts))
  (define sources
    (append
     (list (cons "marker" "entry(S) :- explicit_entry(S)."))
     (list (cons "implicit" "entry(S) :- name(S, N), implicit_name(N)."))
     (for/list ([gt gates] #:when (gate-entry-prelude gt)) (cons (symbol->string (gate-name gt)) (gate-entry-prelude gt)))
     (list (cons "user" user-rules-text))))
  (define admitted-by (make-hash))
  (for ([src sources])
    (define text (cdr src))
    (define rules
      (with-handlers ([exn:fail? (λ (e) '())])
        (define-values (_f rs _q) (parse-rules text (car src)))
        rs))
    (when (pair? rules)
      (define rels (eval-datalog all-facts rules))
      (for ([t (tuples-of rels 'entry)])
        (hash-update! admitted-by (car t) (λ (l) (cons (car src) l)) '()))))
  (values (sort (hash-keys admitted-by) string<?) admitted-by))

;; `%layer NAME GLOB` lines in the SAME rules.dl format architecture rules use (self-contained here so
;; entries.rkt never requires rules.rkt - the opposite direction avoids a require cycle).
(define (parse-layer-directives-of text)
  (for*/list ([line (string-split text "\n")]
              [m (in-value (regexp-match #px"^\\s*%\\s*layer\\s+(\\S+)\\s+(\\S+)\\s*$" line))]
              #:when m)
    (cons (cadr m) (glob-rx (caddr m)))))
(define (glob-rx g)
  (define body
    (let loop ([cs (string->list g)] [acc '()])
      (cond [(null? cs) (apply string-append (reverse acc))]
            [(and (char=? (car cs) #\*) (pair? (cdr cs)) (char=? (cadr cs) #\*))
             (if (and (pair? (cddr cs)) (char=? (caddr cs) #\/))
                 (loop (cdddr cs) (cons "(?:.*/)?" acc))
                 (loop (cddr cs) (cons ".*" acc)))]
            [(char=? (car cs) #\*) (loop (cdr cs) (cons "[^/]*" acc))]
            [(char=? (car cs) #\?) (loop (cdr cs) (cons "[^/]" acc))]
            [else (loop (cdr cs) (cons (regexp-quote (string (car cs))) acc))])))
  (pregexp (string-append "^" body "$")))

(define (entry-admitted-by admitted-by id) (hash-ref admitted-by id '()))
(define (entry-id->display id) id)
