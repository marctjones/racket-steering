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
         "common.rkt" "store.rkt" "graph.rkt" "lang.rkt" "datalog.rkt")
(provide project-files-multi build-project-graph base-facts helper-facts
         entry-facts entry-admitted-by entry-id->display)

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
(define (build-project-graph root)
  (define files (project-files-multi root))
  (define fs-list
    (for/list ([f files])
      (define g (gate-for-path f))
      (with-handlers ([exn:fail? (λ (e) (file-facts f (gate-name g) '() '() '() #f ""))])
        ((gate-extract g) (file->string (build-path root f)) f))))
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
  (define public-syms
    (for/list ([s syms] #:when (and (gnode-exported? s) (not (regexp-match? #rx"[.]" (gnode-qualname s))))) s))
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
