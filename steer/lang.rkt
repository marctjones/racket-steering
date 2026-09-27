#lang racket/base
;; Language routing (catalog A1, milestone XL1; consolidated T58/XL3). One registry maps a file
;; extension to the ONE record that understands that language, so a Python edit is checked by
;; Python's own parser and a C# edit by a C# scanner instead of being fed to the Racket reader (which
;; reported confident, wrong errors on valid .py/.cs files: note 12). A new language is one entry in
;; `gates`. A file whose extension has no gate is reported as *skipped*, never as clean or broken.
;;
;; T58 folds anchors.rkt's separate racket/python/csharp conds into this same record: every language
;; implements the SAME contract (a gate's slots below), and code outside this module never special-cases
;; a language by name again. T59-T64 add real implementations for `extract`/`resolve-import`/
;; `entry-prelude`/`implicit-names`/`dynamic-calls?`; until a language has one, that slot is #f and
;; callers must treat #f as "not available for this language" (report skipped, never wrong) rather
;; than an error.
;;
;; A gate's `check` takes (text file) and returns (values findings unit-count lang), the same shape as
;; syntax-check.rkt's `check-source`. Findings may carry a machine-applicable `edit`; an edit is only
;; applied by `steer syntax --fix` when it is marked verified (the file checks clean afterwards).
;; `find-anchor` takes (text name) and returns the resolve-anchor hasheq shape (found?/line/end/hash/
;; kind/shadowed, or found?=#f/problem/candidates). `list-names` takes (text) and returns every
;; definable name, used only for did-you-mean (never raises).
(require racket/list racket/string racket/path
         "common.rkt" "syntax-check.rkt" "srcread.rkt" "python.rkt" "csharp.rkt" "rkt-extract.rkt" "cs-extract.rkt")
(provide (struct-out gate) gates gate-for-path lang-check lang-find-anchor lang-list-names
         supported-extensions)

;; name: symbol · exts: lowercase extensions with the dot · unit: what `check` counts ("form", "statement", ...)
;; check: (text file) -> (values findings unit-count lang)
;; find-anchor: (text name) -> hasheq (resolve-anchor shape) · list-names: (text) -> (listof string)
;; extract: (text file) -> file-facts, or #f if this language has no graph extractor yet (T59-T63)
;; resolve-import: (import-spec importing-file root) -> (listof path), or #f (T59-T63)
;; entry-prelude: Datalog rule text (string) contributing entry(...) facts, or #f (T64)
;; implicit-names: (listof string) of call names this language dispatches to implicitly, or #f (T64)
;; dynamic-calls?: #t if this language's calls can't be name-resolved with confidence (T59+, e.g. C# reflection)
(struct gate (name exts unit check find-anchor list-names
              extract resolve-import entry-prelude implicit-names dynamic-calls?))

;; entry-prelude (T64): Datalog rules run before the user's own, over the generic base/helper facts
;; entries.rkt builds (module/symbol/in_module/name/kind/decorated/member_of/base_of/exported, plus
;; the 9 Racket-computed helpers test_module/test_name/console_script/package_init/controller_base/
;; public_api/api_module/root_module/has_statements). `explicit_entry` (the `steer: entry` marker) and
;; `implicit_name` (below) are handled by ONE engine-level rule each, not repeated per language.
;; implicit-names: bare names the LANGUAGE itself calls without a visible call-ref anywhere in source
;; (a runtime hook, a dunder method, an implicit Main) - always entries, language-blind to the engine.

;; every rule is scoped with `lang(_, "...")` on at least one variable that reaches every other
;; variable in the rule (directly or via in_module/member_of) - the generic helpers (public_api,
;; root_module, test_name, ...) are language-blind by design, so without this a language's prelude
;; would just as happily match another language's symbols (found running this for real on this
;; repo's own mixed Racket/Python/C# tree: Racket top-level defines were showing up "admitted by
;; python", since nothing separated the fact bases - recorded via `steer note T64`).
;; public_api+root_module (measured on a real library, T68: rebellion, which genuinely `provide`s a
;; curated public surface via `contract-out` and hides everything else - 1017 of 1881 symbols showed
;; up "dead" before rkt-extract tracked real provide-visibility and this rule existed, exactly the
;; same gap Python's own version of this rule already closed).
(define racket-entry-prelude #<<DL
entry(S) :- test_name(S), in_module(S, M), test_module(M), lang(M, "racket").
entry(S) :- console_script(S), lang(S, "racket").
entry(M) :- root_module(M), has_statements(M), lang(M, "racket").
entry(S) :- public_api(S), in_module(S, M), root_module(M), lang(M, "racket").
DL
  )
(define racket-implicit-names '("main"))

(define python-entry-prelude #<<DL
entry(S) :- test_name(S), in_module(S, M), test_module(M), lang(M, "python").
entry(S) :- console_script(S), lang(S, "python").
entry(M) :- package_init(M), lang(M, "python").
entry(S) :- public_api(S), in_module(S, M), root_module(M), lang(M, "python").
DL
  )
;; T68 (measured on a real project, aiofiles): the original list only covered the sync context-
;; manager protocol - the async protocol (`__aenter__`/`__aexit__`/`__aiter__`/`__anext__`/`__await__`)
;; and the common object-protocol dunders (`__repr__`/`__str__`/`__eq__`/`__hash__`/`__len__`/
;; `__iter__`/`__next__`/`__getitem__`/`__setitem__`/`__contains__`/`__del__`) are called by the
;; Python runtime the exact same way - never a named call anywhere in source - and every one of
;; them showed up as a false-positive "dead" finding before this list included them.
(define python-implicit-names
  '("__init__" "__new__" "__del__" "__call__" "__enter__" "__exit__" "__aenter__" "__aexit__"
    "__aiter__" "__anext__" "__await__" "__iter__" "__next__" "__repr__" "__str__" "__format__"
    "__eq__" "__ne__" "__lt__" "__le__" "__gt__" "__ge__" "__hash__" "__bool__" "__len__"
    "__getitem__" "__setitem__" "__delitem__" "__contains__" "__getattr__" "__setattr__" "main"))

;; a public member of a public class is C#'s closest equivalent to Python's "public_api of a root
;; module": C# has no top-level functions at all - every def nests inside a class - so a library's
;; entire purpose IS its public static API surface (found measuring T68 on a real one, GuardClauses:
;; without this rule, 838 of 841 symbols showed up "dead" - nothing had ever admitted them as entries).
(define csharp-entry-prelude #<<DL
entry(S) :- test_name(S), decorated(S, "Fact"), lang(S, "csharp").
entry(S) :- test_name(S), decorated(S, "Test"), lang(S, "csharp").
entry(S) :- test_name(S), decorated(S, "TestMethod"), lang(S, "csharp").
entry(S) :- console_script(S), lang(S, "csharp").
entry(S) :- member_of(S, C), controller_base(C), lang(S, "csharp").
entry(S) :- public_api(S), in_module(S, M), root_module(M), lang(M, "csharp").
DL
  )
(define csharp-implicit-names '("Main" "Dispose" "Equals" "GetHashCode" "ToString"))

(define racket-gate
  (gate 'racket '(".rkt" ".rktl" ".ss" ".scm" ".rkts") "form" check-source
        racket-find-anchor racket-list-names
        rkt-extract rkt-resolve-import racket-entry-prelude racket-implicit-names #f))

(define python-gate
  (gate 'python '(".py" ".pyi") "statement" python-gate-check
        python-find-anchor python-list-names
        py-extract py-resolve-import python-entry-prelude python-implicit-names #t))

(define csharp-gate
  (gate 'csharp '(".cs") "declaration" cs-gate-check
        cs-find-anchor cs-list-names
        cs-extract cs-resolve-import csharp-entry-prelude csharp-implicit-names #t))

;; Add languages here.
(define gates (list racket-gate python-gate csharp-gate))

(define (path-ext path)
  (define e (path-get-extension (if (path? path) path (string->path path))))
  (and e (string-downcase (bytes->string/utf-8 e #\?))))

(define (gate-for-path path)
  (define ext (path-ext path))
  (and ext (findf (λ (g) (member ext (gate-exts g))) gates)))

(define (supported-extensions) (append-map gate-exts gates))

;; → (values findings unit-count lang unit). Unknown extensions give one `skipped` info finding and #f counts.
(define (lang-check path text shown)
  (define g (gate-for-path path))
  (cond
    [g (define-values (fs n lang) ((gate-check g) text shown))
       (values fs n lang (gate-unit g))]
    [else
     (define ext (or (path-ext path) "(none)"))
     (values (list (finding 'info 'skipped
                            (format "no structural gate for `~a` files: steer checks ~a"
                                    ext (string-join (supported-extensions) ", "))
                            #:file shown))
             #f #f "form")]))

;; → hasheq in the resolve-anchor shape, or #f when `path`'s extension has no gate (caller falls back
;; to the generic keyword/indentation heuristic).
(define (lang-find-anchor path text name)
  (define g (gate-for-path path))
  (and g (gate-find-anchor g) ((gate-find-anchor g) text name)))

;; → (listof string), or #f when `path`'s extension has no gate.
(define (lang-list-names path text)
  (define g (gate-for-path path))
  (and g (gate-list-names g) ((gate-list-names g) text)))
