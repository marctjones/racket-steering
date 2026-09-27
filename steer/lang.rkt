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
         "common.rkt" "syntax-check.rkt" "srcread.rkt" "python.rkt" "csharp.rkt")
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

(define racket-gate
  (gate 'racket '(".rkt" ".rktl" ".ss" ".scm" ".rkts") "form" check-source
        racket-find-anchor racket-list-names
        #f #f #f #f #f))

(define python-gate
  (gate 'python '(".py" ".pyi") "statement" python-gate-check
        python-find-anchor python-list-names
        #f #f #f #f #f))

(define csharp-gate
  (gate 'csharp '(".cs") "declaration" cs-gate-check
        cs-find-anchor cs-list-names
        #f #f #f #f #f))

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
