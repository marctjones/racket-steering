#lang racket/base
;; Language routing for `steer syntax` and the post-edit hook (catalog A1, milestone XL1).
;; One registry maps a file extension to the gate that understands that language, so a Python edit is
;; checked by Python's own parser and a C# edit by a C# scanner instead of being fed to the Racket
;; reader (which reported confident, wrong errors on valid .py/.cs files: note 12). A new language is one
;; entry in `gates`. A file whose extension has no gate is reported as *skipped*, never as clean or broken.
;;
;; A gate's `check` takes (text file) and returns (values findings unit-count lang), the same shape as
;; syntax-check.rkt's `check-source`. Findings may carry a machine-applicable `edit`; an edit is only
;; applied by `steer syntax --fix` when it is marked verified (the file checks clean afterwards).
(require racket/list racket/string racket/path
         "common.rkt" "syntax-check.rkt" "python.rkt" "csharp.rkt")
(provide (struct-out gate) gates gate-for-path lang-check supported-extensions)

;; name: symbol · exts: lowercase extensions with the dot · unit: what `check` counts ("form", "statement", ...)
(struct gate (name exts unit check))

(define racket-gate
  (gate 'racket '(".rkt" ".rktl" ".ss" ".scm" ".rkts") "form" check-source))

(define python-gate (gate 'python '(".py" ".pyi") "statement" python-gate-check))

(define csharp-gate (gate 'csharp '(".cs") "declaration" cs-gate-check))

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
