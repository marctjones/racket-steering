#lang racket/base
;; T44: `steer syntax` and the post-edit hook route by file extension. A valid Python or C# file must never
;; get the Racket reader's errors; a file type with no gate is reported as skipped, not as clean.
(require rackunit racket/list racket/string racket/file racket/port racket/runtime-path json
         "../steer/lang.rkt")

;; ---------------------------------------------------------------------------------------------
;; the registry

(check-equal? (gate-name (gate-for-path "a.rkt")) 'racket)
(check-equal? (gate-name (gate-for-path "/x/y/mod.RKT")) 'racket "extensions are case-insensitive")
(check-equal? (gate-name (gate-for-path (string->path "lib.rktl"))) 'racket "paths as well as strings")
(check-false (gate-for-path "README.md"))
(check-false (gate-for-path "Makefile") "no extension")
(check-false (gate-for-path "archive.tar.gz"))
(check-true (and (member ".rkt" (supported-extensions)) #t))
(check-true (andmap (λ (e) (regexp-match? #rx"^[.][a-z]+$" e)) (supported-extensions)) "extensions are lowercase with a dot")
(check-true (andmap gate? gates))
(check-equal? (length (remove-duplicates (append-map gate-exts gates))) (length (append-map gate-exts gates)) "no extension is claimed twice")

(let-values ([(fs n lang unit) (lang-check "x.rkt" "#lang racket/base\n(define x 1)\n" "x.rkt")])
  (check-equal? (list fs n unit) '(() 1 "form")))
(let-values ([(fs n lang unit) (lang-check "notes.txt" "whatever" "notes.txt")])
  (check-false n "unknown types have no unit count")
  (check-equal? (map (λ (f) (hash-ref f 'kind)) fs) '(skipped))
  (check-equal? (hash-ref (car fs) 'severity) 'info "skipped is information, not an error")
  (check-regexp-match #rx"no structural gate for `\\.txt` files: steer checks .*\\.rkt" (hash-ref (car fs) 'message)))

;; ---------------------------------------------------------------------------------------------
;; the CLI

(define-runtime-path main-rkt "../steer/main.rkt")
(define dir (make-temporary-directory "steer-route~a"))
(define (steer #:in [input ""] . args)
  (define-values (p out in err)
    (parameterize ([current-directory dir])
      (apply subprocess #f #f 'stdout (find-executable-path "racket") (path->string main-rkt) args)))
  (write-string input in) (close-output-port in)
  (define text (port->string out))
  (subprocess-wait p) (close-input-port out)
  (values (subprocess-status p) text))
(define (put! name text) (call-with-output-file (build-path dir name) #:exists 'truncate (λ (o) (void (write-string text o)))))

;; valid Python and C# use `#` and braces the Racket reader chokes on: no read-error may appear
(put! "ok.py" "# a comment\ndef f(x):\n    return {x: [1, 2]}\n\nclass A:\n    pass\n")
(put! "ok.cs" "namespace N { class A { int F() { return 1; } } }\n")
(put! "notes.md" "# heading\n\nsome text (with an unbalanced paren\n")
(put! "app.js" "console.log('{');\n")

(for ([f '("ok.py" "ok.cs")])
  (let-values ([(c o) (steer "syntax" f)])
    (check-equal? c 0 (format "~a: exit code\n~a" f o))
    (check-false (regexp-match? #rx"read-error|bad syntax|unclosed" o) (format "~a gets no Racket-reader errors: ~a" f o))))

(let-values ([(c o) (steer "syntax" "notes.md" "app.js")])
  (check-equal? c 0 "a skipped file is not a failure")
  (check-regexp-match #rx"skipped notes.md" o)
  (check-regexp-match #rx"skipped app.js" o)
  (check-regexp-match #rx"info skipped notes.md: no structural gate for `\\.md` files" o))
(let-values ([(c o) (steer "--json" "syntax" "notes.md")])
  (define j (string->jsexpr o))
  (define file (car (hash-ref (hash-ref j 'data) 'files)))
  (check-equal? (list (hash-ref file 'skipped) (hash-ref file 'ok)) '(#t #t)))

;; Racket keeps its behavior: located errors, and the summary line uses the gate's unit
(put! "good.rkt" "#lang racket/base\n(define x 1)\n(define y 2)\n")
(put! "bad.rkt" "#lang racket/base\n(define (f x)\n  (+ x 1)\n\n(define y 2)\n")
(let-values ([(c o) (steer "syntax" "good.rkt")]) (check-regexp-match #rx"ok good.rkt \\(2 forms\\)" o))
(let-values ([(c o) (steer "syntax" "bad.rkt")])
  (check-equal? c 1)
  (check-regexp-match #rx"unclosed-form bad.rkt:3" o))
(let-values ([(c o) (steer "syntax" "--fix" "bad.rkt")])
  (check-equal? c 0 o)
  (check-regexp-match #rx"fixed bad.rkt" o))

;; the hook: silent on files it cannot judge and on clean files, exit 2 with a located message on broken ones
(define (hook file) (steer "hook" "post-edit" #:in (format "{\"tool_input\":{\"file_path\":~s}}" file)))
(for ([f '("ok.py" "ok.cs" "notes.md" "app.js" "good.rkt")])
  (let-values ([(c o) (hook f)])
    (check-equal? (list f c o) (list f 0 "") "silent and exit 0")))
(put! "bad2.rkt" "#lang racket/base\n(define (f x)\n  (+ x 1)\n\n(define y 2)\n")
(let-values ([(c o) (hook "bad2.rkt")])
  (check-equal? c 2)
  (check-regexp-match #rx"unclosed-form bad2.rkt:3" o))
(let-values ([(c o) (hook "does-not-exist.rkt")]) (check-equal? c 0 "a missing file is not the hook's business"))

(delete-directory/files dir)
