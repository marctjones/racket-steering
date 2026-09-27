#lang racket/base
;; T45: the Python syntax gate (stdlib compile via a worker): located findings, verified edits, no false
;; errors on valid code. Skipped, loudly, when python3 is not installed.
(require rackunit racket/list racket/string racket/file racket/port racket/runtime-path json
         "../steer/python.rkt" "../steer/syntax-check.rkt" "../steer/lang.rkt")

(define-runtime-path shapes-py "fixtures/py/shapes.py")
(define-runtime-path main-rkt "../steer/main.rkt")

(cond
  [(not (python-available?))
   (eprintf "py-syntax-test: SKIPPED: python3 not found\n")]
  [else
   (define (check text) (let-values ([(fs n lang) (python-gate-check text "t.py")]) (list fs n lang)))
   (define (findings text) (car (check text)))
   (define (ok? text) (null? (findings text)))
   (define (kind+pos f) (list (hash-ref f 'kind) (hash-ref f 'line) (hash-ref f 'col)))
   (define (fixed text) (let* ([f (car (findings text))] [e (hash-ref f 'edit #f)]) (and e (apply-edit text e))))

   ;; -----------------------------------------------------------------------------------------
   ;; valid code: no findings, and the statement count is the number of top-level statements

   (check-equal? (cdr (check "x = 1\ny = 2\n")) '(2 "python"))
   (check-equal? (cdr (check "")) '(0 "python") "an empty file is valid")
   (let ([shapes (file->string shapes-py)])
     (check-equal? (findings shapes) '() "the fixture with decorators, async, match, walrus, overloads")
     (check-equal? (cadr (check shapes)) 16))
   (for ([t (list "x = f'{a!r:>{w}} and {b}'\n"                       ; nested f-string fields
                  "def f(*, a, b=1, **kw): return a\n"
                  "with open('a') as f, open('b') as g:\n    pass\n"
                  "x = [i for i in range(3)]; y = {k: v for k, v in z}\n"
                  "s = '''triple\n(unbalanced ( inside\n'''\n"
                  "s = \"a # not a comment (\"\nt = 1  # ) really a comment\n"
                  "x = 1 + \\\n    2\n"
                  "é = 1\nπ = 3.14\nprint(é, π)\n"                 ; non-ASCII identifiers
                  "if x:\n\tpass\n"                                  ; tab indentation
                  "x = 1\r\ny = 2\r\n")])                            ; CRLF
     (check-true (ok? t) t))

   ;; -----------------------------------------------------------------------------------------
   ;; errors: kind, position, message; and the edits Python names precisely, each verified

   (let ([f (car (findings "def f(x):\n    y = foo(1,\n            2\n    return y\n\nz = 3\n"))])
     (check-equal? (kind+pos f) '(unclosed-form 2 12) "points at the opener")
     (check-equal? (hash-ref f 'message) "'(' was never closed")
     (check-equal? (hash-ref f 'file) "t.py")
     (check-regexp-match #rx"insert \\) at line 3 col 14 \\(verified" (hash-ref f 'fix))
     (check-equal? (hash-ref (hash-ref f 'edit) 'verified) #t))
   (check-equal? (fixed "def f(x):\n    y = foo(1,\n            2\n    return y\n\nz = 3\n")
                 "def f(x):\n    y = foo(1,\n            2)\n    return y\n\nz = 3\n" "closer goes at the end of the statement, before the next one")
   (check-equal? (fixed "x = [1, 2,\n     3\ny = 1\n") "x = [1, 2,\n     3]\ny = 1\n")
   (check-equal? (fixed "x = {'a': [1, 2\n}\n") "x = {'a': [1, 2]\n}\n" "two brackets open: the inner one is missing")
   (check-equal? (fixed "print(foo(1, 2)  # done\nx = 1\n") "print(foo(1, 2))  # done\nx = 1\n" "the closer goes before a trailing comment")

   (let ([f (car (findings "x = (1 + 2))\nprint(x)\n"))])
     (check-equal? (kind+pos f) '(extra-closer 1 12))
     (check-equal? (hash-ref f 'message) "unmatched ')'"))
   (check-equal? (fixed "x = (1 + 2))\nprint(x)\n") "x = (1 + 2)\nprint(x)\n")
   (let ([f (car (findings "x = [1, 2)\nprint(x)\n"))])
     (check-equal? (kind+pos f) '(mismatched-closer 1 10)))
   (check-equal? (fixed "x = [1, 2)\nprint(x)\n") "x = [1, 2]\nprint(x)\n")

   (check-equal? (kind+pos (car (findings "if x == 1\n    pass\n"))) '(read-error 1 10))
   (check-equal? (fixed "if x == 1\n    pass\n") "if x == 1:\n    pass\n")
   (check-equal? (fixed "def f(x)  # note\n    return x\n") "def f(x):  # note\n    return x\n" "the colon goes before the comment")
   (check-equal? (fixed "class A\n    pass\n") "class A:\n    pass\n")
   (check-equal? (fixed "for i in range(3)\n    pass\n") "for i in range(3):\n    pass\n")

   ;; no edit where Python does not name the fix exactly: a located message, never a guess
   (for ([t '("s = 'abc\nprint(s)\n" "x = 1\n    y = 2\n" "def f():\n        x = 1\n    y = 2\n" "return 5\n" "print 'hello'\n" "x = = 1\n")])
     (define fs (findings t))
     (check-equal? (length fs) 1 t)
     (check-false (hash-ref (car fs) 'edit #f) (format "no edit offered for ~s" t))
     (check-true (and (hash-ref (car fs) 'line #f) (hash-ref (car fs) 'col #f) #t) (format "located: ~s" t)))
   (check-regexp-match #rx"unterminated string literal" (hash-ref (car (findings "s = 'abc\nprint(s)\n")) 'message))
   (check-regexp-match #rx"unexpected indent" (hash-ref (car (findings "x = 1\n    y = 2\n")) 'message))
   (check-regexp-match #rx"'return' outside function" (hash-ref (car (findings "return 5\n")) 'message) "semantic errors from compile, not just parse errors")

   ;; -----------------------------------------------------------------------------------------
   ;; every edit that is offered is verified: applying it gives code that compiles

   (define shapes (file->string shapes-py))
   (define closers (for/list ([c (in-string shapes)] [i (in-naturals)] #:when (memv c '(#\) #\] #\}))) i))
   (define trials
     (for/list ([i closers])
       (define mutant (string-append (substring shapes 0 i) (substring shapes (add1 i))))
       (define fs (findings mutant))
       (define e (and (pair? fs) (hash-ref (car fs) 'edit #f)))
       (list (pair? fs) (and e (ok? (apply-edit mutant e))) (and e (string=? (apply-edit mutant e) shapes)))))
   (check-equal? (length (filter car trials)) (length trials) "every deleted closer is detected")
   (check-true (andmap (λ (t) (or (not (cadr t)) (eq? (cadr t) #t))) trials) "an edit, when offered, always compiles")
   (define offered (length (filter cadr trials)))
   (define exact (length (filter caddr trials)))
   (printf "py-syntax-test: ~a closer deletions in shapes.py: ~a detected, ~a with a verified edit, ~a restore the original exactly\n"
           (length trials) (length (filter car trials)) offered exact)
   (check-true (>= (/ offered (length trials)) 0.7) "an edit is offered for most deletions")
   (check-true (>= (/ exact (max 1 offered)) 0.8) "and most offered edits restore the original")

   ;; -----------------------------------------------------------------------------------------
   ;; routing and the CLI

   (check-equal? (gate-name (gate-for-path "a.py")) 'python)
   (check-equal? (gate-name (gate-for-path "stubs.PYI")) 'python)
   (define dir (make-temporary-directory "steer-py~a"))
   (define (steer #:in [input ""] . args)
     (define-values (p out in err)
       (parameterize ([current-directory dir])
         (apply subprocess #f #f 'stdout (find-executable-path "racket") (path->string main-rkt) args)))
     (write-string input in) (close-output-port in)
     (define text (port->string out))
     (subprocess-wait p) (close-input-port out)
     (values (subprocess-status p) text))
   (define (put! name text) (call-with-output-file (build-path dir name) #:exists 'truncate (λ (o) (void (write-string text o)))))
   (put! "ok.py" "def f(x):\n    return x\n")
   (put! "bad.py" "def f(x):\n    y = foo(1,\n            2\n    return y\n")
   (let-values ([(c o) (steer "syntax" "ok.py")])
     (check-equal? c 0 o)
     (check-regexp-match #rx"ok ok.py \\(1 statement\\)" o))
   (let-values ([(c o) (steer "syntax" "bad.py")])
     (check-equal? c 1)
     (check-regexp-match #rx"error unclosed-form bad.py:2:12: '\\(' was never closed → insert \\) at line 3 col 14" o))
   (let-values ([(c o) (steer "hook" "post-edit" #:in "{\"tool_input\":{\"file_path\":\"bad.py\"}}")])
     (check-equal? c 2 "the hook reports a broken .py")
     (check-regexp-match #rx"steer syntax found structural problems.*bad.py:2:12" o))
   (let-values ([(c o) (steer "hook" "post-edit" #:in "{\"tool_input\":{\"file_path\":\"ok.py\"}}")])
     (check-equal? (list c o) '(0 "")))
   (let-values ([(c o) (steer "syntax" "--fix" "bad.py")])
     (check-equal? c 0 o)
     (check-regexp-match #rx"fixed bad.py: inserted \\) at line 3 col 14" o))
   (check-equal? (file->string (build-path dir "bad.py")) "def f(x):\n    y = foo(1,\n            2)\n    return y\n")
   (let-values ([(c o) (steer "syntax" "bad.py")]) (check-equal? c 0 o))

   ;; speed: the gate is meant for a hook that runs after every edit
   (define times (for/list ([i 5]) (let ([t0 (current-inexact-milliseconds)]) (check shapes) (- (current-inexact-milliseconds) t0))))
   (printf "py-syntax-test: gate time on shapes.py: median ~a ms\n" (round (list-ref (sort times <) 2)))
   (check-true (< (list-ref (sort times <) 2) 1500) "well under a second even on a slow CI machine")

   ;; a missing python is reported as skipped, not as clean
   (putenv "STEER_PYTHON" "/nonexistent/python3")
   (let-values ([(fs n lang) (python-gate-check "x = (\n" "t.py")])
     (check-equal? (map (λ (f) (hash-ref f 'kind)) fs) '(skipped))
     (check-false n))
   (putenv "STEER_PYTHON" "")

   (delete-directory/files dir)])
