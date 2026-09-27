#lang racket/base
;; T46: the C# structural gate: a lexer that knows C#'s string forms, located bracket errors, and edits that
;; are offered only when the brackets balance afterwards. Needs no dotnet, so it always runs (CI included).
(require rackunit racket/list racket/string racket/file racket/port racket/runtime-path json
         "../steer/csharp.rkt" "../steer/lang.rkt" (only-in "../steer/syntax-check.rkt" apply-edit))

(define-runtime-path shapes-cs "fixtures/cs/Shapes.cs")
(define-runtime-path scoped-cs "fixtures/cs/FileScoped.cs")
(define-runtime-path main-rkt "../steer/main.rkt")

(define (lex text) (let-values ([(ts err) (cs-lex text)]) (list ts err)))
(define (toks text) (map (λ (t) (cons (ctok-kind t) (ctok-text t))) (car (lex text))))
(define (check text) (let-values ([(fs n lang) (cs-gate-check text "T.cs")]) (list fs n lang)))
(define (findings text) (car (check text)))
(define (ok? text) (null? (findings text)))
(define (kind+pos f) (list (hash-ref f 'kind) (hash-ref f 'line) (hash-ref f 'col)))
(define (fixed text) (let* ([f (car (findings text))] [e (hash-ref f 'edit #f)]) (and e (apply-edit text e))))

;; ---------------------------------------------------------------------------------------------
;; the lexer: a brace inside any string, char, comment or directive is not a brace

(check-equal? (toks "a { b }") '((id . "a") (punct . "{") (id . "b") (punct . "}")))
(check-equal? (toks "s = \"}\";") '((id . "s") (punct . "=") (str . "\"}\"") (punct . ";")) "regular string")
(check-equal? (toks "s = \"a\\\"}\";") '((id . "s") (punct . "=") (str . "\"a\\\"}\"") (punct . ";")) "escaped quote")
(check-equal? (toks "s = @\"a\"\"}\nb\";") '((id . "s") (punct . "=") (str . "@\"a\"\"}\nb\"") (punct . ";")) "verbatim: \"\" and newlines")
(check-equal? (toks "s = $\"x{y[\"k\"]}{{z}}\";") '((id . "s") (punct . "=") (str . "$\"x{y[\"k\"]}{{z}}\"") (punct . ";")) "interpolated with a nested string and {{ }}")
(check-equal? (length (toks "s = $\"{a:0.##}\" + $@\"{b}\\\";")) 6 "format specifier; $@ verbatim interpolated (backslash is literal)")
(check-equal? (toks "s = @$\"{x}\";") '((id . "s") (punct . "=") (str . "@$\"{x}\"") (punct . ";")) "@$ order")
(check-equal? (map car (toks "s = \"\"\"\n { \"q\" } \n\"\"\";")) '(id punct str punct) "raw string with braces and quotes")
(check-equal? (toks "s = $$\"\"\"{{a}} { \"x\" }\"\"\";") '((id . "s") (punct . "=") (str . "$$\"\"\"{{a}} { \"x\" }\"\"\"") (punct . ";")) "raw interpolated with $$")
(check-equal? (toks "s = \"\";") '((id . "s") (punct . "=") (str . "\"\"") (punct . ";")) "empty string")
(check-equal? (toks "c = '{'; d = '\\''; e = '\"';") (list '(id . "c") '(punct . "=") '(chr . "'{'") '(punct . ";") '(id . "d") '(punct . "=") '(chr . "'\\''") '(punct . ";") '(id . "e") '(punct . "=") '(chr . "'\"'") '(punct . ";")) "char literals")
(check-equal? (toks "a // }\nb /* { */ c") '((id . "a") (id . "b") (id . "c")) "comments produce no tokens")
(check-equal? (toks "#region { x\nint a;\n#endregion }\n") '((id . "int") (id . "a") (punct . ";")) "preprocessor lines are skipped")
(check-equal? (toks "@class = 1;") '((id . "@class") (punct . "=") (num . "1") (punct . ";")) "verbatim identifier")
(check-equal? (toks "x => y; a::b") '((id . "x") (punct . "=>") (id . "y") (punct . ";") (id . "a") (punct . "::") (id . "b")))
(check-equal? (map ctok-text (car (lex "List<List<int>> x"))) '("List" "<" "List" "<" "int" ">" ">" "x") "`>>` stays two tokens: generics")
(let ([t (cadr (car (lex "ab\n  cd")))])
  (check-equal? (list (ctok-line t) (ctok-col t) (ctok-eline t) (ctok-ecol t)) '(2 3 2 5) "positions are 1-based, with an end"))
(let ([t (car (car (lex "s = @\"a\nb\";")))]) (void t))
(let ([ts (car (lex "s = @\"a\nb\"; x"))]) (check-equal? (ctok-line (last ts)) 2 "a token after a multi-line string is on its last line"))

;; ---------------------------------------------------------------------------------------------
;; valid files: no findings

(let ([r (check (file->string shapes-cs))])
  (check-equal? (car r) '() "the fixture: attributes, generics, properties, raw and interpolated strings, #region with a brace")
  (check-equal? (caddr r) "csharp")
  (check-equal? (cadr r) 3 "two usings and the namespace block"))
(check-equal? (car (check (file->string scoped-cs))) '() "file-scoped namespace, indexer, operator, finalizer, event")
(check-true (ok? "class A { }") "a one-liner")
(check-true (ok? "") "an empty file")
(check-true (ok? "using System;\r\nclass A\r\n{\r\n    int F() { return 1; }\r\n}\r\n") "CRLF")
(check-true (ok? "class A { void F() { var s = \"}\"; /* } */ char c = '}'; } }") "braces in strings, comments and chars")

;; ---------------------------------------------------------------------------------------------
;; errors: kind, position, message, and a verified edit where one exists

(define allman "namespace N\n{\n    class A\n    {\n        void F()\n        {\n            x();\n\n        void G()\n        {\n        }\n    }\n}\n")
(let ([f (car (findings allman))])
  (check-equal? (kind+pos f) '(unclosed-form 7 17) "located where the missing brace goes, not at the outer brace left open")
  (check-regexp-match #rx"a `{` opened at line 2 col 1 is never closed: the indentation shows the block ends after line 7" (hash-ref f 'message))
  (check-regexp-match #rx"add `}` on a new line after line 7 \\(verified" (hash-ref f 'fix))
  (check-equal? (hash-ref (hash-ref f 'edit) 'verified) #t))
(check-equal? (fixed allman) "namespace N\n{\n    class A\n    {\n        void F()\n        {\n            x();\n        }\n\n        void G()\n        {\n        }\n    }\n}\n")
(check-equal? (fixed "class A\n{\n    void F()\n    {\n        x();\n    }\n") "class A\n{\n    void F()\n    {\n        x();\n    }\n}\n" "missing at end of file")
(check-true (ok? "class A {\n    void F() {\n        x();\n    }\n\n    int G;\n}\nclass B { }\n") "balanced: nothing to fix")
(check-equal? (fixed "class A {\n    void F() {\n        x();\n\n    void G() { }\n}\n") "class A {\n    void F() {\n        x();\n    }\n\n    void G() { }\n}\n" "K&R style")

(let ([f (car (findings "class A\n{\n    void F() { x(); }\n}\n}\n"))])
  (check-equal? (kind+pos f) '(extra-closer 5 1))
  (check-regexp-match #rx"delete `}` at line 5 col 1" (hash-ref f 'fix)))
(check-equal? (fixed "class A\n{\n    void F() { x(); }\n}\n}\n") "class A\n{\n    void F() { x(); }\n}\n\n")

(let ([f (car (findings "class A { void F() { g(1, 2} } }\n"))])
  (check-equal? (kind+pos f) '(mismatched-closer 1 28))
  (check-regexp-match #rx"closes `\\(` from line 1 col 23" (hash-ref f 'message)))
(check-equal? (fixed "class A { void F() { g(1, 2} } }\n") "class A { void F() { g(1, 2) } }\n")
(check-equal? (kind+pos (car (findings "class A { void F() { g(1, [2, 3); } }\n"))) '(mismatched-closer 1 32))

;; unterminated literals and comments: located at the opening, no edit (the intended end is unknowable)
(for ([c (list (cons "class A { string s = \"abc;\n void F() {} }\n" "unterminated string literal")
               (cons "class A { string s = @\"abc;\n void F() {} }\n" "unterminated verbatim string literal")
               (cons "class A { string s = $\"abc {x;\n }\n" "unterminated interpolat")
               (cons "class A { string s = $\"abc {x}\n }\n" "unterminated interpolated string literal")
               (cons "class A { string s = \"\"\"abc\n }\n" "unterminated raw string literal")
               (cons "class A { /* oops\n void F() {} }\n" "unterminated block comment")
               (cons "class A { char c = 'a;\n }\n" "unterminated character literal"))])
  (define f (car (findings (car c))))
  (check-equal? (hash-ref f 'kind) 'read-error (car c))
  (check-regexp-match (regexp (car (regexp-match* #rx"[a-z ]+" (cdr c)))) (hash-ref f 'message))
  (check-false (hash-ref f 'edit #f) "no edit offered")
  (check-true (and (hash-ref f 'line #f) (hash-ref f 'col #f) #t) "located"))
(check-equal? (kind+pos (car (findings "class A { string s = \"abc;\n }\n"))) '(read-error 1 22) "at the opening quote")

;; ---------------------------------------------------------------------------------------------
;; (a deleted `}` that stood alone on its line leaves a whitespace-only line, so "exact" ignores those)
(define (squash s) (string-join (filter (λ (l) (not (string=? (string-trim l) ""))) (map (λ (l) (string-trim l "\r" #:left? #f)) (string-split s "\n" #:trim? #f))) "\n"))
;; exhaustive: delete each closing brace of the fixture in turn; every deletion is found, every offered edit
;; balances, and most restore the original exactly

(define shapes (file->string shapes-cs))
(define brace-positions
  (let-values ([(ts err) (cs-lex shapes)])
    (for/list ([t ts] #:when (and (eq? (ctok-kind t) 'punct) (equal? (ctok-text t) "}"))) (ctok-start t))))
(define trials
  (for/list ([i brace-positions])
    (define mutant (string-append (substring shapes 0 i) (substring shapes (add1 i))))
    (define fs (findings mutant))
    (define e (and (pair? fs) (hash-ref (car fs) 'edit #f)))
    (define result (and e (apply-edit mutant e)))
    (list (pair? fs) (and e (ok? result)) (and result (string=? (squash result) (squash shapes))))))
(check-equal? (length (filter car trials)) (length trials) "every deleted brace is detected")
(check-true (andmap (λ (t) (or (not (cadr t)) (eq? (cadr t) #t))) trials) "every offered edit balances")
(define offered (length (filter cadr trials)))
(define exact (length (filter caddr trials)))
(printf "cs-syntax-test: ~a brace deletions in Shapes.cs: ~a detected, ~a with a verified edit, ~a restore the original exactly\n"
        (length trials) (length (filter car trials)) offered exact)
(check-true (>= (/ offered (length trials)) 0.9) "an edit for nearly every deletion")
(check-true (>= (/ exact (max 1 offered)) 0.6) "and most of them are exact")

;; ---------------------------------------------------------------------------------------------
;; routing, the CLI and the hook

(check-equal? (gate-name (gate-for-path "A.cs")) 'csharp)
(check-equal? (gate-name (gate-for-path "A.CS")) 'csharp)
(define dir (make-temporary-directory "steer-cs~a"))
(define (steer #:in [input ""] . args)
  (define-values (p out in err)
    (parameterize ([current-directory dir])
      (apply subprocess #f #f 'stdout (find-executable-path "racket") (path->string main-rkt) args)))
  (write-string input in) (close-output-port in)
  (define text (port->string out))
  (subprocess-wait p) (close-input-port out)
  (values (subprocess-status p) text))
(define (put! name text) (call-with-output-file (build-path dir name) #:exists 'truncate (λ (o) (void (write-string text o)))))
(put! "Ok.cs" (file->string shapes-cs))
(put! "Bad.cs" allman)
(let-values ([(c o) (steer "syntax" "Ok.cs")])
  (check-equal? c 0 o)
  (check-regexp-match #rx"ok Ok.cs \\([0-9]+ declarations\\)" o))
(let-values ([(c o) (steer "syntax" "Bad.cs")])
  (check-equal? c 1)
  (check-regexp-match #rx"error unclosed-form Bad.cs:7:17: a `\\{` opened at line 2 col 1 is never closed.*add `\\}` on a new line after line 7" o))
(let-values ([(c o) (steer "hook" "post-edit" #:in "{\"tool_input\":{\"file_path\":\"Bad.cs\"}}")])
  (check-equal? c 2 "the hook reports a broken .cs")
  (check-regexp-match #rx"Bad.cs:7:17" o))
(let-values ([(c o) (steer "hook" "post-edit" #:in "{\"tool_input\":{\"file_path\":\"Ok.cs\"}}")])
  (check-equal? (list c o) '(0 "")))
(let-values ([(c o) (steer "syntax" "--fix" "Bad.cs")])
  (check-equal? c 0 o)
  (check-regexp-match #rx"fixed Bad.cs:" o))
(let-values ([(c o) (steer "syntax" "Bad.cs")]) (check-equal? c 0 o))
(check-equal? (file->string (build-path dir "Bad.cs"))
              "namespace N\n{\n    class A\n    {\n        void F()\n        {\n            x();\n        }\n\n        void G()\n        {\n        }\n    }\n}\n")

;; speed: the gate needs no subprocess, so it is cheap even on a large file
(define big (string-join (for/list ([i 400]) (string-append "    public int M" (number->string i) "(int x)\n    {\n        return x + " (number->string i) ";\n    }\n")) "\n"))
(define big-text (string-append "class Big\n{\n" big "\n}\n"))
(define times (for/list ([i 5]) (let ([t0 (current-inexact-milliseconds)]) (check big-text) (- (current-inexact-milliseconds) t0))))
(printf "cs-syntax-test: gate time on a ~a-line file: median ~a ms\n" (length (string-split big-text "\n" #:trim? #f)) (round (list-ref (sort times <) 2)))
(check-true (< (list-ref (sort times <) 2) 1000) "a 1600-line file well under a second even on a slow machine")
(let ([t0 (current-inexact-milliseconds)]) (check (file->string shapes-cs)) (printf "cs-syntax-test: gate time on Shapes.cs: ~a ms\n" (round (- (current-inexact-milliseconds) t0))))

(delete-directory/files dir)
