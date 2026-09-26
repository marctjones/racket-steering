#lang racket/base
;; `steer rules`: the Datalog evaluator, glob and require extraction, and the CLI on a small violating
;; project. Also: this repository must satisfy its own architecture rules (.steer/rules.dl).
(require rackunit racket/list racket/string racket/file racket/port racket/runtime-path
         "../steer/rules.rkt" (only-in "../steer/common.rkt" exn:steer? exn:steer-kind))

;; ---------------------------------------------------------------------------------------------
;; evaluator

(define closure
  "edge(a,b). edge(b,c). edge(c,d).
   reach(X,Y) :- edge(X,Y).
   reach(X,Z) :- edge(X,Y), reach(Y,Z).")
(let ([r (run-datalog closure)])
  (check-equal? (hash-ref r 'reach) '((a b) (a c) (a d) (b c) (b d) (c d)) "transitive closure, right-recursive")
  (check-equal? (length (hash-ref r 'edge)) 3))

(check-equal? (hash-ref (run-datalog "edge(a,b). edge(b,c). edge(c,d).
                                       reach(X,Y) :- edge(X,Y).
                                       reach(X,Z) :- reach(X,Y), reach(Y,Z).") 'reach)
              '((a b) (a c) (a d) (b c) (b d) (c d)) "and non-linear recursion")

(check-equal? (hash-ref (run-datalog "edge(a,b). edge(b,a). edge(b,c).
                                       reach(X,Y) :- edge(X,Y).
                                       reach(X,Z) :- edge(X,Y), reach(Y,Z).
                                       cyc(X) :- reach(X,X).") 'cyc)
              '((a) (b)) "cycles terminate, and are visible as reach(X,X)")

(check-equal? (hash-ref (run-datalog "layer(\"ui/a.rkt\",\"ui\"). layer(\"db/c.rkt\",\"db\"). requires(\"ui/a.rkt\",\"db/c.rkt\").
                                       violation(A,B,\"ui must not use db\") :- requires(A,B), layer(A,\"ui\"), layer(B,\"db\").") 'violation)
              '(("ui/a.rkt" "db/c.rkt" "ui must not use db")) "constants in the head, string constants in the body")

(check-equal? (hash-ref (run-datalog "p(1). p(2). q(X) :- p(X), p(X).") 'q) '((1) (2)) "repeated variables")

(define (rules-error-kind program)
  (with-handlers ([exn:steer? exn:steer-kind]) (run-datalog program) #f))
(check-equal? (rules-error-kind "p(X) :- q(Y).") 'rules-unsafe "a head variable not in the body")
(check-equal? (rules-error-kind "p(X).") 'rules-unsafe "a fact with a variable")
(check-equal? (rules-error-kind "p(a) :- q(a)") 'rules-syntax "a missing full stop")
(check-equal? (rules-error-kind "p(a) :- not q(a).") 'rules-syntax "negation is not supported (note 06)")

;; ---------------------------------------------------------------------------------------------
;; globs and requires

(define (glob g s) (regexp-match? (glob->regexp g) s))
(check-true (glob "ui/**" "ui/a.rkt"))
(check-true (glob "ui/**" "ui/deep/er/a.rkt"))
(check-false (glob "ui/**" "uix/a.rkt"))
(check-true (glob "steer/cmd-*.rkt" "steer/cmd-tasks.rkt"))
(check-false (glob "steer/cmd-*.rkt" "steer/cmd/tasks.rkt") "* does not cross a directory")
(check-true (glob "**/a.rkt" "a.rkt") "**/ matches zero directories")
(check-true (glob "**/a.rkt" "x/y/a.rkt"))
(check-true (glob "a?.rkt" "ab.rkt"))
(check-false (glob "a.rkt" "aXrkt") "dots are literal")

(define (reqs text) (map car (extract-requires text "t.rkt")))
(check-equal? (reqs "#lang racket/base\n(require \"a.rkt\" racket/list (only-in \"b.rkt\" x) (prefix-in p: \"c.rkt\"))")
              '("a.rkt" racket/list "b.rkt" "c.rkt"))
(check-equal? (reqs "#lang racket/base\n(require (file \"d.rkt\") (submod \"e.rkt\" test) (lib \"racket/string\") (for-syntax \"f.rkt\"))")
              '("d.rkt" "e.rkt" racket/string "f.rkt"))
(check-equal? (reqs "#lang racket/base\n(module+ test (require rackunit \"g.rkt\"))\n(require (for-label \"h.rkt\") (submod \".\" x))")
              '(rackunit "g.rkt") "requires inside module+ count; for-label and (submod \".\" x) do not")
(check-equal? (map cdr (extract-requires "#lang racket/base\n\n(require \"a.rkt\")\n(require\n  \"b.rkt\")" "t.rkt")) '(3 5) "lines")

;; ---------------------------------------------------------------------------------------------
;; CLI on a small project

(define-runtime-path main-rkt "../steer/main.rkt")
(define-runtime-path repo-root "..")
(define (steer dir . args)
  (define-values (p out in err)
    (parameterize ([current-directory dir])
      (apply subprocess #f #f 'stdout (find-executable-path "racket") (path->string main-rkt) args)))
  (close-output-port in)
  (define text (port->string out))
  (subprocess-wait p) (close-input-port out)
  (values (subprocess-status p) text))
(define (write-file! dir rel text)
  (define p (build-path dir rel))
  (make-directory* (let-values ([(d _n _x) (split-path p)]) d))
  (call-with-output-file p #:exists 'truncate (λ (o) (void (write-string text o)))))

(define dir (make-temporary-directory "steer-rules~a"))
(write-file! dir "db/conn.rkt" "#lang racket/base\n(provide connect)\n(define (connect) 1)\n")
(write-file! dir "util/helper.rkt" "#lang racket/base\n(require \"../db/conn.rkt\" racket/list)\n(provide help)\n(define (help) (connect))\n")
(write-file! dir "ui/view.rkt" "#lang racket/base\n(require \"../db/conn.rkt\")\n(define v (connect))\n")
(write-file! dir "ui/page.rkt" "#lang racket/base\n(require (only-in \"../util/helper.rkt\" help))\n(help)\n")
(write-file! dir "ui/ok.rkt" "#lang racket/base\n(require racket/string)\n")

(let-values ([(c o) (steer dir "init")]) (check-equal? c 0 o))
(let-values ([(c o) (steer dir "rules" "check")])
  (check-equal? c 2 o)
  (check-regexp-match #rx"no rules file.*rules init" o))
(let-values ([(c o) (steer dir "rules" "init")]) (check-equal? c 0 o))
(let-values ([(c o) (steer dir "rules" "check")])
  (check-equal? c 1 o)
  (check-regexp-match #rx"5 modules, 3 require edges" o)
  (check-regexp-match #rx"2 violations" o)
  (check-regexp-match #rx"ui/view.rkt:2: the UI must not reach the database: ui/view.rkt → db/conn.rkt" o "direct violation, on the require's line")
  (check-regexp-match #rx"ui/page.rkt:2: .*ui/page.rkt → db/conn.rkt \\(via util/helper.rkt\\)" o "transitive violation shows the path")
  (check-regexp-match #rx"remove the require of db/conn.rkt from util/helper.rkt" o "fix names the last link"))
(let-values ([(c o) (steer dir "--json" "rules" "check")])
  (check-equal? c 1)
  (check-regexp-match #rx"\"kind\":\"architecture-violation\"" o))
(let-values ([(c o) (steer dir "rules" "facts")])
  (check-regexp-match #rx"requires: 3" o)
  (check-regexp-match #rx"uses: 2" o "racket/list and racket/string are library uses"))
;; break the chain and the violations go away
(write-file! dir "ui/view.rkt" "#lang racket/base\n(define v 1)\n")
(write-file! dir "ui/page.rkt" "#lang racket/base\n(define p 2)\n")
(let-values ([(c o) (steer dir "rules" "check")])
  (check-equal? c 0 o)
  (check-regexp-match #rx"0 violations" o))
;; a typo'd predicate is a warning, not silence
(write-file! dir ".steer/rules.dl" "%layer ui ui/**\nviolation(A,B) :- reach(A,B), layr(A,\"ui\").\n")
(let-values ([(c o) (steer dir "rules" "check")])
  (check-regexp-match #rx"warning unknown-predicate.*layr" o))
(delete-directory/files dir)

;; ---------------------------------------------------------------------------------------------
;; this repository satisfies its own rules

(let-values ([(c o) (steer repo-root "rules" "check")])
  (check-equal? c 0 (string-append "steer violates its own architecture rules:\n" o))
  (check-regexp-match #rx"0 violations" o)
  (check-regexp-match #rx"8 rules" o))
