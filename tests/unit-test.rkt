#lang racket/base
;; Unit tests for the pure parts: graph, plan checking, anchors, clones, syntax scanner, API diff.
(require rackunit racket/list racket/string racket/file
         "../steer/common.rkt" "../steer/store.rkt" "../steer/tasks.rkt" "../steer/plan.rkt"
         "../steer/srcread.rkt" "../steer/anchors.rkt" "../steer/dup.rkt" "../steer/syntax-check.rkt"
         "../steer/api.rkt")

(define (t id status . after) (hasheq 'id id 'title id 'status status 'after after 'priority 2))

;; ---------------------------------------------------------------------------------------------
;; graph

(let ([g (index (list (t "T1" 'done) (t "T2" 'open "T1") (t "T3" 'open "T2") (t "T4" 'open "T1")
                      (t "T5" 'open "T3" "T4") (t "T6" 'dropped)))])
  (check-equal? (map (λ (x) (hash-ref x 'id)) (ready-order g)) '("T2" "T4") "T2 unblocks more work than T4")
  (check-equal? (derived-status (hash-ref g "T3") g) 'blocked)
  (check-equal? (blockers (hash-ref g "T5") g) '(("T3" open) ("T4" open)))
  (check-false (find-cycle g))
  (check-equal? (layers g) '(("T2" "T4") ("T3") ("T5")))
  (check-equal? (critical-path g) '("T2" "T3" "T5")))

(let ([g (index (list (t "T1" 'open "T3") (t "T2" 'open "T1") (t "T3" 'open "T2")))])
  (define c (find-cycle g))
  (check-equal? (length c) 4)
  (check-equal? (first c) (last c) "cycle is closed"))

(let ([g (index (list (t "T1" 'open "T9") (t "T2" 'open "T3") (t "T3" 'dropped)))])
  (check-equal? (sort (map (λ (f) (hash-ref f 'kind)) (graph-findings g)) symbol<?)
                '(dropped-dependency missing-dependency)))

(check-equal? (normalize-id "t7") "T7")
(check-equal? (normalize-id "12") "T12")
(check-exn exn:steer? (λ () (normalize-id "task-1")))

;; ---------------------------------------------------------------------------------------------
;; plan parsing and resolution

(define (plan-errors text [existing '()])
  (define-values (specs perrs) (parse-plan text "p"))
  (define-values (_new rerrs _labels) (resolve-plan specs existing "p"))
  (map (λ (f) (hash-ref f 'kind)) (append perrs rerrs)))

(check-equal? (plan-errors "(task \"A\" #:id a #:check \"true\")\n(task \"B\" #:after (a) #:check \"true\")") '())
(check-equal? (plan-errors "(task \"A\" #:chek \"x\")") '(unknown-keyword))
(check-equal? (plan-errors "(task \"A\" #:id a #:after (b))\n(task \"B\" #:id b #:after (a))") '(cycle))
(check-equal? (plan-errors "(task \"A\" #:after (T9))") '(unknown-task))
(check-equal? (plan-errors "(task \"A\" #:after (T1))" (list (t "T1" 'done))) '())
(check-equal? (plan-errors "(task \"A\" #:id a)\n(task \"B\" #:id a)") '(duplicate-label))
(check-equal? (plan-errors "(tsk \"A\")") '(not-a-task))
(check-equal? (plan-errors "(task \"A\" #:priority 11)") '(bad-value))
(check-equal? (plan-errors "(task \"A\"") '(read-error))
(let-values ([(specs errs) (parse-plan "#lang steer/plan\n\n(task \"A\" #:chek 1)" "p")])
  (check-equal? (hash-ref (car errs) 'line) 3 "line numbers survive the #lang line"))
(let*-values ([(specs _) (parse-plan "(task \"A\" #:id a)\n(task \"B\" #:after (a T1))" "p")]
              [(new errs labels) (resolve-plan specs (list (t "T1" 'done)) "p")])
  (check-equal? (map (λ (x) (hash-ref x 'id)) new) '("T2" "T3"))
  (check-equal? (hash-ref (second new) 'after) '("T2" "T1")))

;; ---------------------------------------------------------------------------------------------
;; source reading and anchors

(define (defs text) (let-values ([(fs _l _t) (read-racket-source text)]) (map car (find-definitions fs))))
(check-equal? (defs "#lang racket/base\n(define x 1)\n(define (f a) a)\n(define ((g a) b) b)\n(struct pt (x y))\n(module+ test (define y 2))\n(define-values (p q) (values 1 2))")
              '(x f g pt y p q))

(let-values ([(fs lang _t) (read-racket-source "#lang racket/base\n(define x 1)")])
  (check-equal? lang "racket/base")
  (check-equal? (syntax-line (car fs)) 2))

(define dir (make-temporary-directory "steer-test~a"))
(define (put! name text) (call-with-output-file (build-path dir name) #:exists 'truncate (λ (o) (void (write-string text o)))))
(put! "a.rkt" "#lang racket/base\n(define (f x) (+ x 1))\n")
(define base (baseline-anchor dir "a.rkt#f"))
(put! "a.rkt" "#lang racket/base\n;; comment\n(define (f x)\n   (+ x   1))\n")
(check-equal? (anchor-state base (resolve-anchor dir "a.rkt#f")) 'ok "formatting and comments do not make an anchor stale")
(put! "a.rkt" "#lang racket/base\n(define (f x) (+ x 2))\n")
(check-equal? (anchor-state base (resolve-anchor dir "a.rkt#f")) 'changed)
(put! "a.rkt" "#lang racket/base\n(define (g x) x)\n")
(check-equal? (anchor-state base (resolve-anchor dir "a.rkt#f")) 'missing)
(define pending (baseline-anchor dir "a.rkt#h"))
(check-false (hash-ref pending 'hash))
(check-equal? (anchor-state pending (resolve-anchor dir "a.rkt#h")) 'pending)
(put! "a.rkt" "#lang racket/base\n(define (h) 1)\n")
(check-equal? (anchor-state pending (resolve-anchor dir "a.rkt#h")) 'created)

;; names that print specially (Rosette defines ||, the empty symbol) are still anchorable
(put! "o.rkt" "#lang racket/base\n(define (|| a b) (or a b))\n(define (|x y| z) z)\n")
(check-equal? (symbol->anchor-name '||) "||")
(check-true (hash-ref (resolve-anchor dir "o.rkt#||") 'found?))
(check-true (hash-ref (resolve-anchor dir (string-append "o.rkt#" (symbol->anchor-name '|x y|))) 'found?))

;; Python now resolves exactly via the ast (T49); .py is no longer a heuristic-path example.
;; Ruby exercises the heuristic path here instead.
(put! "m.rb" "require 'set'\n\ndef load(p)\n  File.open(p) do |f|\n    return f.read\n  end\nend\n\ndef other\nend\n")
(let ([r (resolve-anchor dir "m.rb#load")])
  (check-equal? (list (hash-ref r 'line) (hash-ref r 'end) (hash-ref r 'method)) '(3 7 heuristic)))
(put! "m.js" "export async function fetchAll(url) {\n  return 1;\n}\nconst x = 2;\n")
(let ([r (resolve-anchor dir "m.js#fetchAll")])
  (check-equal? (list (hash-ref r 'line) (hash-ref r 'end)) '(1 3)))
(put! "m.go" "package m\n\nfunc (s *Srv) Handle(w int) error {\n\treturn nil\n}\n")
(check-equal? (hash-ref (resolve-anchor dir "m.go#Handle") 'line) 3)

;; ---------------------------------------------------------------------------------------------
;; clones

(define (clone-groups text #:min [m 10] #:loose? [loose? #f])
  (define-values (gs _s) (find-clones (list (cons "c.rkt" text)) #:min-size m #:loose? loose?))
  (for/list ([g gs]) (map (λ (x) (hash-ref x 'in)) (hash-ref g 'members))))

(check-equal? (clone-groups "(define (a xs) (for/list ([x xs] #:when (even? x)) (* x x)))\n(define (b ys) (for/list ([y ys] #:when (even? y)) (* y y)))\n(define (c xs) (for/list ([x xs] #:when (odd? x)) (* x x)))")
              '((a b)) "renamed locals match; a different free function (odd?) does not")
(check-equal? (clone-groups "(define (a x) (list x 1 2 3 4 5 6 7))\n(define (b x) (list x 9 9 9 9 9 9 9))") '())
(check-equal? (clone-groups "(define (a x) (list x 1 2 3 4 5 6 7))\n(define (b x) (list x 9 9 9 9 9 9 9))" #:loose? #t) '((a b)))
(check-equal? (length (clone-groups "(define (a x) (let ([y (+ x 1)]) (let ([z (* y 2)]) (list x y z))))\n(define (b p) (let ([q (+ p 1)]) (let ([r (* q 2)]) (list p q r))))"))
              1 "nested sub-clones are not reported separately")

;; ---------------------------------------------------------------------------------------------
;; structural check

(define (kinds text) (let-values ([(fs _n _l) (check-source text "x.rkt")]) (map (λ (f) (hash-ref f 'kind)) fs)))
(check-equal? (kinds "#lang racket/base\n(define s \"(\")\n(define c #\\()\n(define r #rx\"[(]\")\n#| ( |#\n; (\n(define h #<<E\n(((\nE\n  )\n") '())
(check-equal? (kinds "#lang racket/base\n(define (f x)\n  (+ x 1)\n\n(define y 2)\n") '(read-error unclosed-form))
(check-equal? (kinds "#lang racket/base\n(define (f x) x))\n") '(read-error extra-closer))
(check-equal? (kinds "#lang racket/base\n(let ([x 1)) x)\n") '(read-error mismatched-closer))
(check-equal? (kinds "(define x 1)\n") '(no-lang))
(let-values ([(fs _n _l) (check-source "#lang racket/base\n(define (f x)\n  (+ x 1)\n\n(define y 2)\n" "x.rkt")])
  (check-regexp-match #rx"end of line 3" (hash-ref (second fs) 'message))
  (check-equal? (hash-ref (hash-ref (second fs) 'edit) 'line) 3)
  (check-regexp-match #rx"verified" (hash-ref (second fs) 'message)))

;; headers found on the installation corpus (each was a false positive before)
(check-equal? (kinds "#!/usr/bin/env racket\n#lang racket/base\n(define x 1)\n") '() "shebang line before #lang")
(check-equal? (header-lang "#! /usr/bin/env racket\n;; c\n#lang racket/base\n") "racket/base")
(define htdp-src ";; The first three lines of this file were inserted by DrRacket.\n;; about the language level of this file in a form that our tools can easily process.\n#reader(lib \"htdp-beginner-reader.ss\" \"lang\")((modname foo) (read-case-sensitive #t) (teachpacks ()))\n(define (f x) x)\n")
(check-equal? (kinds htdp-src) '() "DrRacket teaching-language #reader header")
(let-values ([(fs _l _t) (read-racket-source htdp-src)])
  (check-equal? (map syntax->datum fs) '((define (f x) x)) "reader spec and settings datum are blanked")
  (check-equal? (syntax-line (car fs)) 4 "line numbers survive blanking"))
(check-equal? (kinds "#reader scribble/reader\n@title{a ( b}\n") '(not-sexp))
(check-equal? (kinds "#reader(lib\"read.ss\"\"wxme\")WXME0108 ## \n#|\n   This file uses the GRacket editor format.\n") '(not-sexp))
(check-equal? (kinds "#lang racklog\nparent(john, doug).\n") '(not-sexp))
(check-equal? (kinds "#lang scribble/manual\n@title{x}\n") '(not-sexp))

;; machine-applicable edits: applying the suggested edit restores the original program
(define (repair text)
  (let-values ([(fs _n _l) (check-source text "x.rkt")])
    (define e (for/first ([f fs] #:when (hash-ref f 'edit #f)) (hash-ref f 'edit)))
    (and e (apply-edit text e))))
(define good "#lang racket/base\n(define (f x)\n  (+ x 1)) ; one\n\n(define y 2)\n")
(check-equal? (repair "#lang racket/base\n(define (f x)\n  (+ x 1) ; one\n\n(define y 2)\n") good
              "closer inserted before the trailing comment")
(check-equal? (repair "#lang racket/base\n(define (f x)\n  (+ x 1))) ; one\n\n(define y 2)\n") good "extra closer deleted")
(check-equal? (repair "#lang racket/base\n(let ([x 1)) x)\n") "#lang racket/base\n(let ([x 1]) x)\n" "mismatched closer replaced")
(check-equal? (code-end-col "(a \"x;y\") ; c" 1) 10)
(check-equal? (code-end-col "(a #\\;)  ; c" 1) 8)

;; ---------------------------------------------------------------------------------------------
;; API diff classification

(define (diff-kinds old new) (map (λ (f) (hash-ref f 'kind)) (api-diff "m.rkt" old new)))
(check-equal? (diff-kinds '((f procedure 1 (() ()) #f)) '((f procedure (1 2) (() ()) #f))) '(arity-widened))
(check-equal? (diff-kinds '((f procedure (1 2) (() ()) #f)) '((f procedure 1 (() ()) #f))) '(arity-narrowed))
(check-equal? (diff-kinds '((f procedure (>= 1) (() ()) #f)) '((f procedure (1 2 3) (() ()) #f))) '(arity-narrowed))
(check-equal? (diff-kinds '((f procedure 1 (() (#:k)) #f)) '((f procedure 1 ((#:k) (#:k)) #f))) '(keywords-changed))
(check-equal? (diff-kinds '((f procedure 1 (() ()) "(-> any)")) '((f procedure 1 (() ()) "(-> int)"))) '(contract-changed))
(check-equal? (diff-kinds '((f procedure 1 (() ()) #f) (g macro #f #f #f)) '((f macro #f #f #f) (h value #f #f #f)))
              '(removed-export kind-changed added-export))

;; ---------------------------------------------------------------------------------------------
;; small helpers

(check-equal? (closest "--afer" '("--after" "--anchor" "--goal")) '("--after"))
(check-equal? (iso->seconds (now-iso 1700000000)) 1700000000)
(let-values ([(pos o) (parse-args "x" '("a" "--after" "T1,T2" "--after=T3" "--flag") '(("--after" many) ("--flag" bool)))])
  (check-equal? pos '("a"))
  (check-equal? (opt-ids o 'after) '("T1" "T2" "T3"))
  (check-true (hash-ref o 'flag)))
(check-exn exn:steer? (λ () (parse-args "x" '("--nope") '())))

(check-equal? (kinds "(module m '#%kernel\n  (define-values (x) 1))\n") '() "explicit module form needs no #lang")
