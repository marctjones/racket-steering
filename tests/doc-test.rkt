#lang racket/base
;; `steer doc`: HTML parsing on fixtures (always runs), and end-to-end lookups against the installed
;; Racket documentation (skipped, loudly, when racket or its docs are missing).
(require rackunit racket/list racket/string racket/file racket/port racket/runtime-path json
         setup/dirs "../steer/doc.rkt")

;; ---------------------------------------------------------------------------------------------
;; pure: the fragment format is the real docs HTML for string-contains? (Racket 9.3), shortened

(define fragment
  (string-append
   "<p class=\"RForeground\"><span class=\"RktPn\">(</span><a name=\"(def._((lib._racket/string..rkt)._string-contains~3f))\"></a>"
   "<span title=\"Provided from: racket/string, racket | Package: base\"><span class=\"RktSym\">"
   "<a href=\"#x\" class=\"RktValDef RktValLink\" data-pltdoc=\"x\">string-contains?</a></span></span>"
   "<span class=\"hspace\">&nbsp;</span><span class=\"RktVar\">s</span><span class=\"hspace\">&nbsp;</span>"
   "<span class=\"RktVar\">contained</span><span class=\"RktPn\">)</span><span class=\"hspace\">&nbsp;</span>&rarr;"
   "<span class=\"hspace\">&nbsp;</span><span class=\"RktSym\"><a href=\"booleans.html#x\">boolean?</a></span></p>"
   "</blockquote></td></tr>"
   "<tr><td><span class=\"hspace\">&nbsp;&nbsp;</span><span class=\"RktVar\">s</span><span class=\"hspace\">&nbsp;</span>:"
   "<span class=\"hspace\">&nbsp;</span><span class=\"RktSym\"><a href=\"#y\">string?</a></span></td></tr>"
   "<tr><td><span class=\"hspace\">&nbsp;&nbsp;</span><span class=\"RktVar\">contained</span><span class=\"hspace\">&nbsp;</span>:"
   "<span class=\"hspace\">&nbsp;</span><span class=\"RktSym\"><a href=\"#y\">string?</a></span></td></tr>"
   "</table></td></tr></table></blockquote></div>"
   "<div class=\"SIntrapara\">Checks whether <span class=\"RktVar\">s</span> includes at any location, starts with, or ends with\n"
   "the second argument, respectively. The <span class=\"RktSym\"><a href=\"#z\">string-find</a></span> function returns the position.</div>"
   "</p><p><div class=\"SIntrapara\">Examples:</div>"))

(let-values ([(sig args desc) (parse-doc-fragment fragment)])
  (check-equal? sig "(string-contains? s contained) → boolean?")
  (check-equal? args '("s : string?" "contained : string?"))
  (check-equal? desc "Checks whether s includes at any location, starts with, or ends with the second argument, respectively. The string-find function returns the position."))

(let-values ([(sig args desc) (parse-doc-fragment "<p>not a definition</p>")])
  (check-false sig) (check-equal? args '()) (check-false desc "unrecognised markup gives nothing, not garbage"))

(check-equal? (html->text "a&nbsp;<b>b</b> &rarr; c &lt;d&gt; &#233;t&eacute;") "a b → c <d> ét&eacute;" "known entities decoded, unknown left as is")
(check-equal? (first-sentences "One two. Three four five six.") "One two. Three four five six.")
(check-equal? (first-sentences (string-append "First sentence. " (make-string 300 #\x)) 40) "First sentence.")
(check-equal? (string-length (first-sentences (make-string 500 #\y) 50)) 50 "no sentence end: hard cut with an ellipsis")
(check-equal? (doc-url "/opt/racket/doc/reference/strings.html" "(def._x)")
              "https://docs.racket-lang.org/reference/strings.html#(def._x)")
(check-false (doc-url "/somewhere/else.html" #f))

;; ---------------------------------------------------------------------------------------------
;; suggestions (pure)

(define pool '("sort" "take" "append" "string-prefix?" "string-suffix?" "hash-ref" "hash-set" "hash-remove" "list" "list?"
               "list*" "foldl" "string?" "string-append" "string-join" "string-split" "remove-duplicates" "add1" "sub1"
               "map" "filter" "list-ref" "empty?" "first" "hash" "string-upcase" "number->string"))
(define (top id) (let ([r (suggest-names id pool)]) (and (pair? r) (car r))))
(check-equal? (top "list-sort") "sort" "type prefix dropped, verb matched")
(check-equal? (top "list-append") "append")
(check-equal? (top "string-starts-with?") "string-prefix?" "phrase synonym")
(check-equal? (top "hash-put") "hash-set" "hash-set is an operation, not a bare type")
(check-equal? (top "1+") "add1" "whole-name synonym")
(check-equal? (top "nth") "list-ref")
(check-equal? (top "list-reduce") "foldl" "synonym applied to the verb part")
(check-equal? (top "is-string?") "string?" "is- habit becomes a predicate")
(check-equal? (top "split-string") "string-split" "token order does not matter")
(check-equal? (top "num->string") "number->string" "edit distance still works")
(check-equal? (top "list-unique") "remove-duplicates")
(check-false (for/or ([id '("list-sort" "list-append" "list-map")]) (member "list" (suggest-names id pool)))
             "a bare type name is never a suggestion")
(check-equal? (suggest-names "zzzzzz" pool) '() "no suggestion beats a wrong one")
(check-equal? (name-tokens "string->number") '("string" "number"))
(check-equal? (name-tokens "for*/list") '("for" "list"))

;; ---------------------------------------------------------------------------------------------
;; end to end

(define-runtime-path main-rkt "../steer/main.rkt")
(define docs? (and (find-executable-path "racket")
                   ;; the cross-reference index, not just the HTML, must be built (`raco setup` writes out*.sxref)
                   (let ([d (find-doc-dir)])
                     (and d (directory-exists? (build-path d "reference"))
                          (pair? (for/list ([f (directory-list (build-path d "reference"))]
                                            #:when (regexp-match? #rx"^out[0-9]*[.]sxref$" (path->string f)))
                                   f))))))

(define (steer . args)
  (define-values (p out in err)
    (apply subprocess #f #f 'stdout (find-executable-path "racket") (path->string main-rkt) args))
  (close-output-port in)
  (define text (port->string out))
  (subprocess-wait p) (close-input-port out)
  (values (subprocess-status p) text))

(cond
  [(not docs?)
   (eprintf "doc-test: SKIPPED end-to-end tests: racket or its installed documentation not found\n")]
  [else
   (let-values ([(c o) (steer "doc" "exists" "string-contains?")])
     (check-equal? c 0 o)
     (check-regexp-match #rx"yes: string-contains\\? · procedure · \\(require racket/string\\)" o))
   (let-values ([(c o) (steer "doc" "exists" "string-contain")])
     (check-equal? c 1 "a hallucinated name exits 1")
     (check-regexp-match #rx"not documented.*closest names: string-contains\\?" o))
   (let-values ([(c o) (steer "doc" "exists" "string-starts-with?")])
     (check-equal? c 1)
     (check-regexp-match #rx"closest names: string-prefix\\?" o))
   (let-values ([(c o) (steer "doc" "exists" "fold")])
     (check-equal? c 0 "it exists in srfi/1, so exit 0 with a warning")
     (check-regexp-match #rx"warning not-in-racket.*not in racket/\\*.*use foldl" o))
   (let-values ([(c o) (steer "doc" "sig" "foldl")])
     (check-equal? c 0 o)
     (check-regexp-match #rx"\\(foldl proc init lst \\.\\.\\.\\+\\)" o)
     (check-regexp-match #rx"proc : procedure\\?" o)
     (check-regexp-match #rx"runtime: arity 3\\+" o)
     (check-regexp-match #rx"^foldl · procedure · \\(require racket/base\\)" o "main library ranks before teaching languages"))
   (let-values ([(c o) (steer "doc" "sig" "string-trim" "racket/string")])
     (check-equal? c 0 o)
     (check-regexp-match #rx"string-trim" o))
   (let-values ([(c o) (steer "doc" "search" "string" "contains")])
     (check-equal? c 0 o)
     (check-regexp-match #rx"string-contains\\? · procedure · \\(require racket/string\\)" o))
   (let-values ([(c o) (steer "doc" "exports" "racket/string")])
     (check-equal? c 0 o)
     (check-regexp-match #rx"string-join · procedure arity 1\\|2" o "contract-wrapped exports are not mistaken for macros"))
   (let-values ([(c o) (steer "--json" "doc" "exists" "string-contains?")])
     (define j (string->jsexpr o))
     (check-equal? c 0)
     (check-true (hash-ref (hash-ref j 'data) 'exists))
     (check-equal? (hash-ref (car (hash-ref (hash-ref j 'data) 'hits)) 'libs) '("racket/string" "racket")))
   (let-values ([(c o) (steer "--json" "doc" "exists" "no-such-thing-at-all")])
     (define j (string->jsexpr o))
     (check-equal? c 1)
     (check-false (hash-ref j 'ok))
     (check-false (hash-ref (hash-ref j 'data) 'exists)))
   ;; a project module: contract-out is read at runtime, and it is marked undocumented
   (let* ([dir (make-temporary-directory "steer-doc~a")]
          [f (build-path dir "m.rkt")])
     (call-with-output-file f
       (λ (o) (void (write-string "#lang racket/base\n(require racket/contract)\n(provide (contract-out [add2 (-> integer? integer? integer?)]) helper)\n(define (add2 a b) (+ a b))\n(define (helper x [y 1]) x)\n" o))))
     (let-values ([(c o) (steer "doc" "exports" (path->string f))])
       (check-equal? c 0 o)
       (check-regexp-match #rx"add2 · procedure arity 2 \\(-> integer\\? integer\\? integer\\?\\) \\(undocumented\\)" o)
       (check-regexp-match #rx"helper · procedure arity 1\\|2 \\(undocumented\\)" o))
     (delete-directory/files dir))
   (let-values ([(c o) (steer "doc" "sig")])
     (check-equal? c 2 "usage error"))])
