#lang racket/base
;; T51: `add`/`import`/`edit` distinguish "not yet created" (the file is absent, or exists and the
;; task is plausibly about to add the name) from "unresolved" (the file exists and never defined
;; that name — almost always a typo or the wrong qualification), and suggest the closest real name.
(require rackunit racket/list racket/string racket/file racket/port racket/runtime-path json)

(define-runtime-path main-rkt "../steer/main.rkt")
(define dir (make-temporary-directory "steer-suggest~a"))
(define (steer #:in [input ""] . args)
  (define-values (p out in err)
    (parameterize ([current-directory dir])
      (apply subprocess #f #f 'stdout (find-executable-path "racket") (path->string main-rkt) args)))
  (write-string input in) (close-output-port in)
  (define text (port->string out))
  (subprocess-wait p) (close-input-port out)
  (values (subprocess-status p) text))
(define (put! name text) (call-with-output-file (build-path dir name) #:exists 'truncate (λ (o) (void (write-string text o)))))

(call-with-values (λ () (steer "init")) void)
(put! "lib.rkt" "#lang racket/base\n(provide export-csv helper)\n(define (helper x) x)\n(define (export-csv rows) rows)\n")
(put! "mod.py" "class First:\n    def __init__(self):\n        pass\n\n\ndef top(x):\n    return x\n")
(put! "Guard.cs" "namespace N { public class Guard { public int Limit => 3; } }\n")

;; ---------------------------------------------------------------------------------------------
;; add: a file that does not exist yet is "pending" — a plausible future creation, no suggestion needed

(let-values ([(c o) (steer "add" "t" "--anchor" "new-file.rkt#thing")])
  (check-equal? c 0 o)
  (check-regexp-match #rx"info anchor-pending T1: anchor new-file.rkt#thing does not exist yet" o)
  (check-false (regexp-match? #rx"anchor-unresolved" o)))

;; ---------------------------------------------------------------------------------------------
;; add: an existing file with a wrong/typo'd name is "unresolved", with a suggestion, and it is a
;; warning (not merely info): this is the case note 12 found silently mislabelled

(let-values ([(c o) (steer "add" "t2" "--anchor" "lib.rkt#helpr")])
  (check-equal? c 0 "a warning does not fail the command")
  (check-regexp-match #rx"warning anchor-unresolved T2: anchor lib.rkt#helpr: no definition of helpr" o)
  (check-regexp-match #rx"did you mean helper\\?" o))

(let-values ([(c o) (steer "add" "t3" "--anchor" "mod.py#First.__init_")])
  (check-regexp-match #rx"warning anchor-unresolved T3: anchor mod.py#First.__init_: no definition of First.__init_" o)
  (check-regexp-match #rx"did you mean First.__init__\\?" o))

(let-values ([(c o) (steer "add" "t4" "--anchor" "Guard.cs#Guard.Limt")])
  (check-regexp-match #rx"warning anchor-unresolved T4: anchor Guard.cs#Guard.Limt" o)
  (check-regexp-match #rx"did you mean Guard.Limit\\?" o))

;; a name that exists but is ambiguous (ties into the overload work) still reads as unresolved, with
;; the real candidates as the suggestion, not a fuzzy guess
(put! "mod.py" "class First:\n    def __init__(self):\n        pass\n\nclass Second:\n    def __init__(self):\n        pass\n")
(let-values ([(c o) (steer "add" "t5" "--anchor" "mod.py#__init__")])
  (check-regexp-match #rx"warning anchor-unresolved T5: anchor mod.py#__init__" o)
  (check-regexp-match #rx"did you mean First.__init__\\?" o))

;; --json carries the same distinction as structured data
(let-values ([(c o) (steer "--json" "add" "t6" "--check" "true" "--anchor" "lib.rkt#helpr")])
  (define f (findf (λ (x) (equal? (hash-ref x 'kind) "anchor-unresolved")) (hash-ref (string->jsexpr o) 'findings)))
  (check-true (and f #t) "an anchor-unresolved finding is present")
  (check-regexp-match #rx"did you mean helper" (hash-ref f 'fix)))
(let-values ([(c o) (steer "--json" "add" "t7" "--check" "true" "--anchor" "new-file.rkt#thing")])
  (define f (findf (λ (x) (equal? (hash-ref x 'kind) "anchor-pending")) (hash-ref (string->jsexpr o) 'findings)))
  (check-true (and f #t) "an anchor-pending finding is present"))

;; a language with no name lister (the indentation heuristic) still distinguishes the two cases,
;; just without a specific suggestion
(put! "app.rb" "def existing\nend\n")
(let-values ([(c o) (steer "add" "t8" "--anchor" "app.rb#missing")])
  (check-regexp-match #rx"warning anchor-unresolved T8: anchor app.rb#missing" o)
  (check-regexp-match #rx"check the name and its qualification" o))
(let-values ([(c o) (steer "add" "t9" "--anchor" "new.rb#thing")])
  (check-regexp-match #rx"info anchor-pending T9" o))

;; ---------------------------------------------------------------------------------------------
;; import: the same distinction, reported per created task

(let-values ([(c o) (steer "import" "-" #:in "(task \"a\" #:anchor \"lib.rkt#helpr\" #:check \"true\")\n(task \"b\" #:anchor \"brand-new.rkt#x\" #:check \"true\")\n")])
  (check-equal? c 0 o)
  (check-regexp-match #rx"warning anchor-unresolved T[0-9]+: anchor lib.rkt#helpr.*did you mean helper" o)
  (check-regexp-match #rx"info anchor-pending T[0-9]+: anchor brand-new.rkt#x does not exist yet" o))

;; ---------------------------------------------------------------------------------------------
;; edit --add-anchor: the same check runs when an anchor is added after the fact

(let-values ([(c o) (steer "add" "t10")]) (void))
(let-values ([(c o) (steer "edit" "T12" "--add-anchor" "lib.rkt#exprt-csv")])
  (check-equal? c 0 o)
  (check-regexp-match #rx"warning anchor-unresolved T12: anchor lib.rkt#exprt-csv.*did you mean export-csv" o))
(let-values ([(c o) (steer "edit" "T12" "--add-anchor" "lib.rkt#export-csv")])
  (check-false (regexp-match? #rx"anchor-unresolved" o) "the correct name raises no warning"))

(delete-directory/files dir)
