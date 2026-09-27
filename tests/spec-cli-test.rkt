#lang racket/base
;; T32 (milestone M1): an agent can run `steer spec check -` on a block of criteria and get either the
;; structured form or located, fixable errors. Drives the CLI as an agent does; set STEER_BIN to run it
;; against the compiled binary (`make build && STEER_BIN=$PWD/build/steer raco test tests/spec-cli-test.rkt`).
(require rackunit racket/list racket/string racket/file racket/port racket/runtime-path json)

(define-runtime-path main-rkt "../steer/main.rkt")
(define bin (getenv "STEER_BIN"))

(define (steer #:in [input ""] . args)
  (define-values (p out in err)
    (if bin
        (apply subprocess #f #f 'stdout bin args)
        (apply subprocess #f #f 'stdout (find-executable-path "racket") (path->string main-rkt) args)))
  (write-string input in) (close-output-port in)
  (define text (port->string out))
  (subprocess-wait p) (close-input-port out)
  (values (subprocess-status p) text))

;; ---------------------------------------------------------------------------------------------
;; 1. the agent drafts criteria the way agents do: prose-ish, vague, weak modals

(define draft #<<DRAFT
# CSV export
The export should write one row per record.
The export SHALL write a header row, etc.
WHEN the output file exists and force is not set, the export SHALL refuse and leave it unchanged.
The export must be fast and handle errors gracefully.
The export SHALL write several rows per second.
DRAFT
  )

(define-values (code1 out1) (steer "spec" "check" "-" #:in draft))
(check-equal? code1 1 out1)
(check-regexp-match #rx"5 criteria: 1 ok, 4 refused" out1)
(check-regexp-match #rx"L4 event: export → refuse and leave it unchanged" out1 "the valid line comes back structured")
;; every refusal is located and carries a fix the agent can apply
(check-regexp-match #rx"error weak-modal <stdin>:2:12: .*→ write SHALL instead of `should`" out1)
(check-regexp-match #rx"error open-ended <stdin>:3:" out1)
(check-regexp-match #rx"error weak-modal <stdin>:5:12: `must`" out1)
(check-regexp-match #rx"(?s:error [a-z-]+ <stdin>:6:[0-9]+: .*several)" out1)

(define (list->set* l) (sort (remove-duplicates l) string<?))

;; ---------------------------------------------------------------------------------------------
;; 2. --json gives the same facts as data, with stable kinds for a program to branch on

(let-values ([(c o) (steer "--json" "spec" "check" "-" #:in draft)])
  (define j (string->jsexpr o))
  (check-equal? c 1)
  (check-equal? (hash-ref j 'ok) #f)
  (check-equal? (hash-ref (hash-ref j 'data) 'refused) 4)
  (check-equal? (list->set* (map (λ (f) (hash-ref f 'kind)) (hash-ref j 'findings)))
                (list->set* '("weak-modal" "open-ended" "vague-quantity")) "a weak modal is reported at the modal, not again as a missing verb")
  (for ([f (hash-ref j 'findings)])
    (check-true (and (hash-ref f 'line #f) (hash-ref f 'col #f) (hash-ref f 'fix #f) #t) "located and fixable")))

;; ---------------------------------------------------------------------------------------------
;; 3. the agent applies the fixes it was given, and the block passes

(define fixed #<<FIXED
# CSV export
The export SHALL write one row per record.
The export SHALL write a header row.
WHEN the output file exists and force is not set, the export SHALL refuse and leave it unchanged.
The export SHALL exit with code 2 and print the error, WHEN the input is malformed.
WHEN the input has 3 records, the export SHALL write at least 3 rows.
FIXED
  )
;; (line 5 above deliberately misplaces WHEN: the tool must say so rather than pass it)
(let-values ([(c o) (steer "spec" "check" "-" #:in fixed)])
  (check-equal? c 1 "a misplaced WHEN is still refused")
  (check-regexp-match #rx"error misplaced-condition <stdin>:5:.*move it to the front" o))
(define fixed2 (string-replace fixed "The export SHALL exit with code 2 and print the error, WHEN the input is malformed."
                               "IF the input is malformed, THEN the export SHALL exit with code 2 and print the error."))
(let-values ([(c o) (steer "spec" "check" "-" #:in fixed2)])
  (check-equal? c 0 o)
  (check-regexp-match #rx"5 criteria: 5 ok, 0 refused \\(0 errors, 0 warnings\\)" o)
  (check-regexp-match #rx"L5 unwanted: export → exit with code 2 and print the error  \\(if: the input is malformed\\)" o))

;; ---------------------------------------------------------------------------------------------
;; 4. canonical prose for humans, and it is itself accepted input

(let*-values ([(c o) (steer "spec" "render" "-" #:in fixed2)]
              [(c2 o2) (steer "spec" "check" "-" #:in o)])
  (check-equal? c 0 o)
  (check-regexp-match #rx"IF the input is malformed, THEN the export SHALL exit with code 2 and print the error\\." o)
  (check-equal? c2 0 o2))

;; ---------------------------------------------------------------------------------------------
;; 5. discoverable: help lists the command and states the templates

(let-values ([(c o) (steer "help")])
  (check-regexp-match #rx"spec +acceptance criteria in a controlled form" o))
(let-values ([(c o) (steer "help" "spec")])
  (check-regexp-match #rx"WHERE <feature>, the <system> SHALL" o))
(let-values ([(c o) (steer "--version")])
  (check-regexp-match #rx"^steer [0-9]+\\.[0-9]+\\.[0-9]+" o))
