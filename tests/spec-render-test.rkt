#lang racket/base
;; T30: the deterministic renderer. Canonical prose, JSON, and the round-trip property parse(render(x)) = x.
(require rackunit racket/list racket/string racket/port json "../steer/spec.rkt")

(define (parse t #:line [line 1]) (let-values ([(c fs) (parse-criterion t #:line line)]) (check-equal? fs '() t) c))
(define (canon t) (render-criterion (parse t)))

;; ---------------------------------------------------------------------------------------------
;; canonical form: keywords upper-case, verb lower-case, single spaces, one full stop

(check-equal? (canon "The export SHALL write a header row.") "The export SHALL write a header row.")
(check-equal? (canon "the export shall WRITE   a  header row .") "The export SHALL write a header row." "case, spacing and the full stop are normalised")
(check-equal? (canon "when the input is empty, the export shall write only the header row.")
              "WHEN the input is empty, the export SHALL write only the header row.")
(check-equal? (canon "while the file is locked, the editor shall refuse edits.") "WHILE the file is locked, the editor SHALL refuse edits.")
(check-equal? (canon "if the output file exists, then the export shall refuse and leave the file unchanged.")
              "IF the output file exists, THEN the export SHALL refuse and leave the file unchanged.")
(check-equal? (canon "where force is set, the export shall overwrite the file.") "WHERE force is set, the export SHALL overwrite the file.")
(check-equal? (canon "The tool SHALL exit.") "The tool SHALL exit." "no object, no trailing space")
(check-equal? (canon "The tool SHALL retry, then exit.") "The tool SHALL retry, then exit." "a comma right after the verb")
(check-equal? (canon "The tool SHALL return a list, sorted by name.") "The tool SHALL return a list, sorted by name.")
(check-equal? (canon "The EXPORT SHALL write x.") "The EXPORT SHALL write x." "the system's own capitalisation is kept")

;; render is idempotent: rendering what was rendered changes nothing
(for ([t '("the tool shall  exit   with code 2 ." "WHEN a and b, the tool shall write x, y." "IF the file exists, THEN the tool SHALL refuse.")])
  (define once (canon t))
  (check-equal? (canon once) once t))

;; ---------------------------------------------------------------------------------------------
;; blocks

(let-values ([(cs fs) (parse-criteria "The export SHALL write a header row.\n# c\nWHEN empty, the export SHALL write nothing.\n")])
  (check-equal? (render-criteria cs) "The export SHALL write a header row.\nWHEN empty, the export SHALL write nothing.")
  ;; the output can be fed straight back to the checker
  (let-values ([(cs2 fs2) (parse-criteria (render-criteria cs))])
    (check-equal? fs2 '())
    (check-true (andmap criterion=? cs cs2))))
(check-equal? (render-criteria '()) "")

;; ---------------------------------------------------------------------------------------------
;; JSON (note 03 protocol data)

(let ([j (criterion->jsexpr (parse "WHEN the input is empty, the export SHALL write only the header row." #:line 5))])
  (check-equal? (hash-ref j 'shape) "event")
  (check-equal? (hash-ref j 'condition) "the input is empty")
  (check-equal? (hash-ref j 'system) "export")
  (check-equal? (hash-ref j 'verb) "write")
  (check-equal? (hash-ref j 'object) "only the header row")
  (check-equal? (hash-ref j 'line) 5)
  (check-equal? (hash-ref j 'canonical) "WHEN the input is empty, the export SHALL write only the header row."))
(check-equal? (hash-ref (criterion->jsexpr (parse "The tool SHALL exit.")) 'condition) 'null "no condition serialises as JSON null")
(check-true (string? (jsexpr->string (criteria->jsexpr (list (parse "The tool SHALL exit."))))) "the data is valid JSON")
(check-equal? (string->jsexpr (jsexpr->string (criterion->jsexpr (parse "The tool SHALL exit with code 2."))))
              (criterion->jsexpr (parse "The tool SHALL exit with code 2.")) "JSON round trip")

;; ---------------------------------------------------------------------------------------------
;; the property: parse(render(x)) = x, over fixed fixtures and random criteria

(define fixtures
  '("The report module SHALL export records as CSV, one row per record."
    "The export SHALL write a header row taken from the record keys."
    "WHEN the output file exists and force is not set, the export SHALL refuse and leave the file unchanged."
    "IF the input is malformed, THEN the tool SHALL exit with code 2 and print the line number."
    "WHILE the file is locked, the editor SHALL refuse edits."
    "WHERE force is set, the export SHALL overwrite the file."
    "The tool SHALL keep the file if necessary."
    "WHEN the user is idle while the file is open, the tool SHALL exit."
    "The tool SHALL retry twice, then exit."))
(for ([t fixtures])
  (define c (parse t))
  (define-values (c2 fs) (parse-criterion (render-criterion c)))
  (check-equal? fs '() t)
  (check-true (criterion=? c c2) t))

(random-seed 20260927)
(define (pick l) (list-ref l (random (length l))))
(define systems '("export" "report module" "editor" "parser" "tool" "cache of the index" "the-thing"))
(define phrase-words '("the" "input" "is" "empty" "file" "exists" "if" "when" "while" "then" "where" "user" "is" "idle" "and" "not" "set" "of" "code" "2" "a" "header" "row" "rows" "in" "order"))
(define verbs '("write" "return" "refuse" "exit" "print" "create" "leave" "retry" "export"))
(define (phrase n) (string-join (for/list ([i (in-range (add1 (random n)))]) (pick phrase-words)) " "))
(define (random-criterion)
  (define shape (pick '(ubiquitous event state unwanted optional)))
  (define obj (case (random 4)
                [(0) ""]
                [(1) (phrase 4)]
                [(2) (string-append (phrase 3) ", " (phrase 3))]
                [else (string-append ", " (phrase 3))]))
  (hasheq 'shape shape
          'condition (if (eq? shape 'ubiquitous) #f (phrase 5))
          'system (pick systems) 'verb (pick verbs) 'object obj 'line 1))

(define failures
  (for/list ([i (in-range 400)]
             #:unless (let* ([c (random-criterion)] [text (render-criterion c)])
                        (define-values (c2 fs) (parse-criterion text))
                        (and c2 (null? fs) (criterion=? c c2) (equal? (render-criterion c2) text))))
    i))
(check-equal? failures '() "400 random criteria survive render → parse → render unchanged")
