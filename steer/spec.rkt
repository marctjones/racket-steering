#lang racket/base
;; Controlled acceptance criteria (catalog G2, note 11): one EARS-shaped sentence per criterion.
;;
;;   The <system> SHALL <verb> <what>.                       ubiquitous
;;   WHEN <trigger>, the <system> SHALL <verb> <what>.       event
;;   WHILE <state>, the <system> SHALL <verb> <what>.        state
;;   IF <trigger>, THEN the <system> SHALL <verb> <what>.    unwanted behaviour
;;   WHERE <feature>, the <system> SHALL <verb> <what>.      optional feature
;;
;; <verb> must be an *observable* one, in the base form (write, return, refuse, exit, print, contain, ...): a criterion
;; a test can check. That rule lives in the grammar (the lexer classes observable verbs as VERB), so
;; "the code SHALL work" is a parse error, with the verb list as the fix, not a lint afterthought.
;; parser-tools (lex + yacc) is in the main Racket distribution, so the standalone binary keeps working.
;; Vagueness that still parses (open-ended lists, subjective adjectives, ...) is the lint in this file.
(require parser-tools/lex parser-tools/yacc (prefix-in : parser-tools/lex-sre)
         racket/list racket/string
         "common.rkt")
(provide parse-criterion parse-criteria
         render-criterion render-criteria criterion->jsexpr criteria->jsexpr criterion=?
         lint-criterion lint-criteria lint-rules
         observable-verbs weak-modals templates-text shapes)

;; ---------------------------------------------------------------------------------------------
;; Data (grows from measured failures, note 04)

(define observable-verbs
  '("write" "return" "refuse" "reject" "print" "output" "display" "show" "list" "exit" "create" "delete"
    "remove" "rename" "append" "leave" "contain" "include" "exclude" "report" "log" "emit" "send"
    "respond" "read" "compute" "produce" "sort" "keep" "preserve" "store" "save" "open" "close" "fail"
    "raise" "signal" "accept" "ignore" "truncate" "overwrite" "retry" "wait" "revert" "restore"
    "update" "set" "increment" "decrement" "copy" "move" "parse" "format" "encode" "decode"
    ;; widened after note 11's own example ("SHALL export records as CSV") was rejected: false rejects are
    ;; the known risk of this grammar, measured in M3 (T38)
    "export" "import" "generate" "render" "build" "publish" "upload" "download" "install" "uninstall"
    "start" "stop" "terminate" "notify" "warn" "prompt" "call" "invoke" "run" "load" "mark" "flag" "tag"
    "label" "name" "number" "assign" "add" "insert" "replace" "merge" "split" "join" "skip" "echo"
    "convert" "translate" "normalize" "escape" "quote" "trim" "pad" "hash" "sign" "compress"
    "decompress" "extract" "select" "filter" "group" "count" "sum" "round" "truncate" "limit" "cap"))

;; "SHALL writes" is the natural slip: the modal takes the base form
(define (third-person v)
  (cond [(regexp-match? #rx"[^aeiou]y$" v) (string-append (substring v 0 (sub1 (string-length v))) "ies")]
        [(regexp-match? #rx"(s|x|z|ch|sh)$" v) (string-append v "es")]
        [else (string-append v "s")]))

(define third-person-forms (for/hash ([v observable-verbs]) (values (third-person v) v)))

;; not allowed as the modal: the template says SHALL
(define weak-modals '("should" "may" "might" "could" "would" "can" "ought" "must" "will"))

(define shapes '(ubiquitous event state unwanted optional))

(define templates-text
  (string-join '("The <system> SHALL <verb> <what>."
                 "WHEN <trigger>, the <system> SHALL <verb> <what>."
                 "WHILE <state>, the <system> SHALL <verb> <what>."
                 "IF <trigger>, THEN the <system> SHALL <verb> <what>."
                 "WHERE <feature>, the <system> SHALL <verb> <what>.")
               "   "))

;; ---------------------------------------------------------------------------------------------
;; Lexer and grammar

(define-tokens value-tokens (WORD VERB WEAK))
(define-empty-tokens punct-tokens (WHEN WHILE IF THEN WHERE SHALL THE COMMA PERIOD EOF))

(define (classify lexeme)
  (define low (string-downcase lexeme))
  (cond [(equal? low "when") (token-WHEN)]
        [(equal? low "while") (token-WHILE)]
        [(equal? low "if") (token-IF)]
        [(equal? low "then") (token-THEN)]
        [(equal? low "where") (token-WHERE)]
        [(equal? low "shall") (token-SHALL)]
        [(equal? low "the") (token-THE)]
        [(member low weak-modals) (token-WEAK lexeme)]
        [(member low observable-verbs) (token-VERB lexeme)]
        [else (token-WORD lexeme)]))

(define crit-lexer
  (lexer-src-pos
   [(:+ whitespace) (return-without-pos (crit-lexer input-port))]
   [#\, (token-COMMA)]
   [(:+ (:~ whitespace #\,)) (classify lexeme)]
   [(eof) (token-EOF)]))

;; a syntax failure carries the failing token; `explain` turns it into a message
(struct parse-failure (name value start end))

(define crit-parser
  (parser
   (start crit)
   (end EOF)
   (src-pos)
   (tokens value-tokens punct-tokens)
   (error (λ (ok? name value start end) (raise (parse-failure name value start end))))
   (grammar
    (crit [(clause) (list 'ubiquitous #f $1)]
          [(WHEN text COMMA clause) (list 'event $2 $4)]
          [(WHILE text COMMA clause) (list 'state $2 $4)]
          [(IF text COMMA THEN clause) (list 'unwanted $2 $5)]
          [(WHERE text COMMA clause) (list 'optional $2 $4)])
    (clause [(THE text SHALL VERB obj PERIOD) (list $2 $4 $5)])
    ;; the response may contain commas ("return a list, sorted by name"); a condition may not
    (obj [() ""]
         [(otext) $1]
         [(COMMA otext) (string-append ", " $2)])                ; "SHALL retry, then exit" (the renderer joins without a space)
    (otext [(tok) $1]
           [(otext tok) (string-append $1 " " $2)]
           [(otext COMMA) (string-append $1 ",")])
    (text [(tok) $1]
          [(text tok) (string-append $1 " " $2)])
    ;; clause keywords are ordinary words inside a phrase ("keep the file if necessary"); they only start a clause
    (tok [(WORD) $1]
         [(VERB) $1]
         [(THE) "the"]
         [(WHEN) "when"] [(WHILE) "while"] [(IF) "if"] [(THEN) "then"] [(WHERE) "where"]))))

;; ---------------------------------------------------------------------------------------------
;; Tokens with positions (for explaining errors)

(struct tk (name value line col))                 ; col is 1-based

(define (lex-all body line)
  (define in (open-input-string body))
  (port-count-lines! in)
  (let loop ([acc '()])
    (define pt (crit-lexer in))
    (define t (position-token-token pt))
    (define st (position-token-start-pos pt))
    (define name (if (token? t) (token-name t) t))
    (define value (and (token? t) (token-value t)))
    (define col (add1 (position-col st)))
    (if (eq? name 'EOF)
        (reverse acc)
        (loop (cons (tk name value line col) acc)))))

(define (keyword-text name value)
  (case name
    [(WHEN) "WHEN"] [(WHILE) "WHILE"] [(IF) "IF"] [(THEN) "THEN"] [(WHERE) "WHERE"] [(SHALL) "SHALL"]
    [(THE) "the"] [(COMMA) ","] [(PERIOD) "."] [(EOF) "the end of the line"]
    [else (format "~a" value)]))

;; ---------------------------------------------------------------------------------------------
;; Explaining a syntax error: (values kind message fix) from what came before the failing token

(define (explain toks idx bad-name bad-value)
  (define before (take toks (min idx (length toks))))
  (define names (map tk-name before))
  (define seen (keyword-text bad-name bad-value))
  (define first-name (and (pair? toks) (tk-name (car toks))))
  (define clause-kw (and (pair? names) (memq (car names) '(WHEN WHILE IF WHERE)) (car names)))
  (define has-comma? (and (memq 'COMMA names) #t))
  (define after-shall? (and (pair? names) (eq? (last names) 'SHALL)))
  (define verbs-hint (string-join (take observable-verbs 8) ", "))
  (cond
    [(eq? bad-name 'WEAK)
     (values 'weak-modal
             (format "`~a` is not allowed: a criterion uses SHALL, so a test can hold it to the letter" bad-value)
             (format "write SHALL instead of `~a`" bad-value))]
    [(and after-shall? (eq? bad-name 'WORD) (hash-ref third-person-forms (string-downcase (format "~a" bad-value)) #f))
     => (λ (base)
          (values 'verb-form
                  (format "SHALL takes the base form of the verb: `~a`, not `~a`" base bad-value)
                  (format "write `SHALL ~a`" base)))]
    [after-shall?
     (values 'no-observable
             (format "after SHALL comes an observable verb (~a, ...), found `~a`" verbs-hint seen)
             "say what a test can see, e.g. `SHALL return <value>`, `SHALL write <file>`, `SHALL refuse <input>`, `SHALL exit with code 2`")]
    [(and (null? before) (not (memq bad-name '(THE WHEN WHILE IF WHERE))))
     (values 'bad-start
             (format "a criterion starts with The, WHEN, WHILE, IF or WHERE, found `~a`" seen)
             (string-append "use one of: " templates-text))]
    [(and (eq? clause-kw 'IF) has-comma? (not (memq 'THEN names)) (not (eq? bad-name 'THEN)))
     (values 'missing-then (format "IF <trigger>, THEN the <system> SHALL ...: expected THEN after the comma, found `~a`" seen)
             "write `IF <trigger>, THEN the <system> SHALL <verb> <what>.`")]
    [(and clause-kw (not has-comma?) (memq bad-name '(SHALL PERIOD EOF)))
     (values 'missing-comma (format "expected a comma after the ~a condition, before `the <system> SHALL`" (symbol->string clause-kw))
             (format "write `~a <condition>, ~athe <system> SHALL <verb> <what>.`" (keyword-text clause-kw #f)
                     (if (eq? clause-kw 'IF) "THEN " "")))]
    [(and (pair? names) (eq? (last names) 'COMMA) (not (eq? bad-name 'THE)) (not (eq? first-name 'IF)))
     (values 'missing-the (format "expected `the <system>` after the comma, found `~a`" seen)
             "name the system after `the`, e.g. `the export SHALL write ...`")]
    [(memq bad-name '(PERIOD EOF))
     (values 'missing-shall (format "the sentence ends before SHALL: expected `the <system> SHALL <verb> <what>.`")
             (string-append "use one of: " templates-text))]
    [else
     (values 'unexpected (format "unexpected `~a` here" seen)
             (string-append "use one of: " templates-text))]))

;; ---------------------------------------------------------------------------------------------
;; Public

;; One criterion. text: the line (no newline). line: 1-based line number in the source.
;; → (values criterion-or-#f findings). A criterion is a hasheq:
;;   shape system verb object condition line text
(define (parse-criterion text #:line [line 1] #:source [source "criteria"])
  (define trimmed (string-trim text))
  (define m (regexp-match-positions #px"\\s*[.]\\s*$" text))
  (define body (if m (substring text 0 (caar m)) text))
  (define lead (- (string-length text) (string-length (string-trim text #:right? #f))))
  (define toks (lex-all body line))
  (define end-col (add1 (string-length body)))
  (define ended-toks (append toks (list (tk 'PERIOD #f line end-col))))
  (define (err kind msg fix col)
    (values #f (list (finding 'error kind msg #:file source #:line line #:col col #:fix fix))))
  (cond
    [(string=? trimmed "") (err 'empty-criterion "the line is empty" "write a criterion, or delete the line" 1)]
    [else
     ;; the parser sees the tokens plus PERIOD, then EOF
     (define stream
       (let ([rest (append ended-toks (list (tk 'EOF #f line end-col)))] [i 0])
         (λ ()
           (define t (car rest))
           (set! rest (if (null? (cdr rest)) rest (cdr rest)))
           (set! i (add1 i))
           (define pos (make-position i line (sub1 (tk-col t))))
           (define out
             (case (tk-name t)
               [(WORD) (token-WORD (tk-value t))]
               [(VERB) (token-VERB (tk-value t))]
               [(WEAK) (token-WEAK (tk-value t))]
               [else (tk-name t)]))
           (make-position-token out pos pos))))
     (define result
       (with-handlers ([parse-failure? values])
         (crit-parser stream)))
     (cond
       [(parse-failure? result)
        (define start (parse-failure-start result))
        (define idx (let ([off (sub1 (position-offset start))]) off))
        (define bad-tk (and (< idx (length ended-toks)) (list-ref ended-toks idx)))
        (define-values (kind msg fix)
          (explain ended-toks idx (parse-failure-name result) (parse-failure-value result)))
        (err kind msg fix (if bad-tk (tk-col bad-tk) end-col))]
       [(not m)
        (err 'missing-period "a criterion ends with a full stop" "add `.` at the end of the line" end-col)]
       [else
        (define shape (car result))
        (define condition (cadr result))
        (define clause (caddr result))
        (values (hasheq 'shape shape 'condition condition 'system (car clause) 'verb (string-downcase (cadr clause))
                        'object (caddr clause) 'line line 'text trimmed 'indent lead)
                '())])]))

;; A whole block: one criterion per line; blank lines and lines starting with `#` are skipped.
;; → (values criteria findings) in line order
(define (parse-criteria text #:source [source "criteria"])
  (for/fold ([crits '()] [fs '()] #:result (values (reverse crits) (reverse fs)))
            ([l (string-split text "\n" #:trim? #f)] [n (in-naturals 1)]
             #:unless (let ([t (string-trim l)]) (or (string=? t "") (regexp-match? #rx"^#" t))))
    (define-values (c f) (parse-criterion (string-trim l "\r" #:left? #f) #:line n #:source source))
    (values (if c (cons c crits) crits) (append (reverse f) fs))))

;; ---------------------------------------------------------------------------------------------
;; Lint: vagueness that still parses. The rules are data (id severity scope matcher message fix) and
;; meant to grow from measured failures (note 04's taxonomy). severity error = refused, warning = flagged.
;; scope `response` only looks after SHALL: a condition such as "the file is locked" describes a state.
;; matcher: a regexp (each match is a finding) or a procedure text -> list of (start . end).

(struct lint-rule (id severity scope matcher message fix))

(define (words rx-alts) (pregexp (string-append "(?i:\\b(?:" rx-alts ")\\b)")))

(define stop-after-number '("and" "or" "of" "the" "a" "an" "to" "than" "from" "but" "in" "on"))

;; a number after a bound word ("within 5", "at least 3") must carry a unit: "5 seconds", "200ms", "3 rows"
(define (bare-quantities text)
  (for*/list ([m (regexp-match-positions*
                  #px"(?i:\\b(?:within|under|less than|fewer than|more than|at least|at most|up to|over|after|before|every|each|for)\\s+)(\\d+(?:[.]\\d+)?)([a-zA-Z%]*)(\\s+([A-Za-z]+))?"
                  text #:match-select values)]
              [num (in-value (cadr m))]
              [attached (in-value (substring text (car (caddr m)) (cdr (caddr m))))]
              [next (in-value (and (list-ref m 3) (string-downcase (substring text (car (list-ref m 4)) (cdr (list-ref m 4))))))]
              #:when (and (string=? attached "") (or (not next) (member next stop-after-number))))
    (cons (car num) (cdr num))))

(define lint-rules
  (list
   (lint-rule 'open-ended 'error 'all
              (words "etc[.]?|and so on|and the like|and similar|and more|among others")
              "an open-ended list: a test cannot tell when it is complete"
              "list every item, or bound it: `at least <n> of ...`")
   (lint-rule 'and-or 'error 'all
              #px"(?i:\\band/or\\b|\\bor/and\\b)"
              "`and/or` is ambiguous: inclusive or exclusive?"
              "write `and` or `or`, or split it into two criteria")
   (lint-rule 'subjective 'error 'all
              (words "fast|faster|quick|quickly|slow|slowly|efficient|efficiently|user-friendly|user friendly|easy|easily|simple|simply|intuitive|intuitively|robust|reasonable|reasonably|adequate|adequately|sufficient|sufficiently|appropriate|appropriately|flexible|seamless|seamlessly|modern|state-of-the-art|good|better|best|nice|nicely|properly|proper|correctly|gracefully|cleanly|as expected")
              "a subjective word: two people can disagree whether a result has it"
              "state the measurable condition, e.g. `within 200 ms`, `with exit code 0`, `without printing anything`")
   (lint-rule 'hedge 'error 'all
              (words "as needed|as appropriate|as required|as applicable|if necessary|where possible|when possible|if possible|whenever possible|where appropriate|if applicable")
              "a hedge: a test cannot check `as needed`"
              "say exactly when it applies: `WHEN <trigger>, ...` or `WHERE <feature>, ...`")
   (lint-rule 'vague-quantity 'error 'all
              (words "several|many|few|some|multiple|various|numerous|(?<!at )most|a lot of|a number of|a couple of|a few")
              "a vague quantity: how many?"
              "give the number or a bound: `exactly 3`, `at least 3`, `at most 10`")
   ;; keywords are plain words inside a phrase ("keep the file if necessary"), so a condition tacked on
   ;; after the response would otherwise pass unseen: a comma before one, or the all-caps form, gives it away
   (lint-rule 'misplaced-condition 'error 'response
              #px"(?:,\\s*(?i:when|while|if|where)\\b|\\b(?-i:WHEN|WHILE|IF|WHERE)\\b)"
              "a condition after the response: the trigger is hidden from the parser and the test"
              "move it to the front: `WHEN <trigger>, the <system> SHALL <verb> <what>.` (or IF ..., THEN ...)")
   (lint-rule 'bare-quantity 'warning 'all
              bare-quantities
              "a number without a unit"
              "add the unit: `5 seconds`, `200 ms`, `3 rows`")
   (lint-rule 'passive 'warning 'response
              #px"(?i:\\b(?:is|are|was|were|be|been|being)\\s+(?!open\\b|given\\b|empty\\b)[a-z]+(?:ed|en)\\b(?!\\s+by\\b))"
              "passive voice hides who does it"
              "name the actor: `the <system> SHALL <verb> <what>` (or add `by <who>`)")))

;; → list of (start . end) matches of `rule` in `text`, honouring its scope
(define (rule-matches rule text)
  (define shall-end (let ([m (regexp-match-positions #px"(?i:\\bSHALL\\b)" text)]) (if m (cdar m) 0)))
  (define m (lint-rule-matcher rule))
  (define all (if (procedure? m) (m text) (regexp-match-positions* m text)))
  (if (eq? (lint-rule-scope rule) 'response)
      (filter (λ (p) (>= (car p) shall-end)) all)
      all))

;; findings for one parsed criterion (a hasheq from parse-criterion), sorted by column
(define (lint-criterion crit #:source [source "criteria"] #:lax? [lax? #f])
  (define text (hash-ref crit 'text))
  (define indent (hash-ref crit 'indent 0))
  ;; a passive inside a misplaced condition ("..., WHEN the input is malformed") is noise beside the real error
  (define misplaced-at
    (let ([r (findf (λ (r) (eq? (lint-rule-id r) 'misplaced-condition)) lint-rules)])
      (define ms (if r (rule-matches r text) '()))
      (and (pair? ms) (apply min (map car ms)))))
  (sort
   (for*/list ([rule lint-rules]
               [p (rule-matches rule text)]
               #:unless (and misplaced-at (eq? (lint-rule-id rule) 'passive) (>= (car p) misplaced-at)))
     (define matched (substring text (car p) (cdr p)))
     (finding (if (and lax? (eq? (lint-rule-severity rule) 'error)) 'warning (lint-rule-severity rule))
              (lint-rule-id rule)
              (format "`~a`: ~a" (string-trim matched) (lint-rule-message rule))
              #:file source #:line (hash-ref crit 'line) #:col (+ indent (car p) 1)
              #:fix (lint-rule-fix rule)))
   < #:key (λ (f) (hash-ref f 'col))))

(define (lint-criteria crits #:source [source "criteria"] #:lax? [lax? #f])
  (append* (for/list ([c crits]) (lint-criterion c #:source source #:lax? lax?))))

;; ---------------------------------------------------------------------------------------------
;; Rendering: a structured criterion back to its one canonical sentence. Keywords upper-case, the verb
;; lower-case, single spaces. Humans read this prose; the model never had to write it in this exact form.
;; Property (tested, including over random criteria): parse(render(x)) = x on the structural fields.

(define (render-criterion c)
  (define shape (hash-ref c 'shape))
  (define condition (hash-ref c 'condition))
  (define object (hash-ref c 'object))
  (define clause
    (string-append "SHALL " (hash-ref c 'verb)
                   (cond [(string=? object "") ""]
                         [(regexp-match? #rx"^," object) object]       ; "retry, then exit": no space before the comma
                         [else (string-append " " object)])
                   "."))
  (define subject (string-append "the " (hash-ref c 'system) " " clause))
  (case shape
    [(ubiquitous) (string-append "The " (hash-ref c 'system) " " clause)]
    [(event) (string-append "WHEN " condition ", " subject)]
    [(state) (string-append "WHILE " condition ", " subject)]
    [(unwanted) (string-append "IF " condition ", THEN " subject)]
    [(optional) (string-append "WHERE " condition ", " subject)]))

(define (render-criteria cs) (string-join (map render-criterion cs) "\n"))

;; the structural fields only: line numbers and the original wording are not part of a criterion's meaning
(define (criterion=? a b)
  (for/and ([k '(shape condition system verb object)]) (equal? (hash-ref a k) (hash-ref b k))))

;; note-03 protocol data (symbols become strings; absent condition becomes null)
(define (criterion->jsexpr c)
  (hasheq 'shape (symbol->string (hash-ref c 'shape))
          'condition (or (hash-ref c 'condition) 'null)
          'system (hash-ref c 'system)
          'verb (hash-ref c 'verb)
          'object (hash-ref c 'object)
          'line (hash-ref c 'line)
          'canonical (render-criterion c)))

(define (criteria->jsexpr cs) (map criterion->jsexpr cs))
