#lang racket/base
;; C# support (milestone XL1), without dotnet: a lexer that knows C#'s string syntax, and a bracket check
;; built on it. A brace inside a string, a comment, a char literal or a preprocessor line is not a brace,
;; and C# has more string forms than most languages: "regular" with escapes, @"verbatim" with "" and
;; newlines, $"interpolated {expr} with {{ }} and nested "strings"", $@"both", and raw """strings""" with
;; $$ variants. Getting those wrong is what made the generic scanner report false errors on valid files.
;;
;;   cs-lex        text → (values tokens error-or-#f), tokens for anchors (T50) and the checks here
;;   cs-gate-check the hook/`steer syntax` gate: unclosed / extra / mismatched brackets, unterminated
;;                 strings and comments, each located; for a bracket problem a candidate edit is tried
;;                 and offered only when the brackets balance afterwards (verified) and, of the
;;                 candidates that do, the one with the fewest indentation inconsistencies (formatted C#
;;                 carries the missing information in its indentation, exactly as Racket does).
(require racket/list racket/string
         "common.rkt")
(provide (struct-out ctok) (struct-out lexerr) cs-lex cs-gate-check line-indent-table)

;; kind: 'id 'num 'str 'chr 'punct · line/col: start (1-based) · eline/ecol: end · start/end: string indexes
(struct ctok (kind text line col eline ecol start end) #:transparent)
(struct lexerr (msg line col) #:transparent)

(define (ident-start? c) (or (char-alphabetic? c) (char=? c #\_)))
(define (ident-char? c) (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_)))

;; ---------------------------------------------------------------------------------------------
;; Lexer

(define (cs-lex text)
  (define n (string-length text))
  (define i 0)
  (define line 1)
  (define line-start 0)
  (define tokens '())
  (define at-line-start? #t)                       ; only whitespace so far on this line (for # directives)
  (define (col-of idx) (add1 (- idx line-start)))
  (define (peek k) (let ([j (+ i k)]) (if (< j n) (string-ref text j) #\nul)))
  (define (newline!) (set! line (add1 line)) (set! line-start (add1 i)))
  (define (fail! msg l c) (raise (lexerr msg l c)))

  ;; consume text[i, j) keeping line bookkeeping
  (define (advance-to! j)
    (let loop ()
      (when (< i j)
        (when (char=? (string-ref text i) #\newline) (newline!))
        (set! i (add1 i))
        (loop))))

  (define (emit! kind start sline scol)
    (set! tokens (cons (ctok kind (substring text start i) sline scol line (col-of i) start i) tokens))
    (set! at-line-start? #f))

  ;; skip a plain "..." char/regular string body; i is just after the opening quote
  (define (skip-regular! sline scol)
    (let loop ()
      (cond [(>= i n) (fail! "unterminated string literal" sline scol)]
            [else
             (define c (string-ref text i))
             (cond [(char=? c #\\) (advance-to! (min n (+ i 2))) (loop)]
                   [(char=? c #\") (set! i (add1 i))]
                   [(char=? c #\newline) (fail! "unterminated string literal" sline scol)]
                   [else (set! i (add1 i)) (loop)])])))

  (define (skip-verbatim! sline scol)
    (let loop ()
      (cond [(>= i n) (fail! "unterminated verbatim string literal" sline scol)]
            [(and (char=? (string-ref text i) #\") (char=? (peek 1) #\")) (advance-to! (+ i 2)) (loop)]
            [(char=? (string-ref text i) #\") (set! i (add1 i))]
            [else (advance-to! (add1 i)) (loop)])))

  ;; an interpolation hole: i is just after the `{`; consume through the matching `}` (nested strings,
  ;; braces and parentheses count; a top-level `:` starts a format specifier that runs to the `}`)
  (define (skip-hole! sline scol)
    (let loop ([brace 0] [paren 0])
      (when (>= i n) (fail! "unterminated interpolation: a `{` in an interpolated string is never closed" sline scol))
      (define c (string-ref text i))
      (cond
        [(char=? c #\newline) (advance-to! (add1 i)) (loop brace paren)]
        [(char=? c #\{) (set! i (add1 i)) (loop (add1 brace) paren)]
        [(char=? c #\}) (set! i (add1 i)) (unless (zero? brace) (loop (sub1 brace) paren))]
        [(char=? c #\() (set! i (add1 i)) (loop brace (add1 paren))]
        [(char=? c #\)) (set! i (add1 i)) (loop brace (max 0 (sub1 paren)))]
        [(or (char=? c #\") (char=? c #\$) (char=? c #\@))
         (define-values (ok? j) (string-prefix-at i))
         (cond [ok? (scan-string! i j) (loop brace paren)]
               [else (set! i (add1 i)) (loop brace paren)])]
        [(char=? c #\') (scan-char!) (loop brace paren)]
        [(and (char=? c #\:) (zero? brace) (zero? paren) (not (char=? (peek 1) #\:)))
         ;; format specifier: literal text up to the closing brace
         (let skip () (when (and (< i n) (not (char=? (string-ref text i) #\}))) (advance-to! (add1 i)) (skip)))
         (when (< i n) (set! i (add1 i)))]
        [else (set! i (add1 i)) (loop brace paren)])))

  ;; at index k: is there a string opener? → (values #t index-of-first-quote) when text[k..] is [$@]*"
  (define (string-prefix-at k)
    (let loop ([j k])
      (cond [(>= j n) (values #f k)]
            [(memv (string-ref text j) '(#\$ #\@)) (loop (add1 j))]
            [(char=? (string-ref text j) #\") (values #t j)]
            [else (values #f k)])))

  ;; scan any string form starting at index k (prefix included) whose first quote is at index q
  (define (scan-string! k q)
    (define prefix (substring text k q))
    (define verbatim? (regexp-match? #rx"@" prefix))
    (define dollars (length (regexp-match* #rx"[$]" prefix)))
    (define sline line)
    (define scol (col-of k))
    (define quotes (let loop ([j q]) (if (and (< j n) (char=? (string-ref text j) #\")) (loop (add1 j)) (- j q))))
    (set! i (+ q 1))
    (cond
      [(and (>= quotes 3) (not verbatim?))
       ;; raw string: closes at a run of the same number of quotes; with $s, {{...}} (n braces) are holes
       (set! i (+ q quotes))
       (let loop ()
         (cond [(>= i n) (fail! "unterminated raw string literal" sline scol)]
               [(char=? (string-ref text i) #\")
                (define run (let r ([j i]) (if (and (< j n) (char=? (string-ref text j) #\")) (r (add1 j)) (- j i))))
                (if (>= run quotes) (set! i (+ i quotes)) (begin (advance-to! (+ i run)) (loop)))]
               [(and (> dollars 0) (char=? (string-ref text i) #\{))
                (define run (let r ([j i]) (if (and (< j n) (char=? (string-ref text j) #\{)) (r (add1 j)) (- j i))))
                (cond [(>= run dollars) (set! i (+ i run)) (skip-hole! sline scol) (loop)]
                      [else (set! i (+ i run)) (loop)])]
               [else (advance-to! (add1 i)) (loop)]))]
      [(= quotes 2) (set! i (+ q 2))]                                   ; the empty string ""
      [(> dollars 0)
       (let loop ()
         (cond [(>= i n) (fail! "unterminated interpolated string literal" sline scol)]
               [else
                (define c (string-ref text i))
                (cond [(char=? c #\newline) (if verbatim? (begin (advance-to! (add1 i)) (loop)) (fail! "unterminated interpolated string literal" sline scol))]
                      [(and (char=? c #\{) (char=? (peek 1) #\{)) (set! i (+ i 2)) (loop)]
                      [(char=? c #\{) (set! i (add1 i)) (skip-hole! sline scol) (loop)]
                      [(and (char=? c #\}) (char=? (peek 1) #\})) (set! i (+ i 2)) (loop)]
                      [(and verbatim? (char=? c #\") (char=? (peek 1) #\")) (set! i (+ i 2)) (loop)]
                      [(char=? c #\") (set! i (add1 i))]
                      [(and (not verbatim?) (char=? c #\\)) (advance-to! (min n (+ i 2))) (loop)]
                      [else (set! i (add1 i)) (loop)])]))]
      [verbatim? (skip-verbatim! sline scol)]
      [else (skip-regular! sline scol)]))

  ;; a char literal: i is at the opening quote
  (define (scan-char!)
    (define sline line)
    (define scol (col-of i))
    (set! i (add1 i))
    (let loop ()
      (cond [(or (>= i n) (char=? (string-ref text i) #\newline)) (fail! "unterminated character literal" sline scol)]
            [(char=? (string-ref text i) #\\) (set! i (min n (+ i 2))) (loop)]
            [(char=? (string-ref text i) #\') (set! i (add1 i))]
            [else (set! i (add1 i)) (loop)])))

  (define (lex!)
    (let loop ()
      (when (< i n)
        (define c (string-ref text i))
        (define sline line)
        (define scol (col-of i))
        (define start i)
        (cond
          [(char=? c #\newline) (newline!) (set! i (add1 i)) (set! at-line-start? #t)]
          [(char-whitespace? c) (set! i (add1 i))]
          [(and (char=? c #\/) (char=? (peek 1) #\/))
           (let skip () (when (and (< i n) (not (char=? (string-ref text i) #\newline))) (set! i (add1 i)) (skip)))]
          [(and (char=? c #\/) (char=? (peek 1) #\*))
           (define close (let find ([j (+ i 2)]) (cond [(>= (add1 j) n) #f] [(and (char=? (string-ref text j) #\*) (char=? (string-ref text (add1 j)) #\/)) j] [else (find (add1 j))])))
           (unless close (fail! "unterminated block comment" sline scol))
           (advance-to! (+ close 2))]
          [(and (char=? c #\#) at-line-start?)                         ; #if, #region, #pragma ...: the whole line
           (let skip () (when (and (< i n) (not (char=? (string-ref text i) #\newline))) (set! i (add1 i)) (skip)))]
          [(or (char=? c #\") (char=? c #\$) (char=? c #\@))
           (define-values (str? q) (string-prefix-at i))
           (cond [str? (scan-string! i q) (emit! 'str start sline scol)]
                 [(and (char=? c #\@) (ident-start? (peek 1)))          ; verbatim identifier @class
                  (set! i (add1 i))
                  (let ident () (when (and (< i n) (ident-char? (string-ref text i))) (set! i (add1 i)) (ident)))
                  (emit! 'id start sline scol)]
                 [else (set! i (add1 i)) (emit! 'punct start sline scol)])]
          [(char=? c #\') (scan-char!) (emit! 'chr start sline scol)]
          [(char-numeric? c)
           (let num ()
             (when (and (< i n) (or (ident-char? (string-ref text i))
                                    (and (char=? (string-ref text i) #\.) (< (add1 i) n) (char-numeric? (string-ref text (add1 i))))))
               (set! i (add1 i)) (num)))
           (emit! 'num start sline scol)]
          [(ident-start? c)
           (let ident () (when (and (< i n) (ident-char? (string-ref text i))) (set! i (add1 i)) (ident)))
           (emit! 'id start sline scol)]
          [(and (char=? c #\=) (char=? (peek 1) #\>)) (set! i (+ i 2)) (emit! 'punct start sline scol)]
          [(and (char=? c #\:) (char=? (peek 1) #\:)) (set! i (+ i 2)) (emit! 'punct start sline scol)]
          [else (set! i (add1 i)) (emit! 'punct start sline scol)])
        (loop))))

  (with-handlers ([lexerr? (λ (e) (values (reverse tokens) e))])
    (lex!)
    (values (reverse tokens) #f)))

;; ---------------------------------------------------------------------------------------------
;; Indentation: formatted C# nests by indentation, which is what locates a missing brace

;; visual indent (tab = 4) of every line, as a vector indexed by line number - 1
(define (line-indent-table text)
  (for/vector ([l (string-split text "\n" #:trim? #f)])
    (for/fold ([w 0]) ([c (in-string l)] #:break (not (memv c '(#\space #\tab))))
      (+ w (if (char=? c #\tab) 4 1)))))

(define (indent-of table line) (if (<= 1 line (vector-length table)) (vector-ref table (sub1 line)) 0))

;; number of lines whose indentation contradicts the brace structure
(define (indent-violations tokens table)
  (define stack '())                                ; indent of the line that opened each open `{`
  (define prev-eline 0)
  (for/fold ([count 0]) ([t tokens])
    (define first? (> (ctok-line t) prev-eline))
    (set! prev-eline (ctok-eline t))
    (define ind (indent-of table (ctok-line t)))
    (define txt (ctok-text t))
    (define v
      (cond
        [(not first?) 0]
        [(and (equal? txt "}") (pair? stack)) (if (= ind (car stack)) 0 1)]
        [(pair? stack)
         (if (or (> ind (car stack)) (member txt '("else" "catch" "finally"))) 0 1)]
        [else 0]))
    (cond [(equal? txt "{") (set! stack (cons (indent-of table (ctok-line t)) stack))]
          [(and (equal? txt "}") (pair? stack)) (set! stack (cdr stack))])
    (+ count v)))

;; The lines where indentation contradicts the brace structure, as (token . indent-of-the-open-block). A line
;; indented at or left of a still-open block's opener means that block should have closed before it.
(define (violation-sites tokens table)
  (define stack '())
  (define prev-eline 0)
  (define out '())
  (for ([t tokens])
    (define first? (> (ctok-line t) prev-eline))
    (set! prev-eline (ctok-eline t))
    (define ind (indent-of table (ctok-line t)))
    (define txt (ctok-text t))
    (when (and first? (pair? stack))
      (cond [(equal? txt "}") (unless (= ind (car stack)) (set! out (cons (cons t (car stack)) out)))]
            [(member txt '("else" "catch" "finally")) (void)]
            [(<= ind (car stack)) (set! out (cons (cons t (car stack)) out))]))
    (cond [(equal? txt "{") (set! stack (cons (indent-of table (ctok-line t)) stack))]
          [(and (equal? txt "}") (pair? stack)) (set! stack (cdr stack))]))
  (reverse out))

;; ---------------------------------------------------------------------------------------------
;; Bracket check

(define closer-of (hash "{" "}" "(" ")" "[" "]"))
(define opener-of (hash "}" "{" ")" "(" "]" "["))

;; first problem in token order: (list kind message line col) or #f
(define (first-bracket-problem tokens)
  (let loop ([ts tokens] [stack '()])
    (cond
      [(null? ts)
       (and (pair? stack)
            (let ([o (car stack)])
              (list 'unclosed-form (format "`~a` was never closed" (ctok-text o)) (ctok-line o) (ctok-col o) o)))]
      [else
       (define t (car ts))
       (define txt (ctok-text t))
       (cond
         [(and (eq? (ctok-kind t) 'punct) (hash-ref closer-of txt #f)) (loop (cdr ts) (cons t stack))]
         [(and (eq? (ctok-kind t) 'punct) (hash-ref opener-of txt #f))
          (cond
            [(null? stack) (list 'extra-closer (format "`~a` closes nothing" txt) (ctok-line t) (ctok-col t) t)]
            [(not (equal? (ctok-text (car stack)) (hash-ref opener-of txt)))
             (define o (car stack))
             (list 'mismatched-closer
                   (format "`~a` at line ~a col ~a closes `~a` from line ~a col ~a"
                           txt (ctok-line t) (ctok-col t) (ctok-text o) (ctok-line o) (ctok-col o))
                   (ctok-line t) (ctok-col t) t o)]
            [else (loop (cdr ts) (cdr stack))])]
         [else (loop (cdr ts) stack)])])))

(define (balanced? text)
  (define-values (tokens err) (cs-lex text))
  (and (not err) (not (first-bracket-problem tokens))))

;; text with the edit applied; edit = (hasheq 'op 'line 'col 'text), same as syntax-check's apply-edit
(define (apply-cs-edit text e)
  (define lines-before (hash-ref e 'line))
  (define start (let loop ([idx 0] [l 1]) (cond [(= l lines-before) idx] [(>= idx (string-length text)) (string-length text)]
                                                [(char=? (string-ref text idx) #\newline) (loop (add1 idx) (add1 l))]
                                                [else (loop (add1 idx) l)])))
  (define at (+ start (sub1 (hash-ref e 'col))))
  (case (hash-ref e 'op)
    [(insert) (string-append (substring text 0 at) (hash-ref e 'text) (substring text at))]
    [(delete) (string-append (substring text 0 at) (substring text (add1 at)))]
    [(replace) (string-append (substring text 0 at) (hash-ref e 'text) (substring text (add1 at)))]))

(define (line-length text line)                  ; characters in `line` without a trailing \r
  (define lines (string-split text "\n" #:trim? #f))
  (if (<= 1 line (length lines)) (string-length (string-trim (list-ref lines (sub1 line)) "\r" #:left? #f)) 0))

;; candidate edits for a problem; each is tried and kept only if the brackets balance afterwards
(define (candidates text problem tokens table)
  (define kind (car problem))
  (define tok (list-ref problem 4))
  (define line-count (length (string-split text "\n" #:trim? #f)))
  (define (closing-at-end-of-line ln indent closer)
    (hasheq 'op 'insert 'line ln 'col (add1 (line-length text ln)) 'text (string-append "\n" (make-string indent #\space) closer)))
  (case kind
    [(unclosed-form)
     ;; the opener is `{`, `(` or `[`; find the first later line indented at or left of the opener's line
     (define opener-line (ctok-line tok))
     (define base (indent-of table opener-line))
     (define closer (hash-ref closer-of (ctok-text tok)))
     (define first-tokens                          ; (line . first token) for every later line that starts a token
       (let loop ([ts tokens] [prev 0] [acc '()])
         (cond [(null? ts) (reverse acc)]
               [(and (> (ctok-line (car ts)) prev) (> (ctok-line (car ts)) opener-line))
                (loop (cdr ts) (ctok-eline (car ts)) (cons (car ts) acc))]
               [else (loop (cdr ts) (max prev (ctok-eline (car ts))) acc)])))
     (define at-or-left (filter (λ (t) (<= (indent-of table (ctok-line t)) base)) first-tokens))
     ;; where the previous code ends, for a closer that belongs before the token `t`
     (define (line-before t)
       (define prior (for/last ([x tokens] #:when (< (ctok-eline x) (ctok-line t))) (ctok-eline x)))
       (or prior (max 1 (sub1 (ctok-line t)))))
     ;; when a closer is missing *inside*, the brace left open at end of file belongs to an outer block, so the
     ;; opener's own indentation is not the place to look: also try every line where indentation breaks the structure
     (define sites (violation-sites tokens table))
     (append
      (for/list ([site (if (> (length sites) 8) (take sites 8) sites)])
        (closing-at-end-of-line (line-before (car site)) (cdr site) "}"))
      (for/list ([t (if (> (length at-or-left) 6) (take at-or-left 6) at-or-left)])
        (closing-at-end-of-line (line-before t) (if (equal? (ctok-text tok) "{") base 0) closer))
      ;; at the end of the file: after the last line that has code, not after a trailing empty line
      (list (closing-at-end-of-line (if (null? tokens) line-count (ctok-eline (last tokens))) (if (equal? (ctok-text tok) "{") base 0) closer)))]
    [(extra-closer)
     (list (hasheq 'op 'delete 'line (ctok-line tok) 'col (ctok-col tok) 'text (ctok-text tok)))]
    [(mismatched-closer)
     (define opener (list-ref problem 5))
     (define want (hash-ref closer-of (ctok-text opener)))
     (define same-line? (= (ctok-line tok) (ctok-line opener)))
     (define replace (hasheq 'op 'replace 'line (ctok-line tok) 'col (ctok-col tok) 'text want))
     (define insert-prev (closing-at-end-of-line (max 1 (sub1 (ctok-line tok))) (indent-of table (ctok-line opener)) want))
     (define insert-here (hasheq 'op 'insert 'line (ctok-line tok) 'col (ctok-col tok) 'text want))
     (if same-line? (list replace insert-here insert-prev) (list insert-prev insert-here replace))]
    [else '()]))

;; the edit whose result balances and has the fewest indentation inconsistencies; earliest wins ties
(define (best-edit text problem tokens)
  (define table (line-indent-table text))
  (define scored
    (for*/list ([e (candidates text problem tokens table)]
                [fixed (in-value (apply-cs-edit text e))]
                #:when (balanced? fixed))
      (define-values (ts _e) (cs-lex fixed))
      (cons (indent-violations ts (line-indent-table fixed)) e)))
  (and (pair? scored)
       (cdr (for/fold ([best (car scored)]) ([s (cdr scored)]) (if (< (car s) (car best)) s best)))))

(define (edit-fix-text e)
  (define t (hash-ref e 'text))
  (case (hash-ref e 'op)
    [(insert) (if (regexp-match? #rx"^\n" t)
                  (format "add `~a` on a new line after line ~a" (string-trim t) (hash-ref e 'line))
                  (format "insert `~a` at line ~a col ~a" t (hash-ref e 'line) (hash-ref e 'col)))]
    [(delete) (format "delete `~a` at line ~a col ~a" t (hash-ref e 'line) (hash-ref e 'col))]
    [(replace) (format "replace the closer at line ~a col ~a with `~a`" (hash-ref e 'line) (hash-ref e 'col) t)]))

;; → (values findings declaration-count lang): the shape every gate returns (lang.rkt)
(define (cs-gate-check text file)
  (define-values (tokens err) (cs-lex text))
  (cond
    [err (values (list (finding 'error 'read-error (lexerr-msg err) #:file file #:line (lexerr-line err) #:col (lexerr-col err)
                                #:fix "close it with the matching quote or `*/` (no automatic edit: the intended end is not knowable)"))
                 0 "csharp")]
    [else
     (define problem (first-bracket-problem tokens))
     (cond
       [(not problem) (values '() (top-level-count tokens) "csharp")]
       [else
        (define e (best-edit text problem tokens))
        (define edit (and e (hash-set e 'verified #t)))
        ;; a missing brace inside a block leaves an *outer* brace unmatched at end of file, so when an edit is
        ;; known the finding sits where the edit goes and says why (blaming the outer brace would mislead)
        (define located? (and edit (eq? (car problem) 'unclosed-form)))
        (values (list (finding 'error (car problem)
                               (if located?
                                   (format "a `~a` opened at line ~a col ~a is never closed: the indentation shows the block ends after line ~a"
                                           (ctok-text (list-ref problem 4)) (caddr problem) (cadddr problem) (hash-ref e 'line))
                                   (cadr problem))
                               #:file file
                               #:line (if located? (hash-ref e 'line) (caddr problem))
                               #:col (if located? (hash-ref e 'col) (cadddr problem))
                               #:fix (and edit (string-append (edit-fix-text e) " (verified: the brackets balance after this edit)"))
                               #:edit edit))
                0 "csharp")])]))

;; items at nesting depth 0: each top-level `{...}` block or `;`-terminated statement (using, namespace X;)
(define (top-level-count tokens)
  (let loop ([ts tokens] [depth 0] [count 0] [pending? #f])
    (cond
      [(null? ts) (+ count (if pending? 1 0))]
      [else
       (define txt (ctok-text (car ts)))
       (define punct? (eq? (ctok-kind (car ts)) 'punct))
       (cond
         [(and punct? (member txt '("{" "(" "[")))
          (loop (cdr ts) (add1 depth) count (or pending? #t))]
         [(and punct? (member txt '("}" ")" "]")))
          (define d (max 0 (sub1 depth)))
          (loop (cdr ts) d (if (and (zero? d) (equal? txt "}")) (add1 count) count) (if (and (zero? d) (equal? txt "}")) #f pending?))]
         [(and punct? (equal? txt ";") (zero? depth)) (loop (cdr ts) depth (add1 count) #f)]
         [else (loop (cdr ts) depth count (or pending? #t))])])))

;; ---------------------------------------------------------------------------------------------
;; Anchors (T50): a member/type scanner over the same tokens, so `file.cs#Class.Member` resolves
;; exactly instead of by indentation. Handles generics (`Name<T>(`), attributes, properties (`{ get; }`
;; and `=>`), indexers, operators, destructors, events, fields, partial classes and file-scoped
;; namespaces. Overloads and same-named members across partial declarations disambiguate by
;; `Name/arity` or `Name(type,type)`; with neither, one unambiguous match is required.
(provide cs-find-anchor cs-list-names scan-members (struct-out cs-member)
         scan-types (struct-out cs-type) scan-usings)

(struct frame (kind name open-depth) #:transparent)   ; open-depth: brace depth while inside this frame
(struct cs-member (qualname kind arity paramtypes start end) #:transparent)  ; start/end: token indices, inclusive

(define type-keywords '("class" "struct" "interface" "enum" "record"))
;; "this" is not here: an indexer's return type precedes it ("public int this[...]"), so `this` must
;; stay visible to find-member-name, which is what tells try-member it found an indexer.
(define member-modifiers
  '("public" "private" "protected" "internal" "static" "readonly" "virtual" "override" "sealed"
    "abstract" "async" "partial" "unsafe" "extern" "new" "const" "volatile" "required" "delegate"
    "fixed" "explicit" "implicit" "ref" "in" "out" "params"))

(define (tv-ref tv i) (vector-ref tv i))
(define (tv-text tv i) (ctok-text (vector-ref tv i)))
(define (tv-kind tv i) (ctok-kind (vector-ref tv i)))
(define (punct? tv i txt) (and (eq? (tv-kind tv i) 'punct) (equal? (tv-text tv i) txt)))
(define (ident? tv i) (eq? (tv-kind tv i) 'id))
(define (ident-text? tv i txt) (and (ident? tv i) (equal? (tv-text tv i) txt)))

;; Skip a balanced [...] or (...) group; tv[i] must be the opener. → index just after the matching closer.
(define (skip-group tv i open close)
  (let loop ([i (add1 i)] [d 1])
    (cond [(>= i (vector-length tv)) i]
          [(punct? tv i open) (loop (add1 i) (add1 d))]
          [(punct? tv i close) (if (= d 1) (add1 i) (loop (add1 i) (sub1 d)))]
          [else (loop (add1 i) d)])))

;; Skip a generic argument/parameter list; tv[i] must be "<". → index just after the matching ">".
(define (skip-generic tv i)
  (let loop ([i (add1 i)] [d 1])
    (cond [(>= i (vector-length tv)) i]
          [(punct? tv i "<") (loop (add1 i) (add1 d))]
          [(punct? tv i ">") (if (= d 1) (add1 i) (loop (add1 i) (sub1 d)))]
          [else (loop (add1 i) d)])))

;; Skip attribute lists (`[Attr] [Attr2(...)]`) and modifier keywords, starting at i. → new index.
(define (skip-prefix tv i)
  (let loop ([i i])
    (cond [(punct? tv i "[") (loop (skip-group tv i "[" "]"))]
          [(and (ident? tv i) (member (tv-text tv i) member-modifiers)) (loop (add1 i))]
          [else i])))

;; Forward from i to the first top-level `{` or `;` (paren/bracket-balanced, so base lists, generic
;; constraints and constructor initializers do not confuse it). → that index.
(define (skip-to-brace-or-semi tv i)
  (let loop ([i i] [pd 0] [bd 0])
    (cond
      [(>= i (vector-length tv)) i]
      [(punct? tv i "(") (loop (add1 i) (add1 pd) bd)]
      [(punct? tv i ")") (loop (add1 i) (max 0 (sub1 pd)) bd)]
      [(punct? tv i "[") (loop (add1 i) pd (add1 bd))]
      [(punct? tv i "]") (loop (add1 i) pd (max 0 (sub1 bd)))]
      [(and (zero? pd) (zero? bd) (or (punct? tv i "{") (punct? tv i ";"))) i]
      [else (loop (add1 i) pd bd)])))

;; The next top-level `;`, tracking paren AND brace depth: an expression body or initializer may
;; contain an object initializer (`new Foo { X = 1 }`) or a call, and a `;` inside those does not end
;; the declaration (there usually isn't one, but this stays correct either way). → that index.
(define (skip-through-semi tv i)
  (define n (vector-length tv))
  (let loop ([i i] [pd 0] [bd 0])
    (cond
      [(>= i n) (sub1 i)]
      [(punct? tv i "(") (loop (add1 i) (add1 pd) bd)]
      [(punct? tv i ")") (loop (add1 i) (max 0 (sub1 pd)) bd)]
      [(punct? tv i "{") (loop (add1 i) pd (add1 bd))]
      [(punct? tv i "}") (loop (add1 i) pd (max 0 (sub1 bd)))]
      [(and (zero? pd) (zero? bd) (punct? tv i ";")) i]
      [else (loop (add1 i) pd bd)])))

;; A parameter list's arity and a best-effort type text per parameter; tv[open] = "(".
;; `last` is the outer closing paren's own index: the loop must stop there without consuming it,
;; not merely once i reaches `close` (which is one past it) — otherwise that closer joins the last
;; parameter's tokens and param-type-text drops the wrong (final) token instead of the real name.
(define (parse-params tv open)
  (define close (skip-group tv open "(" ")"))
  (define last (sub1 close))
  (cond
    [(= (add1 open) last) (values 0 '() close)]             ; ()
    [else
     (let loop ([i (add1 open)] [depth 0] [cur '()] [types '()])
       (cond
         [(= i last)
          (define ps (reverse (cons (reverse cur) types)))
          (values (length ps) (map param-type-text ps) close)]
         [(and (zero? depth) (punct? tv i ","))
          (loop (add1 i) depth '() (cons (reverse cur) types))]
         [(member (tv-text tv i) '("(" "[" "<")) (loop (add1 i) (add1 depth) (cons (tv-text tv i) cur) types)]
         [(member (tv-text tv i) '(")" "]" ">")) (loop (add1 i) (max 0 (sub1 depth)) (cons (tv-text tv i) cur) types)]
         [else (loop (add1 i) depth (cons (tv-text tv i) cur) types)]))]))

;; A parameter's tokens (in order) → its declared type, dropping modifiers, the name and a default value.
(define (param-type-text toks)
  (define kept (filter (λ (s) (not (member s '("ref" "out" "in" "params" "this")))) toks))
  (define eq-pos (index-of kept "="))
  (define before-default (if eq-pos (take kept eq-pos) kept))
  (if (null? before-default) "" (string-join (reverse (cdr (reverse before-default))) "")))  ; drop the trailing name token

;; ---------------------------------------------------------------------------------------------
;; The scanner: one forward pass, tracking a container stack; members are only looked for directly
;; inside a class/struct/interface/record/enum body (not inside a namespace, and not inside a nested
;; block, which is skipped wholesale by brace matching before we get there).

(define (scan-members tokens)
  (define tv (list->vector tokens))
  (define n (vector-length tv))
  (define stack '())                                  ; frame list, most recently entered first
  (define members '())
  (define (qualname)
    (string-join (for/list ([f (reverse stack)] #:unless (eq? (frame-kind f) 'namespace)) (frame-name f)) "."))
  (define (top-open-depth) (if (null? stack) 0 (frame-open-depth (car stack))))
  ;; enum bodies use `Name, Name = value,` syntax, not the class-member grammar; members are not
  ;; scanned inside one (a known gap: enum values are not anchorable)
  (define (top-is-type?) (and (pair? stack) (not (memq (frame-kind (car stack)) '(namespace enum)))))

  (let loop ([i 0] [depth 0])
    (cond
      [(>= i n) (void)]
      [(punct? tv i "{") (loop (add1 i) (add1 depth))]
      [(punct? tv i "}")
       (define d (max 0 (sub1 depth)))
       (when (and (pair? stack) (= (add1 d) (frame-open-depth (car stack)))) (set! stack (cdr stack)))
       (loop (add1 i) d)]
      [(not (= depth (top-open-depth))) (loop (add1 i) depth)]   ; inside a member's body: skip token by token
      [else
       (define j (skip-prefix tv i))
       (cond
         ;; a namespace: `namespace A.B { ... }` or file-scoped `namespace A.B;`. Tracked only for depth
         ;; (frames of kind 'namespace are excluded from qualname by `qualname` above).
         [(and (< j n) (ident-text? tv j "namespace"))
          (define after-name
            (let scan ([i (add1 j)])
              (cond [(and (< i n) (or (ident? tv i) (punct? tv i "."))) (scan (add1 i))]
                    [else i])))
          (cond
            [(and (< after-name n) (punct? tv after-name "{"))
             (set! stack (cons (frame 'namespace "namespace" (add1 depth)) stack))
             (loop (add1 after-name) (add1 depth))]
            [(and (< after-name n) (punct? tv after-name ";"))
             (set! stack (cons (frame 'namespace "namespace" depth) stack))
             (loop (add1 after-name) depth)]
            [else (loop (max (add1 after-name) (add1 j)) depth)])]
         ;; a nested type: class/struct/interface/enum/record Name<...>? ... { or ;
         [(and (< j n) (ident? tv j) (member (tv-text tv j) type-keywords))
          (define name-i (add1 j))
          (cond
            [(and (< name-i n) (ident? tv name-i))
             (define after-name (if (and (< (add1 name-i) n) (punct? tv (add1 name-i) "<")) (skip-generic tv (add1 name-i)) (add1 name-i)))
             ;; a positional record's own parameter list, e.g. `record Point(int X, int Y)`
             (define after-params (if (and (< after-name n) (punct? tv after-name "(")) (skip-group tv after-name "(" ")") after-name))
             (define brace-or-semi (skip-to-brace-or-semi tv after-params))
             (cond
               [(and (< brace-or-semi n) (punct? tv brace-or-semi "{"))
                (set! stack (cons (frame (string->symbol (tv-text tv j)) (tv-text tv name-i) (add1 depth)) stack))
                (loop (add1 brace-or-semi) (add1 depth))]
               [else (loop (add1 brace-or-semi) depth)])]                ; forward declaration or malformed: skip
            [else (loop (add1 j) depth)])]
         ;; a member, only meaningful directly inside a type body
         [(not (top-is-type?)) (loop (add1 j) depth)]
         [else
          (define-values (consumed member) (try-member tv j depth (qualname)))
          (cond [member (set! members (cons member members)) (loop consumed depth)]
                [(> consumed i) (loop consumed depth)]
                [else (loop (add1 i) depth)])])]))
  (reverse members))

;; ---------------------------------------------------------------------------------------------
;; T63: type declarations (class/struct/interface/enum/record), each with its nested-dotted qualname
;; (the SAME addressing scan-members uses), base/interface list and attribute names as written, and
;; `using` directives - the graph-ir extras scan-members has no reason to carry (it only ever needed
;; members, for anchors). A separate, smaller pass over the same tokens: reusing scan-members' own
;; frame/depth tracking for this would tangle two different outputs into one already-delicate loop.

(struct cs-type (qualname kind bases attrs start end) #:transparent)

;; identifiers that start a top-level, comma-separated attribute usage inside `[ ... ]` (tv[open]="[").
(define (attr-names-in tv open close)
  (let loop ([i (add1 open)] [acc '()] [want-name? #t])
    (cond
      [(>= i close) (reverse acc)]
      [(and want-name? (ident? tv i)) (loop (skip-to-comma tv (add1 i) close) (cons (tv-text tv i) acc) #f)]
      [(punct? tv i ",") (loop (add1 i) acc #t)]
      [else (loop (add1 i) acc want-name?)])))
(define (skip-to-comma tv i close)
  (let loop ([i i] [d 0])
    (cond [(>= i close) i]
          [(member (tv-text tv i) '("(" "[")) (loop (add1 i) (add1 d))]
          [(member (tv-text tv i) '(")" "]")) (loop (add1 i) (max 0 (sub1 d)))]
          [(and (zero? d) (punct? tv i ",")) i]
          [else (loop (add1 i) d)])))

;; Skip attribute lists, collecting each one's leading identifier(s), then modifier keywords. → (values
;; index-after-prefix, attr-names).
(define (skip-prefix/attrs tv i)
  (let loop ([i i] [attrs '()])
    (cond [(punct? tv i "[")
           (define close (skip-group tv i "[" "]"))
           (loop close (append (reverse (attr-names-in tv i (sub1 close))) attrs))]
          [(and (ident? tv i) (member (tv-text tv i) member-modifiers)) (loop (add1 i) attrs)]
          [else (values i (reverse attrs))])))

;; From just after a type's `:`, the base/interface list as bare dotted names (generic arguments and
;; a record primary constructor's base(args) call are dropped - only the leading name matters for
;; inherits/implements resolution, which matches by bare name or an exact/imported qualname anyway).
;; → (values list-of-base-names, index-of-'{'/'where'/';').
(define (scan-base-list tv i)
  (define n (vector-length tv))
  (define (flush cur bases) (if (pair? cur) (cons (string-join (reverse cur) "") bases) bases))
  (let loop ([i i] [cur '()] [bases '()])
    (cond
      [(>= i n) (values (reverse (flush cur bases)) i)]
      [(ident-text? tv i "where") (values (reverse (flush cur bases)) i)]
      [(or (punct? tv i "{") (punct? tv i ";")) (values (reverse (flush cur bases)) i)]
      [(punct? tv i ",") (loop (add1 i) '() (flush cur bases))]
      [(punct? tv i "<") (loop (skip-generic tv i) cur bases)]
      [(punct? tv i "(") (loop (skip-group tv i "(" ")") cur bases)]
      [(or (ident? tv i) (punct? tv i ".")) (loop (add1 i) (cons (tv-text tv i) cur) bases)]
      [else (loop (add1 i) cur bases)])))

(define (scan-types tokens)
  (define tv (list->vector tokens))
  (define n (vector-length tv))
  (define stack '())         ; each entry: (list 'ns depth) or (list 'type kind qualname bases attrs start-idx depth)
  (define types '())
  (define (cur-qualname) (string-join (for/list ([f (reverse stack)] #:unless (eq? (car f) 'ns)) (caddr f)) "."))
  (define (top-open-depth) (if (null? stack) 0 (last (car stack))))
  (let loop ([i 0] [depth 0])
    (cond
      [(>= i n) (void)]
      [(punct? tv i "{") (loop (add1 i) (add1 depth))]
      [(punct? tv i "}")
       (define d (max 0 (sub1 depth)))
       (when (and (pair? stack) (= (add1 d) (last (car stack))))
         (define fr (car stack))
         (when (eq? (car fr) 'type)
           (set! types (cons (cs-type (caddr fr) (cadr fr) (cadddr fr) (list-ref fr 4) (list-ref fr 5) i) types)))
         (set! stack (cdr stack)))
       (loop (add1 i) d)]
      [(not (= depth (top-open-depth))) (loop (add1 i) depth)]
      [else
       (define-values (j attrs) (skip-prefix/attrs tv i))
       (cond
         [(and (< j n) (ident-text? tv j "namespace"))
          (define after-name
            (let scan ([k (add1 j)]) (if (and (< k n) (or (ident? tv k) (punct? tv k "."))) (scan (add1 k)) k)))
          (cond
            [(and (< after-name n) (punct? tv after-name "{"))
             (set! stack (cons (list 'ns (add1 depth)) stack))
             (loop (add1 after-name) (add1 depth))]
            [(and (< after-name n) (punct? tv after-name ";"))
             (set! stack (cons (list 'ns depth) stack))
             (loop (add1 after-name) depth)]
            [else (loop (max (add1 after-name) (add1 j)) depth)])]
         [(and (< j n) (ident? tv j) (member (tv-text tv j) type-keywords))
          (define kind (tv-text tv j))
          (define name-i (add1 j))
          (cond
            [(and (< name-i n) (ident? tv name-i))
             (define parent-q (cur-qualname))
             (define qn (if (string=? parent-q "") (tv-text tv name-i) (string-append parent-q "." (tv-text tv name-i))))
             (define after-name (if (and (< (add1 name-i) n) (punct? tv (add1 name-i) "<")) (skip-generic tv (add1 name-i)) (add1 name-i)))
             (define after-params (if (and (< after-name n) (punct? tv after-name "(")) (skip-group tv after-name "(" ")") after-name))
             (define-values (bases after-bases)
               (if (and (< after-params n) (punct? tv after-params ":")) (scan-base-list tv (add1 after-params)) (values '() after-params)))
             (define brace-or-semi (skip-to-brace-or-semi tv after-bases))
             (cond
               [(and (< brace-or-semi n) (punct? tv brace-or-semi "{"))
                (set! stack (cons (list 'type kind qn bases attrs name-i (add1 depth)) stack))
                (loop (add1 brace-or-semi) (add1 depth))]
               [else (loop (add1 brace-or-semi) depth)])]
            [else (loop (add1 j) depth)])]
         [else (loop (add1 j) depth)])]))
  (reverse types))

;; `using X;` / `using static X;` / `using X = Y.Z;` (an import), never `using (expr) { }` or
;; `using var x = ...;` (a resource-disposal statement, which happens to share the same keyword).
;; → list of (spec alias line).
(define (scan-usings tokens)
  (define tv (list->vector tokens))
  (define n (vector-length tv))
  (define (dotted-name-at i)
    (let loop ([k i] [acc '()])
      (if (and (< k n) (or (ident? tv k) (punct? tv k "."))) (loop (add1 k) (cons (tv-text tv k) acc)) (values (string-join (reverse acc) "") k))))
  (let loop ([i 0] [acc '()])
    (cond
      [(>= i n) (reverse acc)]
      [(and (ident-text? tv i "using") (< (add1 i) n) (or (punct? tv (add1 i) "(") (ident-text? tv (add1 i) "var")))
       (loop (add1 i) acc)]                              ; a resource-disposal `using`, not an import
      [(ident-text? tv i "using")
       (define line (ctok-line (tv-ref tv i)))
       (define static? (ident-text? tv (add1 i) "static"))
       (define-values (name after) (dotted-name-at (if static? (+ i 2) (add1 i))))
       (cond
         [(and (< after n) (punct? tv after "="))
          (define-values (target after2) (dotted-name-at (add1 after)))
          (loop (add1 (skip-to-brace-or-semi tv after2)) (cons (list target name line) acc))]
         [else (loop (add1 (skip-to-brace-or-semi tv after)) (cons (list name #f line) acc))])]
      [else (loop (add1 i) acc)])))

;; Try to parse one member declaration starting at j (after attributes/modifiers were already skipped
;; by the caller's skip-prefix). → (values next-index member-or-#f). next-index always makes progress.
(define (try-member tv j depth qualname-prefix)
  (define n (vector-length tv))
  (define (mk name kind start end #:arity [arity #f] #:paramtypes [pt '()])
    (cs-member (if (string=? qualname-prefix "") name (string-append qualname-prefix "." name)) kind arity pt start end))
  (cond
    ;; destructor: ~ Name ( ) { ... }
    [(and (< j n) (punct? tv j "~") (< (add1 j) n) (ident? tv (add1 j)))
     (define name-i (add1 j))
     (define open (add1 name-i))
     (cond [(and (< open n) (punct? tv open "(")) (finish-callable tv (mk (string-append "~" (tv-text tv name-i)) "destructor" j open))]
           [else (values (add1 j) #f)])]
    ;; operator overload: [Type] operator OP ( ... ) ...
    [(ident-text? tv j "operator")
     (cond
       [(< (add1 j) n)
        (define op-i (add1 j))
        (define op-end (let loop ([k op-i]) (if (and (< k n) (not (punct? tv k "("))) (loop (add1 k)) k)))
        (define open op-end)
        (cond [(and (< open n) (punct? tv open "("))
               (finish-callable tv (mk (string-append "operator" (string-join (for/list ([k (in-range op-i op-end)]) (tv-text tv k)) "")) "operator" j open))]
              [else (values (add1 j) #f)])]
       [else (values (add1 j) #f)])]
    ;; indexer: this [ ... ] { or =>
    [(ident-text? tv j "this")
     (cond [(and (< (add1 j) n) (punct? tv (add1 j) "["))
            (define close (skip-group tv (add1 j) "[" "]"))
            (finish-property tv (mk "this[]" "indexer" j close))]
           [else (values (add1 j) #f)])]
    [else
     ;; type-tokens... Name ( -> method/ctor | Name { or => -> property | Name ; or , or = -> field(s)
     (define name-i (find-member-name tv j))
     (cond
       [(not name-i) (values (add1 j) #f)]
       [else
        (define after (if (and (< (add1 name-i) n) (punct? tv (add1 name-i) "<")) (skip-generic tv (add1 name-i)) (add1 name-i)))
        (cond
          ;; an indexer's return type precedes `this`, so it reaches here as an ordinary "name", with
          ;; `[` (not `(`) right after: public int this[int index] => ...
          [(and (ident-text? tv name-i "this") (< after n) (punct? tv after "["))
           (finish-property tv (mk "this[]" "indexer" j (skip-group tv after "[" "]")))]
          [(and (< after n) (punct? tv after "("))
           (finish-callable tv (mk (tv-text tv name-i) "method" j after))]
          [(and (< after n) (or (punct? tv after "{") (punct? tv after "=>")))
           (finish-property tv (mk (tv-text tv name-i) "property" j after))]
          [(and (< after n) (member (tv-text tv after) '(";" "," "=")))
           (finish-field tv (mk (tv-text tv name-i) "field" j after))]
          [else (values (add1 j) #f)])])]))

;; Scan forward from j over "type-ish" tokens (identifiers, `.`, generics, `[]`, `?`) and return the
;; index of the identifier that names the member (the one right before `(`, `{`, `=>`, `;`, `,` or `=`).
(define (find-member-name tv j)
  (define n (vector-length tv))
  (let loop ([i j] [last-id #f])
    (cond
      [(>= i n) last-id]
      [(and (ident? tv i) (< (add1 i) n) (punct? tv (add1 i) "<"))
       (loop (skip-generic tv (add1 i)) i)]
      [(ident? tv i) (loop (add1 i) i)]
      [(member (tv-text tv i) '("." "?" "*")) (loop (add1 i) last-id)]
      [(punct? tv i "[") (loop (skip-group tv i "[" "]") last-id)]
      [(member (tv-text tv i) '("(" "{" "=>" ";" "," "=")) last-id]
      [else #f])))

(define (finish-callable tv m)
  (define open (cs-member-end m))                      ; end temporarily holds the "(" index
  (define-values (arity paramtypes close) (parse-params tv open))
  (define body-end (member-body-end tv close))
  (values (add1 body-end) (cs-member (cs-member-qualname m) (cs-member-kind m) arity paramtypes (cs-member-start m) body-end)))

(define (finish-property tv m)
  (define opener (cs-member-end m))
  (define n (vector-length tv))
  (define body-end0
    (cond [(punct? tv opener "{") (skip-group tv opener "{" "}")]     ; index just past the closing "}"
          [else (add1 (member-body-end tv opener))]))                 ; "=> expr;" already ends at the ";"
  ;; an auto-property may carry a trailing initializer after its accessor block: `{ get; } = expr;`
  (define final-end
    (if (and (< body-end0 n) (punct? tv body-end0 "=")) (add1 (skip-through-semi tv (add1 body-end0))) body-end0))
  (values final-end (cs-member (cs-member-qualname m) (cs-member-kind m) #f '() (cs-member-start m) (sub1 final-end))))

(define (finish-field tv m)
  ;; consume through the top-level `;`, so `int a = 1, b = 2;` is one span; the requested name must be
  ;; among the comma-separated identifiers, which the caller matches by qualname alone (arity #f).
  (define end (skip-to-brace-or-semi tv (cs-member-end m)))
  (values (add1 end) (cs-member (cs-member-qualname m) (cs-member-kind m) #f '() (cs-member-start m) end)))

;; After a `)` (methods) or `]` (indexers): a `{...}` body, an `=> expr;` body, or a bare `;` (abstract/
;; interface/partial declaration, or extern). → the index of the last token of the member.
(define (member-body-end tv close)
  (define n (vector-length tv))
  (cond
    [(>= close n) close]
    [(punct? tv close "{") (sub1 (skip-group tv close "{" "}"))]
    [(punct? tv close "=>") (skip-through-semi tv (add1 close))]
    [(punct? tv close ";") close]
    [(and (< close n) (ident-text? tv close "where"))              ; a generic method's constraint clause
     (member-body-end tv (skip-to-brace-or-semi tv close))]
    [else close]))

;; ---------------------------------------------------------------------------------------------
;; Lookup: request = "Class.Member", optionally with a trailing "/N" (arity) or "(t1,t2)" (parameter types).

(define (parse-request qualname)
  (cond
    [(regexp-match #px"^(.*)/(\\d+)$" qualname) => (λ (m) (values (cadr m) 'arity (string->number (caddr m))))]
    [(regexp-match #px"^(.*)\\(([^()]*)\\)$" qualname)
     => (λ (m) (values (cadr m) 'types (map string-trim (if (string=? (caddr m) "") '() (string-split (caddr m) ",")))))]
    [else (values qualname 'none #f)]))

(define (matches-types? paramtypes hints)
  (and (= (length paramtypes) (length hints))
       (for/and ([p paramtypes] [h hints]) (string-contains? (string-downcase p) (string-downcase h)))))

(define (member-tag m)
  (if (pair? (cs-member-paramtypes m))
      (format "~a(~a)" (cs-member-qualname m) (string-join (cs-member-paramtypes m) ", "))
      (format "~a/~a" (cs-member-qualname m) (or (cs-member-arity m) 0))))

(define (tokens-hash tv start end)
  (short-sha1 (format "~s" (for/list ([i (in-range start (add1 end))]) (cons (ctok-kind (vector-ref tv i)) (ctok-text (vector-ref tv i)))))))
(define (hex bs) (apply string-append (for/list ([b bs]) (string-append (if (< b 16) "0" "") (number->string b 16)))))
(define (short-sha1 s) (substring (hex (sha1-bytes (open-input-bytes (string->bytes/utf-8 s)))) 0 12))

;; Every qualified member name in the file, for a did-you-mean suggestion. '() on a lex error.
(define (cs-list-names text)
  (define-values (tokens err) (cs-lex text))
  (if err '() (remove-duplicates (map cs-member-qualname (scan-members tokens)))))

;; → hasheq: found? line end hash kind shadowed problem candidates (same shape python-find-anchor uses)
(define (cs-find-anchor text qualname)
  (define-values (tokens err) (cs-lex text))
  (cond
    [err (hasheq 'found? #f 'problem (string-append "unreadable: " (lexerr-msg err)))]
    [else
     (define tv (list->vector tokens))
     (define members (scan-members tokens))
     (define-values (base-name mode arg) (parse-request qualname))
     (define same-base (filter (λ (m) (equal? (cs-member-qualname m) base-name)) members))
     (cond
       [(null? same-base) (hasheq 'found? #f 'problem (format "no definition of ~a" qualname))]
       [(= (length same-base) 1)
        (define m (car same-base))
        (hasheq 'found? #t 'method 'csharp 'kind (cs-member-kind m)
                'line (ctok-line (vector-ref tv (cs-member-start m))) 'end (ctok-eline (vector-ref tv (cs-member-end m)))
                'hash (tokens-hash tv (cs-member-start m) (cs-member-end m)) 'shadowed #f)]
       [else
        (define picked
          (case mode
            [(arity) (findf (λ (m) (equal? (cs-member-arity m) arg)) same-base)]
            [(types) (findf (λ (m) (matches-types? (cs-member-paramtypes m) arg)) same-base)]
            [else #f]))
        (cond
          [picked
           (hasheq 'found? #t 'method 'csharp 'kind (cs-member-kind picked)
                   'line (ctok-line (vector-ref tv (cs-member-start picked))) 'end (ctok-eline (vector-ref tv (cs-member-end picked)))
                   'hash (tokens-hash tv (cs-member-start picked) (cs-member-end picked))
                   'shadowed (sub1 (length same-base)))]
          [else
           (hasheq 'found? #f
                   'problem (format "~a overloads of ~a: specify Name/arity or Name(types)" (length same-base) base-name)
                   'candidates (map member-tag same-base))])])]))
