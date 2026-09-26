#lang racket/base
;; Does `steer doc exists` catch plausible-but-wrong identifiers, and rank the right one high?
;;   dev   28 pairs from the T3 mutation list of scripts/samples.rkt. The suggestion table in
;;         steer/doc.rkt was designed while looking at these, so this set is NOT held out.
;;   fresh 20 pairs written before the table was frozen; NOT used to tune the table, but its failures
;;         motivated the verb-token ranking change, so it is no longer fully held out either.
;;   fresh2 20 pairs written before that change and never looked at while designing it.
;; Both are our guesses at what a model writes, not observed model output: the number that matters
;; is the same table on real failures from the E1 logger (T7).
;;   racket scripts/doc-hallucination.rkt [--details]
(require racket/list racket/string racket/port racket/runtime-path json)

(define-runtime-path main-rkt "../steer/main.rkt")
(define details? (member "--details" (vector->list (current-command-line-arguments))))

(define dev
  '((string-contains . string-contains?) (string-starts-with? . string-prefix?) (hash-get . hash-ref)
    (string-concat . string-join) (split-string . string-split) (nth . list-ref) (fold . foldl)
    (1+ . add1) (-1+ . sub1) (for-list . for/list) (upcase . string-upcase) (num->string . number->string)
    (to-float . exact->inexact) (hash-put . hash-set) (vector-get . vector-ref) (char-to-upper . char-upcase)
    (unique . remove-duplicates) (select . filter) (string-len . string-length) (list-reverse . reverse)
    (string-index . string-find) (str-trim . string-trim) (list-length . length) (hash-has-key . hash-has-key?)
    (string-empty? . non-empty-string?) (sum . apply) (concat . append) (map-list . map)))

(define fresh
  '((string-includes? . string-contains?) (list-append . append) (string-replace-all . string-replace)
    (hash-contains? . hash-has-key?) (list-sort . sort) (string-lowercase . string-downcase)
    (list-index . index-of) (string-substring . substring) (list-last . last) (list-filter . filter)
    (list-take . take) (string-to-number . string->number) (number-to-string . number->string)
    (list-contains? . member) (is-empty? . empty?) (string-uppercase . string-upcase) (list-remove . remove)
    (hash-delete . hash-remove) (hash-length . hash-count) (string-pad-left . ~a)))

(define fresh2
  '((list-map . map) (list-reduce . foldl) (list-find . findf) (list-any . ormap) (list-all . andmap)
    (string-endswith? . string-suffix?) (hash-items . hash->list) (list-first . first) (list-rest . rest)
    (list-count . count) (is-string? . string?) (string-format . format) (list-flatten . flatten)
    (list-unique . remove-duplicates) (string-char-at . string-ref) (list-get . list-ref) (sort-by . sort)
    (exit-program . exit) (print-string . display) (list-empty? . empty?)))

(define (lookup id)
  (define-values (p out in err)
    (subprocess #f #f 'stdout (find-executable-path "racket") (path->string main-rkt) "--json" "doc" "exists" id))
  (close-output-port in)
  (define j (string->jsexpr (port->string out)))
  (subprocess-wait p) (close-input-port out)
  j)

;; → (list wrong right usable? suggestions)
(define (measure p)
  (define wrong (symbol->string (car p)))
  (define j (lookup wrong))
  (define data (hash-ref j 'data))
  (define usable? (and (hash-ref data 'exists) (hash-ref data 'in_racket #t)))
  (define sugg
    (cond [usable? '()]
          [(pair? (hash-ref data 'suggestions '())) (hash-ref data 'suggestions)]
          [else                                       ; not-in-racket warning: "... or use a or b or c"
           (define fs (hash-ref j 'findings '()))
           (define m (and (pair? fs) (regexp-match #rx"or use (.*)$" (hash-ref (car fs) 'fix ""))))
           (if m (filter (λ (x) (not (equal? x "or"))) (string-split (cadr m))) '())]))
  (list wrong (symbol->string (cdr p)) usable? sugg))

(define (report title pairs)
  (define rows (map measure pairs))
  (define (rank r) (let ([i (index-of (fourth r) (second r))]) (and i (add1 i))))
  (define n (length rows))
  (define (top k) (length (filter (λ (r) (let ([x (rank r)]) (and x (<= x k)))) rows)))
  (printf "~a (~a pairs): flagged ~a · right name top-1 ~a · top-3 ~a · top-5 ~a\n"
          title n (length (filter (λ (r) (not (third r))) rows)) (top 1) (top 3) (top 5))
  (for ([r rows] #:when (or details? (not (eqv? (rank r) 1))))
    (printf "  ~a → ~a: ~a ~a\n" (first r) (second r)
            (cond [(third r) "NOT FLAGGED (usable from racket)"] [(rank r) (format "rank ~a" (rank r))] [else "not suggested"])
            (if (null? (fourth r)) "" (format "[~a]" (string-join (fourth r) " "))))))

(report "dev (tuned against)" dev)
(report "fresh (motivated the ranking change)" fresh)
(report "fresh2 (held out)" fresh2)
