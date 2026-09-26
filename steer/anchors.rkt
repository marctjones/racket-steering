#lang racket/base
;; Symbol anchors (catalog F2). A task points at "path#name" (or just "path"); the store keeps a
;; baseline hash taken when the plan was written. If the code there changes, the plan may be stale.
;; Racket files: exact definition lookup, hash over the datum (formatting-insensitive).
;; Other files: a keyword/indentation heuristic, clearly labelled as such.
(require racket/list racket/string "srcread.rkt")
(provide parse-anchor resolve-anchor anchor-state baseline-anchor symbol->anchor-name)

(define (parse-anchor s)
  (define m (regexp-match #rx"^([^#]+)(?:#(.+))?$" s))
  (unless m (raise-argument-error 'parse-anchor "path or path#name" s))
  (values (cadr m) (caddr m)))

;; → hasheq: ref found? method line end hash problem
(define (resolve-anchor root ref)
  (define-values (rel name) (parse-anchor ref))
  (define p (build-path root rel))
  (define (miss problem [method 'none]) (hasheq 'ref ref 'found? #f 'problem problem 'method method))
  (cond
    [(not (file-exists? p)) (miss "file not found")]
    [else
     (define text (file->text p))
     (cond
       [(not name)
        (hasheq 'ref ref 'found? #t 'method 'file 'line 1
                'end (add1 (length (regexp-match-positions* #rx"\n" text))) 'hash (text-hash text))]
       [(racket-file? p)
        (with-handlers ([exn:fail:read? (λ (e) (miss (string-append "unreadable: " (clip-msg e)) 'racket))])
          (define-values (forms _lang _t) (read-racket-source text #:source rel))
          (define d (for/first ([d (find-definitions forms)]
                                #:when (or (equal? (symbol->anchor-name (car d)) name)
                                           (equal? (symbol->string (car d)) name)))
                      d))
          (cond
            [d
             (define f (cdr d))
             (hasheq 'ref ref 'found? #t 'method 'racket
                     'line (syntax-line f)
                     'end (position->line text (+ (syntax-position f) (max 0 (sub1 (syntax-span f)))))
                     'hash (datum-hash f))]
            [else (miss (format "no definition of ~a" name) 'racket)]))]
       [else (heuristic text ref name)])]))

(define (clip-msg e) (car (string-split (exn-message e) "\n")))

;; How a definition name is written after `#`: plainly, or in printed form when the plain text would
;; be empty or ambiguous (Rosette defines `||`, which Racket reads as the empty symbol).
(define (symbol->anchor-name s)
  (define str (symbol->string s))
  (if (or (string=? str "") (regexp-match? #px"[\\s#|]" str)) (format "~s" s) str))

;; ---------------------------------------------------------------------------------------------
;; Heuristic for non-Racket files: find a definition line by keyword, then take the indented block.

(define def-keywords
  "(?:def|function\\*?|fn|func(?:\\s*\\([^)]*\\))?|class|struct|enum|interface|type|trait|impl|const|let|var|val|module|macro|defmacro|defn|defun|object|record|protocol|sub|proc)")
(define modifiers
  "(?:(?:export|pub(?:\\([^)]*\\))?|public|private|protected|static|async|default|abstract|final|override|inline|extern)\\s+)*")

(define (heuristic text ref name)
  (define lines (list->vector (string-split text "\n" #:trim? #f)))
  (define q (regexp-quote name))
  (define kw-rx (pregexp (string-append "^\\s*" modifiers def-keywords "\\s+" q "\\b")))
  (define c-rx (pregexp (string-append "^\\s*[A-Za-z_][\\w<>,\\[\\]\\*&:\\s]*\\b" q "\\s*\\([^;]*$")))
  (define bad-start #px"^\\s*(?:return|if|while|for|switch|else|case|throw|new)\\b")
  (define start
    (or (for/first ([l lines] [i (in-naturals)] #:when (regexp-match? kw-rx l)) i)
        (for/first ([l lines] [i (in-naturals)]
                    #:when (and (regexp-match? c-rx l) (not (regexp-match? bad-start l))))
          i)))
  (cond
    [(not start) (hasheq 'ref ref 'found? #f 'method 'heuristic
                         'problem (format "no definition line for ~a found" name))]
    [else
     (define end (block-end lines start))
     (define body (string-join (for/list ([i (in-range start (add1 end))]) (vector-ref lines i)) "\n"))
     (hasheq 'ref ref 'found? #t 'method 'heuristic 'line (add1 start) 'end (add1 end)
             'hash (text-hash body))]))

(define (indent-of l) (string-length (car (regexp-match #px"^[ \t]*" l))))
(define (blank? l) (regexp-match? #px"^\\s*$" l))

;; Last line index of the block starting at `start`: lines indented deeper belong to it; a closer
;; at the same indentation (}, end, ...) ends it inclusively; an Allman-style "{" line continues it.
(define (block-end lines start)
  (define n (vector-length lines))
  (define i0 (indent-of (vector-ref lines start)))
  (let loop ([i (add1 start)] [last start] [first-body? #t])
    (cond
      [(>= i n) last]
      [(blank? (vector-ref lines i)) (loop (add1 i) last first-body?)]
      [else
       (define l (vector-ref lines i))
       (define ind (indent-of l))
       (cond [(> ind i0) (loop (add1 i) i #f)]
             [(and first-body? (regexp-match? #px"^\\s*\\{\\s*$" l)) (loop (add1 i) i #f)]
             [(regexp-match? #px"^\\s*(?:[\\]})]|end\\b)" l) i]
             [else last])])))

;; ---------------------------------------------------------------------------------------------
;; Baselines and states

;; The stored form of an anchor: ref + hash at planning time (#f when the symbol does not exist yet).
(define (baseline-anchor root ref)
  (define r (resolve-anchor root ref))
  (hasheq 'ref ref 'hash (and (hash-ref r 'found? #f) (hash-ref r 'hash))))

;; ok | changed | missing | created | pending
(define (anchor-state baseline current)
  (define h (hash-ref baseline 'hash #f))
  (define found? (hash-ref current 'found? #f))
  (cond [(not h) (if found? 'created 'pending)]
        [(not found?) 'missing]
        [(equal? h (hash-ref current 'hash)) 'ok]
        [else 'changed]))
