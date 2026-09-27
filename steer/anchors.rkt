#lang racket/base
;; Symbol anchors (catalog F2). A task points at "path#name" (or just "path"); the store keeps a
;; baseline hash taken when the plan was written. If the code there changes, the plan may be stale.
;; Racket files: exact definition lookup, hash over the datum (formatting-insensitive).
;; Other files: a keyword/indentation heuristic, clearly labelled as such.
(require racket/list racket/string "srcread.rkt" "python.rkt" "csharp.rkt"
         (only-in "common.rkt" closest))
(provide parse-anchor resolve-anchor anchor-state baseline-anchor symbol->anchor-name anchor-names-in-file)

(define (parse-anchor s)
  (define m (regexp-match #rx"^([^#]+)(?:#(.+))?$" s))
  (unless m (raise-argument-error 'parse-anchor "path or path#name" s))
  (values (cadr m) (caddr m)))

;; Every definable name in `p` (Racket, Python, C#; '() for a language without a name lister, and
;; for a file that fails to parse). Used only for did-you-mean; never raises.
(define (anchor-names-in-file p)
  (with-handlers ([(λ (e) #t) (λ (e) '())])
    (define text (file->text p))
    (cond
      [(racket-file? p)
       (define-values (forms _lang _t) (read-racket-source text))
       (map (λ (d) (symbol->anchor-name (car d))) (find-definitions forms))]
      [(python-file? p) (python-list-names text)]
      [(csharp-file? p) (cs-list-names text)]
      [else '()])))

;; T51: a task pointing past an EXISTING file at a name that file does not define is very likely a
;; typo or the wrong qualification, not "the task will create this" (that reading only makes sense
;; when the file itself does not exist yet). Distinguishing the two, with a suggestion, is the point:
;; a wrong name should not sit silently mislabelled "pending" until someone happens to look at it.
(define (miss-with-suggestion ref problem method existing-candidates p name)
  (define candidates
    (if (pair? existing-candidates) existing-candidates
        (and name (file-exists? p) (closest name (anchor-names-in-file p) #:max 5))))
  (hasheq 'ref ref 'found? #f 'method method
          'problem (if (pair? candidates) (format "~a (closest: ~a)" problem (string-join candidates ", ")) problem)
          'file-exists? (and (file-exists? p) #t)
          'suggestions (or candidates '())))

;; → hasheq: ref found? method line end hash problem file-exists? suggestions
(define (resolve-anchor root ref)
  (define-values (rel name) (parse-anchor ref))
  (define p (build-path root rel))
  (define (miss problem [method 'none] [candidates '()]) (miss-with-suggestion ref problem method candidates p name))
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
       ;; Python: exact resolution of any dotted qualification (Class.method) via the stdlib ast,
       ;; instead of the indentation heuristic (which finds the wrong block on multi-line signatures
       ;; and the wrong same-named definition on overloads: note 12).
       [(python-file? p)
        (define r (python-find-anchor text name))
        (cond
          [(hash-ref r 'found? #f)
           (hasheq 'ref ref 'found? #t 'method 'python 'kind (hash-ref r 'kind) 'line (hash-ref r 'line) 'end (hash-ref r 'end)
                   'hash (hash-ref r 'hash) 'shadowed (hash-ref r 'shadowed #f))]
          ;; an ambiguity list (the name exists, just not uniquely) is a better suggestion than fuzzy
          ;; matching against every name in the file, so it takes priority over the did-you-mean fallback
          [else (miss (hash-ref r 'problem "not found") 'python (hash-ref r 'candidates '()))])]
       ;; C#: exact resolution via a member scanner (generics, properties, indexers, operators,
       ;; partial classes, Allman/K&R bodies), instead of the indentation heuristic. Overloads and
       ;; same-named members across partial declarations need `Name/arity` or `Name(type,type)`.
       [(csharp-file? p)
        (define r (cs-find-anchor text name))
        (cond
          [(hash-ref r 'found? #f)
           (hasheq 'ref ref 'found? #t 'method 'csharp 'kind (hash-ref r 'kind) 'line (hash-ref r 'line) 'end (hash-ref r 'end)
                   'hash (hash-ref r 'hash) 'shadowed (hash-ref r 'shadowed #f))]
          [else (miss (hash-ref r 'problem "not found") 'csharp (hash-ref r 'candidates '()))])]
       [else (heuristic text ref name)])]))

(define (python-file? p) (regexp-match? #rx"[.]pyi?$" (path->string p)))
(define (csharp-file? p) (regexp-match? #rx"[.]cs$" (path->string p)))

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
    [(not start) (hasheq 'ref ref 'found? #f 'method 'heuristic 'file-exists? #t 'suggestions '()
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
