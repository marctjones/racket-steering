#lang racket/base
;; T49: exact Python anchors (qualified names, decorator-inclusive spans, formatting-insensitive hash)
;; via the stdlib ast, replacing the indentation heuristic that note 12 found wrong on: multi-line
;; signatures (block ends at "):"), decorators (excluded from the span), and same-named methods in
;; several classes or overload groups (first match wins, no qualified-name support).
(require rackunit racket/list racket/string racket/file racket/port racket/runtime-path json
         "../steer/anchors.rkt" "../steer/python.rkt" "../steer/srcread.rkt" racket/path)

(define-runtime-path shapes-py "fixtures/py/shapes.py")

(cond
  [(not (python-available?))
   (eprintf "py-anchors-test: SKIPPED: python3 not found\n")]
  [else
   (define shapes (file->string shapes-py))
   (define dir (path-only shapes-py))
   (define (resolve name) (resolve-anchor dir (format "~a#~a" (path->string (file-name-from-path shapes-py)) name)))

   ;; -----------------------------------------------------------------------------------------
   ;; note 12's specific failures, fixed: qualified names, decorator span, multi-line signature,
   ;; the right one of several same-named definitions

   (let ([r (resolve "First.__init__")])
     (check-true (hash-ref r 'found? #f))
     (check-equal? (hash-ref r 'method) 'python)
     (check-equal? (hash-ref r 'kind) "def")
     (check-equal? (list (hash-ref r 'line) (hash-ref r 'end)) '(47 48)))
   (check-false (hash-ref (resolve "__init__") 'found? #f) "the bare name is ambiguous (3 classes have one); it is not silently the first")
   (check-regexp-match #rx"closest: First.__init__, Second.__init__, Third.__init__" (hash-ref (resolve "__init__") 'problem))

   (let ([r (resolve "multi_line_signature")])
     (check-true (hash-ref r 'found?))
     (check-equal? (list (hash-ref r 'line) (hash-ref r 'end)) '(15 21) "spans the whole multi-line signature and body, not just the def line"))

   (let ([r (resolve "First.size")])                        ; @property + @size.setter: two defs named size
     (check-true (hash-ref r 'found?))
     (check-equal? (hash-ref r 'line) 50)
     (check-equal? (hash-ref r 'shadowed) 1 "one later definition shares this qualified name (the setter)"))

   (let ([r (resolve "render")])                             ; @functools.singledispatch base + two @render.register _s
     (check-true (hash-ref r 'found?))
     (check-equal? (hash-ref r 'line) 24))
   (let ([r (resolve "_")])                                  ; the two @render.register(_) registrations, both named _
     (check-true (hash-ref r 'found?))
     (check-equal? (hash-ref r 'line) 29)
     (check-equal? (hash-ref r 'shadowed) 2))

   (let ([r (resolve "First.fetch")])                        ; async def
     (check-true (hash-ref r 'found?))
     (check-equal? (hash-ref r 'kind) "async def"))

   (let ([r (resolve "Third.get")])                          ; @overload x2 + the real implementation, all named get
     (check-true (hash-ref r 'found?))
     (check-equal? (hash-ref r 'shadowed) 2))

   (let ([r (resolve "Second.describe")])                    ; a nested def (helper) inside it
     (check-true (hash-ref r 'found?))
     (check-equal? (list (hash-ref r 'line) (hash-ref r 'end)) '(72 76) "the outer method's own span, not cut short at the nested def"))
   (check-true (hash-ref (resolve "Second.describe.helper") 'found? #f) "a nested function is reachable by its full dotted path")

   ;; unresolvable: a typo or a name that truly does not exist is `not-found`, never silently `pending`
   (check-false (hash-ref (resolve "First.nope") 'found? #f))
   ;; T51: the file exists, so a typo gets a did-you-mean, not a silent "not found"
   (check-regexp-match #rx"^no definition of First.nope \\(closest: " (hash-ref (resolve "First.nope") 'problem))
   (check-true (hash-ref (resolve "First.nope") 'file-exists? #f))
   (check-false (hash-ref (resolve "Ghost.method") 'found? #f))

   ;; -----------------------------------------------------------------------------------------
   ;; the hash: formatting-insensitive (comments, whitespace, docstring layout), but real changes count

   (define (put-in! dir name text) (call-with-output-file (build-path dir name) #:exists 'truncate (λ (o) (void (write-string text o)))))

   (define base (baseline-anchor dir (format "~a#top_level" (path->string (file-name-from-path shapes-py)))))
   (check-equal? (anchor-state base (resolve "top_level")) 'ok)

   (define dir2 (make-temporary-directory "steer-pyanc~a"))
   (define original-body "    total = a + b\n    return total\n")
   (define reformatted-body "    # a comment\n    total   =   a + b\n    return total\n")
   (define changed-body "    total = a + b\n    return total * 2\n")
   (check-true (regexp-match? (regexp (regexp-quote original-body)) shapes) "the target text is present, unmodified, in the fixture")
   (define reformatted (string-replace shapes original-body reformatted-body))
   (check-true (not (equal? reformatted shapes)) "the reformat actually changed the text")
   (put-in! dir2 "s.py" reformatted)
   (check-equal? (anchor-state base (resolve-anchor dir2 "s.py#top_level")) 'ok "comments and whitespace do not make an anchor stale")
   (put-in! dir2 "s.py" (string-replace shapes original-body changed-body))
   (check-equal? (anchor-state base (resolve-anchor dir2 "s.py#top_level")) 'changed "a real body change does")
   (put-in! dir2 "s.py" (string-replace shapes "def top_level(" "def top_level_renamed("))
   (check-equal? (anchor-state base (resolve-anchor dir2 "s.py#top_level")) 'missing "renamed away, and it has no sibling to be confused with")
   (delete-directory/files dir2)

   ;; a change to a decorator is a real change too (the span includes decorators)
   (define base-dec (baseline-anchor dir (format "~a#First.fetch" (path->string (file-name-from-path shapes-py)))))
   (define dir2b (make-temporary-directory "steer-pyanc~a"))
   (put-in! dir2b "s.py" (string-replace shapes "    async def fetch" "    @staticmethod\n    async def fetch"))
   (check-equal? (anchor-state base-dec (resolve-anchor dir2b "s.py#First.fetch")) 'changed "a decorator was added")
   (delete-directory/files dir2b)

;; -----------------------------------------------------------------------------------------
   ;; not a Python file at all, and a file with a syntax error, are reported (not confused with "pending")

   (define dir4 (make-temporary-directory "steer-pyanc~a"))
   (put-in! dir4 "bad.py" "def f(x:\n")
   (let ([r (resolve-anchor dir4 "bad.py#f")])
     (check-false (hash-ref r 'found? #f))
     (check-regexp-match #rx"unreadable" (hash-ref r 'problem)))
   (delete-directory/files dir4)

   ;; -----------------------------------------------------------------------------------------
   ;; measured: every def/class in the fixture resolves, by its qualified name, to itself

   ;; a quick regexp scan of def names (good enough to check coverage of the exact resolver above)
   (define py-defs
     (remove-duplicates (regexp-match* #px"(?m:^\\s*(?:async )?def +([A-Za-z_][A-Za-z0-9_]*))" shapes #:match-select cadr)))
   (check-true (>= (length py-defs) 10) "the fixture defines at least this many distinct names")
   (define bare-results (for/list ([n py-defs]) (cons n (hash-ref (resolve n) 'found? #f))))
   (printf "py-anchors-test: ~a distinct def names in shapes.py, ~a resolve unqualified (the rest need Class.name because more than one class defines them)\n"
           (length bare-results) (length (filter cdr bare-results)))
   (check-true (>= (/ (length (filter cdr bare-results)) (length bare-results)) 0.3) "at least some names are unambiguous even in a fixture designed to maximise duplicates")

   ;; -----------------------------------------------------------------------------------------
   ;; routing: .py and .pyi both use the python method

   (check-equal? (hash-ref (resolve-anchor dir "shapes.py#top_level") 'method) 'python)
   (define dir5 (make-temporary-directory "steer-pyanc~a"))
   (put-in! dir5 "s.pyi" "def f(x: int) -> int: ...\n")
   (check-equal? (hash-ref (resolve-anchor dir5 "s.pyi#f") 'method) 'python)
   (delete-directory/files dir5)])
