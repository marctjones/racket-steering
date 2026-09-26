#lang racket/base
;; Claude Code skills shipped inside the binary (catalog F7). The files under ../skills are read at
;; compile time and registered as dependencies, so a standalone executable can install them into
;; any project (.claude/skills) or for the user (~/.claude/skills) without the repo being present.
(require (for-syntax racket/base racket/file racket/path compiler/cm-accomplice)
         racket/file racket/list racket/string)
(provide skill-files skill-names install-skills!)

(define-syntax (embedded-skills stx)
  (define-values (dir _n _d) (split-path (syntax-source stx)))
  (define skills-dir (simplify-path (build-path dir 'up "skills")))
  (define files
    (if (directory-exists? skills-dir)
        (sort (for/list ([p (in-directory skills-dir)]
                         #:when (file-exists? p)
                         #:unless (regexp-match? #rx"(^|/)\\." (path->string (find-relative-path skills-dir p))))
                p)
              path<?)
        '()))
  (for ([f files]) (register-external-file (path->complete-path f)))
  (datum->syntax stx `(quote ,(for/list ([f files])
                                (cons (path->string (find-relative-path skills-dir f)) (file->string f))))))

;; (list (relative-path . content) ...), e.g. ("steer-tasks/SKILL.md" . "---\nname: ...")
(define skill-files (embedded-skills))

(define (skill-names)
  (remove-duplicates (for/list ([f skill-files]) (car (string-split (car f) "/")))))

;; Writes every embedded skill under dest. → (list (name . status) ...)
(define (install-skills! dest #:force? [force? #f] #:only [only #f])
  (for/list ([name (skill-names)] #:when (or (not only) (member name only)))
    (define files (filter (λ (f) (string-prefix? (car f) (string-append name "/"))) skill-files))
    (define statuses
      (for/list ([f files])
        (define p (build-path dest (car f)))
        (cond [(and (file-exists? p) (equal? (file->string p) (cdr f))) 'unchanged]
              [(and (file-exists? p) (not force?)) 'kept]
              [else
               (make-directory* (let-values ([(d _n _x) (split-path p)]) d))
               (define existed? (file-exists? p))
               (call-with-output-file p #:exists 'truncate (λ (o) (write-string (cdr f) o)))
               (if existed? 'updated 'installed)])))
    (cons name
          (cond [(memq 'kept statuses) (format "~a differs from this steer version; kept yours (use --force to overwrite)" (path->string (build-path dest name)))]
                [(memq 'updated statuses) (format "updated in ~a" (path->string (build-path dest name)))]
                [(memq 'installed statuses) (format "installed in ~a" (path->string (build-path dest name)))]
                [else "up to date"]))))
