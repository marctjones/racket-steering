#lang racket/base
;; `steer github sync`: the pure planner, and the CLI against a fake `gh` that logs its arguments.
(require rackunit racket/list racket/string racket/file racket/port racket/runtime-path
         "../steer/github.rkt" "../steer/tasks.rkt")

;; ---------------------------------------------------------------------------------------------
;; planner

(define (t id title #:status [status 'open] #:after [after '()] #:tags [tags '()] #:github [gh #f])
  (define base (hasheq 'id id 'title title 'status status 'after after 'tags tags 'checks '("raco test x.rkt")
                       'goal (format "goal of ~a" id) 'anchors '() 'priority 2))
  (if gh (hash-set base 'github gh) base))

(define ms (t "T9" "M1 · ship" #:after '("T1" "T2") #:tags '("g" "milestone")))
(define a (t "T1" "first" #:tags '("g")))
(define b (t "T2" "second" #:status 'done #:after '("T1") #:tags '("g" "x")))
(define by-id (index (list ms a b)))
(define plan (sync-plan (list ms a b) by-id))

(check-equal? (map (λ (x) (list (car x) (cadr x) (caddr x))) plan)
              '((create milestone "T9") (create issue "T1") (create issue "T2"))
              "milestones first, then issues in dependency order")
(let ([d (cadddr (third plan))])
  (check-equal? (hash-ref d 'milestone) "T9")
  (check-equal? (hash-ref d 'labels) '("g" "x") "milestone tag is not a label")
  (check-equal? (hash-ref d 'state) "closed")
  (check-equal? (hash-ref d 'state_reason) "completed")
  (check-regexp-match #rx"<!-- steer:T2 -->" (hash-ref d 'body))
  (check-regexp-match #rx"`raco test x.rkt`" (hash-ref d 'body)))

;; after publishing (numbers + digests stored) nothing is left to do
(define (published task n action) (hash-set task 'github (hasheq 'kind (cadr action) 'number n 'digest (list-ref action 4))))
(define ms* (published ms 1 (first plan)))
(define a* (published a 10 (second plan)))
(define b0 (published b 11 (third plan)))
(define by-id* (index (list ms* a* b0)))
;; b's body mentions T1, which now has an issue number: that changes b's digest once
(define plan2 (sync-plan (list ms* a* b0) by-id*))
(check-equal? (map (λ (x) (list (car x) (caddr x))) plan2) '((update "T2")) "dependency links are filled in on the next run")
(check-regexp-match #rx"T1 \\(#10\\)" (hash-ref (cadddr (car plan2)) 'body))
(define b* (published b0 11 (car plan2)))
(check-equal? (sync-plan (list ms* a* b*) (index (list ms* a* b*))) '() "idempotent")
;; a change is an update, not a new issue
(define a2 (hash-set a* 'title "first, renamed"))
(check-equal? (map (λ (x) (list (car x) (caddr x))) (sync-plan (list ms* a2 b*) (index (list ms* a2 b*)))) '((update "T1")))

;; ---------------------------------------------------------------------------------------------
;; CLI with a fake gh

(define-runtime-path main-rkt "../steer/main.rkt")
(define dir (make-temporary-directory "steer-gh~a"))
(define log (build-path dir "gh.log"))
(define fake (build-path dir "fake-gh"))
(call-with-output-file fake
  (λ (o) (void (write-string (string-append
                        "#!/bin/sh\n"
                        "printf '%s\\n<<END>>\\n' \"$*\" >> '" (path->string log) "'\n"
                        "n=$(cat '" (path->string dir) "/n' 2>/dev/null || echo 0); n=$((n+1)); echo $n > '" (path->string dir) "/n'\n"
                        "echo \"{\\\"number\\\": $n}\"\n")
                       o))))
(file-or-directory-permissions fake #o755)

(define (steer #:in [input ""] . args)
  (define-values (p out in err)
    (parameterize ([current-directory dir])
      (putenv "STEER_GH" (path->string fake))
      (apply subprocess #f #f 'stdout (find-executable-path "racket") (path->string main-rkt) args)))
  (write-string input in) (close-output-port in)
  (define text (port->string out))
  (subprocess-wait p) (close-input-port out)
  (values (subprocess-status p) text))
(define (calls)                                   ; one entry per gh call (bodies span lines)
  (if (file-exists? log) (filter (λ (x) (not (string=? x ""))) (string-split (file->string log) "\n<<END>>\n")) '()))

(let-values ([(c _) (steer "init")]) (check-equal? c 0))
(let-values ([(c o) (steer "import" "-" #:in "(task \"A\" #:id a #:check \"true\" #:tag g)\n(task \"B\" #:id b #:after (a) #:check \"true\" #:tag g)\n(task \"M\" #:after (a b) #:check \"true\" #:tag (g milestone))\n(task \"not selected\" #:check \"true\" #:tag other)")])
  (check-equal? c 0 o))
(let-values ([(c o) (steer "github" "sync")]) (check-equal? c 2 "refuses without --tag/--all") (check-regexp-match #rx"--tag" o))
(let-values ([(c o) (steer "github" "sync" "--tag" "g" "--repo" "o/r")])
  (check-equal? c 0 o)
  (check-regexp-match #rx"milestones 1 created 0 updated · issues 2 created" o))
(check-equal? (length (filter (λ (l) (regexp-match? #rx"repos/o/r/milestones" l)) (calls))) 1)
(check-equal? (length (filter (λ (l) (regexp-match? #rx"^api repos/o/r/issues " l)) (calls))) 2)
(check-equal? (length (calls)) 4 "one label + one milestone + two issues")
(check-true (for/or ([l (calls)]) (regexp-match? #rx"repos/o/r/labels -f name=g" l)) "labels are created once")
(check-false (for/or ([l (calls)]) (regexp-match? #rx"not selected" l)))
(let-values ([(c o) (steer "show" "T1")]) (check-regexp-match #rx"github: issue #" o))
;; second run: B's body now links A's issue number (one update), third run: nothing
(call-with-values (λ () (steer "github" "sync" "--tag" "g" "--repo" "o/r")) void)
(define before (length (calls)))
(let-values ([(c o) (steer "github" "sync" "--tag" "g" "--repo" "o/r")])
  (check-regexp-match #rx"0 created 0 updated · issues 0 created 0 updated" o)
  (check-equal? (length (calls)) before "an unchanged store sends nothing"))
;; finishing a task closes its issue
(call-with-values (λ () (steer "claim" "T1")) void)
(call-with-values (λ () (steer "done" "T1")) void)
(let-values ([(c o) (steer "github" "sync" "--tag" "g" "--repo" "o/r")])
  (check-regexp-match #rx"issues 0 created 1 updated" o))
(check-true (regexp-match? #rx"-X PATCH repos/o/r/issues/[0-9]+ .*state=closed.*state_reason=completed" (last (calls))))

(delete-directory/files dir)
