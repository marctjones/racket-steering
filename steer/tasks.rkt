#lang racket/base
;; The task graph as pure functions: readiness, blockers, cycles, ordering, layers, critical path.
;; Kept free of I/O so it is easy to test. Readiness needs negation ("no undone dependency"),
;; which is why this is plain Racket rather than #lang datalog (note 06 §2).
(require racket/list racket/string "common.rkt" "store.rkt")
(provide statuses index ready? blockers derived-status
         find-cycle dependents transitive-dependent-count ready-order
         layers critical-path graph-findings)

(define statuses '(open active done dropped))

(define (index tasks) (for/hash ([t tasks]) (values (task-ref t 'id) t)))

(define (status-of by-id id) (let ([t (hash-ref by-id id #f)]) (and t (task-ref t 'status))))

;; Dependencies that are not yet satisfied: (list id reason) where reason ∈ missing|dropped|<status>.
(define (blockers t by-id)
  (for*/list ([d (task-ref t 'after '())]
              [s (in-value (status-of by-id d))]
              #:unless (eq? s 'done))
    (list d (or s 'missing))))

(define (ready? t by-id)
  (and (eq? (task-ref t 'status) 'open) (null? (blockers t by-id))))

;; What an agent should call the task: done, dropped, active, ready or blocked.
(define (derived-status t by-id)
  (case (task-ref t 'status)
    [(open) (if (ready? t by-id) 'ready 'blocked)]
    [else (task-ref t 'status)]))

;; First dependency cycle found, as a list of ids with the start repeated at the end, or #f.
(define (find-cycle by-id)
  (define color (make-hash))                   ; id → 'grey | 'black
  (let/ec return
    (define (visit id path)
      (case (hash-ref color id #f)
        [(grey) (return (member id (reverse path)))]  ; path already ends with id again
        [(black) (void)]
        [else
         (hash-set! color id 'grey)
         (define t (hash-ref by-id id #f))
         (when t
           (for ([d (task-ref t 'after '())]) (visit d (cons d path))))
         (hash-set! color id 'black)]))
    (for ([id (sort (hash-keys by-id) < #:key id-number)])
      (visit id (list id)))
    #f))

;; Reverse edges: id → ids that list it in `after`.
(define (dependents by-id)
  (for*/fold ([m (hash)]) ([(id t) (in-hash by-id)] [d (task-ref t 'after '())])
    (hash-update m d (λ (l) (cons id l)) '())))

(define (transitive-dependent-count id rev)
  (let loop ([todo (hash-ref rev id '())] [seen (set-empty)])
    (cond [(null? todo) (set-count seen)]
          [(set-has? seen (car todo)) (loop (cdr todo) seen)]
          [else (loop (append (hash-ref rev (car todo) '()) (cdr todo)) (set-add seen (car todo)))])))

;; tiny immutable set on hash (avoids racket/set load time)
(define (set-empty) (hash))
(define (set-has? s x) (hash-ref s x #f))
(define (set-add s x) (hash-set s x #t))
(define (set-count s) (hash-count s))

;; Ready tasks, best first: priority (0 = most urgent), then how much work each unblocks, then age.
(define (ready-order by-id)
  (define rev (dependents by-id))
  (sort (filter (λ (t) (ready? t by-id)) (hash-values by-id))
        (λ (a b)
          (define (key t) (list (task-ref t 'priority 2)
                                (- (transitive-dependent-count (task-ref t 'id) rev))
                                (id-number (task-ref t 'id))))
          (let cmp ([x (key a)] [y (key b)])
            (cond [(null? x) #f]
                  [(< (car x) (car y)) #t]
                  [(> (car x) (car y)) #f]
                  [else (cmp (cdr x) (cdr y))])))))

(define (open-work? t) (memq (task-ref t 'status) '(open active)))

;; Kahn layers over unfinished work (open + active); done deps are already satisfied.
(define (layers by-id)
  (define work (for/hash ([(id t) (in-hash by-id)] #:when (open-work? t)) (values id t)))
  (let loop ([placed (hash)] [acc '()])
    (define layer
      (sort (for/list ([(id t) (in-hash work)]
                       #:unless (hash-ref placed id #f)
                       #:when (for/and ([d (task-ref t 'after '())])
                                (or (hash-ref placed d #f) (not (hash-ref work d #f)))))
              id)
            < #:key id-number))
    (if (null? layer)
        (reverse acc)
        (loop (for/fold ([p placed]) ([id layer]) (hash-set p id #t)) (cons layer acc)))))

;; Longest chain of unfinished work (assumes acyclic; call after find-cycle).
(define (critical-path by-id)
  (define memo (make-hash))
  (define (longest id)                        ; longest chain ending at id, as a list of ids
    (hash-ref! memo id
               (λ ()
                 (define t (hash-ref by-id id))
                 (define preds (filter (λ (d) (let ([u (hash-ref by-id d #f)]) (and u (open-work? u))))
                                       (task-ref t 'after '())))
                 (define best (for/fold ([b '()]) ([d preds])
                                (define c (longest d))
                                (if (> (length c) (length b)) c b)))
                 (append best (list id)))))
  (for/fold ([b '()]) ([(id t) (in-hash by-id)] #:when (open-work? t))
    (define c (longest id))
    (if (or (> (length c) (length b))
            (and (= (length c) (length b)) (pair? c) (pair? b)
                 (< (id-number (last c)) (id-number (last b)))))
        c b)))

;; Structural problems in the whole graph.
(define (graph-findings by-id)
  (define cyc (find-cycle by-id))
  (append
   (if cyc
       (list (finding 'error 'cycle (string-append "dependency cycle: " (string-join cyc " → "))
                      #:task (car cyc) #:fix (format "remove one edge, e.g. `steer edit ~a --rm-after ~a`"
                                                     (car cyc) (cadr cyc))))
       '())
   (for*/list ([id (sort (hash-keys by-id) < #:key id-number)]
               [t (in-value (hash-ref by-id id))]
               #:when (open-work? t)
               [d (task-ref t 'after '())]
               [s (in-value (status-of by-id d))]
               #:when (memq s '(#f dropped)))
     (if s
         (finding 'warning 'dropped-dependency (format "~a waits for ~a, which was dropped" id d)
                  #:task id #:fix (format "`steer edit ~a --rm-after ~a` or `steer reopen ~a`" id d d))
         (finding 'error 'missing-dependency (format "~a waits for ~a, which does not exist" id d)
                  #:task id #:fix (format "`steer edit ~a --rm-after ~a`" id d))))))
