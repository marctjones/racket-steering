#lang racket/base
;; T60 fixture: shapes exercised for the Racket extractor's conformance test. Covers a same-file
;; exact call, a cross-file declared call resolved through a require, a mutable struct's generated
;; accessor being a resolvable call target, a locally-bound name shadowing a real function (which
;; must NOT show up as a call to that function), an explicit `;; steer: entry` marker, and a helper
;; reachable from nothing (so a later reachability pass has something real to call dead).
(require "helper.rkt")
(provide area perimeter)

;; steer: entry
(define (area shape)
  (compute-area shape))

(define (compute-area shape)
  (double (rect-w shape)))

(struct rect (w h) #:mutable)

(define (perimeter r)
  (let ([w (rect-w r)] [h (rect-h r)])
    (+ (double w) (double h))))

;; a local variable named the same as the real (imported) `triple`: the call inside must be invisible
;; to the graph as a call to helper.rkt's `triple` (dup.rkt's collect-binders subtracts it)
(define (shadow-triple x)
  (let ([triple (lambda (y) (+ y 1))])
    (triple x)))

;; never called from `area` (the only entry here): a forward walk must not reach it
(define (unused-helper x)
  (triple x))
