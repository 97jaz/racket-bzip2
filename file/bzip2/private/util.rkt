#lang racket/base

(provide (all-defined-out))

(define ash arithmetic-shift)
(define & bitwise-and)

(define (vector-inc! vec idx)
  (define val (vector-ref vec idx))
  (vector-set! vec idx (add1 val)))

(define (vector-dec! vec idx)
  (define val (vector-ref vec idx))
  (vector-set! vec idx (sub1 val)))

(define (ash32 x y)
  (& #xffffffff (ash x y)))
