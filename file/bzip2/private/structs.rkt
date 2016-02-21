#lang racket/base

(require racket/match)

(provide (all-defined-out))

(define RADIX 2)
(define QSORT 12)
(define SHELL 18)
(define OVERSHOOT (+ RADIX QSORT SHELL 2))
(define MAX-ALPHA-SIZE 258)
(define MAX-CODE-LEN 23)
(define RUNA 0)
(define RUNB 1)
(define LESSER-ICOST 0)
(define GREATER-ICOST 15)
(define G-SIZE 50)
(define MAX-SELECTORS (+ 2 (quotient 900000 G-SIZE)))
(define ITERS 4)
(define MTFA-SIZE 4096)
(define MTFL-SIZE 16)

(struct run (byte len) #:transparent #:mutable)
(struct block (idx data fmap mtfv in-use len crc maxlen size100k) #:transparent #:mutable)
(struct compressor (out live buffer run block crc) #:mutable #:transparent)
(struct decompressor (in live buffer state size100k crc tt tpos nblock nblock-used run-len run-byte repeat-len repeat-byte block-crc) #:mutable #:transparent)
(struct exn:fail:bzip2 exn:fail ())
