#lang racket/base

(require racket/match
         "structs.rkt"
         "util.rkt")

(provide (all-defined-out))

(define (put-bytes! cp bs)
  (for ([b (in-bytes bs)]) (put-byte! cp b)))

(define (put-byte! cp b)
  (put! cp 8 b))

(define (put-int! cp i)
  (put-byte! cp (& #xff (ash i -24)))
  (put-byte! cp (& #xff (ash i -16)))
  (put-byte! cp (& #xff (ash i -8)))
  (put-byte! cp (& #xff i)))

(define (put! cp n v)
  (match-define (struct* compressor ([out out] [live live] [buffer buffer])) cp)

  (let loop ([live live] [buffer buffer])
    (cond [(>= live 8)
           (write-byte (& #xff (ash buffer -24)) out)
           (loop (- live 8) (ash32 buffer 8))]
          [else
           (set-compressor-buffer! cp (bitwise-ior buffer (ash32 v (- 32 live n))))
           (set-compressor-live! cp (+ live n))])))

(define (drain-buffer! cp)
  (match-define (struct* compressor ([out out] [live live] [buffer buffer])) cp)

  (let loop ([live live] [buffer buffer])
    (when (> live 0)
      (write-byte (& #xff (ash buffer -24)) out)
      (loop (- live 8) (ash32 buffer 8)))))
