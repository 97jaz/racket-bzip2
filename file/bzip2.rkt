#lang racket/base

(require racket/contract/base
         "bzip2/private/compress.rkt"
         "bzip2/private/decompress.rkt")

(provide/contract
 [bzip2-output-port (->* (output-port?) (#:block-size (integer-in 1 9)) output-port?)]
 [bzip2-input-port  (-> input-port? input-port?)])

(define (bzip2-output-port out #:block-size [block-size 9])
  (define cp (make-compressor out block-size))
  
  (make-output-port
   'bzip2-compress

   out

   (λ (bs start end buffer? breakable?)
     (define len (- end start))
     
     (for ([b (in-bytes bs start end)])
       (compressor-write-byte cp b))

     (when (zero? len) (flush-output out))
     
     len)

   (λ ()
     (compressor-finish cp)
     (set! cp #f)
     (flush-output out))))

(define (bzip2-input-port in)
  (define dc (make-decompressor in))

  (make-input-port
   'bzip2-decompress

   (λ (bs)
     (define len (bytes-length bs))
     
     (let loop ([i 0])
       (cond [(< i len) (define b (decompressor-read-byte dc))

                        (cond [(eof-object? b) (if (zero? i) b i)]
                              [else (bytes-set! bs i b)
                                    (loop (add1 i))])]
             [else len])))

   #f

   (λ ()
     (set! dc #f))))
