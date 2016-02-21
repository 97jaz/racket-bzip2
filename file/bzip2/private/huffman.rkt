#lang racket/base

(require "structs.rkt"
         "util.rkt")
(provide (all-defined-out))

(define (assign-codes! code length minlen maxlen alpha-size)
  (for/fold ([v 0]) ([n (in-range minlen (add1 maxlen))])
    (ash
     (for/fold ([v v]) ([i (in-range alpha-size)])
       (cond [(= (bytes-ref length i) n)
              (vector-set! code i v)
              (add1 v)]
             [else v]))
     1)))

(define (make-code-lengths! len freq alpha-size maxlen)
  (define heap   (make-vector (+ 2 MAX-ALPHA-SIZE) 0))
  (define weight (make-vector (* 2 MAX-ALPHA-SIZE) 0))
  (define parent (make-vector (* 2 MAX-ALPHA-SIZE) 0))

  (for ([i (in-range alpha-size)])
    (define fi (vector-ref freq i))
    (vector-set! weight (add1 i) (ash32 (if (zero? fi) 1 fi) 8)))

  (let loop ()
    (vector-set! heap 0 0)
    (vector-set! weight 0 0)
    (vector-set! parent 0 -2)

    (let*-values
        ([(n-heap)
          (for/fold ([n-heap 0]) ([i (in-range 1 (add1 alpha-size))])
            (define nh (add1 n-heap))
            (vector-set! parent i -1)
            (vector-set! heap nh i)
            (upheap! nh heap weight)
            nh)]

         [(n-heap n-nodes)
          (let inner ([n-heap n-heap] [n-nodes alpha-size])
            (cond [(> n-heap 1)
                   (define (down! n-heap)
                     (define n (vector-ref heap 1))
                     (vector-set! heap 1 (vector-ref heap n-heap))
                     (downheap! 1 heap weight (sub1 n-heap))
                     (values n (sub1 n-heap)))
                   
                   (let*-values ([(n1 n-heap) (down! n-heap)]
                                 [(n2 n-heap) (down! n-heap)]
                                 [(n-nodes)   (add1 n-nodes)])
                     (vector-set! parent n1 n-nodes)
                     (vector-set! parent n2 n-nodes)
                     (vector-set! weight n-nodes (add-weights weight n1 n2))
                     (vector-set! parent n-nodes -1)
                     (let ([n-heap (add1 n-heap)])
                       (vector-set! heap n-heap n-nodes)
                       (upheap! n-heap heap weight)
                       (inner n-heap n-nodes)))]
                  [else
                   (values n-heap n-nodes)]))])
      
      (define too-long
        (for/fold ([tl #f]) ([i (in-range 1 (add1 alpha-size))])
          (let inner ([j 0] [k i])
            (cond [(>= (vector-ref parent k) 0)
                   (inner (add1 j) (vector-ref parent k))]
                  [else
                   (bytes-set! len (sub1 i) j)
                   (or tl (> j maxlen))]))))

      (when too-long
        (for ([i (in-range 1 (add1 alpha-size))])
          (let* ([j (ash32 (vector-ref weight i) -8)]
                 [j (add1 (quotient j 2))])
            (vector-set! weight i (ash32 j 8))))

        (loop)))))

(define (upheap! z heap weight)
  (define tmp (vector-ref heap z))
  
  (let loop ([zz z])
    (define zz1 (ash32 zz -1))
    
    (cond [(< (vector-ref weight tmp)
              (vector-ref weight (vector-ref heap zz1)))
           (vector-set! heap zz (vector-ref heap zz1))
           (loop zz1)]
          [else
           (vector-set! heap zz tmp)])))

(define (downheap! z heap weight n-heap)
  (define tmp (vector-ref heap z))

  (let loop ([zz z])
    (let ([yy (ash32 zz 1)])
      (cond [(> yy n-heap)
             (vector-set! heap zz tmp)]
            [else
             (define w1 (vector-ref weight (vector-ref heap (add1 yy))))
             (define w2 (vector-ref weight (vector-ref heap yy)))
             
             (let ([yy (if (and (< yy n-heap) (< w1 w2)) (add1 yy) yy)])
               (cond [(< (vector-ref weight tmp)
                         (vector-ref weight (vector-ref heap yy)))
                      (vector-set! heap zz tmp)]
                     [else
                      (vector-set! heap zz (vector-ref heap yy))
                      (loop yy)]))]))))

(define (add-weights vec n1 n2)
  (define w1 (vector-ref vec n1))
  (define w2 (vector-ref vec n2))
  (bitwise-ior (+ (weightof w1) (weightof w2))
               (add1 (max (depthof w1) (depthof w2)))))
                        
(define (weightof w) (& w #xffffff00))
(define (depthof w)  (& w #x000000ff))


(define (create-decode-tables! limit base perm length minlen maxlen alpha-size)
  (for/fold ([pp 0]) ([i (in-range minlen (add1 maxlen))])
    (for/fold ([pp pp]) ([j (in-range alpha-size)])
      (cond [(= (bytes-ref length j) i)
             (vector-set! perm pp j)
             (add1 pp)]
            [else
             pp])))

  (for ([i (in-range (vector-length base))])
    (vector-set! base i 0))
  
  (for ([i (in-range alpha-size)])
    (vector-inc! base (add1 (bytes-ref length i))))
  
  (for ([i (in-range 1 (vector-length base))])
    (vector-set! base i (+ (vector-ref base i)
                           (vector-ref base (sub1 i)))))
  
  (for ([i (in-range (vector-length limit))])
    (vector-set! limit i 0))

  (for/fold ([vec 0]) ([i (in-range minlen (add1 maxlen))])
    (let ([vec (+ vec (- (vector-ref base (add1 i))
                         (vector-ref base i)))])
      (vector-set! limit i (sub1 vec))
      (ash32 vec 1)))

  (for ([i (in-range (add1 minlen) (add1 maxlen))])
    (vector-set! base i (- (ash32 (add1 (vector-ref limit (sub1 i))) 1)
                           (vector-ref base i)))))

                  