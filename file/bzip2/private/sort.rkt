#lang racket/base

(require racket/match
         racket/list
         racket/vector
         "structs.rkt"
         "util.rkt")

(provide (all-defined-out))

(define SETMASK (ash32 1 21))
(define CLEARMASK (& #xffffffff (bitwise-not SETMASK)))
(define SMALL-THRESHOLD 10)
(define DEPTH-THRESHOLD (- RADIX QSORT))

(define (sort-block! blk)
  (match-define (struct* block ([data data] [fmap fmap] [mtfv quad] [len blen])) blk)
  (define ftab (make-vector 65537 0))

  (main-sort! data fmap quad ftab blen)

  (for/first ([i (in-range blen)]
              #:when (zero? (vector-ref fmap i)))
    i))

(define (main-sort! data fmap quad ftab blen)
  (define big-done (make-vector 256 #f))
  (define copy-start (make-vector 256 0))
  (define copy-end (make-vector 256 0))

  (init-main-sort! data fmap quad ftab blen)

  (define running-order (compute-running-order ftab))

  (for ([i (in-range 256)])
    (define ss (vector-ref running-order i))
    (main-loop1! ss data fmap quad ftab blen)
    (main-loop2! ss copy-start copy-end big-done data fmap ftab blen)
    (vector-set! big-done ss #t)
    (when (< i 255)
      (main-loop3! ss fmap quad ftab blen))))

  
;; initializes data structures
(define (init-main-sort! data fmap quad ftab blen)
  (define-syntax-rule (fidx i j)
    (bitwise-ior (ash32 j -8) (ash32 (bytes-ref data i) 8)))
    
  ;; set up the 2-byte frequency table
  ;; (we don't have to 0-fill ftab, because it's freshly allocated at this point)
  (for/fold ([j (ash32 (bytes-ref data 0) 8)]) ([i (in-range (sub1 blen) -1 -1)])
    (define j1 (fidx i j))
    (vector-set! quad i 0)
    (vector-inc! ftab j1)
    j1)

  (for ([i (in-range OVERSHOOT)])
    (bytes-set!  data (+ blen i) (bytes-ref data i))
    (vector-set! quad (+ blen i) 0))

  ;; complete the initial radix sort
  (for ([i (in-range 1 65537)])
    (vector-set! ftab i (+ (vector-ref ftab i)
                           (vector-ref ftab (sub1 i)))))

  (for/fold ([s (ash32 (bytes-ref data 0) 8)]) ([i (in-range (sub1 blen) -1 -1)])
    (define s1 (fidx i s))
    (define j (sub1 (vector-ref ftab s1)))
    (vector-set! ftab s1 j)
    (vector-set! fmap j i)
    s1))

;; computes running order; init-main-sort! must be called first,
;; so that ftab holds the first position of every small bucket
(define (compute-running-order ftab)
  (define running-order (build-vector 256 (λ (i) i)))

  (define-syntax-rule (bigfreq b)
    (- (vector-ref ftab (ash32 (add1 b) 8))
       (vector-ref ftab (ash32 b 8))))

  (let loop ([h 364])
    (unless (= h 1)
      (let ([h (quotient h 3)])
        (for ([i (in-range h 256)])
          (define vv (vector-ref running-order i))
          (define j
            (let inner ([j i])
              (define j1 (- j h))
                      
              (cond [(> (bigfreq (vector-ref running-order j1)) (bigfreq vv))
                     (vector-set! running-order j (vector-ref running-order j1))
                     (cond [(<= j1 (sub1 h)) j1]
                           [else (inner j1)])]
                    [else
                     j])))
          (vector-set! running-order j vv))
        (loop h))))

  running-order)

(define (main-loop1! ss data fmap quad ftab blen)
  (for ([j (in-range 256)])
    (unless (= j ss)
      (define sb (+ (ash32 ss 8) j))
      
      (when (zero? (& (vector-ref ftab sb) SETMASK))
        (define lo (& (vector-ref ftab sb) CLEARMASK))
        (define hi (sub1 (& (vector-ref ftab (add1 sb)) CLEARMASK)))
        
        (when (> hi lo)
          (main-qsort3! data fmap quad blen lo hi RADIX)))
      
      (vector-set! ftab sb (bitwise-ior (vector-ref ftab sb) SETMASK)))))

(define (main-loop2! ss copy-start copy-end big-done data fmap ftab blen)
  (for ([j (in-range 256)])
    (define idx (+ (ash32 j 8) ss))
    (vector-set! copy-start j (& (vector-ref ftab idx) CLEARMASK))
    (vector-set! copy-end j (sub1 (& (vector-ref ftab (add1 idx)) CLEARMASK))))

  (let loop ([j (& (vector-ref ftab (ash32 ss 8)) CLEARMASK)])
    (when (< j (vector-ref copy-start ss))
      (let* ([k (sub1 (vector-ref fmap j))]
             [k (if (< k 0) (+ k blen) k)])
        (define c1 (bytes-ref data k))
        (unless (vector-ref big-done c1)
          (define idx (vector-ref copy-start c1))
          (vector-set! copy-start c1 (add1 idx))
          (vector-set! fmap idx k)))
      (loop (add1 j))))

  (let loop ([j (sub1 (& (vector-ref ftab (ash32 (add1 ss) 8)) CLEARMASK))])
    (when (> j (vector-ref copy-end ss))
      (let* ([k (sub1 (vector-ref fmap j))]
             [k (if (< k 0) (+ k blen) k)])
        (define c1 (bytes-ref data k))
        (unless (vector-ref big-done c1)
          (define idx (vector-ref copy-end c1))
          (vector-set! copy-end c1 (sub1 idx))
          (vector-set! fmap idx k)))
      (loop (sub1 j))))

  (for ([j (in-range 256)])
    (define idx (+ (ash32 j 8) ss))
    (define val (vector-ref ftab idx))
    (vector-set! ftab idx (bitwise-ior val SETMASK))))

(define (main-loop3! ss fmap quad ftab blen)
  (define bbstart (& (vector-ref ftab (ash32 ss 8)) CLEARMASK))
  (define bbsize (- (& (vector-ref ftab (ash32 (add1 ss) 8)) CLEARMASK) bbstart))
  (define shifts
    (let loop ([s 0])
      (cond [(> (ash32 bbsize s) 65534) (loop (sub1 s))]
            [else s])))

  (for ([j (in-range (sub1 bbsize) -1 -1)])
    (define a2update (vector-ref fmap (+ bbstart j)))
    (define qval (ash32 j shifts))
    (vector-set! quad a2update qval)
    (when (< a2update OVERSHOOT)
      (vector-set! quad (+ a2update blen) qval))))


(define (main-qsort3! data fmap quad blen lo hi d)
  (cond [(or (< (- hi lo) SMALL-THRESHOLD)
             (> d DEPTH-THRESHOLD))
         (main-simple-sort! data fmap quad blen lo hi d)]
        [else
         (define med
           (med3 (bytes-ref data (+ (vector-ref fmap lo) d))
                 (bytes-ref data (+ (vector-ref fmap hi) d))
                 (bytes-ref data (+ (vector-ref fmap (ash32 (+ lo hi) -1)) d))))

         (define (lo-loop unlo ltlo unhi gthi)
           (cond [(> unlo unhi)
                  (hi-loop unlo ltlo unhi gthi)]
                 [else
                  (define n (- (bytes-ref data (+ (vector-ref fmap unlo) d)) med))
                  (cond [(zero? n)
                         (mswap! fmap unlo ltlo)
                         (lo-loop (add1 unlo) (add1 ltlo) unhi gthi)]
                        [(> n 0)
                         (hi-loop unlo ltlo unhi gthi)]
                        [else
                         (lo-loop (add1 unlo) ltlo unhi gthi)])]))

         (define (hi-loop unlo ltlo unhi gthi)
           (cond [(> unlo unhi)
                  (post-loop unlo ltlo unhi gthi)]
                 [else
                  (define n (- (bytes-ref data (+ (vector-ref fmap unhi) d)) med))
                  (cond [(zero? n)
                         (mswap! fmap unhi gthi)
                         (hi-loop unlo ltlo (sub1 unhi) (sub1 gthi))]
                        [(< n 0)
                         (post-loop unlo ltlo unhi gthi)]
                        [else
                         (hi-loop unlo ltlo (sub1 unhi) gthi)])]))

         (define (post-loop unlo ltlo unhi gthi)
           (cond [(> unlo unhi)
                  (values unlo ltlo unhi gthi)]
                 [else
                  (mswap! fmap unlo unhi)
                  (lo-loop (add1 unlo) ltlo (sub1 unhi) gthi)]))

         (define-values (unlo ltlo unhi gthi) (lo-loop lo lo hi hi))

         (cond [(< gthi ltlo)
                (main-qsort3! data fmap quad blen lo hi (add1 d))]
               [else
                (let ([n (min (- ltlo lo) (- unlo ltlo))]
                      [m (min (- hi gthi) (- gthi unhi))])
                  (mvswap! fmap lo (- unlo n) n)
                  (mvswap! fmap unlo (+ hi (- m) 1) m))

                (let ([n (+ lo unlo (- ltlo) -1)]
                      [m (+ hi (- (- gthi unhi)) 1)])
                  ;; We're ordering the recursive calls from smallest to largest range.
                  (define sorted-args
                    (sort (list (list lo n d)
                                (list m hi d)
                                (list (add1 n) (sub1 m) (add1 d)))
                          (λ (x y)
                            (< (- (second x) (first x))
                               (- (second y) (first y))))))

                  (for-each (λ (xs)
                              (match-define (list lo hi d) xs)
                              (main-qsort3! data fmap quad blen lo hi d))
                            sorted-args))])]))

                  
(define (main-simple-sort! data fmap quad blen lo hi d)
  (define bign (add1 (- hi lo)))

  (when (>= bign 2)
    (define hp0
      (let loop ([hp 0])
        (cond [(< (vector-ref incs hp) bign) (loop (add1 hp))]
              [else (sub1 hp)])))

    (for ([hp (in-range hp0 -1 -1)])
      (define h (vector-ref incs hp))

      (for ([i (in-range (+ lo h) (add1 hi))])
        (define v (vector-ref fmap i))
        (define j
          (let loop ([j i])
            (define j1 (- j h))
            (cond [(main-gtu? (+ (vector-ref fmap j1) d) (+ v d) data quad blen)
                   (vector-set! fmap j (vector-ref fmap j1))
                   (cond [(<= j1 (sub1 (+ lo h))) j1]
                         [else (loop j1)])]
                  [else
                   j])))

        (vector-set! fmap j v)))))

(define (main-gtu? i1 i2 data quad blen)
  (let loop ([n 0] [i1 i1] [i2 i2])
    (cond [(< n 12)
           (define c1 (bytes-ref data i1))
           (define c2 (bytes-ref data i2))

           (cond [(not (= c1 c2)) (> c1 c2)]
                 [else (loop (add1 n) (add1 i1) (add1 i2))])]
          [else
           (let k-loop ([k (+ blen 8)] [i1 i1] [i2 i2])
             (cond [(>= k 0)
                    (let inner ([n 0] [i1 i1] [i2 i2])
                      (cond [(< n 8)
                             (define c1 (bytes-ref data i1))
                             (define c2 (bytes-ref data i2))

                             (cond [(not (= c1 c2))
                                    (> c1 c2)]
                                   [else
                                    (define s1 (vector-ref quad i1))
                                    (define s2 (vector-ref quad i2))

                                    (cond [(not (= s1 s2)) (> s1 s2)]
                                          [else (inner (add1 n) (add1 i1) (add1 i2))])])]
                            [else
                             (k-loop (- k 8)
                                     (if (>= i1 blen) (- i1 blen) i1)
                                     (if (>= i2 blen) (- i2 blen) i2))]))]
                   [else
                    #f]))])))

(define (med3 a b c)
  (if (< a b)
      (if (< b c)
          b
          (if (< a c) c a))
      (if (> b c)
          b
          (if (> a c) c a))))

(define (mswap! vec x y)
  (define t (vector-ref vec x))
  (vector-set! vec x (vector-ref vec y))
  (vector-set! vec y t))

(define (mvswap! vec x y n)
  (for ([i (in-range n)])
    (mswap! vec (+ x i) (+ y i))))

(define incs '#(1 4 13 40 121 364 1093 3280 9841 29524 88573 265720 797161 2391484))
