#lang racket/base

(require racket/match)

(require "structs.rkt"
         "util.rkt"
         "crc.rkt"
         "io.rkt"
         "sort.rkt"
         "huffman.rkt")

(provide make-compressor
         compressor-write-byte
         compressor-finish)

(define (make-compressor out block-size100k)
  (define n (* block-size100k 100000))
  
  (compressor
   out
   0
   0
   (run #f 0)
   (block 0
          (make-bytes (+ n OVERSHOOT) 0)
          (make-vector n 0)
          (make-vector (+ n OVERSHOOT) 0)
          (make-vector 256 #f)
          0
          INITIAL-CRC
          (- n 19)
          block-size100k)
   0))

(define (compressor-write-byte cp b)
  (match-define (struct* block ([len len] [maxlen maxlen])) (compressor-block cp))
  
  (add-byte! cp b)
  (when (>= len maxlen)
    (compress-block cp)
    (init-block cp)))

(define (compressor-write-bytes cp bs [start 0] [end (bytes-length bs)])
  (for ([b (in-bytes bs start end)])
    (compressor-write-byte cp b)))

(define (init-block cp)
  (define blk (compressor-block cp))
  (set-block-idx! blk (add1 (block-idx blk)))
  (set-block-crc! blk INITIAL-CRC)
  (set-block-len! blk 0)
  (vector-fill! (block-in-use blk) #f))

(define (compress-block cp)
  (match-define
    (struct* compressor ([crc combined-crc]
                         [block (and blk
                                     (struct* block ([idx idx]
                                                     [len blen]
                                                     [crc blk-crc])))]))
    cp)

  (define orig-ptr
    (when (> blen 0)
      (define final-blk-crc (final-crc blk-crc))
      (set-block-crc! blk final-blk-crc)
      (set-compressor-crc! cp (combine-crc combined-crc final-blk-crc))
      (sort-block! blk)))
    
  (when (zero? idx)
    (write-stream-header! cp))

  (when (> blen 0)
    (write-block! cp orig-ptr)))

(define (write-stream-header! cp)
  (define blk (compressor-block cp))
  
  (put-bytes! cp #"BZh")
  (put-byte! cp (+ (char->integer #\0) (block-size100k blk))))

(define (write-block! cp orig-ptr)
  ;; magic
  (put-bytes! cp (bytes #x31 #x41 #x59 #x26 #x53 #x59))
  ;; crc
  (put-int! cp (block-crc (compressor-block cp)))
  ;; deprecated randomization
  (put! cp 1 0)
  ;; compressed block data
  (put! cp 24 orig-ptr)
  (define-values (n-in-use n-mtf mtf-freq) (generate-mtf! cp))
  (send-mtf! cp n-in-use n-mtf mtf-freq)
  (values n-in-use n-mtf))

(define (compressor-finish cp)
  (when (> (run-len (compressor-run cp)) 0)
    (block-add-run! cp))
  (compress-block cp)
  ;; end-of-stream magic
  (put-bytes! cp (bytes #x17 #x72 #x45 #x38 #x50 #x90))
  (put-int! cp (compressor-crc cp))
  (drain-buffer! cp))

(define (add-byte! cp b)
  (match-define (struct* compressor ([block blk] [run r])) cp)
  (match r
    [(run (and b0 (not (== b))) 1)
     (block-add-byte! blk b0)
     (set-run-byte! r b)]
    [(or (run (and b0 (not (== b))) n)
         (run b0 (and n 255)))
     (when b0
       (block-add-run! cp))
     (set-run-byte! r b)
     (set-run-len! r 1)]
    [(run _ n)
     (set-run-len! r (add1 n))]))

(define (block-add-byte! blk b)
  (match-define (struct* block ([data data] [len len] [in-use in-use] [crc crc])) blk)

  (vector-set! in-use b #t)
  (bytes-set! data len b)
  (set-block-len! blk (add1 len))
  (set-block-crc! blk (update-crc crc b)))

(define (block-add-run! cp)
  (match-define (and blk (struct* block ([data data] [len blen] [in-use in-use] [crc crc]))) (compressor-block cp))
  (match-define (and r (struct* run ([byte b] [len rlen]))) (compressor-run cp))

  (define new-crc
    (for/fold ([crc crc]) ([i (in-range (min rlen 4))])
      (bytes-set! data (+ i blen) b)
      (update-crc crc b)))

  (vector-set! in-use b #t)

  (cond [(< rlen 4)
         (set-block-len! blk (+ blen rlen))
         (set-block-crc! blk new-crc)]
        [else
         (define rem (- rlen 4))
         (vector-set! in-use rem #t)
         (bytes-set! data (+ blen 4) rem)
         (set-block-len! blk (+ blen 5))
         (set-block-crc! blk (for/fold ([crc new-crc]) ([i (in-range rem)]) (update-crc crc b)))]))

(define (generate-mtf! cp)
  (define blk (compressor-block cp))
  (match-define
    (struct* block ([in-use in-use] [len blen] [data data] [fmap fmap] [mtfv mtfv]))
    blk)

  (define unseq (make-bytes 256 0))
  (define n-in-use
    (for/fold ([n 0]) ([i (in-range 256)])
      (cond [(vector-ref in-use i)
             (bytes-set! unseq i n)
             (add1 n)]
            [else n])))

  (define EOB (add1 n-in-use))
  (define yy (make-bytes 256 0))
  (define mtf-freq (make-vector MAX-ALPHA-SIZE 0))

  (for ([i (in-range n-in-use)]) (bytes-set! yy i i))
  
  (define-values (wr zpend)
    (for/fold ([wr 0] [zpend 0]) ([i (in-range blen)])
      (define j
        (let ([j (sub1 (vector-ref fmap i))])
          (if (< j 0) (+ j blen) j)))
      (define lli (bytes-ref unseq (bytes-ref data j)))

      (cond [(= (bytes-ref yy 0) lli)
             (values wr (add1 zpend))]
            [else
             (let ([wr (reduce-zpend! wr zpend mtfv mtf-freq)])
               (define tmp (bytes-ref yy 1))
               (bytes-set! yy 1 (bytes-ref yy 0))
               (let loop ([tmp tmp] [j 1])
                 (cond [(not (= lli tmp))
                        (define j1 (add1 j))
                        (define tmp1 (bytes-ref yy j1))
                        (bytes-set! yy j1 tmp)
                        (loop tmp1 j1)]
                       [else
                        (bytes-set! yy 0 tmp)
                        (vector-set! mtfv wr (add1 j))
                        (vector-inc! mtf-freq (add1 j))
                        (values (add1 wr) 0)])))])))

  (define wr1 (reduce-zpend! wr zpend mtfv mtf-freq))
  (vector-set! mtfv wr1 EOB)
  (vector-inc! mtf-freq EOB)

  (values n-in-use (add1 wr1) mtf-freq))

(define (reduce-zpend! wr zpend mtfv mtf-freq)
  (cond [(> zpend 0)
         (let loop ([wr wr] [zpend (sub1 zpend)])
           (define run (if (zero? (& zpend 1)) RUNA RUNB))
           (vector-set! mtfv wr run)
           (vector-inc! mtf-freq run)
           (cond [(< zpend 2) (add1 wr)]
                 [else (loop (add1 wr) (quotient (- zpend 2) 2))]))]
        [else
         wr]))

(define (send-mtf! cp n-in-use n-mtf mtf-freq)
  (define blk (compressor-block cp))
  (define mtfv (block-mtfv blk))
  (define alpha-size (+ n-in-use 2))
  (define n-groups (n-mtf->n-groups n-mtf))
  (define group-len (make-group-len n-groups n-mtf alpha-size mtf-freq))
  (define selector (make-bytes MAX-SELECTORS 0))

  (define n-selectors
    (for/last ([i (in-range ITERS)])
      (improve-lengths! n-groups n-mtf alpha-size group-len mtfv selector)))

  (define selector-mtf (compute-selector-mtf n-groups n-selectors selector))
  (define code (compute-codes n-groups alpha-size group-len))

  (transmit-mapping-table! cp)
  (transmit-selectors! cp n-groups n-selectors selector-mtf)
  (transmit-coding-tables! cp n-groups alpha-size group-len)
  (transmit-data! cp n-mtf selector mtfv group-len code))

(define (make-group-len n-groups n-mtf alpha-size mtf-freq)
  (define result (build-vector n-groups (λ (i) (make-bytes alpha-size GREATER-ICOST))))
  
  (let loop ([n-part n-groups] [remf n-mtf] [gs 0])
    (cond [(> n-part 0)
           (define tfreq (quotient remf n-part))
           (let*-values ([(ge afreq)
                          (let inner ([ge (sub1 gs)] [afreq 0])
                            (cond [(and (< afreq tfreq)
                                        (< ge (sub1 alpha-size)))
                                   (inner (add1 ge)
                                          (+ afreq (vector-ref mtf-freq (add1 ge))))]
                                  [else (values ge afreq)]))]
                         
                         [(ge afreq)
                          (cond [(and (> ge gs)
                                      (not (= n-part n-groups))
                                      (not (= n-part 1))
                                      (= (remainder (- n-groups n-part) 2) 1))
                                 (values (sub1 ge)
                                         (- afreq (vector-ref mtf-freq ge)))]
                                [else (values ge afreq)])])

             (for ([v (in-range alpha-size)])
               (if (and (>= v gs) (<= v ge))
                   (bytes-set! (vector-ref result (sub1 n-part)) v LESSER-ICOST)
                   (bytes-set! (vector-ref result (sub1 n-part)) v GREATER-ICOST)))
             
             (loop (sub1 n-part) (- remf afreq) (add1 ge)))]
          [else
           result])))

(define (improve-lengths! n-groups n-mtf alpha-size group-len mtfv selector)
  (define fave (make-vector n-groups 0))
  (define cost (make-vector n-groups 0))
  (define rfreq (build-vector n-groups (λ (i) (make-vector alpha-size 0))))

  ;; set group start and end marks
  (define-values (n-selectors totc)
    (let loop ([n-selectors 0] [totc 0] [gs 0])
      (cond [(>= gs n-mtf)
             (values n-selectors totc)]
            [else
             (define ge (min (sub1 (+ gs G-SIZE)) (sub1 n-mtf)))

             (vector-fill! cost 0)

             ;; calculate group costs
             (for ([i (in-range gs (add1 ge))])
               (define icv (vector-ref mtfv i))
               (for ([t (in-range n-groups)])
                 (vector-set! cost t (+ (vector-ref cost t)
                                        (bytes-ref (vector-ref group-len t) icv)))))

             ;; find the best coding table per group
             (define-values (bc bt)
               (for/fold ([bc 999999999] [bt -1]) ([t (in-range n-groups)])
                 (if (< (vector-ref cost t) bc)
                     (values (vector-ref cost t) t)
                     (values bc bt))))

             (vector-inc! fave bt)
             (bytes-set! selector n-selectors bt)

             ;; increment symbol frequencies for the selected table
             (for ([i (in-range gs (add1 ge))])
               (vector-inc! (vector-ref rfreq bt)
                            (vector-ref mtfv i)))

             (loop (add1 n-selectors) (+ totc bc) (add1 ge))])))

  ;; recompute the tables based on the new frequencies
  (for ([t (in-range n-groups)])
    (make-code-lengths! (vector-ref group-len t)
                        (vector-ref rfreq t)
                        alpha-size
                        17))
  n-selectors)

(define (compute-selector-mtf n-groups n-selectors selector)
  (define pos (make-bytes n-groups))
  (define selector-mtf (make-bytes n-selectors))
  
  (for ([i (in-range n-groups)]) (bytes-set! pos i i))

  (for ([i (in-range n-selectors)])
    (define lli (bytes-ref selector i))
    (let loop ([j 0] [tmp (bytes-ref pos 0)])
      (cond [(not (= lli tmp))
             (let ([j (add1 j)])
               (define t (bytes-ref pos j))
               (bytes-set! pos j tmp)
               (loop j t))]
            [else
             (bytes-set! pos 0 tmp)
             (bytes-set! selector-mtf i j)])))

  selector-mtf)

(define (compute-codes n-groups alpha-size len)
  (define code (build-vector n-groups (λ (i) (make-vector alpha-size 0))))

  (for ([t (in-range n-groups)])
    (define lent (vector-ref len t))
    (define-values (minlen maxlen)
      (for/fold ([minlen 32] [maxlen 0]) ([i (in-range alpha-size)])
        (values (min (bytes-ref lent i) minlen)
                (max (bytes-ref lent i) maxlen))))
    (assign-codes! (vector-ref code t) lent minlen maxlen alpha-size))

  code)

(define (transmit-mapping-table! cp)
  (define in-use (block-in-use (compressor-block cp)))
  (define in-use16 (make-vector 16 #f))

  (for ([i (in-range 16)])
    (for ([j (in-range 16)])
      (when (vector-ref in-use (+ (* i 16) j))
        (vector-set! in-use16 i #t))))

  (for ([i (in-range 16)])
    (cond [(vector-ref in-use16 i) (put! cp 1 1)]
          [else                    (put! cp 1 0)]))

  (for ([i (in-range 16)])
    (when (vector-ref in-use16 i)
      (for ([j (in-range 16)])
        (cond [(vector-ref in-use (+ (* i 16) j)) (put! cp 1 1)]
              [else                               (put! cp 1 0)])))))

(define (transmit-selectors! cp n-groups n-selectors selector-mtf)
  (put! cp 3 n-groups)
  (put! cp 15 n-selectors)

  (for ([i (in-range n-selectors)])
    (for ([j (in-range (bytes-ref selector-mtf i))])
      (put! cp 1 1))
    (put! cp 1 0)))

(define (transmit-coding-tables! cp n-groups alpha-size len)
  (for ([t (in-range n-groups)])
    (define lent (vector-ref len t))
    (define curr (bytes-ref lent 0))
    (put! cp 5 curr)

    (for/fold ([curr curr]) ([i (in-range alpha-size)])
      (let lo ([curr curr])
        (cond [(< curr (bytes-ref lent i))
               (put! cp 2 2)
               (lo (add1 curr))]
              [else (let hi ([curr curr])
                      (cond [(> curr (bytes-ref lent i))
                             (put! cp 2 3)
                             (hi (sub1 curr))]
                            [else
                             (put! cp 1 0)
                             curr]))])))))

(define (transmit-data! cp n-mtf selector mtfv len code)
  (let loop ([gs 0] [sel-ctr 0])
    (when (< gs n-mtf)
      (define ge (min (sub1 (+ gs G-SIZE)) (sub1 n-mtf)))

      (for ([i (in-range gs (add1 ge))])
        (define sel (bytes-ref selector sel-ctr))
        (define mtfvi (vector-ref mtfv i))
        (put! cp
              (bytes-ref (vector-ref len sel) mtfvi)
              (vector-ref (vector-ref code sel) mtfvi)))

      (loop (add1 ge) (add1 sel-ctr)))))

              

(define (n-mtf->n-groups n)
  (cond [(< n 200)  2]
        [(< n 600)  3]
        [(< n 1200) 4]
        [(< n 2400) 5]
        [else       6]))
