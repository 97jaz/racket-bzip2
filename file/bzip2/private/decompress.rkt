#lang racket/base

(require racket/match
         "structs.rkt"
         "util.rkt"
         "crc.rkt"
         "huffman.rkt")

(provide make-decompressor
         decompressor-read-byte)

(define (make-decompressor in)
  (decompressor in 0 0 initial-state #f 0 #f #f #f #f #f #f #f #f INITIAL-CRC))

(define (decompressor-read-byte dc)
  (define state (decompressor-state dc))
  (state dc))

(define (initial-state dc)
  (set-decompressor-size100k! dc (read-stream-header dc))
  (read-block-or-eos dc))

(define (read-block-or-eos dc)
  (define magic (for/vector #:length 6 ([i (in-range 6)]) (get-bits dc 8)))
  (cond [(equal? magic '#(#x31 #x41 #x59 #x26 #x53 #x59)) (read-block dc)]
        [(equal? magic '#(#x17 #x72 #x45 #x38 #x50 #x90)) (read-eos dc)]
        [else (err "bad magic number at start of block")]))

(define (read-eos dc)
  (define expected-crc (get-bits dc 32))
  (define combined-crc (decompressor-crc dc))
  (unless (= expected-crc combined-crc)
    (format "expected file CRC [~a], got [~a] instead" expected-crc combined-crc))
  (set-decompressor-state! dc eof-state)
  eof)

(define (read-block dc)
  (define block-crc (get-bits dc 32))
  (define randomized (not (zero? (get-bit dc))))
  (define orig-ptr (get-bits dc 24))

  (when (> orig-ptr (+ 10 (* 100000 (decompressor-size100k dc))))
    (err "block starts at non-existent address"))

  (when randomized (err "randomized blocks not supported"))
  
  ;; mapping table
  (define in-use16 (for/vector #:length 16 ([i (in-range 16)]) (not (zero? (get-bit dc)))))
  (define in-use (make-vector 256 #f))
  (for* ([i (in-range 16)]
         #:when (vector-ref in-use16 i)
         [j (in-range 16)])
    (vector-set! in-use (+ (* i 16) j) (not (zero? (get-bit dc)))))

  (define seq-to-unseq (make-bytes 256 0))
  (define n-in-use
    (for/fold ([n 0]) ([i (in-range 256)])
      (cond [(vector-ref in-use i) (bytes-set! seq-to-unseq n i)
                                   (add1 n)]
            [else n])))
  (define alpha-size (+ n-in-use 2))
  (when (zero? n-in-use) (err "block maps no data"))

  ;; selectors
  (define n-groups (get-bits dc 3))
  (define n-selectors (get-bits dc 15))
  (define selector-mtf (make-bytes n-selectors 0))
  (when (or (< n-groups 2) (> n-groups 6))
    (err (format "illegal number of groups [~a]" n-groups)))

  (for ([i (in-range n-selectors)])
    (let loop ([j 0])
      (cond [(zero? (get-bit dc)) (bytes-set! selector-mtf i j)]                                  
            [else (let ([j (add1 j)])
                    (when (>= j n-groups) (err "bad selector table"))
                    (loop j))])))

  ;; MTF values for selectors
  (define pos (make-bytes n-groups))
  (define selector (make-bytes n-selectors 0))
  (for ([i (in-range n-groups)]) (bytes-set! pos i i))
  (for ([i (in-range n-selectors)])
    (define v (bytes-ref selector-mtf i))
    (define tmp (bytes-ref pos v))
    (let loop ([v v])
      (cond [(> v 0) (bytes-set! pos v (bytes-ref pos (sub1 v)))
                     (loop (sub1 v))]
            [else (bytes-set! pos 0 tmp)
                  (bytes-set! selector i tmp)])))

  ;; coding tables
  (define len (build-vector n-groups (λ (i) (make-bytes alpha-size 0))))
  (for* ([t    (in-range n-groups)]
         [lent (in-value (vector-ref len t))])
    (for/fold ([curr (get-bits dc 5)]) ([i (in-range alpha-size)])
      (let loop ([curr curr])
        (when (> curr 20) (err "bad coding table length"))
        (cond [(zero? (get-bit dc)) (bytes-set! lent i curr)
                                    curr]
              [(zero? (get-bit dc)) (loop (add1 curr))]
              [else                 (loop (sub1 curr))]))))

  ;; Huffman decoding tables
  (define limit (build-vector n-groups (λ (i) (make-vector alpha-size 0))))
  (define base (build-vector n-groups (λ (i) (make-vector alpha-size 0))))
  (define perm (build-vector n-groups (λ (i) (make-vector alpha-size 0))))
  (define minlens (make-vector n-groups))

  (for ([t (in-range n-groups)])
    (define lent (vector-ref len t))
    (define-values (minlen maxlen)
      (for/fold ([minlen 32] [maxlen 0]) ([i (in-range alpha-size)])
        (define lenti (bytes-ref lent i))
        (values (if (< lenti minlen) lenti minlen)
                (if (> lenti maxlen) lenti maxlen))))
    (create-decode-tables! (vector-ref limit t) (vector-ref base t) (vector-ref perm t) lent minlen maxlen alpha-size)
    (vector-set! minlens t minlen))

  ;; MTF values (this part is awfully imperative)
  (define EOB (add1 n-in-use))
  (define n-block-max (* 100000 (decompressor-size100k dc)))
  (define group-no -1)
  (define group-pos 0)
  (define unzftab (make-vector 256 0))
  (define mtfa (make-bytes MTFA-SIZE 0))
  (define mtfbase (make-vector (quotient 256 MTFL-SIZE)))
  (define tt (make-vector n-block-max 0))

  ;; helper
  (define gsel #f)
  (define gminlen #f)
  (define glimit #f)
  (define gperm #f)
  (define gbase #f)
  
  (define (get-mtf-val)
    (when (zero? group-pos)
      (set! group-no (add1 group-no))
      (when (>= group-no n-selectors) (err "bad group in MTF values"))
      (set! group-pos G-SIZE)
      (set! gsel (bytes-ref selector group-no))
      (set! gminlen (vector-ref minlens gsel))
      (set! glimit (vector-ref limit gsel))
      (set! gperm (vector-ref perm gsel))
      (set! gbase (vector-ref base gsel)))
    (set! group-pos (sub1 group-pos))
    (define-values (zn zvec)
      (let loop ([zn gminlen] [zvec (get-bits dc gminlen)])
        (when (> zn 20) (err "huffman string too long"))
        (cond [(<= zvec (vector-ref glimit zn)) (values zn zvec)]
              [else (loop (add1 zn) (bitwise-ior (ash32 zvec 1) (get-bit dc)))])))
    (define bl (- zvec (vector-ref gbase zn)))
    (when (or (< bl 0) (>= bl MAX-ALPHA-SIZE))
      (err "bad data in MTF values"))
    (vector-ref gperm bl))
      

  ;; -- init
  (let ([kk (sub1 MTFA-SIZE)])
    (for ([ii (in-range (sub1 (quotient 256 MTFL-SIZE)) -1 -1)])
      (for ([jj (in-range (sub1 MTFL-SIZE) -1 -1)])
        (bytes-set! mtfa kk (+ (* ii MTFL-SIZE) jj))
        (set! kk (sub1 kk)))
      (vector-set! mtfbase ii (add1 kk))))

  ;; -- main loop
  (define n-block 0)
  (define next-sym (get-mtf-val))
  (let loop ()
    (unless (= next-sym EOB)
      (cond [(or (= next-sym RUNA) (= next-sym RUNB))
             (define es -1)
             (define N 1)
             (let inner ()
               (when (>= N (* 2 1024 1024)) (err "encoded value too long"))
               (cond [(= next-sym RUNA) (set! es (+ es N))]
                     [(= next-sym RUNB) (set! es (+ es (* 2 N)))])
               (set! N (* N 2))
               (set! next-sym (get-mtf-val))
               (when (or (= next-sym RUNA) (= next-sym RUNB)) (inner)))
             
             (set! es (add1 es))
             (define uc (bytes-ref seq-to-unseq (bytes-ref mtfa (vector-ref mtfbase 0))))
             (vector-set! unzftab uc (+ (vector-ref unzftab uc) es))

             (let inner ()
               (when (> es 0)
                 (when (>= n-block n-block-max) (err "block is too long"))
                 (vector-set! tt n-block uc)
                 (set! n-block (add1 n-block))
                 (set! es (sub1 es))
                 (inner)))

             (loop)]

            [else
             (when (>= n-block n-block-max) (err "block is too long"))
             (define nn (sub1 next-sym))
             (define lno (quotient nn MTFL-SIZE))
             (define off (remainder nn MTFL-SIZE))
             (define pp (+ (vector-ref mtfbase lno) off))
             (define uc (bytes-ref mtfa pp))
             (let inner ()
               (when (> pp (vector-ref mtfbase lno))
                 (bytes-set! mtfa pp (bytes-ref mtfa (sub1 pp)))
                 (set! pp (sub1 pp))
                 (inner)))
             (vector-inc! mtfbase lno)
             (let inner ()
               (when (> lno 0)
                 (vector-dec! mtfbase lno)
                 (bytes-set! mtfa
                             (vector-ref mtfbase lno)
                             (bytes-ref mtfa (+ (vector-ref mtfbase (sub1 lno)) MTFL-SIZE -1)))
                 (set! lno (sub1 lno))
                 (inner)))
             (vector-dec! mtfbase 0)
             (bytes-set! mtfa (vector-ref mtfbase 0) uc)
             (when (zero? (vector-ref mtfbase 0))
               (define kk (sub1 MTFA-SIZE))
               (for ([ii (in-range (sub1 (quotient 256 MTFL-SIZE)) -1 -1)])
                 (for ([jj (in-range (sub1 MTFL-SIZE) -1 -1)])
                   (bytes-set! mtfa kk (bytes-ref mtfa (+ (vector-ref mtfbase ii) jj)))
                   (set! kk (sub1 kk)))
                 (vector-set! mtfbase ii (add1 kk))))

             (vector-inc! unzftab (bytes-ref seq-to-unseq uc))
             (vector-set! tt n-block (bytes-ref seq-to-unseq uc))
             (set! n-block (add1 n-block))
             (set! next-sym (get-mtf-val))
             (loop)])))

  ;; validate orig-ptr
  (when (or (< orig-ptr 0) (> orig-ptr n-block))
    (err "illegal value for origPtr"))

  ;; check that unzftab values are in-range
  (for ([i (in-range 256)])
    (when (or (< (vector-ref unzftab i) 0) (> (vector-ref unzftab i) n-block))
      (err "unzftab entry is out-of-range")))

  ;; generate cftab
  (define cftab (make-vector 257))
  (vector-set! cftab 0 0)
  (for ([i (in-range 1 257)]) (vector-set! cftab i (vector-ref unzftab (sub1 i))))
  (for ([i (in-range 1 257)]) (vector-set! cftab i (+ (vector-ref cftab i) (vector-ref cftab (sub1 i)))))
  ;; check that cftab entries are in-range
  (for ([i (in-range 257)])
    (when (or (< (vector-ref cftab i) 0) (> (vector-ref cftab i) n-block))
      (err "cftab entry is out-of-range")))
  ;; check that cftab entries are non-descending
  (for ([i (in-range 1 257)])
    (when (> (vector-ref cftab (sub1 i)) (vector-ref cftab i))
      (err "cftab entries out of order")))

  ;; compute the T^(-1) vector
  (for ([i (in-range n-block)])
    (define uc (& #xff (vector-ref tt i)))
    (define val (vector-ref tt (vector-ref cftab uc)))
    (vector-set! tt (vector-ref cftab uc) (bitwise-ior val (ash32 i 8)))
    (vector-inc! cftab uc))

  (set-decompressor-tt! dc tt)
  (set-decompressor-tpos! dc (ash32 (vector-ref tt orig-ptr) -8))
  (set-decompressor-run-len! dc 0)
  (set-decompressor-run-byte! dc #f)
  (set-decompressor-state! dc output-state)
  (set-decompressor-nblock! dc n-block)
  (set-decompressor-nblock-used! dc 1)
  (let ([b (decompressor-next-byte! dc)])
    (set-decompressor-repeat-byte! dc b)
    (set-decompressor-repeat-len! dc 1)
    b))

(define (decompressor-next-byte! dc)
  (match-define (struct* decompressor ([tpos tpos] [size100k size100k] [tt tt])) dc)
  (when (>= tpos (* 100000 size100k)) (err "position of data is out-of-range"))
  (let ([tpos (vector-ref tt tpos)])
    (set-decompressor-tpos! dc (ash32 tpos -8))
    (& tpos #xff)))

(define (output-state dc)
  (match-define
    (struct* decompressor ([run-len run-len]
                           [run-byte run-byte]
                           [repeat-len repeat-len]
                           [repeat-byte repeat-byte]
                           [nblock nblock]
                           [nblock-used nblock-used]))
    dc)

  (cond [(> run-len 0)
         (define new-run-len (sub1 run-len))
         (set-decompressor-run-len! dc new-run-len)
         run-byte]
        [(>= nblock-used nblock)
         (read-block-or-eos dc)]
        [else
         (define b (decompressor-next-byte! dc))
         (define new-nblock-used (add1 nblock-used))
         (set-decompressor-nblock-used! dc new-nblock-used)
         
         (cond [(equal? repeat-byte b)
                (define new-repeat-len (add1 repeat-len))
                (cond [(= new-repeat-len 4)
                       ;; start a new run unless we're done with the block
                       (when (< new-nblock-used nblock)
                         (set-decompressor-run-len! dc (decompressor-next-byte! dc))
                         (set-decompressor-run-byte! dc b)
                         (set-decompressor-nblock-used! dc (add1 new-nblock-used))
                         (set-decompressor-repeat-len! dc 0)
                         (set-decompressor-repeat-byte! dc #f))]
                      [else                       
                       (set-decompressor-repeat-len! dc (add1 repeat-len))])]
               [else
                (set-decompressor-repeat-byte! dc b)
                (set-decompressor-repeat-len! dc 1)])
         b]))
                

(define (eof-state dc)
  eof)

(define (read-stream-header dc)
  (expect-bytes dc #"BZh" (λ () (err "bad magic number (not a bzip2 file)")))
  (define n (- (get-byte dc) (char->integer #\0)))
  (cond [(and (>= n 1) (<= n 9)) n]
        [else (err "illegal block size in header")]))

(define (expect-bytes dc bs err-thunk)
  (for ([b (in-bytes bs)])
    (unless (= b (get-byte dc)) (err-thunk))))

(define (get-byte dc) (get-bits dc 8))
(define (get-bit dc)  (get-bits dc 1))

(define (get-bits dc n)
  (match-define (struct* decompressor ([in in] [live live] [buffer buffer])) dc)
  
  (let loop ([live live] [buffer buffer])
    (cond [(>= live n)
           (define v (& (ash32 buffer (- (- live n))) (sub1 (ash32 1 n))))
           (set-decompressor-live! dc (- live n))
           (set-decompressor-buffer! dc buffer)
           v]
          [else
           (define b (read-byte in))
           (cond [(eof-object? b) (err "unexpected EOF")]
                 [else (loop (+ live 8) (bitwise-ior (ash32 buffer 8) b))])])))

(define (err str)
  (raise (exn:fail:bzip2 str (current-continuation-marks))))