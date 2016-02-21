#lang racket/base

(require "structs.rkt"
         "util.rkt")

(provide (all-defined-out))

(define INITIAL-CRC #xffffffff)

(define (update-crc crc b)
  (& #xffffffff
     (bitwise-xor (ash32 crc 8)
                  (vector-ref CRC32-TBL (bitwise-xor (ash32 crc -24) b)))))

(define (final-crc crc)
  (& #xffffffff (bitwise-not crc)))

(define (combine-crc combined block)
  (& #xffffffff
     (bitwise-xor block
                  (bitwise-ior (ash32 combined 1)
                               (ash32 combined -31)))))

(define CRC32-TBL
  '#(#x00000000 #x04c11db7 #x09823b6e #x0d4326d9
     #x130476dc #x17c56b6b #x1a864db2 #x1e475005
     #x2608edb8 #x22c9f00f #x2f8ad6d6 #x2b4bcb61
     #x350c9b64 #x31cd86d3 #x3c8ea00a #x384fbdbd
     #x4c11db70 #x48d0c6c7 #x4593e01e #x4152fda9
     #x5f15adac #x5bd4b01b #x569796c2 #x52568b75
     #x6a1936c8 #x6ed82b7f #x639b0da6 #x675a1011
     #x791d4014 #x7ddc5da3 #x709f7b7a #x745e66cd
     #x9823b6e0 #x9ce2ab57 #x91a18d8e #x95609039
     #x8b27c03c #x8fe6dd8b #x82a5fb52 #x8664e6e5
     #xbe2b5b58 #xbaea46ef #xb7a96036 #xb3687d81
     #xad2f2d84 #xa9ee3033 #xa4ad16ea #xa06c0b5d
     #xd4326d90 #xd0f37027 #xddb056fe #xd9714b49
     #xc7361b4c #xc3f706fb #xceb42022 #xca753d95
     #xf23a8028 #xf6fb9d9f #xfbb8bb46 #xff79a6f1
     #xe13ef6f4 #xe5ffeb43 #xe8bccd9a #xec7dd02d
     #x34867077 #x30476dc0 #x3d044b19 #x39c556ae
     #x278206ab #x23431b1c #x2e003dc5 #x2ac12072
     #x128e9dcf #x164f8078 #x1b0ca6a1 #x1fcdbb16
     #x018aeb13 #x054bf6a4 #x0808d07d #x0cc9cdca
     #x7897ab07 #x7c56b6b0 #x71159069 #x75d48dde
     #x6b93dddb #x6f52c06c #x6211e6b5 #x66d0fb02
     #x5e9f46bf #x5a5e5b08 #x571d7dd1 #x53dc6066
     #x4d9b3063 #x495a2dd4 #x44190b0d #x40d816ba
     #xaca5c697 #xa864db20 #xa527fdf9 #xa1e6e04e
     #xbfa1b04b #xbb60adfc #xb6238b25 #xb2e29692
     #x8aad2b2f #x8e6c3698 #x832f1041 #x87ee0df6
     #x99a95df3 #x9d684044 #x902b669d #x94ea7b2a
     #xe0b41de7 #xe4750050 #xe9362689 #xedf73b3e
     #xf3b06b3b #xf771768c #xfa325055 #xfef34de2
     #xc6bcf05f #xc27dede8 #xcf3ecb31 #xcbffd686
     #xd5b88683 #xd1799b34 #xdc3abded #xd8fba05a
     #x690ce0ee #x6dcdfd59 #x608edb80 #x644fc637
     #x7a089632 #x7ec98b85 #x738aad5c #x774bb0eb
     #x4f040d56 #x4bc510e1 #x46863638 #x42472b8f
     #x5c007b8a #x58c1663d #x558240e4 #x51435d53
     #x251d3b9e #x21dc2629 #x2c9f00f0 #x285e1d47
     #x36194d42 #x32d850f5 #x3f9b762c #x3b5a6b9b
     #x0315d626 #x07d4cb91 #x0a97ed48 #x0e56f0ff
     #x1011a0fa #x14d0bd4d #x19939b94 #x1d528623
     #xf12f560e #xf5ee4bb9 #xf8ad6d60 #xfc6c70d7
     #xe22b20d2 #xe6ea3d65 #xeba91bbc #xef68060b
     #xd727bbb6 #xd3e6a601 #xdea580d8 #xda649d6f
     #xc423cd6a #xc0e2d0dd #xcda1f604 #xc960ebb3
     #xbd3e8d7e #xb9ff90c9 #xb4bcb610 #xb07daba7
     #xae3afba2 #xaafbe615 #xa7b8c0cc #xa379dd7b
     #x9b3660c6 #x9ff77d71 #x92b45ba8 #x9675461f
     #x8832161a #x8cf30bad #x81b02d74 #x857130c3
     #x5d8a9099 #x594b8d2e #x5408abf7 #x50c9b640
     #x4e8ee645 #x4a4ffbf2 #x470cdd2b #x43cdc09c
     #x7b827d21 #x7f436096 #x7200464f #x76c15bf8
     #x68860bfd #x6c47164a #x61043093 #x65c52d24
     #x119b4be9 #x155a565e #x18197087 #x1cd86d30
     #x029f3d35 #x065e2082 #x0b1d065b #x0fdc1bec
     #x3793a651 #x3352bbe6 #x3e119d3f #x3ad08088
     #x2497d08d #x2056cd3a #x2d15ebe3 #x29d4f654
     #xc5a92679 #xc1683bce #xcc2b1d17 #xc8ea00a0
     #xd6ad50a5 #xd26c4d12 #xdf2f6bcb #xdbee767c
     #xe3a1cbc1 #xe760d676 #xea23f0af #xeee2ed18
     #xf0a5bd1d #xf464a0aa #xf9278673 #xfde69bc4
     #x89b8fd09 #x8d79e0be #x803ac667 #x84fbdbd0
     #x9abc8bd5 #x9e7d9662 #x933eb0bb #x97ffad0c
     #xafb010b1 #xab710d06 #xa6322bdf #xa2f33668
     #xbcb4666d #xb8757bda #xb5365d03 #xb1f740b4))
