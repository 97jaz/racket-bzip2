#lang scribble/manual

@(require
   racket/require
   (for-label racket/base
              file/bzip2))
                     
@title{bzip2}

@defmodule[file/bzip2]

The @racketmodname[file/bzip2] module provides support for compressing
and decompressing data using the @link["https://en.wikipedia.org/wiki/Bzip2"]{bzip2}
file format.

@local-table-of-contents[]

@defproc[(bzip2-input-port [in input-port?]) input-port?]{
Returns an input port that reads and decompresses bzip2 data from @racket[in].
}

@defproc[(bzip2-output-port [out output-port?] [#:block-size block-size (integer-in 1 9) 9]) output-port?]{
Returns an output port that bzip2-compresses the data written to it, and writes the
compressed data to @racket[out]. The returned output port must be closed to ensure that
all compressed data is written to the underlying port.

The optional @racket[block-size] parameter controls size of the internal buffer used for compression.
In general, the larger the block size, the better the compression — but also the higher the
memory use. To reduce memory use, choose a lower value for @racket[block-size].
}
