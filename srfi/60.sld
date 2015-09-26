(define-library (srfi 60)
  (export
   ;; Bitwise Operations
   logand
   bitwise-and
   logior
   bitwise-ior
   logxor
   bitwise-xor
   lognot
   bitwise-not
   bitwise-if
   bitwise-merge
   logtest
   any-bits-set?
   
   ;; Integer Properties
   logcount
   bit-count
   integer-length
   log2-binary-factors
   first-set-bit
   
   ;; Bit Within Word
   logbit?
   bit-set?
   copy-bit
   
   ;; Field of Bits
   bit-field
   copy-bit-field
   ash
   arithmetic-shift
   rotate-bit-field
   reverse-bit-field
   
   ;; Bits as Booleans
   integer->list
   list->integer
   booleans->integer
   )
  (import (scheme base))
  (include "60.upstream.scm"))
