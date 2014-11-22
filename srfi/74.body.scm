;; Octet-addressed binary objects

;; Copyright (C) Michael Sperber (2005). All Rights Reserved.
;;
;; Copyright (C) Taylan Ulrich Bayırlı/Kammer (2014).
;; 
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define endianness-native (native-endianness))

(define (bytevector-s8-ref b k)
  (u8->s8 (bytevector-u8-ref b k)))

(define (u8->s8 octet)
  (if (> octet 127)
      (- octet 256)
      octet))

(define (bytevector-s8-set! b k val)
  (bytevector-u8-set! b k (s8->u8 val)))

(define (s8->u8 val)
  (if (negative? val)
      (+ val 256)
      val))

(define (index-iterate start count low-first?
		       unit proc)
  (if low-first?
      (let loop ((index 0)
		 (acc unit))
	(if (>= index count)
	    acc
	    (loop (+ index 1)
		  (proc (+ start index) acc))))

      (let loop ((index (- (+ start count) 1))
		 (acc unit))
	(if (< index start)
	    acc
	    (loop (- index 1)
		  (proc index acc))))))

(define (bytevector-uint-ref size endness bytevector index)
  (index-iterate index size
                 (eq? endianness-big endness)
                 0
    (lambda (index acc)
      (+ (bytevector-u8-ref bytevector index)
         (arithmetic-shift acc 8)))))

(define (bytevector-sint-ref size endness bytevector index)
  (let ((high-byte (bytevector-u8-ref bytevector
                                      (if (eq? endness endianness-big)
                                          index
                                          (- (+ index size) 1)))))

    (if (> high-byte 127)
	(- (+ 1
	      (index-iterate index size
			     (eq? endianness-big endness)
			     0
                (lambda (index acc)
                  (+ (- 255 (bytevector-u8-ref bytevector index))
                     (arithmetic-shift acc 8))))))
	(index-iterate index size
		       (eq? endianness-big endness)
		       0
          (lambda (index acc)
            (+ (bytevector-u8-ref bytevector index)
               (arithmetic-shift acc 8)))))))

(define (make-uint-ref size)
  (cut bytevector-uint-ref size <> <> <>))

(define (make-sint-ref size)
  (cut bytevector-sint-ref size <> <> <>))

(define (bytevector-uint-set! size endness bytevector index val)
  (index-iterate index size (eq? endianness-little endness)
		 val
    (lambda (index acc)
      (bytevector-u8-set! bytevector index (remainder acc 256))
      (quotient acc 256)))
  (values))

(define (bytevector-sint-set! size endness bytevector index val)
  (if (negative? val)
      (index-iterate index size (eq? endianness-little endness)
		     (- -1 val)
        (lambda (index acc)
          (bytevector-u8-set! bytevector index (- 255 (remainder acc 256)))
          (quotient acc 256)))
      
      (index-iterate index size (eq? endianness-little endness)
		     val
        (lambda (index acc)
          (bytevector-u8-set! bytevector index (remainder acc 256))
          (quotient acc 256))))
  
  (values))

(define (make-uint-set! size)
  (cut bytevector-uint-set! size <> <> <> <>))
(define (make-sint-set! size)
  (cut bytevector-sint-set! size <> <> <> <>))

(define (make-ref/native base base-ref)
  (lambda (bytevector index)
    (ensure-aligned index base)
    (base-ref endianness-native bytevector index)))

(define (make-set!/native base base-set!)
  (lambda (bytevector index val)
    (ensure-aligned index base)
    (base-set! endianness-native bytevector index val)))

(define (ensure-aligned index base)
  (if (not (zero? (remainder index base)))
      (error "non-aligned bytevector access" index base)))

(define bytevector-u16-ref (make-uint-ref 2))
(define bytevector-u16-set! (make-uint-set! 2))
(define bytevector-s16-ref (make-sint-ref 2))
(define bytevector-s16-set! (make-sint-set! 2))
(define bytevector-u16-native-ref (make-ref/native 2 bytevector-u16-ref))
(define bytevector-u16-native-set! (make-set!/native 2 bytevector-u16-set!))
(define bytevector-s16-native-ref (make-ref/native 2 bytevector-s16-ref))
(define bytevector-s16-native-set! (make-set!/native 2 bytevector-s16-set!))

(define bytevector-u32-ref (make-uint-ref 4))
(define bytevector-u32-set! (make-uint-set! 4))
(define bytevector-s32-ref (make-sint-ref 4))
(define bytevector-s32-set! (make-sint-set! 4))
(define bytevector-u32-native-ref (make-ref/native 4 bytevector-u32-ref))
(define bytevector-u32-native-set! (make-set!/native 4 bytevector-u32-set!))
(define bytevector-s32-native-ref (make-ref/native 4 bytevector-s32-ref))
(define bytevector-s32-native-set! (make-set!/native 4 bytevector-s32-set!))

(define bytevector-u64-ref (make-uint-ref 8))
(define bytevector-u64-set! (make-uint-set! 8))
(define bytevector-s64-ref (make-sint-ref 8))
(define bytevector-s64-set! (make-sint-set! 8))
(define bytevector-u64-native-ref (make-ref/native 8 bytevector-u64-ref))
(define bytevector-u64-native-set! (make-set!/native 8 bytevector-u64-set!))
(define bytevector-s64-native-ref (make-ref/native 8 bytevector-s64-ref))
(define bytevector-s64-native-set! (make-set!/native 8 bytevector-s64-set!))

;; Auxiliary stuff

(define (bytevector->u8-list b)
  (do ((i 0 (+ 1 i))
       (list '() (cons (bytevector-u8-ref b i) list)))
      ((= i (bytevector-length b))
       (reverse list))))
(define (bytevector->s8-list b)
  (map u8->s8 (bytevector->u8-list b)))

(define (u8-list->bytevector l)
  (apply bytevector l))
(define (s8-list->bytevector l)
  (u8-list->bytevector (map s8->u8 l)))

(define (make-bytevector->int-list bytevector-ref)
  (lambda (size endness b)
    (let ((ref (cut bytevector-ref size endness b <>))
	  (length (bytevector-length b)))
      (let loop ((i 0) (r '()))
	(if (>= i length)
	    (reverse r)
	    (loop (+ i size)
		  (cons (ref i) r)))))))

(define bytevector->uint-list (make-bytevector->int-list bytevector-uint-ref))
(define bytevector->sint-list (make-bytevector->int-list bytevector-sint-ref))

(define (make-int-list->bytevector bytevector-set!)
  (lambda (size endness l)
    (let* ((bytevector (make-bytevector (* size (length l))))
	   (set! (cut bytevector-set! size endness bytevector <> <>)))
      (let loop ((i 0) (l l))
	(if (null? l)
	    bytevector
	    (begin
	      (set! i (car l))
	      (loop (+ i size) (cdr l))))))))

(define uint-list->bytevector (make-int-list->bytevector bytevector-uint-set!))
(define sint-list->bytevector (make-int-list->bytevector bytevector-sint-set!))

;; Local Variables:
;; eval: (put 'index-iterate 'scheme-indent-function 4)
;; End:
