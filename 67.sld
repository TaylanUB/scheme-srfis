(define-library (srfi 67)
  (export
   </<=?
   </<?
   <=/<=?
   <=/<?
   <=?
   <?
   =?
   >/>=?
   >/>?
   >=/>=?
   >=/>?
   >=?
   >?
   boolean-compare
   chain<=?
   chain<?
   chain=?
   chain>=?
   chain>?
   char-compare
   char-compare-ci
   compare-by<
   compare-by<=
   compare-by=/<
   compare-by=/>
   compare-by>
   compare-by>=
   complex-compare
   cond-compare
   debug-compare
   default-compare
   if-not=?
   if3
   if<=?
   if<?
   if=?
   if>=?
   if>?
   integer-compare
   kth-largest
   list-compare
   list-compare-as-vector
   max-compare
   min-compare
   not=?
   number-compare
   pair-compare
   pair-compare-car
   pair-compare-cdr
   pairwise-not=?
   rational-compare
   real-compare
   refine-compare
   select-compare
   symbol-compare
   vector-compare
   vector-compare-as-list
   bytevector-compare
   bytevector-compare-as-list
   )
  (import
   (scheme base)
   (scheme case-lambda)
   (scheme char)
   (scheme complex)
   (srfi 27))
  (include "67.upstream.scm")
  (begin
    
    (define (bytevector-compare bv1 bv2)
      (let ((len1 (bytevector-length bv1))
            (len2 (bytevector-length bv2)))
        (cond
         ((< len1 len2) -1)
         ((> len1 len2) +1)
         (else
          (let lp ((i 0))
            (if (= i len1)
                0
                (let ((b1 (bytevector-u8-ref bv1 i))
                      (b2 (bytevector-u8-ref bv2 i)))
                  (cond
                   ((< b1 b2) -1)
                   ((> b1 b2) +1)
                   (else
                    (lp (+ 1 i)))))))))))

    (define (bytevector-compare-as-list bv1 bv2)
      (let ((len1 (bytevector-length bv1))
            (len2 (bytevector-length bv2)))
        (let lp ((i 0))
          (cond
           ((or (= i len1) (= i len2))
            (cond ((< len1 len2) -1)
                  ((> len1 len2) +1)
                  (else 0)))
           (else
            (let ((b1 (bytevector-u8-ref bv1 i))
                  (b2 (bytevector-u8-ref bv2 i)))
              (cond
               ((< b1 b2) -1)
               ((> b1 b2) +1)
               (else
                (lp (+ 1 i))))))))))

    ))
