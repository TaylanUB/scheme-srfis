(define-library (srfi 43)
  (export

   ;; Constructors
   vector-unfold vector-unfold-right
   vector-reverse-copy
   vector-concatenate

   ;; Predicates
   vector-empty?
   vector=

   ;; Iteration
   vector-fold vector-fold-right
   vector-map vector-map!
   vector-for-each
   vector-count

   ;; Searching
   vector-index vector-index-right
   vector-skip vector-skip-right
   vector-binary-search
   vector-any vector-every

   ;; Mutators
   vector-swap!
   vector-reverse!
   vector-reverse-copy!

   ;; Conversion
   reverse-vector->list
   list->vector
   reverse-list->vector

   )
  (import
   (rename (scheme base) (list->vector %list->vector))
   (scheme case-lambda)
   (scheme cxr)
   (srfi 8)
   (srfi aux))
  (begin

    (define-aux-forms check-type let-optionals* :optional)

    ;; (CHECK-INDEX <vector> <index> <callee>) -> index
    ;;   Ensure that INDEX is a valid index into VECTOR; if not, signal an
    ;;   error stating that it is not and that this happened in a call to
    ;;   CALLEE.  Return INDEX when it is valid.  (Note that this does NOT
    ;;   check that VECTOR is indeed a vector.)
    (define check-index
      (if (debug-mode)
          (lambda (vec index callee)
            (let ((index (check-type integer? index callee)))
              (cond ((< index 0)
                     (check-index vec
                                  (error "vector index too low"
                                         index
                                         `(into vector ,vec)
                                         `(while calling ,callee))
                                  callee))
                    ((>= index (vector-length vec))
                     (check-index vec
                                  (error "vector index too high"
                                         index
                                         `(into vector ,vec)
                                         `(while calling ,callee))
                                  callee))
                    (else index))))
          (lambda (vec index callee)
            index)))

    ;; (CHECK-INDICES <vector>
    ;;                <start> <start-name>
    ;;                <end> <end-name>
    ;;                <caller>) -> [start end]
    ;;   Ensure that START and END are valid bounds of a range within
    ;;   VECTOR; if not, signal an error stating that they are not, with
    ;;   the message being informative about what the argument names were
    ;;   called -- by using START-NAME & END-NAME --, and that it occurred
    ;;   while calling CALLEE.  Also ensure that VEC is in fact a vector.
    ;;   Returns no useful value.
    (define check-indices
      (if (debug-mode)
          (lambda (vec start start-name end end-name callee)
            (let ((lose (lambda things
                          (apply error "vector range out of bounds"
                                 (append things
                                         `(vector was ,vec)
                                         `(,start-name was ,start)
                                         `(,end-name was ,end)
                                         `(while calling ,callee)))))
                  (start (check-type integer? start callee))
                  (end   (check-type integer? end   callee)))
              (cond ((> start end)
                     ;; I'm not sure how well this will work.  The intent is that
                     ;; the programmer tells the debugger to proceed with both a
                     ;; new START & a new END by returning multiple values
                     ;; somewhere.
                     (receive (new-start new-end)
                         (lose `(,end-name < ,start-name))
                       (check-indices vec
                                      new-start start-name
                                      new-end end-name
                                      callee)))
                    ((< start 0)
                     (check-indices vec
                                    (lose `(,start-name < 0))
                                    start-name
                                    end end-name
                                    callee))
                    ((>= start (vector-length vec))
                     (check-indices vec
                                    (lose `(,start-name > len)
                                          `(len was ,(vector-length vec)))
                                    start-name
                                    end end-name
                                    callee))
                    ((> end (vector-length vec))
                     (check-indices vec
                                    start start-name
                                    (lose `(,end-name > len)
                                          `(len was ,(vector-length vec)))
                                    end-name
                                    callee))
                    (else
                     (values start end)))))
          (lambda (vec start start-name end end-name callee)
            (values start end))))

    )
  (include "43.body.scm"))
