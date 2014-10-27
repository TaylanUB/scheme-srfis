(define-library (srfi aux)
  (export
   debug-mode
   define-check-arg
   )
  (import (scheme base))
  (begin
    (define debug-mode (make-parameter #f))
    (define-syntax define-check-arg
      (syntax-rules ()
        ((_ check-arg)
         (define check-arg
           (if (debug-mode)
               (lambda (pred val proc) 
                 (if (pred val) val (error "Bad arg" val pred proc)))
               (lambda (pred val proc)
                 #f))))))))
