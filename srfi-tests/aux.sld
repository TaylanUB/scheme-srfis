(define-library (srfi-tests aux)
  (export
   define-tests
   ->string
   test-eqv+
   test-eq+
   test-equal+
   )
  (import
   (scheme base)
   (scheme write)
   (scheme case-lambda)
   (srfi 64))
  (begin

    (define-syntax define-tests
      (syntax-rules ()
        ((_ proc-name suite-name form ...)
         (define proc-name
           (case-lambda
             (() (proc-name (test-runner-create)))
             ((runner)
              (parameterize ((test-runner-current runner))
                (test-begin suite-name)
                form ...
                (test-end suite-name)
                (and (= 0 (test-runner-xpass-count runner))
                     (= 0 (test-runner-fail-count runner))))))))))

    (define (->string obj)
      (let ((port (open-output-string)))
        (write obj port)
        (get-output-string port)))

    (define-syntax test-eqv+
      (syntax-rules ()
        ((_ expected form)
         (test-eqv (->string 'form) expected form))))

    (define-syntax test-eq+
      (syntax-rules ()
        ((_ expected form)
         (test-eq (->string 'form) expected form))))

    (define-syntax test-equal+
      (syntax-rules ()
        ((_ expected form)
         (test-equal (->string 'form) expected form))))

    ))
