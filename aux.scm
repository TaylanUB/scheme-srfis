(define-library (srfi aux)
  (export
   debug-mode
   define-aux-forms
   char-cased?-proc
   char-titlecase-proc
   )
  (import
   (scheme base)
   (scheme char))
  (begin

    (define debug-mode (make-parameter #f))

    (define-syntax define-aux-forms
      (syntax-rules :::
        ()
        ((_ check-arg let-optionals* :optional)
         (begin

           (define check-arg
             (if (debug-mode)
                 (lambda (pred val proc)
                   (if (pred val) val (error "Bad arg" val pred proc)))
                 (lambda (pred val proc)
                   #f)))

           (define check-optional
             (if (debug-mode)
                 (lambda (pred form)
                   (unless (pred)
                     (error "Optional argument guard failed:" form)))
                 (lambda (pred form)
                   #f)))

           (define-syntax let-optionals*
             (syntax-rules ()
               ((_ args () body ...)
                (begin body ...))
               ((_ args ((var default) rest ...) body ...)
                (let-optionals* args ((var default #t) rest ...) body ...))
               ((_ args (((var ...) default-producer guard) rest ...) body ...)
                (let-values (((a) args)
                             ((var ...) (default-producer)))
                  (check-optional (lambda () guard) 'guard)
                  (let-optionals*
                   (if (null? a) a (cdr a)) (rest ...) body ...)))
               ((_ args ((var default guard) rest ...) body ...)
                (let* ((a args)
                       (var (if (null? a) default (car a))))
                  (check-optional (lambda () guard) 'guard)
                  (let-optionals*
                   (if (null? a) a (cdr a)) (rest ...) body ...)))
               ((_ args (restarg) body ...)
                (let ((restarg args))
                  body ...))))

           (define-syntax :optional
             (syntax-rules ()
               ((_ args default)
                (:optional args default #t))
               ((_ args default guard)
                (let ((a args))
                  (if (pair? a)
                      (let ((val (car a)))
                        (check-optional (lambda () guard) 'guard)
                        val)
                      default)))))

           ))))

    (define char-cased?-proc
      (make-parameter
       (lambda (c)
         (not (eqv? (char-upcase c) (char-downcase c))))))

    (define char-titlecase-proc (make-parameter char-upcase))

    ))
