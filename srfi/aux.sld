(define-library (srfi aux)
  (export
   debug-mode
   define-aux-forms
   char-cased?-proc
   char-titlecase-proc
   define/opt
   lambda/opt
   )
  (import
   (scheme base)
   (scheme case-lambda)
   (scheme char)
   (srfi 31))
  (begin

    ;; Emacs indentation help:
    ;; (put 'define/opt 'scheme-indent-function 1)
    ;; (put 'lambda/opt 'scheme-indent-function 1)

    (define-syntax define/opt
      (syntax-rules ()
        ((_ (name . args) . body)
         (define name (lambda/opt args . body)))))

    (define-syntax lambda/opt
      (syntax-rules ()
        ((lambda* args . body)
         (rec name (opt/split-args name () () args body)))))

    (define-syntax opt/split-args
      (syntax-rules ()
        ((_ name non-opts (opts ...) ((opt) . rest) body)
         (opt/split-args name non-opts (opts ... (opt #f)) rest body))
        ((_ name non-opts (opts ...) ((opt def) . rest) body)
         (opt/split-args name non-opts (opts ... (opt def)) rest body))
        ((_ name (non-opts ...) opts (non-opt . rest) body)
         (opt/split-args name (non-opts ... non-opt) opts rest body))
        ;; Rest could be () or a rest-arg here; just propagate it.
        ((_ name non-opts opts rest body)
         (opt/make-clauses name () rest non-opts opts body))))

    (define-syntax opt/make-clauses
      (syntax-rules ()
        ;; Handle special-case with no optargs.
        ((_ name () rest (taken ...) () body)
         (lambda (taken ... . rest)
           . body))
        ;; Add clause where no optargs are provided.
        ((_ name () rest (taken ...) ((opt def) ...) body)
         (opt/make-clauses
          name
          (((taken ...)
            (name taken ... def ...)))
          rest
          (taken ...)
          ((opt def) ...)
          body))
        ;; Add clauses where 1 to n-1 optargs are provided
        ((_ name (clause ...) rest (taken ...) ((opt def) (opt* def*) ... x) body)
         (opt/make-clauses
          name
          (clause
           ...
           ((taken ... opt)
            (name taken ... opt def* ...)))
          rest
          (taken ... opt)
          ((opt* def*) ... x)
          body))
        ;; Add clause where all optargs were given, and possibly more.
        ((_ name (clause ...) rest (taken ...) ((opt def)) body)
         (case-lambda
           clause
           ...
           ((taken ... opt . rest)
            . body)))))

    (define debug-mode (make-parameter #f))

    (define-syntax define-aux-forms
      (syntax-rules :::
        ()
        ((_ check-arg let-optionals* :optional)
         (begin

           (define check-arg
             (if (debug-mode)
                 (lambda (pred val proc)
                   (if (pred val)
                       val
                       (error "Type assertion failed:"
                              `(value ,val)
                              `(expected-type ,pred)
                              `(callee ,proc))))
                 (lambda (pred val proc)
                   val)))

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
