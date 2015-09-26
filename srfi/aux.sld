(define-library (srfi aux)
  (import
   (scheme base)
   (scheme case-lambda)
   (srfi 31))
  (export
   debug-mode
   define/opt
   lambda/opt
   define-check-arg
   )
  (begin

    (define debug-mode (make-parameter #f))

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

    (define-syntax define-check-arg
      (syntax-rules ()
        ((_ check-arg)
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
                 val))))))

    ))
