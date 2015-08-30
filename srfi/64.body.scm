;; Copyright (c) 2005, 2006, 2007, 2012, 2013 Per Bothner
;; Added "full" support for Chicken, Gauche, Guile and SISC.
;;   Alex Shinn, Copyright (c) 2005.
;; Modified for Scheme Spheres by Álvaro Castro-Castilla, Copyright (c) 2012.
;; Support for Guile 2 by Mark H Weaver <mhw@netris.org>, Copyright (c) 2014.
;; Made an R7RS library by Taylan Ulrich Bayırlı/Kammer, Copyright (c) 2014.
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
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


;;; Test runner type

(define-record-type <test-runner>
  (make-test-runner) test-runner?

  (result-alist test-result-alist test-result-alist!)

  (pass-count test-runner-pass-count test-runner-pass-count!)
  (fail-count test-runner-fail-count test-runner-fail-count!)
  (xpass-count test-runner-xpass-count test-runner-xpass-count!)
  (xfail-count test-runner-xfail-count test-runner-xfail-count!)
  (skip-count test-runner-skip-count test-runner-skip-count!)
  (total-count %test-runner-total-count %test-runner-total-count!)

  ;; Stack (list) of (count-at-start . expected-count):
  (count-list %test-runner-count-list %test-runner-count-list!)

  ;; Normally #f, except when in a test-apply.
  (run-list %test-runner-run-list %test-runner-run-list!)

  (skip-list %test-runner-skip-list %test-runner-skip-list!)
  (fail-list %test-runner-fail-list %test-runner-fail-list!)

  (skip-save %test-runner-skip-save %test-runner-skip-save!)
  (fail-save %test-runner-fail-save %test-runner-fail-save!)

  (group-stack test-runner-group-stack test-runner-group-stack!)

  ;; Note: on-test-begin and on-test-end are *not* related to test-begin and
  ;; test-end.  They're called at the beginning/end of each individual test,
  ;; whereas test-begin/test-end demarcate whole test suites.

  (on-group-begin test-runner-on-group-begin test-runner-on-group-begin!)
  (on-test-begin test-runner-on-test-begin test-runner-on-test-begin!)
  (on-test-end test-runner-on-test-end test-runner-on-test-end!)
  (on-group-end test-runner-on-group-end test-runner-on-group-end!)
  (on-final test-runner-on-final test-runner-on-final!)
  (on-bad-count test-runner-on-bad-count test-runner-on-bad-count!)
  (on-bad-end-name test-runner-on-bad-end-name test-runner-on-bad-end-name!)

  (aux-value test-runner-aux-value test-runner-aux-value!))

(define (test-runner-reset runner)
  (test-result-alist! runner '())
  (test-runner-pass-count! runner 0)
  (test-runner-fail-count! runner 0)
  (test-runner-xpass-count! runner 0)
  (test-runner-xfail-count! runner 0)
  (test-runner-skip-count! runner 0)
  (%test-runner-total-count! runner 0)
  (%test-runner-count-list! runner '())
  (%test-runner-run-list! runner #f)
  (%test-runner-skip-list! runner '())
  (%test-runner-fail-list! runner '())
  (%test-runner-skip-save! runner '())
  (%test-runner-fail-save! runner '())
  (test-runner-group-stack! runner '()))

(define (test-runner-group-path runner)
  (reverse (test-runner-group-stack runner)))

(define test-result-ref
  (case-lambda
    ((runner key)
     (test-result-ref runner key #f))
    ((runner key default)
     (let ((entry (assq key (test-result-alist runner))))
       (if entry (cdr entry) default)))))

(define (test-result-set! runner key value)
  (let* ((alist (test-result-alist runner))
         (entry (assq key alist)))
    (if entry
        (set-cdr! entry value)
        (test-result-alist! runner (cons (cons key value) alist)))))

(define (test-result-clear runner)
  (test-result-alist! runner '()))

(define (test-result-remove runner key)
  (test-result-alist! runner (remove (lambda (entry)
                                       (eq? key (car entry)))
                                     (test-result-alist runner))))

(define test-result-name
  (case-lambda
    (() (test-result-name (test-runner-get)))
    ((runner) (test-result-ref runner 'test-name ""))))

(define test-result-name!
  (case-lambda
    ((name) (test-result-name! (test-runner-get) name))
    ((runner name) (test-result-set! runner 'test-name name))))

(define test-result-expression
  (case-lambda
    (() (test-result-expression (test-runner-get)))
    ((runner) (test-result-ref runner 'expression))))

(define test-result-expression!
  (case-lambda
    ((expression) (test-result-expression! (test-runner-get) expression))
    ((runner expression) (test-result-set! runner 'expression expression))))

(define test-result-kind
  (case-lambda
    (() (test-result-kind (test-runner-get)))
    ((runner) (test-result-ref runner 'result-kind))))

(define test-result-kind!
  (case-lambda
    ((kind) (test-result-kind! (test-runner-get) kind))
    ((runner kind) (test-result-set! runner 'result-kind kind))))

(define test-passed?
  (case-lambda
    (() (test-passed? (test-runner-get)))
    ((runner) (memq (test-result-kind runner) '(pass xpass)))))

(define test-runner-test-name test-result-name)

(define test-runner-factory (make-parameter #f))

(define (test-runner-create) ((test-runner-factory)))

(define test-runner-current (make-parameter #f))

(define (test-runner-get)
  (or (test-runner-current)
      (error "test-runner not initialized - test-begin missing?")))

(define test-match-nth
  (case-lambda
    ((n) (test-match-nth n 1))
    ((n count)
     (let ((i 0))
       (lambda (runner)
         (set! i (+ i 1))
         (and (>= i n) (< i (+ n count))))))))

(define (test-match-name name)
  (lambda (runner)
    (equal? name (test-result-name runner))))

(define (make-pred spec)
  (cond ((procedure? spec) spec)
        ((integer? spec) (test-match-nth 1 spec))
        ((string? spec) (test-match-name spec))
        (else
         (error "not a valid test specifier" spec))))

(define (any-pred preds object)
  (any (lambda (pred) (pred object)) preds))

(define (every-pred preds object)
  (every (lambda (pred) (pred object)) preds))

(define (test-match-all . specs)
  (let ((preds (map make-pred specs)))
    (lambda (runner)
      (every-pred preds runner))))

(define (test-match-any . specs)
  (let ((preds (map make-pred specs)))
    (lambda (runner)
      (any-pred preds runner))))


;;; Null runner

(define (test-runner-null)
  (define (%test-null-callback runner) #f)
  (let ((runner (make-test-runner)))
    (test-runner-reset runner)
    (test-runner-on-group-begin! runner (lambda (runner name count) #f))
    (test-runner-on-group-end! runner %test-null-callback)
    (test-runner-on-final! runner %test-null-callback)
    (test-runner-on-test-begin! runner %test-null-callback)
    (test-runner-on-test-end! runner %test-null-callback)
    (test-runner-on-bad-count! runner (lambda (runner count expected) #f))
    (test-runner-on-bad-end-name! runner (lambda (runner begin end) #f))
    runner))


;;; Simple runner

(define (test-on-group-begin-simple runner name count)
  (if (null? (test-runner-group-stack runner))
      (format #t "%%%% Test suite begin: ~a~%" name)
      (format #t "Group begin: ~a~%" name)))

(define (test-on-test-begin-simple runner)
  (values))

(define (test-on-test-end-simple runner)
  (let* ((result-kind (test-result-kind runner))
         (result-kind-name (case result-kind
                             ((pass) "PASS") ((fail) "FAIL")
                             ((xpass) "XPASS") ((xfail) "XFAIL")
                             ((skip) "SKIP")))
         (name (let ((test-name (test-result-name runner)))
                 (if (string=? "" test-name)
                     (test-result-expression runner)
                     test-name)))
         (label (string-join (append (test-runner-group-path runner)
                                     (list name))
                             "/")))
    (format #t "[~a] ~a~%" result-kind-name label)
    (when (eq? result-kind 'fail)
      (let ((error (test-result-ref runner 'actual-error)))
        (when error
          (format #t "Exception: ~a~%" error))))))

(define (test-on-group-end-simple runner)
  (let ((name (car (test-runner-group-stack runner))))
    (if (= 1 (length (test-runner-group-stack runner)))
        (format #t "%%%% Test suite end: ~a~%" name)
        (format #t "Group end: ~a~%" name))))

(define (test-on-final-simple runner)
  (define (maybe-display label value)
    (when (> value 0)
      (display label) (display value) (newline)))
  (maybe-display
   "# of expected passes      " (test-runner-pass-count runner))
  (maybe-display
   "# of expected failures    " (test-runner-xfail-count runner))
  (maybe-display
   "# of unexpected successes " (test-runner-xpass-count runner))
  (maybe-display
   "# of unexpected failures  " (test-runner-fail-count runner))
  (maybe-display
   "# of skipped tests        " (test-runner-skip-count runner)))

(define (test-on-bad-count-simple runner count expected-count)
  (format #t "*** Total number of tests was ~a but should be ~a. ***~%"
          count expected-count)
  (format #t "*** Discrepancy indicates testsuite error or exceptions. ***~%"))

(define (test-on-bad-end-name-simple runner begin-name end-name)
  (error (format #f "test-end ~a does not match test-begin ~a"
                 end-name begin-name)))

(define (test-runner-simple)
  (let ((runner (make-test-runner)))
    (test-runner-reset runner)
    (test-runner-on-group-begin! runner test-on-group-begin-simple)
    (test-runner-on-group-end! runner test-on-group-end-simple)
    (test-runner-on-final! runner test-on-final-simple)
    (test-runner-on-test-begin! runner test-on-test-begin-simple)
    (test-runner-on-test-end! runner test-on-test-end-simple)
    (test-runner-on-bad-count! runner test-on-bad-count-simple)
    (test-runner-on-bad-end-name! runner test-on-bad-end-name-simple)
    runner))

;;; Set default factory to simple.
(test-runner-factory test-runner-simple)


;;; Test execution control

(define-syntax test-begin
  (syntax-rules ()
    ((_ suite-name)
     (test-begin suite-name #f))
    ((_ suite-name count)
     (let ((name suite-name))
       (when (not (test-runner-current))
         (test-runner-current (test-runner-create)))
       (let ((r (test-runner-current)))
         (let ((skip-list (%test-runner-skip-list r))
               (skip-save (%test-runner-skip-save r))
               (fail-list (%test-runner-fail-list r))
               (fail-save (%test-runner-fail-save r))
               (total-count (%test-runner-total-count r))
               (count-list (%test-runner-count-list r))
               (group-stack (test-runner-group-stack r)))
           ((test-runner-on-group-begin r) r name count)
           (%test-runner-skip-save! r (cons skip-list skip-save))
           (%test-runner-fail-save! r (cons fail-list fail-save))
           (%test-runner-count-list! r (cons (cons total-count count)
                                             count-list))
           (test-runner-group-stack! r (cons name group-stack))))))))

(define-syntax test-end
  (syntax-rules ()
    ((_)
     (test-end #f))
    ((_ suite-name)
     (let ((name suite-name))
       (let* ((r (test-runner-get))
              (groups (test-runner-group-stack r)))
         (test-result-clear r)
         (when (null? groups)
           (error "test-end not in a group"))
         (when (and name (not (equal? name (car groups))))
           ((test-runner-on-bad-end-name r) r name (car groups)))
         (let* ((count-list (%test-runner-count-list r))
                (expected-count (cdar count-list))
                (saved-count (caar count-list))
                (group-count (- (%test-runner-total-count r) saved-count)))
           (when (and expected-count
                      (not (= expected-count group-count)))
             ((test-runner-on-bad-count r) r group-count expected-count))
           ((test-runner-on-group-end r) r)
           (test-runner-group-stack! r (cdr (test-runner-group-stack r)))
           (%test-runner-skip-list! r (car (%test-runner-skip-save r)))
           (%test-runner-skip-save! r (cdr (%test-runner-skip-save r)))
           (%test-runner-fail-list! r (car (%test-runner-fail-save r)))
           (%test-runner-fail-save! r (cdr (%test-runner-fail-save r)))
           (%test-runner-count-list! r (cdr count-list))
           (when (null? (test-runner-group-stack r))
             ((test-runner-on-final r) r))))))))

(define (test-skip? runner)
  (let ((run-list (%test-runner-run-list runner))
        (skip-list (%test-runner-skip-list runner)))
    (or (and run-list (not (any-pred run-list runner)))
        (any-pred skip-list runner))))

(define-syntax test-group
  (syntax-rules ()
    ((_ suite-name body body* ...)
     (let ((runner (test-runner-get))
           (name suite-name))
       (test-result-clear runner)
       (test-result-name! runner name)
       (unless (test-skip? runner)
         (dynamic-wind
           (lambda () (test-begin name))
           (lambda () body body* ...)
           (lambda () (test-end name))))))))

(define-syntax test-group-with-cleanup
  (syntax-rules ()
    ((_ suite-name body body* ... cleanup)
     (test-group suite-name
       (dynamic-wind (lambda () #f)
                     (lambda () body body* ...)
                     (lambda () cleanup))))))

(define (test-skip . specs)
  (let ((runner (test-runner-get)))
    (%test-runner-skip-list!
     runner (cons (apply test-match-all specs)
                  (%test-runner-skip-list runner)))))

(define (test-expect-fail . specs)
  (let ((runner (test-runner-get)))
    (%test-runner-fail-list!
     runner (cons (apply test-match-all specs)
                  (%test-runner-fail-list runner)))))

(define (test-prelude runner name expression)
  (test-result-clear runner)
  (when name
    (test-result-name! runner name))
  (test-result-expression! runner expression)
  (let ((skip? (test-skip? runner)))
    (if skip?
        (test-result-kind! runner 'skip)
        (let ((fail-list (%test-runner-fail-list runner)))
          (when (any-pred fail-list runner)
            (test-result-kind! runner 'xfail)))) ;just for later inspection
    ((test-runner-on-test-begin runner) runner)
    (not skip?)))

(define (test-postlude runner)
  (let ((result-kind (test-result-kind runner)))
    (case result-kind
      ((pass)
       (test-runner-pass-count! runner (+ 1 (test-runner-pass-count runner))))
      ((fail)
       (test-runner-fail-count! runner (+ 1 (test-runner-fail-count runner))))
      ((xpass)
       (test-runner-xpass-count! runner (+ 1 (test-runner-xpass-count runner))))
      ((xfail)
       (test-runner-xfail-count! runner (+ 1 (test-runner-xfail-count runner))))
      ((skip)
       (test-runner-skip-count! runner (+ 1 (test-runner-skip-count runner)))))
    (%test-runner-total-count! runner (+ 1 (%test-runner-total-count runner)))
    ((test-runner-on-test-end runner) runner)))

(define (fix-result-kind runner pass?)
  (test-result-kind! runner (if (eq? (test-result-kind runner) 'xfail)
                                (if pass? 'xpass 'xfail)
                                (if pass? 'pass 'fail))))

(define-syntax false-if-error
  (syntax-rules ()
    ((_ expression runner)
     (guard (error
             (else
              (test-result-set! runner 'actual-error error)
              #f))
       expression))))

(define-syntax test-assert
  (syntax-rules ()
    ((_ expr)
     (test-assert #f expr))
    ((_ name-expr expr)
     (let ((runner (test-runner-get))
           (name name-expr))
       (when (test-prelude runner name 'expr)
         (let ((val (false-if-error expr runner)))
           (test-result-set! runner 'actual-value val)
           (fix-result-kind runner val)))
       (test-postlude runner)))))

(define-syntax test-compare
  (syntax-rules ()
    ((_ compare expected expr)
     (test-compare compare #f expected expr))
    ((_ compare name-expr expected-expr expr)
     (let ((runner (test-runner-get))
           (name name-expr))
       (when (test-prelude runner name 'expr)
         (let ((expected expected-expr))
           (test-result-set! runner 'expected-value expected)
           (let ((pass? (false-if-error
                         (let ((val expr))
                           (test-result-set! runner 'actual-value val)
                           (compare expected val))
                         runner)))
             (fix-result-kind runner pass?))))
       (test-postlude runner)))))

(define-syntax test-equal
  (syntax-rules ()
    ((_ . rest)
     (test-compare equal? . rest))))

(define-syntax test-eqv
  (syntax-rules ()
    ((_ . rest)
     (test-compare eqv? . rest))))

(define-syntax test-eq
  (syntax-rules ()
    ((_ . rest)
     (test-compare eq? . rest))))

(define (approx= error)
  (lambda (value expected)
    (let ((rval (real-part value))
          (ival (imag-part value))
          (rexp (real-part expected))
          (iexp (imag-part expected)))
      (and (>= rval (- rexp error))
           (>= ival (- iexp error))
           (<= rval (+ rexp error))
           (<= ival (+ iexp error))))))

(define-syntax test-approximate
  (syntax-rules ()
    ((_ expected expr error)
     (test-approximate #f expected expr error))
    ((_ test-name expected expr error)
     (test-compare (approx= error) test-name expected expr))))

(define (error-matches? error type)
  (cond
   ((eq? type #t)
    #t)
   ((condition-type? type)
    (and (condition? error) (condition-has-type? error type)))
   ((procedure? type)
    (type error))
   (else
    (format #t "WARNING: unknown error type predicate: ~a~%" type)
    (format #t "         error was: ~a~%" error)
    #f)))

(define-syntax test-error
  (syntax-rules ()
    ((_ expr)
     (test-error #f #t expr))
    ((_ error-type expr)
     ((test-error #f error-type expr)))
    ((_ name-expr error-type-expr expr)
     (let ((runner (test-runner-get))
           (name name-expr))
       (when (test-prelude runner name 'expr)
         (let ((error-type error-type-expr))
           (test-result-set! runner 'expected-error error-type)
           (let ((pass? (guard (error (else (test-result-set!
                                             runner 'actual-error error)
                                            (error-matches? error error-type)))
                          (let ((val expr))
                            (test-result-set! runner 'actual-value val))
                          #f)))
             (fix-result-kind runner pass?))))
       (test-postlude runner)))))

(define (test-apply first . rest)
  (let ((runner (if (test-runner? first)
                    first
                    (or (test-runner-current) (test-runner-create))))
        (run-list (if (test-runner? first)
                      (drop-right rest 1)
                      (cons first (drop-right rest 1))))
        (proc (last rest)))
    (test-with-runner runner
      (let ((saved-run-list (%test-runner-run-list runner)))
        (%test-runner-run-list! runner run-list)
        (proc)
        (%test-runner-run-list! runner saved-run-list)))))

(define-syntax test-with-runner
  (syntax-rules ()
    ((_ runner body body* ...)
     (let ((saved-runner (test-runner-current)))
       (dynamic-wind
         (lambda () (test-runner-current runner))
         (lambda () body body* ...)
         (lambda () (test-runner-current saved-runner)))))))

(define test-read-eval-string
  (case-lambda
    ((string)
     (test-read-eval-string string (cond-expand
                                    (guile (current-module))
                                    (else #f))))
    ((string env)
     (let* ((port (open-input-string string))
            (form (read port)))
       (if (eof-object? (read-char port))
           (if env
               (eval form env)
               (eval form))
           (error "(not at eof)"))))))
