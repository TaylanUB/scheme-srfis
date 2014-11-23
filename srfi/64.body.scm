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

(cond-expand
 (chicken
  (require-extension syntax-case))
 (kawa
  (module-compile-options warn-undefined-variable: #t
                          warn-invoke-unknown-method: #t)
  (provide 'srfi-64)
  (provide 'testing))
 (else))

(define-record-type <test-runner>
  (%test-runner-alloc) test-runner?
  ;; Cumulate count of all tests that have passed and were expected to.
  (pass-count test-runner-pass-count test-runner-pass-count!)
  (fail-count test-runner-fail-count test-runner-fail-count!)
  (xpass-count test-runner-xpass-count test-runner-xpass-count!)
  (xfail-count test-runner-xfail-count test-runner-xfail-count!)
  (skip-count test-runner-skip-count test-runner-skip-count!)
  (skip-list %test-runner-skip-list %test-runner-skip-list!)
  (fail-list %test-runner-fail-list %test-runner-fail-list!)
  ;; Normally #t, except when in a test-apply.
  (run-list %test-runner-run-list %test-runner-run-list!)
  (skip-save %test-runner-skip-save %test-runner-skip-save!)
  (fail-save %test-runner-fail-save %test-runner-fail-save!)
  (group-stack test-runner-group-stack test-runner-group-stack!)
  (on-test-begin test-runner-on-test-begin test-runner-on-test-begin!)
  (on-test-end test-runner-on-test-end test-runner-on-test-end!)
  ;; Call-back when entering a group. Takes (runner suite-name count).
  (on-group-begin test-runner-on-group-begin test-runner-on-group-begin!)
  ;; Call-back when leaving a group.
  (on-group-end test-runner-on-group-end test-runner-on-group-end!)
  ;; Call-back when leaving the outermost group.
  (on-final test-runner-on-final test-runner-on-final!)
  ;; Call-back when expected number of tests was wrong.
  (on-bad-count test-runner-on-bad-count test-runner-on-bad-count!)
  ;; Call-back when name in test-end doesn't match test-begin.
  (on-bad-end-name test-runner-on-bad-end-name test-runner-on-bad-end-name!)
  ;; Cumulate count of all tests that have been done.
  (total-count %test-runner-total-count %test-runner-total-count!)
  ;; Stack (list) of (count-at-start . expected-count):
  (count-list %test-runner-count-list %test-runner-count-list!)
  (result-alist test-result-alist test-result-alist!)
  ;; Field can be used by test-runner for any purpose.
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
  (%test-runner-run-list! runner #t)
  (%test-runner-skip-list! runner '())
  (%test-runner-fail-list! runner '())
  (%test-runner-skip-save! runner '())
  (%test-runner-fail-save! runner '())
  (test-runner-group-stack! runner '()))

(define (test-runner-group-path runner)
  (reverse (test-runner-group-stack runner)))

(define (%test-null-callback runner) #f)

(define (test-runner-null)
  (let ((runner (%test-runner-alloc)))
    (test-runner-reset runner)
    (test-runner-on-group-begin! runner (lambda (runner name count) #f))
    (test-runner-on-group-end! runner %test-null-callback)
    (test-runner-on-final! runner %test-null-callback)
    (test-runner-on-test-begin! runner %test-null-callback)
    (test-runner-on-test-end! runner %test-null-callback)
    (test-runner-on-bad-count! runner (lambda (runner count expected) #f))
    (test-runner-on-bad-end-name! runner (lambda (runner begin end) #f))
    runner))

(define (test-runner-simple)
  (let ((runner (%test-runner-alloc)))
    (test-runner-reset runner)
    (test-runner-on-group-begin! runner test-on-group-begin-simple)
    (test-runner-on-group-end! runner test-on-group-end-simple)
    (test-runner-on-final! runner test-on-final-simple)
    (test-runner-on-test-begin! runner test-on-test-begin-simple)
    (test-runner-on-test-end! runner test-on-test-end-simple)
    (test-runner-on-bad-count! runner test-on-bad-count-simple)
    (test-runner-on-bad-end-name! runner test-on-bad-end-name-simple)
    runner))

(define test-runner-current (make-parameter #f))
(define test-runner-factory (make-parameter test-runner-simple))

;; A safer wrapper to test-runner-current.
(define (test-runner-get)
  (let ((r (test-runner-current)))
    (if r r (error "test-runner not initialized - test-begin missing?"))))

(define (%test-specifier-matches spec runner)
  (spec runner))

(define (test-runner-create)
  ((test-runner-factory)))

(define (%test-any-specifier-matches list runner)
  (let ((result #f))
    (let loop ((l list))
      (cond ((null? l) result)
            (else
             (when (%test-specifier-matches (car l) runner)
               (set! result #t))
             (loop (cdr l)))))))

;; Returns #f, #t, or 'xfail.
(define (%test-should-execute runner)
  (let ((run (%test-runner-run-list runner)))
    (cond ((or (not (or (eqv? run #t)
                        (%test-any-specifier-matches run runner)))
               (%test-any-specifier-matches (%test-runner-skip-list runner)
                                            runner))
           (test-result-set! runner 'result-kind 'skip)
           #f)
          ((%test-any-specifier-matches (%test-runner-fail-list runner) runner)
           (test-result-set! runner 'result-kind 'xfail)
           'xfail)
          (else #t))))

(define (%test-begin suite-name count)
  (when (not (test-runner-current))
    (test-runner-current (test-runner-create)))
  (let ((runner (test-runner-current)))
    ((test-runner-on-group-begin runner) runner suite-name count)
    (%test-runner-skip-save! runner
                             (cons (%test-runner-skip-list runner)
                                   (%test-runner-skip-save runner)))
    (%test-runner-fail-save! runner
                             (cons (%test-runner-fail-list runner)
                                   (%test-runner-fail-save runner)))
    (%test-runner-count-list! runner
                              (cons (cons (%test-runner-total-count runner)
                                          count)
                                    (%test-runner-count-list runner)))
    (test-runner-group-stack! runner (cons suite-name
                                           (test-runner-group-stack runner)))))
(cond-expand
 (kawa
  ;; Kawa has test-begin built in, implemented as:
  ;; (begin
  ;;   (cond-expand (srfi-64 #!void) (else (require 'srfi-64)))
  ;;   (%test-begin suite-name [count]))
  ;; This puts test-begin but only test-begin in the default environment.,
  ;; which makes normal test suites loadable without non-portable commands.
  )
 (else
  (define-syntax test-begin
    (syntax-rules ()
      ((test-begin suite-name)
       (%test-begin suite-name #f))
      ((test-begin suite-name count)
       (%test-begin suite-name count))))))

(define (test-on-group-begin-simple runner suite-name count)
  (when (null? (test-runner-group-stack runner))
    (display "%%%% Starting test ")
    (display suite-name)
    (newline))
  (display "Group begin: ")
  (display suite-name)
  (newline))

(define (test-on-group-end-simple runner)
  (display "Group end: ")
  (display (car (test-runner-group-stack runner)))
  (newline))

(define (test-on-bad-count-simple runner count expected-count)
  (display "*** Total number of tests was ")
  (display count)
  (display " but should be ")
  (display expected-count)
  (display ". ***")
  (newline)
  (display "*** Discrepancy indicates testsuite error or exceptions. ***")
  (newline))

(define (test-on-bad-end-name-simple runner begin-name end-name)
  (error (string-append
          (%test-format-line runner)
          "test-end " end-name " does not match test-begin " begin-name)))


(define (%test-final-report1 label value)
  (when (> value 0)
    (display label)
    (display value)
    (newline)))

(define (test-on-final-simple runner)
  (%test-final-report1
   "# of expected passes      " (test-runner-pass-count runner))
  (%test-final-report1
   "# of expected failures    " (test-runner-xfail-count runner))
  (%test-final-report1
   "# of unexpected successes " (test-runner-xpass-count runner))
  (%test-final-report1
   "# of unexpected failures  " (test-runner-fail-count runner))
  (%test-final-report1
   "# of skipped tests        " (test-runner-skip-count runner)))

(define (%test-format-line runner)
  (let* ((line-info (test-result-alist runner))
         (source-file (assq 'source-file line-info))
         (source-line (assq 'source-line line-info))
         (file (if source-file (cdr source-file) "")))
    (if source-line
        (string-append file ":"
                       (number->string (cdr source-line)) ": ")
        "")))

(define (%test-end suite-name line-info)
  (let* ((r (test-runner-get))
         (groups (test-runner-group-stack r))
         (line (%test-format-line r)))
    (test-result-alist! r line-info)
    (when (null? groups)
      (error (string-append line "test-end not in a group")))
    (when (and suite-name (not (equal? suite-name (car groups))))
      ((test-runner-on-bad-end-name r) r suite-name (car groups)))
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
        ((test-runner-on-final r) r)))))

(define-syntax test-group
  (syntax-rules ()
    ((test-group suite-name . body)
     (let ((r (test-runner-current)))
       ;; Ideally should also set line-number, if available.
       (test-result-alist! r (list (cons 'test-name suite-name)))
       (when (%test-should-execute r)
         (dynamic-wind
           (lambda () (test-begin suite-name))
           (lambda () . body)
           (lambda () (test-end  suite-name))))))))

(define-syntax test-group-with-cleanup
  (syntax-rules ()
    ((test-group-with-cleanup suite-name form cleanup-form)
     (test-group suite-name
                 (dynamic-wind
                   (lambda () #f)
                   (lambda () form)
                   (lambda () cleanup-form))))
    ((test-group-with-cleanup suite-name cleanup-form)
     (test-group-with-cleanup suite-name #f cleanup-form))
    ((test-group-with-cleanup suite-name form1 form2 form3 . rest)
     (test-group-with-cleanup suite-name (begin form1 form2) form3 . rest))))

(define (test-on-test-begin-simple runner)
  (let* ((results (test-result-alist runner))
         (source-file (assq 'source-file results))
         (source-line (assq 'source-line results))
         (source-form (assq 'source-form results))
         (test-name (assq 'test-name results)))
    (display "Test begin:")
    (newline)
    (when test-name (%test-write-result1 test-name))
    (when source-file (%test-write-result1 source-file))
    (when source-line (%test-write-result1 source-line))
    (when source-form (%test-write-result1 source-form))))

(define-syntax test-result-ref
  (syntax-rules ()
    ((test-result-ref runner pname)
     (test-result-ref runner pname #f))
    ((test-result-ref runner pname default)
     (let ((p (assq pname (test-result-alist runner))))
       (if p (cdr p) default)))))

(define (test-on-test-end-simple runner)
  (let ((kind (test-result-ref runner 'result-kind)))
    (when (memq kind '(fail xpass))
      (let* ((results (test-result-alist runner))
             (source-file (assq 'source-file results))
             (source-line (assq 'source-line results))
             (test-name (assq 'test-name results)))
        (if (or source-file source-line)
            (begin
              (if source-file (display (cdr source-file)))
              (display ":")
              (if source-line (display (cdr source-line)))
              (display ": ")))
        (display (if (eq? kind 'xpass) "XPASS" "FAIL"))
        (if test-name
            (begin
              (display " ")
              (display (cdr test-name))))
        (newline)))
    (display "Test end:")
    (newline)
    (let loop ((list (test-result-alist runner)))
      (if (pair? list)
          (let ((pair (car list)))
            ;; Write out properties not written out by on-test-begin.
            (unless (memq (car pair)
                          '(test-name source-file source-line source-form))
              (%test-write-result1 pair))
            (loop (cdr list)))))))

(define (%test-write-result1 pair)
  (display "  ")
  (display (car pair))
  (display ": ")
  (write (cdr pair))
  (newline))

(define (test-result-set! runner pname value)
  (let* ((alist (test-result-alist runner))
         (p (assq pname alist)))
    (if p
        (set-cdr! p value)
        (test-result-alist! runner (cons (cons pname value) alist)))))

(define (test-result-clear runner)
  (test-result-alist! runner '()))

(define (test-result-remove runner pname)
  (let* ((alist (test-result-alist runner))
         (p (assq pname alist)))
    (when p
      (test-result-alist! runner
                          (let loop ((r alist))
                            (if (eq? r p) (cdr r)
                                (cons (car r) (loop (cdr r)))))))))

(define (test-result-kind . rest)
  (let ((runner (if (pair? rest) (car rest) (test-runner-current))))
    (test-result-ref runner 'result-kind)))

(define (test-passed? . rest)
  (let ((runner (if (pair? rest) (car rest) (test-runner-get))))
    (memq (test-result-ref runner 'result-kind) '(pass xpass))))

(define (%test-report-result)
  (let* ((r (test-runner-get))
         (result-kind (test-result-kind r)))
    (case result-kind
      ((pass)
       (test-runner-pass-count! r (+ 1 (test-runner-pass-count r))))
      ((fail)
       (test-runner-fail-count! r (+ 1 (test-runner-fail-count r))))
      ((xpass)
       (test-runner-xpass-count! r (+ 1 (test-runner-xpass-count r))))
      ((xfail)
       (test-runner-xfail-count! r (+ 1 (test-runner-xfail-count r))))
      (else
       (test-runner-skip-count! r (+ 1 (test-runner-skip-count r)))))
    (%test-runner-total-count! r (+ 1 (%test-runner-total-count r)))
    ((test-runner-on-test-end r) r)))

(define-syntax false-if-error
  (syntax-rules ()
    ((_ expression)
     (guard (err (else #f)) expression))))

(cond-expand
 ((or kawa mzscheme)
  (cond-expand
   (mzscheme
    (define-for-syntax (%test-syntax-file form)
      (let ((source (syntax-source form)))
        (cond ((string? source) file)
              ((path? source) (path->string source))
              (else #f)))))
   (kawa
    (define (%test-syntax-file form)
      (syntax-source form))))
  (define (%test-source-line2 form)
    (let* ((line (syntax-line form))
           (file (%test-syntax-file form))
           (line-pair (if line (list (cons 'source-line line)) '())))
      (cons (cons 'source-form (syntax-object->datum form))
            (if file (cons (cons 'source-file file) line-pair) line-pair)))))
 (guile-2
  (define (%test-source-line2 form)
    (let* ((src-props (syntax-source form))
           (file (and src-props (assq-ref src-props 'filename)))
           (line (and src-props (assq-ref src-props 'line)))
           (file-alist (if file
                           `((source-file . ,file))
                           '()))
           (line-alist (if line
                           `((source-line . ,(+ line 1)))
                           '())))
      (datum->syntax (syntax here)
                     `((source-form . ,(syntax->datum form))
                       ,@file-alist
                       ,@line-alist)))))
 (else
  (define (%test-source-line2 form)
    '())))

(define (%test-on-test-begin r)
  (%test-should-execute r)
  ((test-runner-on-test-begin r) r)
  (not (eq? 'skip (test-result-ref r 'result-kind))))

(define (%test-on-test-end r result)
  (test-result-set! r 'result-kind
                    (if (eq? (test-result-ref r 'result-kind) 'xfail)
                        (if result 'xpass 'xfail)
                        (if result 'pass 'fail))))

(define (test-runner-test-name runner)
  (test-result-ref runner 'test-name ""))

(define-syntax %test-comp2body
  (syntax-rules ()
    ((%test-comp2body r comp expected expr)
     (begin
       (when (%test-on-test-begin r)
         (let ((exp expected))
           (test-result-set! r 'expected-value exp)
           (let ((res (false-if-error expr)))
             (test-result-set! r 'actual-value res)
             (%test-on-test-end r (comp exp res)))))
       (%test-report-result)))))

(define (%test-approximate= error)
  (lambda (value expected)
    (let ((rval (real-part value))
          (ival (imag-part value))
          (rexp (real-part expected))
          (iexp (imag-part expected)))
      (and (>= rval (- rexp error))
           (>= ival (- iexp error))
           (<= rval (+ rexp error))
           (<= ival (+ iexp error))))))

(define-syntax %test-comp1body
  (syntax-rules ()
    ((%test-comp1body r expr)
     (begin
       (when (%test-on-test-begin r)
         (let ((res (false-if-error expr)))
           (test-result-set! r 'actual-value res)
           (%test-on-test-end r res)))
       (%test-report-result)))))

(cond-expand
 ((or kawa mzscheme guile-2)
  ;; Should be made to work for any Scheme with syntax-case
  ;; However, I haven't gotten the quoting working.  FIXME.
  (define-syntax test-end
    (lambda (x)
      (syntax-case (list x (list (syntax quote) (%test-source-line2 x))) ()
        (((mac suite-name) line)
         (syntax
          (%test-end suite-name line)))
        (((mac) line)
         (syntax
          (%test-end #f line))))))
  (define-syntax test-assert
    (lambda (x)
      (syntax-case (list x (list (syntax quote) (%test-source-line2 x))) ()
        (((mac tname expr) line)
         (syntax
          (let* ((r (test-runner-get))
                 (name tname))
            (test-result-alist! r `((test-name . ,name) . ,line))
            (%test-comp1body r expr))))
        (((mac expr) line)
         (syntax
          (let* ((r (test-runner-get)))
            (test-result-alist! r line)
            (%test-comp1body r expr)))))))
  (define (%test-comp2 comp x)
    (syntax-case (list x (list (syntax quote) (%test-source-line2 x)) comp) ()
      (((mac tname expected expr) line comp)
       (syntax
        (let* ((r (test-runner-get))
               (name tname))
          (test-result-alist! r `((test-name . ,name) . ,line))
          (%test-comp2body r comp expected expr))))
      (((mac expected expr) line comp)
       (syntax
        (let* ((r (test-runner-get)))
          (test-result-alist! r line)
          (%test-comp2body r comp expected expr))))))
  (define-syntax test-eqv
    (lambda (x) (%test-comp2 (syntax eqv?) x)))
  (define-syntax test-eq
    (lambda (x) (%test-comp2 (syntax eq?) x)))
  (define-syntax test-equal
    (lambda (x) (%test-comp2 (syntax equal?) x)))
  (define-syntax test-approximate ;; FIXME - needed for non-Kawa
    (lambda (x)
      (syntax-case (list x (list (syntax quote) (%test-source-line2 x))) ()
        (((mac tname expected expr error) line)
         (syntax
          (let* ((r (test-runner-get))
                 (name tname))
            (test-result-alist! r `((test-name . ,name) . ,line))
            (%test-comp2body r (%test-approximate= error) expected expr))))
        (((mac expected expr error) line)
         (syntax
          (let* ((r (test-runner-get)))
            (test-result-alist! r line)
            (%test-comp2body r (%test-approximate= error) expected expr))))))))
 (else
  (define-syntax test-end
    (syntax-rules ()
      ((test-end)
       (%test-end #f '()))
      ((test-end suite-name)
       (%test-end suite-name '()))))
  (define-syntax test-assert
    (syntax-rules ()
      ((test-assert tname test-expression)
       (let* ((r (test-runner-get))
              (name tname))
         (test-result-alist! r `((test-name . ,name)))
         (%test-comp1body r test-expression)))
      ((test-assert test-expression)
       (let* ((r (test-runner-get)))
         (test-result-alist! r '())
         (%test-comp1body r test-expression)))))
  (define-syntax %test-comp2
    (syntax-rules ()
      ((%test-comp2 comp tname expected expr)
       (let* ((r (test-runner-get))
              (name tname))
         (test-result-alist! r `((test-name . ,name)))
         (%test-comp2body r comp expected expr)))
      ((%test-comp2 comp expected expr)
       (let* ((r (test-runner-get)))
         (test-result-alist! r '())
         (%test-comp2body r comp expected expr)))))
  (define-syntax test-equal
    (syntax-rules ()
      ((test-equal . rest)
       (%test-comp2 equal? . rest))))
  (define-syntax test-eqv
    (syntax-rules ()
      ((test-eqv . rest)
       (%test-comp2 eqv? . rest))))
  (define-syntax test-eq
    (syntax-rules ()
      ((test-eq . rest)
       (%test-comp2 eq? . rest))))
  (define-syntax test-approximate
    (syntax-rules ()
      ((test-approximate tname expected expr error)
       (%test-comp2 (%test-approximate= error) tname expected expr))
      ((test-approximate expected expr error)
       (%test-comp2 (%test-approximate= error) expected expr))))))

(cond-expand
 (guile
  (define-syntax %test-error
    (syntax-rules ()
      ((%test-error r etype expr)
       (cond ((%test-on-test-begin r)
              (let ((et etype))
                (test-result-set! r 'expected-error et)
                (%test-on-test-end
                 r (catch #t
                     (lambda ()
                       (test-result-set! r 'actual-value expr)
                       #f)
                     (lambda (key . args)
                       ;; TODO: decide how to specify expected
                       ;; error types for Guile.
                       (test-result-set! r 'actual-error
                                         (cons key args))
                       #t)))
                (%test-report-result))))))))
 (mzscheme
  (define-syntax %test-error
    (syntax-rules ()
      ((%test-error r etype expr)
       (%test-comp1body
        r (with-handlers (((lambda (h) #t) (lambda (h) #t)))
                         (begin
                           (test-result-set! r 'actual-value expr)
                           #f)))))))
 (chicken
  (define-syntax %test-error
    (syntax-rules ()
      ((%test-error r etype expr)
       (%test-comp1body r (condition-case expr (ex () #t)))))))
 (kawa
  (define-syntax %test-error
    (syntax-rules ()
      ((%test-error r #t expr)
       (cond ((%test-on-test-begin r)
              (test-result-set! r 'expected-error #t)
              (%test-on-test-end
               r (try-catch
                  (begin
                    (test-result-set! r 'actual-value expr)
                    #f)
                  (ex <java.lang.Throwable>
                      (test-result-set! r 'actual-error ex)
                      #t)))
              (%test-report-result))))
      ((%test-error r etype expr)
       (if (%test-on-test-begin r)
           (let ((et etype))
             (test-result-set! r 'expected-error et)
             (%test-on-test-end
              r (try-catch
                 (begin
                   (test-result-set! r 'actual-value expr)
                   #f)
                 (ex <java.lang.Throwable>
                     (test-result-set! r 'actual-error ex)
                     (cond ((and (instance? et <gnu.bytecode.ClassType>)
                                 (gnu.bytecode.ClassType:isSubclass
                                  et <java.lang.Throwable>))
                            (instance? ex et))
                           (else #t)))))
             (%test-report-result)))))))
 (else
  (define-syntax %test-error
    (syntax-rules ()
      ((%test-error r etype expr)
       (%test-comp1body
        r (guard (ex ((condition-type? etype)
                      (and (condition? ex) (condition-has-type? ex etype)))
                     ((procedure? etype)
                      (etype ex))
                     ((equal? etype #t)
                      #t)
                     (else #t))
            expr #f)))))))

(cond-expand
 ((or kawa mzscheme guile-2)
  (define-syntax test-error
    (lambda (x)
      (syntax-case (list x (list (syntax quote) (%test-source-line2 x))) ()
        (((mac tname etype expr) line)
         (syntax
          (let* ((r (test-runner-get))
                 (name tname))
            (test-result-alist! r `((test-name . ,name) . ,line))
            (%test-error r etype expr))))
        (((mac etype expr) line)
         (syntax
          (let* ((r (test-runner-get)))
            (test-result-alist! r line)
            (%test-error r etype expr))))
        (((mac expr) line)
         (syntax
          (let* ((r (test-runner-get)))
            (test-result-alist! r line)
            (%test-error r #t expr))))))))
 (else
  (define-syntax test-error
    (syntax-rules ()
      ((test-error name etype expr)
       (let ((r (test-runner-get)))
         (test-result-alist! r `((test-name . ,name)))
         (%test-error r etype expr)))
      ((test-error etype expr)
       (let ((r (test-runner-get)))
         (test-result-alist! r '())
         (%test-error r etype expr)))
      ((test-error expr)
       (let ((r (test-runner-get)))
         (test-result-alist! r '())
         (%test-error r #t expr)))))))

(define (test-apply first . rest)
  (if (test-runner? first)
      (test-with-runner first (apply test-apply rest))
      (let ((r (test-runner-current)))
        (if r
            (let ((run-list (%test-runner-run-list r)))
              (cond ((null? rest)
                     (%test-runner-run-list! r (reverse run-list))
                     (first)) ;; actually apply procedure thunk
                    (else
                     (%test-runner-run-list!
                      r
                      (if (eq? run-list #t) (list first) (cons first run-list)))
                     (apply test-apply rest)
                     (%test-runner-run-list! r run-list))))
            (let ((r (test-runner-create)))
              (test-with-runner r (apply test-apply first rest))
              ((test-runner-on-final r) r))))))

(define-syntax test-with-runner
  (syntax-rules ()
    ((test-with-runner runner form ...)
     (let ((saved-runner (test-runner-current)))
       (dynamic-wind
         (lambda () (test-runner-current runner))
         (lambda () form ...)
         (lambda () (test-runner-current saved-runner)))))))

;;; Predicates

(define (%test-match-nth n count)
  (let ((i 0))
    (lambda (runner)
      (set! i (+ i 1))
      (and (>= i n) (< i (+ n count))))))

(define-syntax test-match-nth
  (syntax-rules ()
    ((test-match-nth n)
     (test-match-nth n 1))
    ((test-match-nth n count)
     (%test-match-nth n count))))

(define (%test-match-all . pred-list)
  (lambda (runner)
    (let ((result #t))
      (let loop ((l pred-list))
        (if (null? l)
            result
            (begin
              (unless ((car l) runner)
                (set! result #f))
              (loop (cdr l))))))))

(define-syntax test-match-all
  (syntax-rules ()
    ((test-match-all pred ...)
     (%test-match-all (%test-as-specifier pred) ...))))

(define (%test-match-any . pred-list)
  (lambda (runner)
    (let ((result #f))
      (let loop ((l pred-list))
        (if (null? l)
            result
            (begin
              (when ((car l) runner)
                (set! result #t))
              (loop (cdr l))))))))

(define-syntax test-match-any
  (syntax-rules ()
    ((test-match-any pred ...)
     (%test-match-any (%test-as-specifier pred) ...))))

;; Coerce to a predicate function:
(define (%test-as-specifier specifier)
  (cond ((procedure? specifier) specifier)
        ((integer? specifier) (test-match-nth 1 specifier))
        ((string? specifier) (test-match-name specifier))
        (else
         (error "not a valid test specifier"))))

(define-syntax test-skip
  (syntax-rules ()
    ((test-skip pred ...)
     (let ((runner (test-runner-get)))
       (%test-runner-skip-list!
        runner (cons (test-match-all (%test-as-specifier pred) ...)
                     (%test-runner-skip-list runner)))))))

(define-syntax test-expect-fail
  (syntax-rules ()
    ((test-expect-fail pred ...)
     (let ((runner (test-runner-get)))
       (%test-runner-fail-list!
        runner (cons (test-match-all (%test-as-specifier pred) ...)
                     (%test-runner-fail-list runner)))))))

(define (test-match-name name)
  (lambda (runner)
    (equal? name (test-runner-test-name runner))))

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
