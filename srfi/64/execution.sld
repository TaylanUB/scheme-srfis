(define-library (srfi 64 execution)
  (import
   (scheme base)
   (scheme case-lambda)
   (scheme complex)
   (scheme eval)
   (scheme read)
   (srfi 1)
   (srfi 35)
   (srfi 48)
   (srfi 64 test-runner)
   (srfi 64 source-info)
   ;; We don't use any bindings from test-runner-simple, but want it to insert
   ;; itself as the default test-runner-factory, which it does automatically
   ;; upon import.
   (srfi 64 test-runner-simple))
  (include-library-declarations "execution.exports.sld")
  (include "execution.body.scm"))
