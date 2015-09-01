(define-library (srfi 64)
  (import
   (srfi 64 test-runner)
   (srfi 64 test-runner-simple)
   (srfi 64 execution))
  (include-library-declarations "64/test-runner.exports.sld")
  (include-library-declarations "64/test-runner-simple.exports.sld")
  (include-library-declarations "64/execution.exports.sld")
  (begin
    (test-runner-factory test-runner-simple)))
