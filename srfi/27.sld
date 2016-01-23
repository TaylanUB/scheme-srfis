(define-library (srfi 27)
  (export
   random-integer
   random-real
   default-random-source
   make-random-source
   random-source?
   random-source-state-ref
   random-source-state-set!
   random-source-randomize!
   random-source-pseudo-randomize!
   random-source-make-integers
   random-source-make-reals
   )
  (import
   (scheme base)
   (scheme time))
  (begin

    (define-record-type :random-source
      (:random-source-make
       state-ref
       state-set!
       randomize!
       pseudo-randomize!
       make-integers
       make-reals)
      :random-source?
      (state-ref :random-source-state-ref)
      (state-set! :random-source-state-set!)
      (randomize! :random-source-randomize!)
      (pseudo-randomize! :random-source-pseudo-randomize!)
      (make-integers :random-source-make-integers)
      (make-reals :random-source-make-reals))
    
    (define (:random-source-current-time)
      (current-jiffy))

    (define exact->inexact inexact)

    )
  (include "27.mrg32k3a-a.upstream.scm")
  (include "27.mrg32k3a.upstream.scm"))
