(define-library (srfi 63)
  (export
   array?
   equal?
   array-rank
   array-dimensions
   make-array
   make-shared-array
   list->array
   array->list
   vector->array
   array->vector
   array-in-bounds?
   array-ref
   array-set!
   a:floc128b
   a:floc64b
   a:floc32b
   a:floc16b
   a:flor128b
   a:flor64b
   a:flor32b
   a:flor16b
   a:fixz64b
   a:fixz32b
   a:fixz16b
   a:fixz8b
   a:fixn64b
   a:fixn32b
   a:fixn16b
   a:fixn8b
   a:bool
   )
  (import (except (scheme base) equal?))
  (include "63.body.scm"))
