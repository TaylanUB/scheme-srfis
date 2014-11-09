;;; array as-srfi-9-record
;;; 2001 Jussi Piitulainen

;;; Untested.

(define-record-type
 array:srfi-9-record-type-descriptor
 (array:make vec ind shp)
 array:array?
 (vec array:vector)
 (ind array:index)
 (shp array:shape))
