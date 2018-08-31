(in-package :clox)

;; Since this is CL version and not Java,
;; we have to define different bijection between the types
;; We cannot treat Lox's nil the same as the CL's nil because
;; Lox's nil does not imply false, whereas CL's does.
;; Lox type - CL type
;; nil - null (equivalent to nil)
;; boolean - Boolean (T + NIL)
;; number - Number
;; string - String

