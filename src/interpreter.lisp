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

(defgeneric evaluate (expression)
  (:documentation "Evaluate THING."))

(defgeneric is-truthy (thing)
  (:documentation "Return T if the THING is truthy, NIL otherwise."))

(defgeneric add (left right)
  (:documentation "Add LEFT and RIGHT, operation depending on the type."))

(defmethod add ((left string) (right string))
  (concatenate 'string left right))

(defmethod add ((left number) (right number))
  (+ left right))

;; Purely cosmetic for now
;; Will hold environment in the future
(defclass interpreter () ())

;; NULL, NIL and false all evaluate to NIL and are the only false values for Lox.
(defmethod is-truthy ((thing (eql nil)))
  nil)
;; Everything else is truthy.
(defmethod is-truthy (thing)
  t)

(defmethod evaluate ((expression literal-expr))
  (value expression))

(defmethod evaluate ((expression unary-expr))
  (let ((right (evaluate (right expression))))
    (case (token-type (operator expression))
      (:minus (- right))
      (:bang (not (is-truthy right))))))

(defmethod evaluate ((expression binary-expr))
  (let ((left (evaluate (left expression)))
        (right (evaluate (right expression))))
    (case (token-type (operator expression))
      (:minus (- left right))
      (:plus (add left right))
      (:slash (/ left right))
      (:star (* left right))
      (:greater (> left right))
      (:greater-equal (>= left right))
      (:less (< left right))
      (:less-equal (<= left right))
      (:bang-equal (not (equal left right)))
      (:equal-equal (equal left right)))))
