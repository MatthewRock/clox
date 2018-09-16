(in-package :clox)

;; Since this is CL version and not Java,
;; we have to define different bijection between the types
;; We will treat Lox's nil the same as the CL's nil because
;; Lox's nil does imply false, same as  CL's.
;; Lox type - CL type
;; nil - null (equivalent to nil)
;; boolean - Boolean (T + NIL)
;; number - Number
;; string - String

(defgeneric evaluate (interpreter thing)
  (:documentation "Evaluate THING using INTERPRETER."))

(defgeneric is-truthy (thing)
  (:documentation "Return T if the THING is truthy, NIL otherwise."))

(defgeneric add (operator left right)
  (:documentation "Add LEFT and RIGHT, operation depending on the type. OPERATOR is passed to provide meaningful error in case of type error."))

(defmethod add (operator left right)
  (error 'clox-runtime-error :token operator
                             :message (format nil
                                              "Both operands have to be either number or string.~%Left=~S ; Right = ~S"
                                              left right)))

(defmethod add (operator (left string) (right string))
  (concatenate 'string left right))

(defmethod add (operator (left number) (right number))
  (+ left right))

(defmethod add (operator (left string) (right number))
  (add left (stringify right)))

(defmethod add (operator (left number) (right string))
  (add (stringify left) right))

(defgeneric check-number-operand (operator right)
  (:documentation "Signal runtime error if right is not a number."))

(defmethod check-number-operand (operator right)
  (error 'clox-runtime-error :token operator
                             :message (format nil "Right operand is not a number : ~S" right)))

(defmethod check-number-operand (operator (right number))
  t)

(defgeneric check-number-operands (operator left right)
  (:documentation "Signal runtime error if any of the operands is not a number."))

(defmethod check-number-operands (operator (left number) (right number))
  t)

(defmethod check-number-operands (operator (left number) right)
  (error 'clox-runtime-error :token operator
                             :message (format nil "Left operand is not a number.~%Left=~S" left)))

(defmethod check-number-operands (operator left (right number))
  (error 'clox-runtime-error :token operator
                             :message (format nil "Right operand is not a number.~%Right=~S" right) ))

(defmethod check-number-operands (operator left right)
  (error 'clox-runtime-error :token operator
                             :message (format nil
                                              "Left and right operand are not a number.~%Left = ~S ; Right = ~S"
                                              left right)))

;; TODO: Ask on the IRC: Why do I have to define it BEFORE the method?

(defmacro typed-binary-expression-case (operator left right &body clauses)
  "Call proper function provided by CLAUSES depending on the OPERATOR (its token). Clauses look like (TOKEN-NAME FUNCTION-NAME). The names of the LEFT and RIGHT are passed as arguments.

Types:
operator - Token
left - Symbol
right - Symbol
clauses - list (keyword symbol)"
  `(case (token-type ,operator)
     ,@(loop for (token-name function-name) in clauses
             collecting `(,token-name
                          (check-number-operands ,operator ,left ,right)
                          (,function-name ,left, right)))))

(defclass interpreter ()
  ((environment :initarg :environment :accessor environment))
  (:default-initargs :environment (make-instance 'environment)))

;; NULL, NIL and false all evaluate to NIL and are the only false values for Lox.
(defmethod is-truthy ((thing (eql nil)))
  nil)
;; Everything else is truthy.
(defmethod is-truthy (thing)
  t)

(defmethod evaluate (_ (expression literal-expr))
  (value expression))

(defmethod evaluate ((interpreter interpreter) (expression unary-expr))
  (let* ((right (evaluate interpreter (right expression)))
         (operator (operator expression))
         (operator-type (token-type operator)))
    (case operator-type
      (:minus
       (check-number-operand operator right)
       (- right))
      (:bang (not (is-truthy right))))))

(defmethod evaluate ((interpreter interpreter) (expression binary-expr))
  (let* ((left (evaluate interpreter (left expression)))
         (right (evaluate interpreter (right expression)))
         (operator (operator expression))
         (operator-type (token-type operator)))
    (handler-case
        (or
         (typed-binary-expression-case operator left right
           (:minus -)
           (:slash /)
           (:star *)
           (:greater >)
           (:greater-equal >=)
           (:less <)
           (:less-equal <=))
         (ecase operator-type
           (:plus (add operator left right))
           (:bang-equal (not-equal left right))
           (:equal-equal (equal left right))
           (:comma right)))
      (division-by-zero () (error 'clox-runtime-error :token operator :message "Division by zero.")))))

(defmethod evaluate ((interpreter interpreter) (expression ternary-expr))
  (ecase (token-type (operator expression))
    (:question-mark (if (evaluate interpreter (question expression))
                        (evaluate interpreter (result-true expression))
                        (evaluate interpreter (result-false expression))))))

(defmethod evaluate ((interpreter interpreter) (statement expression-stmt))
  (progn
    (evaluate interpreter (expression statement))
    nil))

(defmethod evaluate ((interpreter interpreter) (statement print-stmt))
  (format t "~A" (stringify (evaluate interpreter (expression statement))))
  (force-output)
  nil)

(defmethod evaluate ((interpreter interpreter) (statement var-stmt))
  (let ((value (evaluate interpreter (initializer statement)))
        (name (token-lexeme (name statement))))
    (define-variable (environment interpreter) name value)
    nil))

(defmethod evaluate ((interpreter interpreter) (expression variable-expr))
  (get-variable (environment interpreter) (name expression)))

(defgeneric stringify (thing)
  (:documentation "Convert THING to printable form (does not have to be string)"))

(defmethod stringify (thing)
  thing)

(defmethod stringify ((thing ratio))
  (coerce thing 'double-float))

(defmethod stringify ((thing double-float))
  (multiple-value-bind (num reminder) (floor thing)
    (if (zerop reminder)
        num
        thing)))

(-> interpret (interpreter list) t)
(defun interpret (interpreter statements)
  (loop for statement in statements do (evaluate interpreter statement)))
