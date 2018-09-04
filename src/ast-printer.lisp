(in-package :clox)

(defclass ast-printer () ())

(defclass ugly-ast-printer (ast-printer) ())

(defgeneric pretty-print (printer thing)
  (:documentation "Pretty print a THING using a PRINTER's way to do it."))

(defgeneric parenthesize (printer thing)
  (:documentation "Convert THING into parnethesized form using a PRINTER's way to do it."))

(defmethod pretty-print ((printer ugly-ast-printer) thing)
  (format t "~A~%" (parenthesize printer thing))
  (force-output))

(defmethod parenthesize ((printer ugly-ast-printer) (thing ternary-expr))
  (format nil "(~A ~A (~A ~A ~A))"
          (token-lexeme (operator thing))
          (parenthesize printer (question thing))
          (token-lexeme (separator thing))
          (parenthesize printer (result-true thing))
          (parenthesize printer (result-false thing))))

(defmethod parenthesize ((printer ugly-ast-printer) (thing binary-expr))
  (format nil "(~A ~A ~A)"
          (token-lexeme
           (operator thing))
          (parenthesize printer (left thing))
          (parenthesize printer (right thing))))

(defmethod parenthesize ((printer ugly-ast-printer) (thing broken-binary-expr))
  (format nil "(BROKEN ~A ~A)"
          (token-lexeme
           (operator thing))
          (parenthesize printer (right thing))))

(defmethod parenthesize ((printer ugly-ast-printer) (thing grouping-expr))
  (format nil "(group ~A)" (parenthesize printer (expression thing))))

(defmethod parenthesize ((printer ugly-ast-printer) (thing literal-expr))
  (format nil "~A" (value thing)))

(defmethod parenthesize ((printer ugly-ast-printer) (thing unary-expr))
  (format nil "(~A ~A)" (token-lexeme (operator thing)) (parenthesize printer (right thing))))

(defmethod parenthesize ((printer ugly-ast-printer) thing)
  (format nil "~A" thing))

;; (pretty-print
;;  (make-instance 'ugly-ast-printer)
;;  (binaryexpr
;;   (unaryexpr
;;    (make-instance 'token
;;                   :type :minus
;;                   :lexeme "-"
;;                   :literal nil
;;                   :line 1)
;;    (literalexpr 123))
;;   (make-instance 'token
;;                  :type :star
;;                  :lexeme "*"
;;                  :literal nil
;;                  :line 1)
;;   (groupingexpr
;;    (literalexpr 45.67))))
