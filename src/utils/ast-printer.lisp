(in-package :clox-utils)

(defclass ast-printer () ())

(defclass ugly-ast-printer (ast-printer) ())

(defgeneric pretty-print (printer thing)
  (:documentation "Pretty print a THING using a PRINTER's way to do it."))

(defgeneric parenthesize (printer thing)
  (:documentation "Convert THING into parnethesized form using a PRINTER's way to do it."))

(defast Expr ()
        (Binary -> (left Expr) (operator clox::Token) (right Expr))
        (Grouping -> (expression Expr))
        (Literal -> value)
        (Unary -> (operator clox::Token) (right Expr)))

(defmethod pretty-print ((printer ugly-ast-printer) thing)
  (format t "~A" (parenthesize printer thing)))

(defmethod parenthesize ((printer ugly-ast-printer) (thing BinaryExpr))
  (format nil "(~A ~A ~A)"
          (clox::token-lexeme
           (operator thing))
          (parenthesize printer (left thing))
          (parenthesize printer (right thing))))

(defmethod parenthesize ((printer ugly-ast-printer) (thing GroupingExpr))
  (format nil "(group ~A)" (parenthesize printer (expression thing))))

(defmethod parenthesize ((printer ugly-ast-printer) (thing LiteralExpr))
  (format nil "~A" (value thing)))

(defmethod parenthesize ((printer ugly-ast-printer) (thing UnaryExpr))
  (format nil "(~A ~A)" (clox::token-lexeme (operator thing)) (parenthesize printer (right thing))))

(defmethod parenthesize ((printer ugly-ast-printer) thing)
  (format nil "~A" thing))

;; (pretty-print
;;  (make-instance 'ugly-ast-printer)
;;  (binaryexpr
;;   (unaryexpr
;;    (make-instance 'clox::token
;;                   :type :minus
;;                   :lexeme "-"
;;                   :literal nil
;;                   :line 1)
;;    (literalexpr 123))
;;   (make-instance 'clox::token
;;                  :type :star
;;                  :lexeme "*"
;;                  :literal nil
;;                  :line 1)
;;   (groupingexpr
;;    (literalexpr 45.67))))
