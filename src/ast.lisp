(in-package :clox)

;; Abbreviation for the "Expression"
(defast Expr (:naming-convention :lisp-postfix)
  (Ternary -> (question Expr) (operator Token) (result-true Expr) (separator Token) (result-false Expr))
  (Binary -> (left Expr) (operator Token) (right Expr))
  (Broken-Binary -> (operator Token) (right Expr))
  (Grouping -> (expression Expr))
  (Literal -> value)
  (Unary -> (operator Token) (right Expr))
  (Variable -> (name Token)))

;; Abbreviation for a "statement"
(defast stmt (:naming-convention :lisp-postfix)
  (Expression -> (expression Expr))
  (Print -> (expression Expr))
  (Var -> (name Token) (initializer Expr)))
