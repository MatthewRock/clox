(in-package :clox-utils)

;; ast-generator package
;; The book uses the Java tool that transforms some data into the code.
;; This is a hack that is the result of Java's lack of expressiveness.
;; Lisp has a built-in tool - macro - which solves this problem,, so we'll generate the DSL
;; within the Lisp.

;; The Java version utilizes the Java type system by creating class hierarchy,
;; which in turn allows for compile-time validation of certain operations.
;; CLOS isn't static (compiler might still infer some types and signal warnings/errors)
;; but I believe that proper class hierarchy would help during debugging.

;; The AST generator has to do N things:

;; 1. Take an AST as an input
;; 2. Generate a valid hierarchy of types as an output
;; 3. Provide static typing at compile time, at least partially
;; 4. Provide pretty printing of AST.

;; (defast Expr (:append-to :end)
;;   (Binary -> (Expr left) (Token operator) (Expr right))
;;   (Grouping -> (Expr expression))
;;   (Literal -> value) ; the value has the type T
;;   (Unary -> (Token operator) (Expr right)))

;; ->

;; (defclass Expr () ())

;; (defclass BinaryExpr (Expr)
;;   (left :type 'Expr)
;;   (operator :type 'Token)
;;   (right :type 'Expr))

;; (defun BinaryExpr (left operator right)
;;   (make-instance
;;    'BinaryExpr
;;    :left left
;;    :operator operator
;;    :right right))


;; TODO: Improve defast to be able to split ast definition to more files (?)

(-> base-class-definition (symbol) list)
(defun base-class-definition (name)
  "Return a definition of a base class for AST."
  ;; This class is purely for deriving.
  `(defclass ,name () ()))

(deftype naming-convention ()
  '(member
    :postfix                         ; Append the base class name to the end
    :prefix                          ; Append the base class name to the beginning.
    :none                            ; Don't append the base class name.
    ))

(defmacro defast (name (&key (naming-convention :postfix)) &rest rules)
  "Define class hierarchy for AST with name NAME.
\The AST class are defined basing on the RULES, which are of the form:
\(RuleName -> value1 value2) and so on. The values would have a type T assumed. Another allowed form is:
\(RuleName -> (value1 type1) (value2 type2)) and so on. This allows for explicit type declaration.
\Additionally, one might provide NAMING-CONVENTION (which by default is :postfix).
\The convention defines the name of the created classes - the base name
\can be appended to the resulting classes' names.
\By default, :POSTFIX is used, meaning that the resulting class name will be ResultingClassBaseClass."
  `(progn
     ,(base-class-definition name)
     ,@(loop
         for rule in rules
         collect
         (class-definition-from-rule rule :naming naming-convention
                                          :ast name)
         collect
         (class-constructor-from-rule rule :naming naming-convention
                                           :ast name))))

(-> class-definition-from-rule (list &key (:naming naming-convention) (:ast (or string symbol))) list)
(defun class-definition-from-rule (rule &key naming ast)
  "Return class definition for AST following the NAMING convention."
  (let ((class-name (class-name-from-rule rule :naming naming :ast ast)))
    `(defclass ,class-name (,ast)
       ,(class-slots-from-rule rule))))

(-> class-constructor-from-rule (list &key (:naming naming-convention) (:ast (or string symbol))) list)
(defun class-constructor-from-rule (rule &key naming ast)
  "Return a function with the name identical as a class generated from the rule,
which serves as a constructor for the class instance."
  (let ((class-name (class-name-from-rule rule :naming naming :ast ast))
        (class-slot-names (class-slot-names-from-rule rule)))
    `(defun ,class-name ,class-slot-names
       (make-instance ',class-name ,@(make-arglist-for-args class-slot-names) ))))

(-> make-arglist-for-args (list) list)
(defun make-arglist-for-args (args)
  "Accept list of argument names. Return list of :arg1 arg1 :arg2 arg2 ..."
  (loop for arg in args collect (make-keyword arg) collect arg))

(-> class-slot-names-from-rule (list) list)
(defun class-slot-names-from-rule (rule)
  "Return the list of slot names for class formed from the rule RULE."
  (mapcar #'car (class-slots-from-rule rule)))

;; TODO: Change ecase to custom error
(-> class-name-from-rule (list &key (naming naming-convention) (ast (or string symbol))) symbol)
(defun class-name-from-rule (rule &key naming ast)
  "Return a class name for RULE that belongs to AST created according to the NAMING convention"
  (let ((base-class-name ast)
        (rule-class-name (car rule)))
    (ecase naming
      ((:none) rule-class-name)
      ((:postfix) (intern (format nil "~@:(~A~A~)" rule-class-name base-class-name)))
      ((:prefix)  (intern (format nil "~@:(~A~A~)" base-class-name rule-class-name))))))

(-> class-slots-from-rule (list) list)
(defun class-slots-from-rule (rule)
  "Create a list of slots for a rule class defined by RULE."
  (let* ((maybe-with-arrow-rule-definition (cdr rule))
         (rule-definitions (if (equal (car maybe-with-arrow-rule-definition)
                                      '->)
                               (cdr maybe-with-arrow-rule-definition)
                               maybe-with-arrow-rule-definition)))
    (loop for definition in rule-definitions collecting (class-slot-from-definition definition))))


;; TODO: Change etypecase to custom error
(-> class-slot-from-definition ((or list symbol)) list)
(defun class-slot-from-definition (definition)
  (etypecase definition
    (symbol `(,definition :accessor ,definition :initarg ,(make-keyword definition)))
    (list (let ((definition-name (car definition))
                (definition-type (cadr definition)))
            `(,definition-name :accessor ,definition-name :initarg ,(make-keyword definition-name)
                               :type ,definition-type)))))

(-> make-keyword (symbol) keyword)
(defun make-keyword (symbol)
  (intern (string symbol) :keyword))
