(in-package :clox)

;; TODO: Change advance, match etc. to be generic functions, implement as methods
;; for parser and scanner.

(defun clox-parser-error (&key (token (error 'keyword-argument-missing-error
                                             :field-name 'token))
                            message)

  (restart-case
      (error 'clox-parser-error :line (token-line token)
                                :place (if (eql :eof (token-type token))
                                           "at the end"
                                           (format nil "at '~A'" (token-lexeme token)))
                                :message message)
    (ignore () "Ignore the error and continue parsing.")))

(defclass Parser ()
  ((%tokens :type vector :initarg :tokens :accessor tokens)
   (%current-position :initarg :current-position :type integer :accessor current-position))
  (:default-initargs
   :current-position 0))

(-> parse (parser) list)

(defun parse (parser)
  (loop until (is-at-end parser) collecting (clox-declaration parser)))

;; TODO: Revise the architecture to get rid of passing parser everywhere.

(defmacro defrule ((name &optional (parser-name 'parser)) &body body)
  `(defmethod ,name ((,parser-name Parser))
     (flet ((previous ()
              (parser-previous ,parser-name))
            (match (&rest args)
              (parser-match ,parser-name args))
            (consume (token-type message)
              (parser-consume ,parser-name token-type message)))
       ,@body)))

(-> parser-current (parser) token)
(defun parser-current (parser)
  (aref (tokens parser) (current-position parser)))

(defmethod is-at-end ((parser Parser))
  (eql (token-type (parser-current parser)) :eof))

(-> parser-advance (parser) token)
(defun parser-advance (parser)
  (unless (is-at-end parser)
    (incf (current-position parser)))
  (parser-current parser))

(-> parser-previous (parser) token)
(defun parser-previous (parser)
  (aref (tokens parser) (1- (current-position parser))))

(-> parser-check (parser token-type) boolean)
(defun parser-check (parser token-type)
  (log4cl:log-info "Checking for ~A, and token: ~A" token-type
                   (token-type (parser-current parser)))
  (and (not (is-at-end parser))
       (equal token-type (token-type (parser-current parser)))))

(-> parser-match (parser list) boolean)
(defun parser-match (parser args)
  (when (some (alexandria:curry #'parser-check parser) args)
    (and (parser-advance parser) t)))

(-> parser-consume (parser token-type string) token)
(defun parser-consume (parser token-type message)
  (if (parser-check parser token-type)
      (progn
        (parser-advance parser)
        (parser-previous parser))
      (clox-parser-error
             :token (parser-current parser)
             :message message)))

(-> parser-synchronize (parser) nil)
(defun parser-synchronize (parser)
  ;; Discard the erroneous token
  (parser-advance parser)

  ;; Discard tokens until the end of the statement:
  (loop until (or
               (is-at-end parser)       ; statement ends at EOF
               (equal (token-type (parser-previous parser)) :semicolon) ; or at semicolon
               ;; There might be no semicolon, so we also stop at the next keyword.
               (some (alexandria:curry #'equal (token-type (parser-current parser)))
                     (list :class :fun :var
                           :for :if :while
                           :print :return)))
        do (parser-advance parser)))

;; Clox-declaration is used because "declaration" is a reserved keyword.
(defrule (clox-declaration)
  (handler-case (if (match :var)
                    (var-declaration parser)
                    (statement parser))
    (clox-parser-error () (progn
                            (parser-synchronize parser)
                            nil))))

(defrule (var-declaration)
  (let ((name (consume :identifier "Expected a variable name."))
        (initializer (if (match :equal)
                         (expression parser)
                         nil)))
    (consume :semicolon "Expected ';' after a variable declaration")
    (var-stmt name initializer)))

(defrule (statement)
  (if (match :print)
      (print-statement parser)
      (expression-statement parser)))

(defrule (print-statement)
  ;; Print-stmt does not do anything but create an instance of a class
  ;; With the expression as its value.
  ;; Therefore it should not fail unless parsing expression fails too
  ;; (in which case we simply propagate the error)
  ;; But placing it over the expression rather than a whole prog1
  ;; saves us from passing a broken value to the print-stmt.
  (prog1
      (print-stmt (expression parser))
    (consume :semicolon "Expected ';' after a value.")))

(defrule (expression-statement)
  (prog1
      (expression-stmt (expression parser))
    (consume :semicolon "Expected ';' after an expression.")))

(defrule (expression)
  (ternary-expression parser))

(defrule (ternary-expression)
  (let ((expr (comma-expression parser)))
    (when (match :question-mark)
      (let ((question-operator (previous))
            (true-expr (ternary-expression parser))
            (comma-operator (consume :colon "Ternary operator clauses must be separated by a colon (':')"))
            (false-expr (ternary-expression parser)))
        (setf expr (ternary-expr expr question-operator
                                 true-expr comma-operator false-expr))))
    expr))

(defrule (comma-expression)
  (loop with expr = (equality parser)
        while (match :comma)
        for operator = (previous)
        for right = (equality parser) do
          (setf expr (binary-expr expr operator right))
        finally (return expr)))

(defrule (equality)
  (loop with expr = (comparison parser)
        while (match :bang-equal :equal-equal)
        for operator = (previous)
        for right = (comparison parser) do
          (setf expr (binary-expr expr operator right))
        finally (return expr)))

(defrule (comparison)
  (loop with expr = (addition parser)
        while (match :greater :greater-equal :less :less-equal)
        for operator = (previous)
        for right = (addition parser) do
          (setf expr (binary-expr expr operator right))
        finally (return expr)))

(defrule (addition)
  (loop with expr = (multiplication parser)
        while (match :minus :plus)
        for operator = (previous)
        for right = (multiplication parser) do
          (setf expr (binary-expr expr operator right))
        finally (return expr)))

(defrule (multiplication)
  (loop with expr = (unary parser)
        while (match :slash :star)
        for operator = (previous)
        for right = (unary parser) do
          (setf expr (binary-expr expr operator right))
        finally (return expr)))

(defrule (unary)
  (if (match :bang :minus)
      (let ((operator (previous))
            (right (unary parser)))
        (unary-expr operator right))
      (primary parser)))

(defrule (primary)
  (cond
    ((match :false) (literal-expr nil))
    ((match :true) (literal-expr t))
    ((match :nil) (literal-expr nil))
    ((match :number :string) (literal-expr (token-literal (previous))))
    ((match :identifier) (variable-expr (previous)))
    ((match :left-paren) (let ((expr (expression parser)))
                           (consume :right-paren "Expect ')' after an expression.")
                           (grouping-expr expr)))
    (t (let ((result (error-binary-operator parser)))
         ;; Might be NIL if we can't assign any error.
         result))))

(defrule (error-binary-operator)
  (or (error-comma-expression parser)
      (error-equality parser)
      (error-comparison parser)
      (error-addition parser)
      (error-multiplication parser)))

(defun missing-first-arg-in-binary-error (token message)
  (restart-case
      (clox-parser-error :token token
                         :message (format nil "~A with no left-hand side argument encountered." message))
    (ignore () :report "Ignore the error and continue parsing."
      nil)))

(defrule (error-comma-expression)
  (when (match :comma)
    (let ((operator (previous))
          (right (equality parser)))
      (missing-first-arg-in-binary-error operator "Comma expression")
      (broken-binary-expr operator right))))

(defrule (error-equality)
  (when (match :bang-equal :equal-equal)
    (let ((operator (previous))
          (right (comparison parser)))
      (missing-first-arg-in-binary-error operator "Equality expression")
      (broken-binary-expr operator right))))

(defrule (error-comparison)
  (when (match :greater :greater-equal :less :less-equal)
    (let ((operator (previous))
          (right (addition parser)))
      (missing-first-arg-in-binary-error operator "Comparison expression")
      (broken-binary-expr operator right))))

(defrule (error-addition)
  (when (match :minus :plus)
    (let ((operator (previous))
          (right (multiplication parser)))
      (missing-first-arg-in-binary-error operator "Addition expression")
      (broken-binary-expr operator right))))

(defrule (error-multiplication)
  (when (match :slash :star)
    (let ((operator (previous))
          (right (unary parser)))
      (missing-first-arg-in-binary-error operator "Multiplication expression")
      (broken-binary-expr operator right))))
