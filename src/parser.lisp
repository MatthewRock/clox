(in-package :clox)

(define-condition clox-parser-error (clox-error)
  ;; Add initarg for message
  ((message :initarg :message)))

(defun clox-parser-error (&key (token (error 'keyword-argument-missing-error
                                             :field-name 'token))
                            message)

  (error 'clox-parser-error :line (token-line token)
                            :place (if (eql :eof (token-type token))
                                       "at the end"
                                       (format nil "at '~A'" (token-lexeme token)))
                            :message message))

(defclass Parser ()
  ((%tokens :type vector :initarg :tokens :accessor tokens)
   (%current-position :initarg :current-position :type integer :accessor current-position))
  (:default-initargs
   :current-position 0))


(-> parse (parser) (or expr nil))
(defun parse (parser)
  (handler-case (expression parser)
    (clox-parser-error () nil)))

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
      (parser-advance parser)
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

(defrule (expression)
  (equality parser))

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
    ((match :left-paren) (let ((expr (expression parser)))
                           (consume :right-paren "Expect ')' after an expression.")
                           (grouping-expr expr)))))