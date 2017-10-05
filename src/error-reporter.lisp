(in-package :clox)

(define-condition clox-error (error)
  ((line :initarg :line :reader line)
   (message :reader message))
  (:report (lambda (err stream)
             (format stream "[line ~D] ~A: ~A~%" (line err) (clox-error-name err) (message err)))))

(defmethod initialize-instance :after ((obj clox-error) &key &allow-other-keys)
  (when (eq (class-of obj) (find-class 'clox-error))
    (error "~S is an abstract class and can't be instantiated." obj)))

(defmethod clox-error-name ((obj clox-error))
  (string-capitalize (type-of obj)))

(define-condition unexpected-character-error (clox-error)
  ((message :initform "Unexpected character found." )))

(define-condition unterminated-string-error (clox-error)
  ((message :initform "The string has not been terminated.")))

(defmacro handle-scanner-errors (error-flag &body body)
  "Evaluate BODY, setting error-flag to T if any error is encountered and reporting it, but invoking CONTINUE."
  `(handler-bind ((clox-error
                    (lambda (condition)
                      (format *error-output* "~A" condition)
                      ,(if (eq error-flag nil) nil `(setf ,error-flag t))
                      (continue))))
     ,@body))
