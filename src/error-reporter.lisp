(in-package :clox)

(define-condition keyword-argument-missing-error (error)
  ((field-name :initarg :field-name :reader field-name))
  (:report (lambda (err stream)
             (format stream "Keyword argument ~A is required, but has not been supplied."
                     (field-name err)))))

(define-condition clox-error (error)
  ((line :initarg :line :reader line)
   (message :reader clox-error-message)
   (place :reader place :initform -1 :initarg :place))
  (:report (lambda (err stream)
             (format stream "[line ~D : ~A] ~A: ~A~%" (line err) (place err)
                     (clox-error-name err) (clox-error-message err)))))

(defmethod initialize-instance :after ((obj clox-error) &key &allow-other-keys)
  (when (eq (class-of obj) (find-class 'clox-error))
    (error "~S is an abstract class and can't be instantiated." obj)))

(defmethod clox-error-name ((obj clox-error))
  (string-capitalize (type-of obj)))

(define-condition unexpected-character-error (clox-error)
  ((character :initform "<unknown character>" :reader wrong-character :initarg :character)))

(defmethod clox-error-message ((obj unexpected-character-error))
  (format nil "Unexpected character: ~A." (wrong-character obj)))

(define-condition unterminated-string-error (clox-error)
  ((message :initform "The string has not been terminated.")))

(define-condition clox-parser-error (clox-error)
  ;; Add initarg for message
  ((message :initarg :message)))

(defmacro handle-scanner-errors (error-flag &body body)
  "Evaluate BODY, setting error-flag to T if any error is encountered and reporting it, but invoking CONTINUE."
  `(handler-bind ((clox-error
                    (lambda (condition)
                      (format *error-output* "~A" condition)
                      ,(if (eq error-flag nil) nil `(setf ,error-flag t))
                      ;; Invoke CONTINUE if it exists.
                      (let ((continue-restart (find-restart 'continue)))
                        (when continue-restart
                          (invoke-restart continue-restart))))))
     ,@body))

(defmacro handle-parser-errors (&body body)
  `(handler-bind ((clox-parser-error
                    (lambda (condition)
                      (format *error-output* "~A" condition)
                      ;; Invoke IGNORE if it exists.
                      (let ((restart (find-restart 'ignore)))
                        (when restart
                          (invoke-restart restart))))))
     ,@body))
