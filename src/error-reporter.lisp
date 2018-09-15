(in-package :clox)

;; Conditions

(define-condition keyword-argument-missing-error (error)
  ((field-name :initarg :field-name :reader field-name))
  (:report (lambda (err stream)
             (format stream "Keyword argument ~A is required, but has not been supplied."
                     (field-name err)))))

(define-condition clox-base-error (error) ())

(define-condition clox-error (clox-base-error)
  ((line :initarg :line :reader line)
   (message :reader clox-error-message)
   (place :reader place :initform -1 :initarg :place))
  (:report (lambda (err stream)
             (format stream "[line ~D : ~A] ~A: ~A~%" (line err) (place err)
                     (clox-error-name err) (clox-error-message err)))))

(define-condition unexpected-character-error (clox-error)
  ((character :initform "<unknown character>" :reader wrong-character :initarg :character)))

(define-condition unterminated-string-error (clox-error)
  ((message :initform "The string has not been terminated.")))

(define-condition clox-parser-error (clox-error)
  ;; Add initarg for message
  ((message :initarg :message)))

;; TODO: Make clox-parser-error and clox-runtime-error use the same methods
(define-condition clox-runtime-error (clox-error)
  ((token :initarg :token :reader token)
   (message :initarg :message))
  (:default-initargs
   :token (make-instance 'keyword-argument-missing-error :field-name 'token)))

;; Methods, macros, stuff.

(defmethod line ((err clox-runtime-error))
  (token-line (token err)))

(defmethod place ((err clox-runtime-error))
  (let ((token (token err)))
    (if (eql :eof (token-type token))
       "at the end"
       (format nil "at '~A" (token-lexeme token)))))

(defmethod initialize-instance :after ((obj clox-error) &key &allow-other-keys)
  (when (eq (class-of obj) (find-class 'clox-error))
    (error "~S is an abstract class and can't be instantiated." obj)))

(defmethod clox-error-name ((obj clox-error))
  (string-capitalize (type-of obj)))

(defmethod clox-error-message ((obj unexpected-character-error))
  (format nil "Unexpected character: ~A." (wrong-character obj)))

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

(defmacro handle-parser-errors (error-flag &body body)
  `(handler-bind ((clox-parser-error
                    (lambda (condition)
                      (format *error-output* "~A" condition)
                      (setf ,error-flag t)
                      ;; Invoke IGNORE if it exists.
                      (let ((restart (find-restart 'ignore)))
                        (when restart
                          (invoke-restart restart))))))
     ,@body))
