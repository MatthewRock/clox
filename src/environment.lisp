(in-package :clox)

(defparameter *nothing* (gensym "nothing"))

(defclass environment ()
  ((storage :type 'hash-table :accessor storage :initarg :storage))
  (:default-initargs :storage (make-hash-table :test 'equal))
  (:documentation "Environment holding variables etc. Variables are stored in STORAGE hash-table
with keys being strings and values being objects (t)."))

(-> define-variable (environment string t) null)
(defun define-variable (environment name val)
  (setf (gethash name (storage environment)) val)
  nil)

(-> get-variable (environment token) t)
(defun get-variable (environment name-token)
  (let ((val (gethash (token-lexeme name-token) (storage environment) *nothing*)))
    (if (eq val *nothing*)
        (error 'clox-runtime-error
               :token name-token
               :message (format nil "Undefined variable \"~A\"." (token-lexeme name-token)))
        val)))
