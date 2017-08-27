(in-package :clox)

(let ((had-error nil))

  (-> had-error () boolean)
  (defun had-error ()
    had-error)

  (-> raise-error (integer string) null)
  (defun raise-error (line message)
    (report line "" message))

  (-> report (integer string string) null)
  (defun report (line location message)
    (format *error-output* "[line ~D] Error ~A: ~A~%" line location message)
    (setf had-error t)))

;; TODO: Show actual place of problem instead of line
