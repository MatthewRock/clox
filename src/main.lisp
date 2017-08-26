(in-package :clox)

(defparameter +command-line-spec+
  '((("help" #\h #\?) :type boolean :optional t :documentation "Show help.")
    (("verbose" #\v) :type boolean :optional t :documentation "Become verbose, whatever it means.")))

(defun main ()
  (handle-command-line
   +command-line-spec+
   #'run-clox
   :command-line (uiop:command-line-arguments)
   :name "CL's implementation of Lox interpreter."
   :positional-arity 0
   :rest-arity t))

(defun run-clox (args &key verbose help)
  (when help
    (show-option-help +command-line-spec+ :sort-names t)
    (uiop:quit))
  (format t "Working: ~A~%" args))
