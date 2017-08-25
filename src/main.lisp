(in-package :clox)

(defparameter +command-line-spec+
  '(((#\h #\? "help") :type boolean :optional t :documentation "Show help.")
    ((#\v "verbose") :type boolean :optional t :documentation "Become verbose, whatever it means.")))

(defun main (args)
  (handle-command-line
   +command-line-spec+
   'run-clox
   :command-line args
   :name "CL's implementation of Lox interpreter."
   :positional-arity 0
   :rest-arity t))

(defun run-clox ()
  (print "Working."))
