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
    (clox-help))
  (let ((args-len (length args)))
    (cond
      ((> args-len 1) (clox-help))
      ((= 1 args-len) (run-file (first args)))
      (t (run-prompt))))
  (uiop:quit))

(defun clox-help ()
  (show-option-help +command-line-spec+ :sort-names t)
  (uiop:quit))

(-> run-file (string) null)
(defun run-file (path)
  (run (load-file-to-string (pathname path))))

(-> run-prompt () null)
(defun run-prompt ()
  (loop do
       (format t "> ")
       (run (read-line))))

(-> run (string) integer)
(defun run (source)
  (loop for token in (scan-tokens source)
     do (format t "~A~%" token))
  (if (had-error)
      (uiop:quit -1)))
