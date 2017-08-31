(in-package :clox)

(defparameter *debug* t)

(defparameter +command-line-spec+
  '((("help" #\h #\?) :type boolean :optional t :documentation "Show help.")
    (("verbose" #\v) :type boolean :optional t :documentation "Become verbose, whatever it means.")))

(defun quit (&optional (code 0))
  (if *debug*
      (error "I would quit now.")
      (uiop:quit code)))

(defun main ()
  (handle-command-line
   +command-line-spec+
   #'run-clox
   :command-line (uiop:command-line-arguments)
   :name "CL's implementation of Lox interpreter."
   :positional-arity 0
   :rest-arity t))

(-> run-clox (list &key (:verbose boolean) (:help boolean)) null)
(defun run-clox (args &key verbose help)
  (when help
    (clox-help))
  (when verbose
    (log4cl:log-info "Clox v. 0.1. Running."))
  (let ((args-len (length args)))
    (cond
      ((> args-len 1) (clox-help))
      ((= 1 args-len) (run-file (first args)))
      (t (run-prompt))))
  (quit)
  nil)

(-> clox-help () null)
(defun clox-help ()
  (show-option-help +command-line-spec+ :sort-names t)
  (quit)
  nil)

(-> run-file (string) null)
(defun run-file (path)
  (run (load-file-to-string (pathname path)))
  nil)

(-> run-prompt () null)
(defun run-prompt ()
  (loop do
       (format t "> ")
       (run (read-line)))
  nil)

(-> run (string) null)
(defun run (source)
  (loop for token across (scan-tokens source)
     do (format t "~A~%" token))
  (when (had-error)
    (quit 1))
  nil)
