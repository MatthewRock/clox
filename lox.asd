;;;; lox.asd

(asdf:defsystem #:lox
  :description "Common Lisp implementation of Lox language. Following http://www.craftinginterpreters.com"
  :author "Mateusz Malisz <maliszmat@gmail.com>"
  :license "MIT"
  :depends-on (:alexandria :log4cl)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "lox")))
