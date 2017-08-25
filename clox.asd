;;;; lox.asd

(asdf:defsystem #:clox
  :description "Common Lisp implementation of Lox language. Following http://www.craftinginterpreters.com"
  :author "Mateusz Malisz <maliszmat@gmail.com>"
  :license "MIT"

  :defsystem-depends-on '(:deploy); make build process easier
  :build-operation "deploy-op"
  :build-pathname #P"clox"

  :depends-on (:alexandria ; common utils
               :log4cl ; logging
               )

  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "main")))
