;;;; clox.asd

(asdf:defsystem #:clox
  :description "Common Lisp implementation of Lox language. Following http://www.craftinginterpreters.com"
  :author "Mateusz Malisz <maliszmat@gmail.com>"
  :license "MIT"

  :build-operation program-op
  :build-pathname #P"bin/clox"
  :entry-point "clox::main"

  :depends-on (:alexandria ; common utils
               :log4cl
               :command-line-arguments)

  :components ((:module
                "src"
                :serial t
                :components
                ((:file "package")
                 (:file "main")))))
