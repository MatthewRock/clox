;;;; package.lisp

(in-package :common-lisp-user)

(defpackage :clox
  (:use #:cl
        #:command-line-arguments)
  (:export :main))
