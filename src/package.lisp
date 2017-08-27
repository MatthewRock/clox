;;;; package.lisp

(in-package :common-lisp-user)

(defpackage :clox
  (:use #:cl
        #:command-line-arguments)
  (:import-from :serapeum :->)
  (:export :main))
