;;;; package.lisp

(in-package :common-lisp-user)

(defpackage :clox
  (:use #:cl
        #:command-line-arguments
        #:clox-utils)
  (:import-from :serapeum :->)
  (:export :main))
