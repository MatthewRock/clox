(in-package :cl-user)

(defpackage :clox-utils
  (:use #:cl)
  (:import-from :serapeum :->)
  (:export

   ;; misc
   :not-equal

   ;; file
   :load-file-to-string

   ;; ast-generator
   :defast
   :naming-convention))
