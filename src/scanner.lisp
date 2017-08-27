(in-package :clox)

(defparameter +token-types+
  '(left-paren right-paren))

(-> scan-tokens (string) list)
(defun scan-tokens (input)
  (list input))
