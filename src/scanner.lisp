(in-package :clox)

;; ;;;;;;;;;;;;;;
;; Defining Token
;; ;;;;;;;;;;;;;;
(defconstant +1char-token-associations+
  '((#\( :left-paren)
    (#\) :right-paren)
    (#\{ :left-brace)
    (#\} :right-brace)
    (#\, :comma)
    (#\. :dot)
    (#\- :minus)
    (#\+ :plus)
    (#\* :star)
    (#\/ :slash)
    (#\; :semicolon)))

(deftype token-type ()
  `(member
    ;; Single-character tokens
    :left-paren :right-paren
    :left-brace :right-brace
    :comma :dot :minus :plus :star :slash :semicolon

    ;; One or two character tokens
    :bang :bang-equal
    :equal :equal-equal
    :greater :greater-equal
    :less :less-equal

    ;; Literals
    :identifier :string :number

    ;; Keywords.
    :and :class :else :false :fun :for :if :nil :or
    :print :return :super :this :true :var :while

    :eof))

(defclass Token ()
  ((%type :initarg :type :type token-type)
   (%lexeme :initarg :lexeme :type string)
   (%literal :initarg :literal)
   (%line :initarg :line :type integer)))

(-> token->string (token) string)
(defun token->string (token)
  (with-slots (%type %lexeme %literal) token
    (format nil "~A ~A ~A" %type %lexeme %literal)))

;; ;;;;;;;;;;;;;;;;
;; Defining Scanner
;; ;;;;;;;;;;;;;;;;

(defclass Scanner ()
  ((%source :type string :initarg :source :reader source)
   (%tokens :type vector :initarg :tokens :accessor tokens)
   (%start :type integer :initform 0 :accessor start)
   (%current :type integer :initform 0 :accessor current)
   (%line :type integer :initform 1 :accessor line))
  (:default-initargs
   :source "" :tokens (make-array '(50)
                                  :element-type 'token
                                  :adjustable t :fill-pointer 0)))

(defmacro bind-tokens-for-scanner (scanner tokens &body input)
  "Bind tokens together and process the same token as INPUT."
  (alexandria:once-only (scanner)
    `(case (progn
             ,@input)
       ,@(loop for (char token-type) in tokens
            collect `(,char
                      (add-token ,scanner ,token-type)))
       (otherwise (raise-error
                   (line ,scanner)
                   "Unexpected character.")))))

(-> is-at-end (scanner) boolean)
(defun is-at-end (scanner)
  "Return T if scanner reached EOF, nil otherwise."
  (>= (current scanner) (length (source scanner))))

(-> advance (scanner) character)
(defun advance (scanner)
  "Advance scanner's pointer."
  (aref (source scanner) (1- (incf (current scanner)))))

(-> scan-token (scanner) null)
(defun scan-token (scanner)
  "Add next token from the scanner's source."
  (bind-tokens-for-scanner scanner #.+1char-token-associations+
    (advance scanner)))

(-> scan-tokens (string) vector)
(defun scan-tokens (input)
  "Scan tokens from input. Return vector of scanned tokens."
  (loop with scanner = (make-instance 'scanner :source input)
     until (is-at-end scanner) do (scan-token scanner)
     finally (return (tokens scanner))))

(-> add-token (scanner token-type &optional t) null)
(defun add-token (scanner token-type &optional literal)
  (vector-push-extend
   (make-instance'token :type token-type
                        :lexeme (subseq (source scanner)
                                        (start scanner)
                                        (current scanner))
                        :literal literal
                        :line (line scanner))
   (tokens scanner))
  nil)
