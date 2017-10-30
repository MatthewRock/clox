(in-package :clox)

;; ;;;;;;;;;;;;;;
;; Defining Token
;; ;;;;;;;;;;;;;;

(defparameter +1char-token-associations+
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

(defparameter +keywords+
  #.(let ((keywords-alist
           '(("and" . :and)
             ("class" . :class)
             ("else" . :else)
             ("false" . :false)
             ("for" . :for)
             ("fun" . :fun)
             ("if" . :if)
             ("nil" . :nil)
             ("or" . :or)
             ("print" . :print)
             ("return" . :return)
             ("super" . :super)
             ("this" . :this)
             ("true" . :true)
             ("var" . :var)
             ("while" . :while))))
      (alexandria:alist-hash-table keywords-alist
                                   :test #'equal
                                   :size (length keywords-alist))))

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
  ((%type :initarg :type :type token-type :reader token-type)
   (%lexeme :initarg :lexeme :type string :reader token-lexeme)
   (%literal :initarg :literal :reader token-literal)
   (%line :initarg :line :type integer :reader token-line)))

(defmethod print-object ((object token) stream)
  (print-unreadable-object (object stream)
    (format stream "Token ~A from line ~A with lexeme ~A and literal ~A."
            (token-type object) (token-line object) (token-lexeme object) (token-literal object))))

(-> token->string (token) string)
(defun token->string (token)
  "Return token to a string representation."
  (with-slots (%type %lexeme %literal) token
    (format nil "~A ~A ~A" %type %lexeme %literal)))

;; ;;;;;;;;;;;;;;;;
;; Defining Scanner
;; ;;;;;;;;;;;;;;;;

(defclass Position-Marker ()
  ((%line :type integer :initform 1 :accessor line :initarg :line)
   (%line-position :type integer :initform 0 :accessor line-position :initarg :line-position)
   (%global-position :type integer :initform 0 :accessor global-position :initarg :global-position)))

(-> copy-position-marker (position-marker) position-marker)
(defun copy-position-marker (position-marker)
  (make-instance 'position-marker
                 :line (line position-marker)
                 :line-position (line-position position-marker)
                 :global-position (global-position position-marker)))

(defclass Scanner ()
  ((%source :type string :initarg :source :reader source)
   (%tokens :type vector :initarg :tokens :accessor tokens)
   (%starting-position :initarg :starting-position :type position-marker :accessor starting-position)
   (%current-position :initarg :current-position :type position-marker :accessor current-position))
  (:default-initargs
   :source ""
   :tokens (make-array '(50)
                       :element-type 'token
                       :adjustable t :fill-pointer 0)
   :starting-position (make-instance 'position-marker)
   :current-position (make-instance 'position-marker)))

(defgeneric current (thing)
  (:documentation "Get the current global position of a THING."))

(defgeneric line (thing)
  (:documentation "Get the line number of current THING position."))

(defmethod start ((obj scanner))
  (global-position (starting-position obj)))

(defmethod current ((obj scanner))
  (global-position (current-position obj)))

(defmethod line ((obj scanner))
  (line (current-position obj)))

(-> move-forward (scanner) integer)
(defun move-forward (obj)
  "Move OBJ forward destructively modifying it."
  ;; If we entered new line, reset the line position
  (if (is-at-newline obj)
      (progn
        (setf (line-position (current-position obj)) 0)
        (incf (line (current-position obj))))
      (incf (line-position (current-position obj))))
  (incf (global-position (current-position obj))))

(-> move-start-to-current (scanner) null)
(defun move-start-to-current (scanner)
  (setf (starting-position scanner) (copy-position-marker (current-position scanner))))

(defmacro bind-tokens-for-scanner (scanner tokens &body input)
  "Bind tokens together and process the same token as INPUT."
  (alexandria:once-only (scanner)
    `(case (progn
             ,@input)
       ,@(loop for (char token-type) in tokens
            collect `(,char
                      (add-token ,scanner ,token-type)))
       (otherwise (cerror "Continue scanning."
                          'unexpected-character-error
                          :line (line ,scanner)
                          :place (start ,scanner))))))

(-> is-at-end (scanner) boolean)
(defun is-at-end (scanner)
  "Return T if scanner reached EOF, nil otherwise."
  (>= (current scanner) (length (source scanner))))

(-> advance (scanner) character)
(defun advance (scanner)
  "Advance scanner's pointer and return the character at that point."
  (aref (source scanner) (1- (move-forward scanner))))

(-> match (scanner character) boolean)
(defun match (scanner expected)
  "Consume the next character and return T if it is expected character, return NIL otherwise."
  (unless (or (is-at-end scanner)
              (char/= (current-character scanner) expected))
    (move-forward scanner)
    t))

(-> peek (scanner) character)
(defun peek (scanner)
  "Return the next character in the source or NUL if we're at the end."
  (if (is-at-end scanner)
      #\Nul
      (current-character scanner)))

(-> peek-next (scanner) character)
(defun peek-next (scanner)
  "Return the character after the next character in the source or NUL if there's none.'"
  (if (>= (1+ (current scanner))
          (length (source scanner)))
      #\Nul
      (aref (source scanner)
            (1+ (current scanner)))))

(-> scan-token (scanner) null)
(defun scan-token (scanner)
  "Add next token from the scanner's source."
  (let ((currently-processed-character (advance scanner)))
    (case currently-processed-character
      ;; One-character tokens
      (#\( (add-token scanner :left-paren))
      (#\) (add-token scanner :right-paren))
      (#\{ (add-token scanner :left-brace))
      (#\} (add-token scanner :right-brace))
      (#\, (add-token scanner :comma))
      (#\. (add-token scanner :dot))
      (#\- (add-token scanner :minus))
      (#\+ (add-token scanner :plus))
      (#\; (add-token scanner :semicolon))
      (#\* (add-token scanner :star))

      ;; One-Or-Two-character tokens
      (#\! (add-token scanner (if (match scanner #\=)
                                  :bang-equal
                                  :bang)))
      (#\= (add-token scanner (if (match scanner #\=)
                                  :equal-equal
                                  :equal)))
      (#\< (add-token scanner (if (match scanner #\=)
                                  :less-equal
                                  :less)))
      (#\> (add-token scanner (if (match scanner #\=)
                                  :greater-equal
                                  :greater)))

      ;; Process Comments
      (#\/ (if (match scanner #\/)
               (process-single-line-comment scanner)
               (add-token scanner :slash)))

      ;; Ignore whitespace. Newline is processed automatically when moving position
      ((#\Space #\Tab #\Return #\Linefeed) nil)

      ;; Strings
      (#\" (process-string scanner))

      (otherwise
       (cond
         ((is-digit currently-processed-character) (process-number-literal scanner))
         ((is-alpha currently-processed-character) (process-identifier scanner))
         (t (cerror "Continue scanning."
                    'unexpected-character-error
                    :character currently-processed-character
                    :line (line scanner)
                    :place (start scanner)))))))
  nil)

(-> scan-tokens (string) vector)
(defun scan-tokens (input)
  "Scan tokens from input. Return vector of scanned tokens."
  (loop with scanner = (make-instance 'scanner :source input)
     until (is-at-end scanner) do
       (move-start-to-current scanner)
       (scan-token scanner)
     finally
       (return (progn
                 (move-start-to-current scanner)
                 (add-token scanner :eof)
                 (tokens scanner)))))

(-> add-token (scanner token-type &optional t) null)
(defun add-token (scanner token-type &optional literal)
  "Add token to the vector of scanned tokens."
  (vector-push-extend
   (make-instance'token :type token-type
                        :lexeme (subseq (source scanner)
                                        (start scanner)
                                        (current scanner))
                        :literal literal
                        :line (line scanner))
   (tokens scanner))
  nil)

(-> current-character (scanner) character)
(defun current-character (scanner)
  "Return the current character of the source - the one that will be processed now."
  (aref (source scanner) (current scanner)))

(defun is-at-newline (scanner)
  "Return T if scanner is at newline(next character will be \n)."
  (char= (peek scanner) #\Linefeed))

(-> process-single-line-comment (scanner) null)
(defun process-single-line-comment (scanner)
  "Consume contents of until EOL or EOF."
  (loop until (or (is-at-newline scanner)
                  (is-at-end scanner))
     do (advance scanner)))

(-> process-string (scanner) null)
(defun process-string (scanner)
  "Consume the string from the current point to the closing double quote."
  (flet ((add-string (scanner)
           (add-token scanner :string
                      (subseq (source scanner)
                              (1+ (start scanner))
                              (1- (current scanner))))))

    (loop until (or (char= (peek scanner) #\")
                    (is-at-end scanner))
          do (advance scanner))

    ;; If String's not terminated
    (if (is-at-end scanner)
        ;; Report it
        (cerror "Continue scanning"
                'unterminated-string-error
                :line (line scanner)
                :place (start scanner))
        ;; Consume the closing "
        (advance scanner))
    (add-string scanner)))

(-> is-digit (character) boolean)
(defun is-digit (char)
  "Return T if char is digit, NIL otherwise."
  (and (char>= char #\0)
       (char<= char #\9)))

(-> is-alpha (character) boolean)
(defun is-alpha (char)
  "Return T if char is a latin letter or underscore, NIL otherwise."
  (or (and (char>= #\Z char)
           (char<= #\A char))
      (and (char>= #\z char)
           (char<= #\a char))
      (char= char #\_)))

(-> is-alphanumeric (character) boolean)
(defun is-alphanumeric (char)
  (or (is-alpha char)
      (is-digit char)))

(-> process-number-literal (scanner) null)
(defun process-number-literal (scanner)
  "Process a number literal and return T, or return NIL and don't process anything if the token isn't number literal."
  (loop while (is-digit (peek scanner)) do (advance scanner))
  (when (and (char= (peek scanner) #\.)
             (is-digit (peek-next scanner)))
    ;; Consume the dot
    (advance scanner)
    (loop while (is-digit (peek scanner)) do (advance scanner)))
  (add-token scanner :number (read-from-string
                              (subseq (source scanner)
                                      (start scanner)
                                      (current scanner)))))

(-> process-identifier (scanner) null)
(defun process-identifier (scanner)
  (loop while (is-alphanumeric (peek scanner)) do (advance scanner))
  (add-token scanner
             (gethash
              (print (subseq (source scanner) (start scanner) (current scanner)))
              +keywords+
              :identifier)))
