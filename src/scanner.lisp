(in-package :clox)

;; ;;;;;;;;;;;;;;
;; Defining Token
;; ;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +1char-token-associations+
    ;; Format:
    ;; List of 2-element lists, where the first element is a character
    ;; and second element is the token name.
    ;; ((char token-name) ...)
    ;; A character is a character and a token name is a keyword symbol.
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
      (#\? :question-mark)
      (#\: :colon)
      (#\; :semicolon)))

  (defparameter +1-or-2-char-token-associations+
    ;; Format:
    ;; List of 2 2-element lists, first list being indicating the first character and token name for single character token,
    ;; Second list having the second character and indicating the 2-character token name:
    ;; ((first-char 1char-token-name) (second-char 2char-token-name) ...)
    ;; A character is a character and token name is a keyword symbol
    '(((#\! :bang) (#\= :bang-equal))
      ((#\= :equal) (#\= :equal-equal))
      ((#\> :greater) (#\= :greater-equal))
      ((#\< :less) (#\= :less-equal))))

  (defparameter +literals+
    ;; Format:
    ;; List of token names
    ;; (token1 token2 ...)
    ;; Token names are keyword symbols.
    '(:identifier :string :number))

  (defparameter +keywords-alist+
    ;; Format: association list of lexeme(token) and token name pairs (dotted lists)
    ;; ((lexeme . token-name) ...)
    ;; Lexeme is a string, token name is a keyword symbol.
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
      ("while" . :while)))

  (defparameter +keywords+
    ;; A hash table created from keyword-alist.
    ;; Key being the lexeme and value being the token name.
    ;; Lexeme is a string and token name is a keyword symbol.
    (alexandria:alist-hash-table +keywords-alist+
                                 :test #'equal
                                 :size (length +keywords-alist+))))

(deftype token-type ()
  `(member
    ;; Single-character tokens
    ,@(mapcar #'second +1char-token-associations+)
    ;; One or two character tokens
    ,@(mapcan (alexandria:curry #'mapcar #'second) +1-or-2-char-token-associations+)
    ;; Literals
    ,@+literals+
    ;; Keywords.
    ,@(mapcar #'cdr +keywords-alist+)
    ;; EOF token
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

(defgeneric is-at-end (processor)
  (:documentation "Return a value indicating whether a PROCESSOR is at the end or not."))

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

(defmethod is-at-end ((scanner Scanner))
  "Return T if scanner reached EOF, nil otherwise."
  (>= (current scanner) (length (source scanner))))

(-> advance (scanner) character)
(defun advance (scanner)
  "Advance scanner's pointer and return the character at that point."
  (aref (source scanner) (1- (move-forward scanner))))

(-> match (scanner character) boolean)
(defun match (scanner expected)
  "Consume the next character and return T if it is expected character, return NIL otherwise."
  (unless (char/= (peek scanner) expected)
    (move-forward scanner)
    t))

(-> peek (scanner) character)
(defun peek (scanner)
  "Return the next character in the source or End-of-text token if we're at the end."
  (if (is-at-end scanner)
      #\Etx
      (aref (source scanner) (current scanner))))

(-> peek-next (scanner) character)
(defun peek-next (scanner)
  "Return the character after the next character in the source or ETX if there's none.'"
  (if (>= (1+ (current scanner))
          (length (source scanner)))
      #\Etx
      (aref (source scanner)
            (1+ (current scanner)))))

(-> current-character (scanner) character)
(defun current-character (scanner)
  "Return the current character of the source - the one that is processed right now, or STX if processing hasn't started yet."
  (if (zerop (current scanner))
      #\Stx
      (aref (source scanner) (1- (current scanner)))))

(defun is-at-newline (scanner)
  "Return T if scanner is at newline(next character will be \n)."
  (char= (peek scanner) #\Linefeed))

(-> scan-token (scanner) (or boolean token))
(defun scan-token (scanner)
  "Add next token from the scanner's source."
  ;; FIXME: Make cerror return something else than NIL.
  (advance scanner)
  (or (process-1char-token scanner)
      (process-1-or-2-char-token scanner)
      (process-comment-or-slash scanner)
      (process-whitespace scanner)
      (process-string scanner)
      (process-number-literal scanner)
      (process-identifier scanner)
      (cerror "Continue scanning."
              'unexpected-character-error
              :character currently-processed-character
              :line (line scanner)
              :place (start scanner))))

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

(-> add-token (scanner token-type &optional t) token)
(defun add-token (scanner token-type &optional literal)
  "Add token to the vector of scanned tokens."
  (let ((token (make-instance'token :type token-type
                                    :lexeme (subseq (source scanner)
                                                    (start scanner)
                                                    (current scanner))
                                    :literal literal
                                    :line (line scanner))))
    (vector-push-extend token (tokens scanner))
    token))


(-> process-1char-token (scanner) (or null token))
(defun process-1char-token (scanner)
  (loop
    ;; Potential optimization: create a case form
    ;; at a read-time to save looping
    with currently-processed-character = (current-character scanner)
    for (potential-char potential-token) in +1char-token-associations+
    if (char= potential-char currently-processed-character) return (add-token scanner potential-token)))

(-> process-1-or-2-char-token (scanner) (or null token))
(defun process-1-or-2-char-token (scanner)
  (loop
    ;; Potential optimization: create a case form
    ;; at a read-time to save looping
    with currently-processed-character = (current-character scanner)
    for ((first-char 1-token-name)
         (second-char 2-token-name))
      in +1-or-2-char-token-associations+
    if (char= currently-processed-character first-char)
      if (match scanner second-char)
        return (add-token scanner 2-token-name)
    ;; this else is for the second if, not the first one
    else return (add-token scanner 1-token-name)))

(-> process-comment-or-slash (scanner) (or null token))
(defun process-comment-or-slash (scanner)
  (if (char= (current-character scanner) #\/)
      (if (match scanner #\/)
          (process-single-line-comment scanner)
          (add-token scanner :slash))))

(-> process-whitespace (scanner) boolean)
(defun process-whitespace (scanner)
  (some (alexandria:curry #'char= (current-character scanner))
        (list #\Space #\Tab #\Return #\Linefeed)))

(-> process-single-line-comment (scanner) boolean)
(defun process-single-line-comment (scanner)
  "Consume contents of until EOL or EOF."
  (loop until (or (is-at-newline scanner)
                  (is-at-end scanner))
        do (advance scanner))
  t)

(-> process-string (scanner) (or null token))
(defun process-string (scanner)
  "Consume the string from the current point to the closing double quote."
  (flet ((add-string (scanner)
           (add-token scanner :string
                      (subseq (source scanner)
                              (1+ (start scanner))
                              (1- (current scanner))))))

    ;; Only process the string if it starts as a string
    (when (char= (current-character scanner) #\")
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
          ;; Otherwise consume the closing "
          (advance scanner))
      (add-string scanner))))

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

;; TODO: Add .123 format
(-> process-number-literal (scanner) (or token null))
(defun process-number-literal (scanner)
  "Process a number literal and return T, or return NIL and don't process anything if the token isn't number literal."
  (when (or (is-digit (current-character scanner)))

    (loop while (is-digit (peek scanner)) do (advance scanner))

    ;; Check if the number is a floating point one
    (when (and (char= (peek scanner) #\.)
               (is-digit (peek-next scanner)))
      ;; Consume the dot
      (advance scanner)
      ;; And collect the rest
      (loop while (is-digit (peek scanner)) do (advance scanner)))

    (add-token scanner :number (read-from-string
                                (subseq (source scanner)
                                        (start scanner)
                                        (current scanner))))))

(-> process-identifier (scanner) (or token null))
(defun process-identifier (scanner)
  "Return TOKEN if it is an identifier, NIL otherwise."
  (when (is-alpha (current-character scanner)) ; valid identifier starts with an alphabet character.
    (loop while (is-alphanumeric (peek scanner)) do (advance scanner))
    (add-token scanner
               (gethash
                (subseq (source scanner) (start scanner) (current scanner))
                +keywords+
                :identifier))))
