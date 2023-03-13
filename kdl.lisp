(defpackage :kdl
  (:nicknames :🐈dl)
  (:use
    :common-lisp
    :esrap)
  (:import-from :parse-number :parse-number)
  (:import-from :alexandria :compose)
  (:export
    :read-document
    :from-string
    :from-file
    :write-document
    :to-string
    :to-file))
(in-package :kdl)

(defun code-chars (codes)
  (loop for code in codes collect
    (typecase code
      (cons (apply 'code-chars code))
      (integer (code-char code)))))

(defun range (min max)
  (loop for i upto max upfrom min collect i))

(defun identifier-char-p (char)
  (declare (type character char))
  (let ((crimes
          (concatenate 'list
            (code-chars (range #x0 #x20))
            " "
            "\\/(){}<>;[]=,\"")))
    (not (member char crimes))))

(defun bare-identifier-p (string)
  (let* ((length (length string))
          (first-char (and (> length 0) (char string 0)))
          (second-char (and (> length 1) (char string 1)))
          (first-char-is-digit (and first-char (digit-char-p first-char)))
          (first-char-is-hyphen (and first-char (char= first-char #\-)))
          (second-char-is-digit (and second-char (digit-char-p second-char))))
    (and
      (every 'identifier-char-p string)
      (not first-char-is-digit)
      (not (and first-char-is-hyphen second-char-is-digit))
      (not (string= string "null"))
      (not (string= string "true"))
      (not (string= string "false")))))

;; TODO Maybe the parser should be a different file?
(defrule vertical-space
  (or
    (and #\return #\newline)
    (or
      #\newline
      #\page
      #\return
      #\next-line
      #\line_separator
      #\paragraph_separator))
  (:constant #\newline))

(defrule bom
  (char #\zero_width_no-break_space))

(defrule unicode-whitespace
  (or
       #\tab
       #\space
       #\no-break_space
       #\ogham_space_mark
       (character-ranges (#\en_quad #\thin_space))
       #\narrow_no-break_space
       #\medium_mathematical_space ; lol
       #\ideographic_space))

(defrule multiline-comment-start "/*")
(defrule multiline-comment-end "*/")
(defrule multiline-comment
  (and multiline-comment-start
    (* (or multiline-comment (not multiline-comment-end)))
    multiline-comment-end)
  (:constant nil))

(defrule horizontal-space
  (or unicode-whitespace bom multiline-comment)
  (:constant " "))

(defrule line-comment
  (and "//" (* (not vertical-space)))
  (:constant nil))

(defrule number-sign (or #\+ #\-))

(defun reduce-number (number-list)
  (destructuring-bind (first-number rest-of-the-numbers) number-list
    (concatenate 'string
      (list first-number)
      (remove-if 'underscorep rest-of-the-numbers))))

(defrule digit (character-ranges (#\0 #\9)))
(defrule digits
  (and digit (* (or digit #\_)))
  (:function reduce-number))

;; TODO this is repetititive
(defrule hexit
  (or digit (character-ranges (#\A #\F) (#\a #\f))))
(defrule hexits
  (and hexit (* (or hexit #\_)))
  (:function reduce-number))
(defrule hex-number
  (and "0x" hexits)
  (:destructure (marker number)
    (declare (ignore marker))
    (parse-integer number :radix #x10)))

(defrule octade (character-ranges (#\0 #\7)))
(defrule octades
  (and octade (* (or octade #\_)))
  (:function reduce-number))
(defrule octal-number
  (and "0o" octades)
  (:destructure (marker number)
    (declare (ignore marker))
    (parse-integer number :radix #o10)))

(defrule bit (character-ranges (#\0 #\1)))
(defrule bits
  (and bit (* (or bit #\_)))
  (:function reduce-number))
(defrule binary-number
  (and "0b" bits)
  (:destructure (marker number)
    (declare (ignore marker))
    (parse-integer number :radix #b10)))

(defun sign-function (maybe-sign)
  (if maybe-sign
    (intern maybe-sign)
    '+))

(defun apply-sign (maybe-sign number)
  (flet ((± (number)
           (funcall (sign-function maybe-sign) number)))
    (* (± 1) number)))

(defrule whole-number
  (and (? number-sign)
    (or
      hex-number
      octal-number
      binary-number))
  (:destructure (sign number)
      (apply-sign sign number)))

(defun underscorep (char)
  (typecase char
    (string (string= char "_"))
    (character (char= char #\_))))

(defrule exponent
  (and (or #\e #\E) (? number-sign) digits)
  (:destructure (e sign numbers)
    (declare (ignore e))
    (concatenate 'string
      (list #\d)
      sign
      numbers)))

(defrule decimal-number
  (and (? number-sign)
    digits
    (? (and "." digits))
    (? exponent))
  (:destructure (sign number fraction exponent)
    (parse-number
      (concatenate 'string
        sign
        number
        (when fraction (first fraction))
        (when fraction (second fraction))
        (or exponent "")))))

(defrule number
  (or whole-number decimal-number))

(defrule non-identifier-char
  (or
    (character-ranges
      #\\ #\/ #\( #\) #\{ #\} #\<
      #\> #\; #\[ #\] #\= #\, #\"
      (#\nul #\space))))

(defun parse-initial-char (text position end)
  (let* ((length (- end position))
          (first-chars (if (> length 1)
                         (subseq text position (+ position 1))
                         (subseq text position position))))
    (if (and
          (> length 0)
          (bare-identifier-p first-chars))
      (values (char first-chars 0) (1+ position))
      (values first-chars position))))

(defrule initial-char
  (function parse-initial-char))

(defun parse-bare-identifier (text position end)
  (let (match)
    (loop for e upto end upfrom position
      do (let ((sub (subseq text position e)))
           (if (bare-identifier-p sub)
             (setf match sub)
             (return))))
    (values match (+ (length match) position))))

(defrule bare-identifier
  (function parse-bare-identifier)
  (:text t))

(defrule escape
  (or #\" #\\ #\b #\n #\r #\t
    (and "u"
      hexit
      (? hexit)
      (? hexit)
      (? hexit)
      (? hexit)
      (? hexit)))
  (:text t))

(defrule string-character
  (or (and #\\ escape) (not (or "\\" "\"")))
  (:text t))

(defrule basic-string
  (and #\" (* string-character) #\"))
;; TODO I have no idea how to do this.
;; The greedy (* character) eats the final quote before it can look at it
(defrule raw-string-quotes
  (and #\" (* character) #\"))
(defrule raw-string-hash
  (or (and "#" raw-string-hash "#")
        raw-string-quotes))
(defrule raw-string
  (and "r" raw-string-hash))

(defrule string (or basic-string raw-string)
  (:destructure (lq string rq)
    (declare (ignore lq rq))
    (concatenate 'string (mapcar 'character string))))

;; TODO Should an identifier be a symbol?
;; Probably... Maybe... a keyword?
(defrule identifier (or bare-identifier string)
  (:text t))

(defmacro string=case (keyform &body cases)
  (let ((value (gensym)))
    `(let ((,value ,keyform))
       (declare (ignorable ,value))
       (cond
         ,@(mapcar
             (lambda (case)
               `((string= ,value ,(first case)) ,@(rest case)))
             cases)))))

(defvar *parse-keywords-as-keywords* t)

(defrule keyword
  (or "true" "false" "null")
  (:lambda (text)
    (if *parse-keywords-as-keywords*
      (intern (string-upcase text) :keyword)
      (string=case text
        ("true" t)
        ("false" nil)
        ("null" nil)))))

(defrule type (and #\( identifier #\))
  (:destructure (< type-identifier >)
    (declare (ignore < >))
    ;;(intern (string-upcase type-identifier) :keyword)
    type-identifier))

(defrule value (and (? type) (or string number keyword))
  (:destructure (type value)
    (cons value type)))

(defrule property (and (? type) identifier "=" value)
  (:destructure (type property = value)
    (declare (ignore =))
    (cons (list property type) value)))

(defrule escaped-vertical-space
  (and #\\ (* horizontal-space) vertical-space))

(defrule node-space
  (or
    (and (* horizontal-space) escaped-vertical-space (* horizontal-space))
    (+ horizontal-space)))

(defrule node-terminator (or line-comment vertical-space ";"))
(defrule node-comment-start (and "/-" (* node-space)))
(defrule space
  (or vertical-space horizontal-space line-comment)
  (:constant nil))

(defrule node-property
  (and (? node-comment-start) (or property value))
  (:destructure (commentp property)
    (unless commentp
      (cond
        ((consp (cdr property)) property)
        (t (cons nil property))))))

(defrule node-children
  (and (? node-comment-start) "{" nodes "}")
  (:destructure (commentp { children })
    (declare (ignore { }))
    (unless commentp children)))

(defrule node-property-inline
  (and horizontal-space+ node-property)
  (:destructure (leading-space property)
    (declare (ignore leading-space))
    property))

(defrule node-children-inline
  (and node-space* node-children horizontal-space*)
  (:destructure (leading-space children trailing-space)
    (declare (ignore leading-space trailing-space))
    children))

(defrule node
  (and
    (? node-comment-start)
    (? type)
    identifier
    (* node-property-inline)
    (? node-children-inline)
    node-space*
    node-terminator)
  (:destructure (commentp type name properties children space terminator)
    (declare (ignore space terminator))
    (unless commentp
      (list
        name
        properties
        children
        type))))

(defrule node-space* (* node-space)
  (:constant nil))
(defrule node-space+ (+ node-space)
  (:constant nil))
(defrule horizontal-space+ (+ horizontal-space)
  (:constant nil))
(defrule horizontal-space* (* horizontal-space)
  (:constant nil))

(defrule space* (* space)
  (:constant nil))

(defun fledge (nodes)
  (unless (null nodes)
    (if (= 1 (length nodes))
      nodes
      `(,(first nodes)
         ,@(second nodes)))))

;; TODO there's an extra nil hanging around
;; TODO query language?
(defrule nodes
  (and
    (* space)
    (and (? node) (? nodes))
    (* space))
  (:destructure (leading-space nodes trailing-space)
    (declare (ignore leading-space trailing-space))
    (fledge nodes)))

(defrule document nodes)
(defun parse-document (document)
  (parse 'document
    ;; lol, i don't know if esrap can treat EOF as a parsable thing
    ;; maybe i could use (function) and check the position and length?
    (concatenate 'string document '(#\newline))))

;; TODO setf forms
(defun value (value)
  (car value))
(defun value-type (value)
  (cdr value))
(defun property (property)
  (car (car property)))
(defun property-type (property)
  (cadr (car property)))
(defun property-value (property)
  (value (cdr property)))
(defun property-value-type (property)
  (value-type (cdr property)))
(defun node-name (node)
  (first node))
(defun node-properties (node)
  (second node))
(defun node-children (node)
  (third node))
(defun node-type (node)
  (first (last node)))

;; TODO if i use CLOS i can have one `read-document` generic with methods for
;; streams, strings and files

;; Unfortunately I can't stream the input because esrap doesn't support that
;; TODO esrap-liquid might work?
(defun read-document (&optional (stream *standard-input*))
  "Read the kdl file on STREAM into lisp structures."
  (from-string (alexandria:read-stream-content-into-string stream)))

(defun from-string (string)
  (parse-document string))

(defun from-file (filespec)
  (with-open-file (stream filespec)
    (read-document stream)))

(defparameter *indent* 0)

(defun write-indent ()
  (format t "~v@{~A~:*~}" (* *indent* 4) " "))

(defun write-type (type)
  (write-char #\()
  (write-string type)
  (write-char #\)))

(defun write-property (property)
  (let ((name (property property))
         (type (property-type property))
         (value (property-value property))
         (value-type (property-value-type property)))
    (when (or name value)
      (write-char #\space)
      ;; TODO handle non-identifier
      (when type
        (write-type type))
      (when name
        (write-string name)
        (write-char #\=))
      (when value-type
        (write-type value-type))
      ;; TODO typecase
      (typecase value
        (keyword (format t "~a" (string-downcase (symbol-name value))))
        (double-float (princ (substitute #\E #\d (format nil "~a" value))))
        (null (format t "null"))
        (number (format t "~a" value))
        (string (format t "~s" value))
        (t (format t "~s" value))))))

(defun write-children (children)
  (write-string " {")
  (loop for child in children do
    (let ((*indent* (1+ *indent*)))
      (write-node child)))
  (write-char #\newline)
  (write-indent)
  (write-string "}"))

(defun write-node (node)
  (when node
    (let ((name (node-name node))
           (type (node-type node))
           (properties (node-properties node))
           (children (node-children node)))
      (fresh-line)
      (write-indent)
      (when type (write-type type))
      (write-string name)
      (loop for property in properties do (write-property property))
      (when (and children (not (every 'null children)))
        (write-children children)))))

(defun write-document (document &optional (stream *standard-output*))
  "Write the kdl DOCUMENT to OUTPUT-STREAM."
  (let ((*standard-output* stream))
    (loop for node in document
      do (write-node node))
    (fresh-line)))

(defun to-string (document)
  (with-output-to-string (output)
    (write-document document output)))

(defun to-file (document filespec)
  (with-open-file
    (file-stream filespec
      :direction :output
      :if-exists :supersede)
    (write-document document file-stream)))

(defmethod io ((string string))
  (to-string (from-string string)))

(defmethod io ((file pathname))
  (to-string (from-file file)))
