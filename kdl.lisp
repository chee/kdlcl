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

(defrule digit (character-ranges (#\0 #\9)))

(defrule hexit
  (or digit (character-ranges (#\A #\F) (#\a #\f))))

(defrule hex-number
  (and "0x" (and (and hexit) (* (or hexit #\_))))
  (:destructure (marker (first-number rest-of-the-numbers))
    (declare (ignore marker))
    (parse-integer
      (concatenate 'string
        first-number
        (remove-if 'underscorep rest-of-the-numbers))
      :radix #x10)))

(defrule octade (character-ranges (#\0 #\7)))
(defrule octal-number
  (and "0o" (and (and octade) (* (or octade #\_))))
  (:destructure (marker (first-number rest-of-the-numbers))
    (declare (ignore marker))
    (parse-integer
      (concatenate 'string
        first-number
        (remove-if 'underscorep rest-of-the-numbers))
      :radix #o10)))

(defrule bit (character-ranges (#\0 #\1)))
(defrule binary-number
  (and "0b" (and (and bit) (* (or bit #\_))))
  (:destructure (marker (first-number rest-of-the-numbers))
    (declare (ignore marker))
    (parse-integer
      (concatenate 'string
        first-number
        (remove-if 'underscorep rest-of-the-numbers))
      :radix #b10)))

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
  (and (or #\e #\E) (? number-sign) (* (or digit #\_)))
  (:destructure (e sign numbers)
    (declare (ignore e))
    (concatenate 'string
      (list #\d)
      sign
      (remove-if 'underscorep numbers))))

(defrule decimal-number
  (and (? number-sign)
    (and digit)
    (* (or digit #\_ #\.))
    (? exponent))
  (:destructure (sign first-number rest-of-the-numbers exponent)
      (parse-number (concatenate 'string
                     sign
                     first-number
                     (mapcar
                       'character
                       (remove-if 'underscorep rest-of-the-numbers))
                     (or exponent "")))))

(defrule number
  (or whole-number decimal-number))

(defrule non-number-identifier-character
  (or
    (character-ranges
      #\!
      (#\# #\')
      #\* #\+ #\- #\. #\:
      (#\? #\Z)
      (#\^ #\z)
      #\|
      (#\~ #\UFFFF))))

(defrule bare-identifier
  (and non-number-identifier-character
    (* (or non-number-identifier-character digit number-sign)))
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
  (:text t)
  ;; (:function intern)
  )

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
(defun read-document (&optional (stream *standard-input*))
  "Read the kdl file on STREAM into lisp structures."
  (from-string (alexandria:read-stream-content-into-string stream)))

(defun from-string (string)
  (parse-document string))

(defun from-file (filespec)
  (with-open-file (stream filespec)
    (read-document stream)))

(from-file (asdf:system-relative-pathname :kdl "t/test-cases/input/emoji.kdl"))

(defun write-type (type &optional (stream *standard-output*))
  (write-char #\( stream)
  (write-string type stream)
  (write-char #\) stream))

(defun write-property (property &optional (stream *standard-output*))
  (let ((name (property property))
         (type (property-type property))
         (value (property-value property))
         (value-type (property-value-type property)))
    (when (or name value)
      (write-char #\space stream)
      ;; TODO handle non-identifier
      (when type
        (write-type type stream))
      (when name
        (write-string name stream)
        (write-char #\= stream))
      (when value-type
        (write-type value-type stream))
      ;; TODO typecase
      (typecase value
        (keyword (format stream "~a" (string-downcase (symbol-name value))))
        (double-float (princ (substitute #\E #\d (format nil "~a" value)) stream))
        (null (format stream "null"))
        (number (format stream "~a" value))
        (string (format stream "~s" value))
        (t (format stream "~s" value))))))


(defun write-children (children &optional (stream *standard-output*))
  (write-string " {" stream)
  (loop for child in children do (write-node child stream))
  (write-string "
}" stream))

(defun write-node (node &optional (stream *standard-output*))
  (when node
    (let ((name (node-name node))
           (type (node-type node))
           (properties (node-properties node))
           (children (node-children node)))
      (fresh-line stream)
      (when type (write-type type stream))
      (write-string name stream)
      (loop for property in properties do (write-property property stream))
      (when (and children (not (every 'null children)))
        (write-children children stream)))))

;; There's no reason not to stream output just because I can't stream input
(defun write-document (document &optional (stream *standard-output*))
  "Write the kdl DOCUMENT to OUTPUT-STREAM."
  (loop for node in document
    do (write-node node stream))
  (fresh-line)
  (format stream "~%"))

(defun to-string (document)
  (with-output-to-string (output)
    (write-document document output)))

(defun to-file (document filespec)
  (with-open-file (file-stream filespec
                    :direction :output
                    :if-exists :supersede)
    (write-document document file-stream)))

(defun io (string)
  (to-string (from-string string)))
