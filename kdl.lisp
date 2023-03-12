(defpackage :kdl
  (:nicknames :🐈dl)
  (:use
    :common-lisp
    :esrap)
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
      #\paragraph_separator)))

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
  (or unicode-whitespace bom multiline-comment))


;; TODO Should this include the following expression?
(defrule node-comment-start "/-"
  (:constant nil))
(defrule line-comment
  (and "//" (* (not vertical-space)))
  (:constant nil))

(defrule boolean (or "true" "false"))

(defrule number-sign (or #\+ #\-))

(defrule digit (character-ranges (#\0 #\9)))

(defrule hexit
  (or digit (character-ranges (#\A #\F) (#\a #\f))))

(defrule hex-number
  (and "0x" (and hexit (* (or hexit #\_)))))

(defrule octade (character-ranges (#\0 #\7)))
(defrule octal-number
  (and "0o" (and octade (* (or octade #\_)))))

(defrule bit (or #\0 #\1))
(defrule binary-number
  (and "0b" (and bit (* (or bit #\_)))))

(defrule whole-number
  (and (? number-sign)
    (or hex-number)
    (or octal-number)
    (or binary-number)))

(defrule exponent
  (and (or #\e #\E) (? number-sign) (* (or digit #\_))))

(defrule decimal-number
  (and (? number-sign)
    digit
    (* (or digit #\_ #\.))
    (? exponent)))

(defrule number
  (or decimal-number whole-number))

(defun code-chars (&rest codes)
  (loop for code in codes collect
    (typecase code
      (cons (apply 'code-chars code))
      (integer (code-char code)))))

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
    (* (or non-number-identifier-character digit number-sign))))

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
  ;; TODO is everything else chars? will this be a problem?
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

(defrule string (or basic-string raw-string))
(defrule identifier (or bare-identifier string)
  (:text t))
(defrule keyword
  (or boolean "null")
  (:text t))
(defrule type (and #\( identifier #\)))
(defrule value (and (? type) (or string number keyword))
  (:text t))
(defrule property (and identifier "=" value))
(defrule escaped-vertical-space
  (and #\\ (* horizontal-space) vertical-space))

(defrule node-space
  (or
    (and (* horizontal-space) escaped-vertical-space (* horizontal-space))
    (+ horizontal-space)))

(defrule node-terminator (or line-comment vertical-space ";"))
(defrule node-comment-start (and "/-" (* node-space)))
(defrule space (or vertical-space horizontal-space line-comment))
(defrule node-property-value
  (and (? node-comment-start) (or property value)))
(defrule node-children
  (and (? node-comment-start)
    "{" nodes "}"))
(defrule node
  (and
    (? node-comment-start)
    (? type)
    identifier
    (* (and horizontal-space+ node-property-value))
    (? (and node-space* node-children horizontal-space*))
    node-space*))
(defrule node-properties (* (and (* node-space))))
(defrule node-space* (* node-space))
(defrule node-space+ (+ node-space))
(defrule horizontal-space+ (+ horizontal-space))
(defrule horizontal-space* (* horizontal-space)
  (:constant " "))

(defrule nodes
  (and (* space)
    (? (and node (? nodes))) (* space)))

;; TODO if i use CLOS i can have one `read-document` generic with methods for
;; streams, strings and files
(defun read-document (&optional (stream *standard-input*))
  "Read the kdl file on STREAM into lisp structures."
  (declare (ignore stream)))

(defun from-string (string)
  (with-input-from-string (stream string)
    (read stream)))

(defun from-file (filespec)
  (with-open-file (stream filespec)
    (read stream)))

(defun write-document (document &optional (output-stream *standard-output*))
  "Write the kdl DOCUMENT to OUTPUT-STREAM."
  (declare (ignore document))
  (write-string "node \"arg2\"" output-stream))

(defun to-string (document)
  (declare (ignore document))
  (the string "node 0
"))

(defun to-file nil)
