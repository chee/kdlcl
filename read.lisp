(in-package :kdl)

;; TODO Switch to esrap-liquid so we can parse a stream?
(defun parse-document (document)
  (parse 'document document))

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

(defrule vertical-space
  (or
    (and #\return #\newline)
    (or
      #\newline
      #\page
      #\return
      #\u0085 ; next_line
      #\u2028 ; line_separator
      #\u2029 ; paragraph_separator
      ))
  (:constant #\newline))


;; zero_width_no-break_space
(defrule bom (char #\ufeff))

(defrule unicode-whitespace
  (or
    #\tab
    #\space
    #\no-break_space
    #\u1680 ; ogham_space_mark
    (character-ranges
      ;; enquad to thinspace
      (#\u2000 #\u2009))
    #\u202f ; narrow_no-break_space
    #\u205f ; medium_mathematical_space lol
    #\u3000 ; ideographic_space
    ))

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
    (handler-case
      (parse-number
        (concatenate 'string
          sign
          number
          (when fraction (first fraction))
          (when fraction (second fraction))
          (or exponent "")))
      (floating-point-overflow ()
        (apply-sign sign most-positive-double-float)))))

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
  (let* ((perspective (subseq text position end))
         (first-space-index
           (loop for char across perspective and i upfrom 0
             do (when (not (identifier-char-p char)) (return i))))
          (token (subseq perspective 0 first-space-index)))
    (if (bare-identifier-p token)
      (values token (+ (length token) position))
      (values nil position))))

(defrule bare-identifier
  (function parse-bare-identifier)
  (:text t))

(defparameter *escape-map*
  '(#\n #\newline
     #\r #\return
     #\t #\tab
     #\\ #\\
     #\/ #\solidus
     #\" #\"
     #\b #\backspace
     #\f #\page))

(defrule escape
  (and #\\
    (or #\n #\" #\\ #\b #\r #\t #\f #\/
      (and "u{"
        hexit
        (? hexit)
        (? hexit)
        (? hexit)
        (? hexit)
        (? hexit)
        #\})))
  (:destructure (slash escape)
    (declare (ignore slash))
    (if (listp escape)
      (code-char
        (parse-integer
          (concatenate 'string (remove-if 'null (subseq escape 1 7)))
          :radix 16))
      (getf *escape-map* (char escape 0)))))

(defrule string-character
  (or escape (not (or #\\ #\")))
  (:text t))

(defrule basic-string
  (and #\" (* string-character) #\"))


(defun subseq-recklessly (sequence start end)
  (let ((length (length sequence)))
    (subseq sequence start (min length end))))

(defun collect-raw-string-contents (string+ string-start-delimiter)
  (let
    ((length (length string-start-delimiter)) chars)
    (loop for char across string+
      do (push char chars)
      until (equal
              (subseq-recklessly chars 0 length)
              string-start-delimiter))
    (if (equal
           (subseq-recklessly chars 0 length)
          string-start-delimiter)
      (reverse (subseq chars length))
      nil)))

(defun parse-raw-string (text position end)
  (let ((perspective (subseq text position end)))
    (if (and
          (> (length perspective) 0)
          (member
            (elt perspective 0)
            (list #\" #\#)))
      (let* ((hashes
                (loop for char across perspective
                  collecting char
                  until (char= #\" char)))
              (hashcount (length hashes))
              (string
                (collect-raw-string-contents
                  (subseq perspective hashcount) hashes))
              (string-length (length string))
              (quotes-length (* 2 hashcount)))
        (if string
          (values
            (list hashes string (reverse hashes))
            (+ position string-length quotes-length))
          (values nil position)))
      (values nil position))))

(defrule raw-string
  (and "r" (function parse-raw-string))
  (:function second))

(defrule string (or basic-string raw-string)
  (:destructure (lq string rq)
    (declare (ignore lq rq))
    (concatenate 'string (mapcar 'character string))))

(defrule identifier (or string bare-identifier)
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
  (:lambda (keyword)
    (if *parse-keywords-as-keywords*
      (intern (string-upcase keyword) :keyword)
      (string=case keyword
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

(defrule property (and identifier "=" value)
  (:destructure (property = value)
    (declare (ignore =))
    (cons property value)))

(defrule escaped-vertical-space
  (and #\\ (* horizontal-space) (? line-comment) vertical-space))

(defrule node-space
  (or
    (and (* horizontal-space) escaped-vertical-space (* horizontal-space))
    (+ horizontal-space)))

(defrule node-terminator (or line-comment vertical-space ";" (! character)))

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
  (and node-space+ node-property)
  (:destructure (leading-space property)
    (declare (ignore leading-space))
    property))

(defrule node-children-inline
  (and node-space* node-children horizontal-space*)
  (:destructure (leading-space children trailing-space)
    (declare (ignore leading-space trailing-space))
    children))

(defun same-property (a b)
  (and
    (not (null (car a)))
    (equal (car a) (car b))))

(defrule node-properties
  (* node-property-inline)
  (:lambda (properties)
    (remove-duplicates properties
        :test 'same-property)))

(defrule node
  (and
    (? node-comment-start)
    (? type)
    identifier
    node-properties
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

(defrule nodes
  (and
    (* space)
    (and (? node) (? nodes))
    (* space))
  (:destructure (leading-space nodes trailing-space)
    (declare (ignore leading-space trailing-space))
    (fledge nodes)))

(defrule document nodes
  (:lambda (nodes)
    (remove-if 'null nodes)))
