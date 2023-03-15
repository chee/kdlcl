(in-package :kdl)

(defparameter *indent* 0)

(defun print-indent ()
  (format t "~v@{~A~:*~}" (* *indent* 4) " "))

(defun print-identifier (identifier)
  (if (and (bare-identifier-p identifier)
        (not (string-equal "" identifier)))
    (write-string identifier)
    (format t "~s" identifier)))

(defun print-type (type)
  (write-char #\()
  (print-identifier type)
  (write-char #\)))

(defun print-value (value)
  (typecase value
    (keyword (format t "~a" (string-downcase (symbol-name value))))
    (double-float (princ (substitute #\e #\d (format nil "~a" value))))
    (null (format t "null"))
    (number (format t "~a" value))
    (string
      (write-char #\")
      (write-string (replace-escapes value))
      (write-char #\"))
    (t (format t "~s" value))))

(defun replace-escapes (string)
  (apply 'concatenate 'string
    (loop for char across string
      collect
      (let ((replacement (getf (reverse *escape-map*) char)))
        (if replacement
          (if (char= replacement #\solidus)
            (list #\solidus)
            `(#\\ ,replacement))
          (list char))))))

(defun print-property (property)
  (let ((name (property-name property))
         (value (property-value property))
         (type (property-type property)))
    (when (or name value)
      (write-char #\space)
      (when name
        (print-identifier name)
        (write-char #\=))
      (when type
        (print-type type))
      (print-value value))))

(defun print-children (children)
  (write-string " {")
  (loop for child in children do
    (let ((*indent* (1+ *indent*)))
      (print-node child)))
  (write-char #\newline)
  (print-indent)
  (write-string "}"))

(defun print-node (node)
  (when node
    (let ((name (node-name node))
           (type (node-type node))
           (properties (node-properties node))
           (children (node-children node)))
      (fresh-line)
      (print-indent)
      (when type (print-type type))
      (print-identifier name)
      (loop for property in properties do (print-property property))
      (when (and children (not (every 'null children)))
        (print-children children)))))
