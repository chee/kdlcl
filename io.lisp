(in-package :kdl)

(defmethod read-document ((stream stream))
  "Read kdl document from STREAM."
  (from-stream stream))

(defmethod read-document ((string string))
  "Read kdl document from STRING."
  (from-string string))

(defmethod read-document ((filespec pathname))
  "Read kdl document from the file at path FILESPEC."
  (from-file filespec))

;; TODO Switch to esrap-liquid so we can parse a stream?
(defun from-stream (&optional (stream *standard-input*))
  "Read the kdl file on STREAM into lisp structures."
  (from-string (uiop:read-file-string stream)))

(defun from-string (string)
  (parse-document string))

(defun from-file (filespec)
  (with-open-file (stream filespec)
    (from-stream stream)))

(defun to-stream (document &optional (stream *standard-output*))
  "Format kdl DOCUMENT to OUTPUT-STREAM."
  (let ((*standard-output* stream))
    (loop for node in document
      do (print-node node))
    (format t "~%")))

(defun to-string (document)
  "Format kdl DOCUMENT as a string."
  (with-output-to-string (output)
    (to-stream document output)))

(defun to-file (document filespec)
  "Format kdl DOCUMENT into a file."
  (with-open-file
    (file-stream filespec
      :direction :output
      :if-exists :supersede)
    (to-stream document file-stream)))
