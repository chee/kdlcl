(defpackage :kdl/t
  (:use :cl :kdl))
(in-package :kdl/t)

(defun collect-test-cases ()
  (let* ((test-cases-directory
           (asdf:system-relative-pathname :kdl "t/test-cases/"))
          (test-cases-input-directory
            (merge-pathnames "input/" test-cases-directory))
          (test-cases-expectations-directory
            (merge-pathnames "expected_kdl/" test-cases-directory)))
    (loop for test-file
      in (uiop:directory-files test-cases-input-directory)
      collect (let* ((test-name (pathname-name test-file))
                      (expectation-path
                        (merge-pathnames (format nil "~a.kdl" test-name)
                          test-cases-expectations-directory))
                      (succeedp (probe-file expectation-path))
                      (expected
                        (when succeedp
                          (uiop:read-file-string expectation-path)))
                      (actual (handler-case
                                (kdl:to-string (kdl:from-file test-file))
                                (error nil))))
                (if succeedp
                  (list test-name #'string= actual expected)
                  (list test-name #'null actual nil))))))


(defun print-colourfully (text &optional (colour :black) (style 1) (stream t))
  (let*
    ((colours '(:black :red :green :yellow :blue))
      (colour-number (+ 30 (position colour colours))))
    (format stream
      "~c[~a;~am~a~c[0m"
      #\ESC
      style colour-number text
      #\ESC)))

(defun trim (string)
  (string-trim '(#\Space #\Tab #\Newline) string))

(defun print-result (test)
  (destructuring-bind (test-name fn actual expected) test
    (format t "~%")
    (let ((result
            (if expected
              (funcall fn actual expected)
              (funcall fn actual))))
      (cond
        ((null result)
          (print-colourfully "❌ SORRY " :red)
          (print-colourfully test-name)
          (fresh-line)
          (print-colourfully "Checking " :Black 0)
          (print-colourfully (string-downcase
                               (nth-value 2
                                 (function-lambda-expression fn)))
            :blue 0)
          (when expected
            (fresh-line)
            (print-colourfully "Expected " :Black 0)
            (print-colourfully (trim expected) :green))
          (fresh-line)
          (print-colourfully "Received " :Black 0)
          (print-colourfully (trim actual) :red)
          nil)
        (t
          (print-colourfully "✅ PASS " :green)
          (print-colourfully test-name)
          t)))))


(defun run-test-cases ()
  (let ((results
          (loop
            for test-result
            in (collect-test-cases)
            collect (print-result test-result))))
    (fresh-line)
    (princ (substitute "✅" t (substitute "❌" nil results)))
    (fresh-line)
    (format t "PASS: ~a~%SORRY: ~a~%TOTAL: ~a~%"
      (count t results)
      (count-if 'null results)
      (length results))))

(defun run-test-by-name (name)
  (kdl::io
    (asdf:system-relative-pathname
      :kdl (format nil "t/test-cases/input/~a.kdl" name))))

(defun parse-test-by-name (name)
  (kdl::from-file
    (asdf:system-relative-pathname
      :kdl (format nil "t/test-cases/input/~a.kdl" name))))
