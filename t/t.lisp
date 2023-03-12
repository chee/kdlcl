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
                          (uiop:read-file-string expectation-path))))
                (if succeedp
                  (lambda ()
                    (list test-name #'string=
                      (kdl:to-string (kdl:from-file test-file))
                      expected))
                  (lambda ()
                    (list test-name #'null
                      (handler-case
                        (kdl:to-string (kdl:from-file test-file))
                        (error nil)))))))))



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
  (destructuring-bind (test-name fn &rest args) (funcall test)
    (let ((result (apply fn args)))
      (format t "~%")
      (cond
        ((null result)
          (print-colourfully "❌ SORRY " :red)
          (print-colourfully test-name)
          (fresh-line)
          (print-colourfully "Checking " :Black 0)
          (print-colourfully (string-downcase (nth-value 2 (function-lambda-expression fn))) :blue 0)
          (when (and (second args) (string-not-equal "" (second args)))
            (fresh-line)
            (print-colourfully "Expected " :Black 0)
            (print-colourfully (trim (second args)) :green))
          (fresh-line)
          (print-colourfully "Received " :Black 0)
          (print-colourfully (trim (first args)) :red))
        (t
          (print-colourfully "✅ PASS " :green)
          (print-colourfully test-name))))
    (fresh-line)))


(defun run-test-cases ()
  (loop
    for test-result
    in (collect-test-cases)
    do (print-result test-result)))
