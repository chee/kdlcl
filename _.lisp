(defun u-escape (text start end)
  (print text)
  (or (let ((sub (subseq text start end))
        (capture nil)
        (position start))
   (when (char= (first sub) #\u)
     (when (hexitp (second sub))
       (incf position)
       (incf position)
       (setf capture `(#\u ,(second sub)))
       (loop for i from position upto (+ position 5)
         do (let ((char (nth position text)))
              (cond
                ((hexitp char)
                  (setf capture (append capture char))
                  (incf position))
                (t (return)))))
       (values capture position))))
    (values nil nil)))

(defun hexitp (char)
  (member char
    '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
     #\a #\b #\c #\d #\e #\f
       #\A #\A #\B #\C #\D #\E #\F)))
