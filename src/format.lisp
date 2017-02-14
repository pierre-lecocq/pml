;;;; format.lisp

;; Time-stamp: <2017-02-14 14:54:01>
;; Copyright (C) 2017 Pierre Lecocq

(defun display-group-txt (group title total)
  (format t "~% * ~A~%" title)
  (loop for (k . v) in group
     do (format t "~a~a~a~a~4f%~%" k #\tab v #\tab (/ (* v 100) total))))

(defun display-group-csv (group title total)
  (declare (ignore title))
  (loop for (k . v) in group
     do (format t "\"~a\",~a,\"~4f%\"~%" k v (/ (* v 100) total))))

(defun display-group-json (group title total)
  (format t "\"~A\": {~%" title)
  (loop for (k . v) in group
     do (format t "\"item\": \"~a\",~%\"value\": ~a,~%\"percentage\": ~4f~%" k v (/ (* v 100) total)))
  (format t "}~%"))
