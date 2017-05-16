;;;; format.lisp

;; Time-stamp: <2017-05-16 09:37:43>
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
	do (format t "\"~a\": { \"value\": \"~a\", \"percentage\": \"~4f\" },~%" k v (/ (* v 100) total)))
  (format t "}~%"))

(defun display-group-function (output-format)
  (cond
   ((string= output-format "json") #'display-group-json)
   ((string= output-format "csv") #'display-group-csv)
   (t #'display-group-txt)))

(defun display-group (output-format group title total)
  (funcall (display-group-function output-format) group title total))
