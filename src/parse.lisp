;;;; parse.lisp

;; Time-stamp: <2017-02-07 14:38:04>
;; Copyright (C) 2017 Pierre Lecocq

(defun entry-from-line (line)
  (let ((scanner (ppcre:create-scanner *log-fmt-regex*)))
    (multiple-value-bind (matched data) (ppcre:scan-to-strings scanner line)
      (when matched
        (let ((request (aref data (log-fmt-value-by-metric 'request 'index))))
          (make-instance 'entry
                         :remote-addr (aref data (log-fmt-value-by-metric 'remote-addr 'index))
                         :time-local (aref data (log-fmt-value-by-metric 'time-local 'index))
                         :request request
                         :status (parse-integer (aref data (log-fmt-value-by-metric 'status 'index)))
                         :http-user-agent (aref data (log-fmt-value-by-metric 'http-user-agent 'index))))))))

(defun entries-from-file (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
       while line
       when (entry-from-line line)
       collect it)))

(defun group-by-property (entries property test)
  (let ((groups '()))
    (mapcar #'(lambda (req)
                (when req
                  (if (assoc (slot-value req property) groups :test test)
                      (setf (cdr (assoc (slot-value req property) groups :test test))
                            (+ 1 (cdr (assoc (slot-value req property) groups :test test))))
                      (setq groups (acons (slot-value req property) 1 groups))))) entries)
    (sort groups #'> :key #'cdr)))
