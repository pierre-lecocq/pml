;;;; parse.lisp

;; Time-stamp: <2017-02-14 14:31:35>
;; Copyright (C) 2017 Pierre Lecocq

(defvar *log-parser-data* '((remote-addr       . '((index . 0)
                                                   (eq-func . string=)
                                                   (regex . "(\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3})+")
                                                   (label . "Remote address")))
                            (time-local        . '((index . 3)
                                                   (eq-func . string=)
                                                   (regex . "(\\[([^\\]]+)\\])+")
                                                   (label . "Date")))
                            (request           . '((index . 5)
                                                   (eq-func . string=)
                                                   (regex . "(\"([^\"]+)\")+")
                                                   (label . "Request")))
                            (status            . '((index . 6)
                                                   (eq-func . eq)
                                                   (regex . "(\\d+)")
                                                   (label . "HTTP status")))
                            (http-user-agent   . '((index . 11)
                                                   (eq-func . string=)
                                                   (regex . "(\"([^\"]+)\")+")
                                                   (label . "User agent")))
                            (request-path       . '((index . nil)
                                                    (eq-func . string=)
                                                    (regex . nil)
                                                    (label . "Path")))
                            (request-verb       . '((index . 11)
                                                    (eq-func . string=)
                                                    (regex . nil)
                                                    (label . "HTTP verb")))))

(defun log-fmt-value-by-metric (metric key)
  (cdr (assoc key (caddr (assoc metric *log-parser-data*)))))

(defvar *log-fmt-regex* (concatenate 'string
                                     "(\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3})+" ; 0
                                     " "
                                     "(-\\s-)+" ; 1
                                     " "
                                     "(\\[([^\\]]+)\\])+" ; 2 & 3
                                     " "
                                     "(\"([^\"]+)\")+" ; 4 & 5
                                     " "
                                     "(\\d+)" ; 6
                                     " "
                                     "(\\d+)" ; 7
                                     " "
                                     "(\"([^\"]+)\")+" ; 8 & 9
                                     " "
                                     "(\"([^\"]+)\")+" ; 10 & 11
                                     ))

(defun short-month-to-integer (short-month)
  (cdr (assoc short-month '(("Jan" . 1)
                            ("Feb" . 2)
                            ("Mar" . 3)
                            ("Apr" . 4)
                            ("May" . 5)
                            ("Jun" . 6)
                            ("Jul" . 7)
                            ("Aug" . 8)
                            ("Sept" . 9)
                            ("Oct" . 10)
                            ("Nov" . 11)
                            ("Dec" . 12)) :test #'string=)))

(defun log-date-to-timestamp (raw-date)
  (let ((chunks (ppcre:split "\\s+" (substitute #\Space #\/ (substitute #\Space #\: raw-date)))))
    (local-time:encode-timestamp 0
                                 (parse-integer (nth 5 chunks))
                                 (parse-integer (nth 4 chunks))
                                 (parse-integer (nth 3 chunks))
                                 (parse-integer (nth 0 chunks))
                                 (short-month-to-integer (nth 1 chunks))
                                 (parse-integer (nth 2 chunks)))))

(defun out-of-filters-bounds (data filters)
  (when filters
    (let ((date (log-date-to-timestamp (aref data (log-fmt-value-by-metric 'time-local 'index))))
          (start (cdr (assoc 'start filters)))
          (end (cdr (assoc 'end filters))))
      (or (and start (local-time:timestamp< date start))
          (and end (local-time:timestamp> date end))))))

(defun entry-from-line (line filters)
  (let ((scanner (ppcre:create-scanner *log-fmt-regex*)))
    (multiple-value-bind (matched data) (ppcre:scan-to-strings scanner line)
      (when matched
        (let ((request (aref data (log-fmt-value-by-metric 'request 'index))))
          (if (out-of-filters-bounds data filters)
              nil
              (make-instance 'entry
                             :remote-addr (aref data (log-fmt-value-by-metric 'remote-addr 'index))
                             :time-local (aref data (log-fmt-value-by-metric 'time-local 'index))
                             :request request
                             :status (parse-integer (aref data (log-fmt-value-by-metric 'status 'index)))
                             :http-user-agent (aref data (log-fmt-value-by-metric 'http-user-agent 'index)))))))))

(defun entries-from-file (path filters)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
       while line
       when (entry-from-line line filters)
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
