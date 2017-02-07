;;;; define.lisp

;; Time-stamp: <2017-02-07 14:41:56>
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
