#!/bin/sh
#|
exec sbcl --script "$0" $@
|#

;; Time-stamp: <2017-02-03 17:12:33>
;; Copyright (C) 2017 Pierre Lecocq

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :cl-ppcre :silent t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *log-parser-data* '((remote-addr       . '((index . 0)
                                                   (eq-func . string=)
                                                   (regex . "(\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3})+")
                                                   (label . "By remote address")))
                            (time-local        . '((index . 3)
                                                   (eq-func . string=)
                                                   (regex . "(\\[([^\\]]+)\\])+")
                                                   (label . "By date")))
                            (request           . '((index . 5)
                                                   (eq-func . string=)
                                                   (regex . "(\"([^\"]+)\")+")
                                                   (label . "By request")))
                            (status            . '((index . 6)
                                                   (eq-func . eq)
                                                   (regex . "(\\d+)")
                                                   (label . "By status")))
                            (http-user-agent   . '((index . 11)
                                                   (eq-func . string=)
                                                   (regex . "(\"([^\"]+)\")+")
                                                   (label . "By user agent")))
                            (request-path       . '((index . nil)
                                                    (eq-func . string=)
                                                    (regex . nil)
                                                    (label . "By path")))
                            (request-verb       . '((index . 11)
                                                    (eq-func . string=)
                                                    (regex . nil)
                                                    (label . "By verb")))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass entry ()
  ((remote-addr :accessor entry-remote-addr :initform nil :initarg :remote-addr)
   (time-local :accessor entry-time-local :initform nil :initarg :time-local)
   (request :accessor entry-request :initform nil :initarg :request)
   (status :accessor entry-status :initform nil :initarg :status)
   (http-user-agent :accessor entry-http-user-agent :initform nil :initarg :http-user-agent)
   (request-verb :accessor entry-request-verb :initform nil :initarg :request-verb)
   (request-path :accessor entry-request-path :initform nil :initarg :request-path)))

(defmethod initialize-instance :around ((e entry) &key remote-addr time-local request status http-user-agent)
  (let ((request-chunks (ppcre:split "\\s+" request)))
    (call-next-method e
                      :remote-addr remote-addr
                      :time-local time-local
                      :request request
                      :status status
                      :http-user-agent http-user-agent
                      :request-verb (nth 0 request-chunks)
                      :request-path (ppcre:regex-replace "\\?(.*)" (nth 1 request-chunks) ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun display-group (group title total)
  (format t "~% * ~A~%" title)
  (loop for (k . v) in group
     do (format t "~a~a~a~a~4f%~%" k #\tab v #\tab (/ (* v 100) total))))

(defun pml (logfile metrics)
  (let ((entries (entries-from-file logfile)))
    (mapcar #'(lambda (metric)
                (display-group
                 (group-by-property entries metric (log-fmt-value-by-metric metric 'eq-func))
                 (log-fmt-value-by-metric metric 'label)
                 (length entries)))
            metrics)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main (argv)
  (unless argv
    (error "Arguments required"))
  (let ((logfile (car argv))
        (metrics '()))
    (unless (probe-file logfile)
      (error "~a file not found~%" logfile))
    (dolist (metric (cdr argv))
      (cond ((string= metric "ip") (setq metrics (cons 'remote-addr metrics)))
            ((string= metric "path") (setq metrics (cons 'request-path metrics)))
            ((string= metric "verb") (setq metrics (cons 'request-verb metrics)))
            ((string= metric "status") (setq metrics (cons 'status metrics)))
            ((string= metric "agent") (setq metrics (cons 'http-user-agent metrics)))
            (t (error "Unknown metric '~a'. Supported metrics are ~a~%" metric '("ip" "path" "verb" "status" "agent")))))
    (unless metrics
      (error "Missing metrics in arguments"))
    (pml logfile (reverse metrics))))

(main (cdr sb-ext:*posix-argv*))
