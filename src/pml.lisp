;;;; pml.lisp

;; Time-stamp: <2017-05-16 09:33:19>
;; Copyright (C) 2017 Pierre Lecocq

(defun help-message ()
  (format t "usage: pml.lisp [--logfile FILE] [--format FORMAT] [METRICS] [FILTERS]~%")
  (format t "~%formats:~%")
  (format t "     json~%")
  (format t "     csv~%")
  (format t "     txt (default)~%")
  (format t "~%metrics:~%")
  (format t "     --agent : group by user agent~%")
  (format t "     --ip : group by IP~%")
  (format t "     --path : group by path~%")
  (format t "     --status : group by status~%")
  (format t "     --verb : group by verb~%")
  (format t "~%filters:~%")
  (format t "     --start : from a given date~%")
  (format t "     --end : until a given date~%")
  (sb-ext:exit))

(defun parse-cli-opts (argv)
  (let ((logfile nil)
        (format "txt")
        (metrics '())
        (filters '()))
    (multiple-value-bind (args opts)
        (getopt:getopt argv '(("logfile"   :required)
                              ("format"    :required)
                              ;; metrics
                              ("agent"     :none)
                              ("ip"        :none)
                              ("path"      :none)
                              ("status"    :none)
                              ("verb"      :none)
                              ;; filters
                              ("start"     :required)
                              ("end"       :required)))
      (declare (ignore args))
      (dolist (opt opts)
        (cond
	 ((string= (car opt) "logfile") (setq logfile (cdr opt)))
	 ((string= (car opt) "format") (setq format (cdr opt)))
	 ;; metrics
	 ((string= (car opt) "agent") (setq metrics (cons 'http-user-agent metrics)))
	 ((string= (car opt) "ip") (setq metrics (cons 'remote-addr metrics)))
	 ((string= (car opt) "path") (setq metrics (cons 'request-path metrics)))
	 ((string= (car opt) "status") (setq metrics (cons 'status metrics)))
	 ((string= (car opt) "verb") (setq metrics (cons 'request-verb metrics)))
	 ;; filters
	 ((string= (car opt) "start") (setq filters (acons 'start (local-time:parse-timestring (cdr opt)) filters)))
	 ((string= (car opt) "end") (setq filters (acons 'end (local-time:parse-timestring (cdr opt)) filters))))))
    (values logfile (nreverse metrics) filters format)))

(defun parse-my-log (logfile metrics filters output-format)
  (unless metrics
    (help-message))
  (unless (probe-file logfile)
    (error "~a file not found~%" logfile))
  (let ((entries (entries-from-file logfile filters)))
    (mapcar #'(lambda (metric)
                (display-group
		 output-format
                 (group-by-property entries metric (log-fmt-value-by-metric metric 'eq-func))
                 (log-fmt-value-by-metric metric 'label)
                 (length entries)))
            metrics)))
