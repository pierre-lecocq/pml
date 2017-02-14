#!/bin/sh
#|
exec sbcl --script "$0" $@
|#

;; Time-stamp: <2017-02-09 19:05:32>
;; Copyright (C) 2017 Pierre Lecocq

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :getopt :silent t)
(ql:quickload :cl-ppcre :silent t)
(ql:quickload :local-time :silent t)

(load "src/entry.lisp")
(load "src/parse.lisp")
(load "src/format.lisp")
(load "src/pml.lisp")

(multiple-value-bind (logfile metrics filters format)
    (parse-cli-opts (cdr sb-ext:*posix-argv*))
  (parse-my-log logfile metrics filters format))
