#!/bin/sh
#|
exec sbcl --script "$0" $@
|#

;; Time-stamp: <2017-02-07 15:38:50>
;; Copyright (C) 2017 Pierre Lecocq

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :cl-ppcre :silent t)
(ql:quickload :getopt :silent t)

(load "src/define.lisp")
(load "src/entry.lisp")
(load "src/parse.lisp")
(load "src/format.lisp")
(load "src/pml.lisp")

(multiple-value-bind (logfile metrics format)
    (parse-cli-opts (cdr sb-ext:*posix-argv*))
  (pml logfile metrics format))
