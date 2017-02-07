;;;; entry.lisp

;; Time-stamp: <2017-02-07 14:35:39>
;; Copyright (C) 2017 Pierre Lecocq

(defclass entry ()
  ((remote-addr         :accessor entry-remote-addr
                        :initform nil
                        :initarg :remote-addr)

   (time-local          :accessor entry-time-local
                        :initform nil
                        :initarg :time-local)

   (request             :accessor entry-request
                        :initform nil
                        :initarg :request)

   (status              :accessor entry-status
                        :initform nil
                        :initarg :status)

   (http-user-agent     :accessor entry-http-user-agent
                        :initform nil
                        :initarg :http-user-agent)

   (request-verb        :accessor entry-request-verb
                        :initform nil
                        :initarg :request-verb)

   (request-path        :accessor entry-request-path
                        :initform nil
                        :initarg :request-path)))

(defmethod initialize-instance :around ((ent entry) &key remote-addr time-local request status http-user-agent)
  (let ((request-chunks (ppcre:split "\\s+" request)))
    (call-next-method ent
                      :remote-addr remote-addr
                      :time-local time-local
                      :request request
                      :status status
                      :http-user-agent http-user-agent
                      :request-verb (nth 0 request-chunks)
                      :request-path (ppcre:regex-replace "\\?(.*)" (nth 1 request-chunks) ""))))
