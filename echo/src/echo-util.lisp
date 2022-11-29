(defpackage echo-util
  (:use :cl
        :erlang-term)
  (:export :handle-finished
           :reset-stdout
           :write-debug
           :read-bytes
           :bytes->vec
           :bytes->erlterm))
(in-package :echo-util)

(defun handle-finished (e)
  (log:info "Shutting down Lisp application ...")
  (sb-ext:exit))

(defun reset-stdout ()
  (setf sb-ext::*stdout*
	(sb-ext::make-fd-stream 1
				:name "standard output"
				:output t
				:buffering :line
				:external-format :iso-8859-1)))

(defun write-debug (data)
  (with-open-file (str echo-const:*debug-file*
                      :direction :output
                      :if-exists :supersede
                      :if-does-not-exist :create)
    (format str "~a~%" data)))

(defun read-bytes (delimiter stream)
  (loop for byte = (read-byte stream nil)
        while (and byte (not (= byte delimiter)))
        unless (= byte delimiter)
        collect byte))

(defun bytes->vec (bytes)
  (make-array (length bytes)
              :element-type '(unsigned-byte 8) 
              :initial-contents bytes))

(defun bytes->erlterm (bytes)
  (erlang-term:decode (bytes->vec bytes)))
