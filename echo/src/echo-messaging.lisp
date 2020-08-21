(defpackage echo-messaging
  (:use :cl
        :erlang-term
        :log4cl)
  (:export :error-message
           :send-error
           :send-message
           :send-result))
(in-package :echo-messaging)

(defconstant error-key :|error|)
(defconstant result-key :|result|)

(defun send-message (tuple)
  (log:debug "Got tuple:" tuple)
  (clear-output echo-const:*output*)
  (map nil 
       (lambda (x) (write-byte x echo-const:*output*))
       (erlang-term:encode tuple))
  (write-char #\newline echo-const:*output*)
  (force-output echo-const:*output*))

(defun send-error (msg)
  (send-message (erlang-term:tuple error-key msg)))

(defun send-result (value)
  (send-message (erlang-term:tuple result-key value)))
