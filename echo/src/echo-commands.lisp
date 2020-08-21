(defpackage echo-commands
  (:use :cl)
  (:export :process-command))
(in-package :echo-commands)

(defun process-command (cmd)
  (case cmd
    (:|echo| (echo-messaging:send-result "echo"))
    (:|stop| (quit))
    (:|crash_it| (no-such-function))
    (otherwise (echo-messaging:send-error 
                 (format nil "Received unsupported command: ~a" cmd)))))
