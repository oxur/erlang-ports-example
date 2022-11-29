(defpackage echo-protocol
  (:use :cl
        :erlang-term
        :log4cl)
  (:export :process-port-messages))
(in-package :echo-protocol)

(defconstant command-arity 2)
(defconstant command-key-index 0)
(defconstant command-value-index 1)
(defconstant command-atom :|command|)

(defun tuplep (cmd)
  (eq 'erlang-term:erlang-tuple (type-of cmd)))

(defun command-arityp (cmd)
  (= (erlang-term:arity cmd) command-arity))

(defun command-key (cmd)
  (aref (erlang-term:elements cmd) command-key-index))

(defun command-value (cmd)
  (aref (erlang-term:elements cmd) command-value-index))

(defun command-formatp (cmd)
  (eq (command-key cmd) command-atom))

(defun process-port-messages (delimiter stream)
  (log:debug "Processing messages sent to Lisp language server ...")
  (loop named message-processor
        for cmd-bytes = (echo-util:read-bytes delimiter stream)
        while cmd-bytes
        do (progn 
             (log:debug "Got command bytes: " cmd-bytes)
             (let ((cmd-decoded (echo-util:bytes->erlterm cmd-bytes)))
               (log:debug "Command decoded:" cmd-decoded)
               (if (not (tuplep cmd-decoded))
                 (echo-messaging:send-error 
                   (format nil "Unexpected message type: ~a" (type-of cmd-decoded)))
                 (let ((arity (erlang-term:arity cmd-decoded)))
                   (if (not (command-arityp cmd-decoded))
                     (echo-messaging:send-error 
                       (format nil "Tuple of wrong size; expected ~a, got: ~a" command-arity arity))
                     (if (not (command-formatp cmd-decoded))
                       (echo-messaging:send-error 
                         (format nil "Did not receive expected tuple message format {command, ...}"))
                       (echo-commands:process-command (command-value cmd-decoded))))))))))
