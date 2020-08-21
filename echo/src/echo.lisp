(defpackage echo
  (:use :cl
        :log4cl)
  (:export :run
           :setup-app))
(in-package :echo)

(defun setup-app ()
  (let ((log-level echo-const:*log-level*))
    ;; (echo-util:reset-stdout)
    (log:config log-level :daily echo-const:*debug-file* :backup nil)))

(defun run ()
  (log:info "Starting Lisp language server ...")
  (let ((io echo-const:*input*))
    (loop while
      (echo-protocol::process-port-messages echo-const:*delimiter* io))))
