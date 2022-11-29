(defpackage echo-const
  (:use :cl)
  (:export :*delimiter*
           :*max-buffer-size*
           :*log-level*
           :*debug-file*
           :*input*
           :*output*))
(in-package :echo-const)

(defconstant *delimiter* (char-code #\newline))
(defconstant *max-buffer-size* 4096)
(defconstant *log-level* :warn)
;; (defconstant *debug-file* "debug.log")
;; Legal values for the following:
;; *terminal-io* *query-io*
(defconstant *input* *terminal-io*)
;; Legal values for the following:
;; *terminal-io* *query-io* *standard-output* sb-ext::*stdout*
(defconstant *output* sb-ext::*stdout*)
