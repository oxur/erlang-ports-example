;; #!/opt/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")
(ql:system-apropos :erlang-echo)
(ql:quickload :erlang-echo :silent t)

(defun main ()
  (echo:setup-app)
  (echo:run))

(handler-bind ((t #'echo-util:handle-finished))
  (main))

(main)
