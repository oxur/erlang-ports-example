;;;; erlang-echo.asd

(asdf:defsystem #:erlang-echo
  :description "Echo: An Erlang Ports Example Application"
  :author "Duncan McGreggor <oubiwann@gmail.com>"
  :license  "BSD-2"
  :version "0.1.0"
  :serial t
  :depends-on (#:erlang-term #:log4cl)
  :components ((:file "package")
               (:file "erlang-echo")
               (:module "src"
                :components
                ((:file "echo-const")
                 (:file "echo-util")
                 (:file "echo-messaging")
                 (:file "echo-commands")
                 (:file "echo-protocol")
                 (:file "echo"))))
  :in-order-to ((test-op (test-op "erlang-echo-test"))))
