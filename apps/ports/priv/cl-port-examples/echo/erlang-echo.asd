(defsystem "erlang-echo"
  :version "0.1.0"
  :author "Duncan McGreggor <oubiwann@gmail.com>"
  :license "BSD-2"
  :depends-on ("erlang-term-optima" "log4cl")
  :components ((:module "src"
                :components
                ((:file "echo-const")
                 (:file "echo-util")
                 (:file "echo-messaging")
                 (:file "echo-commands")
                 (:file "echo-protocol")
                 (:file "echo"))))
  :description "Echo: An Erlang Port Axample Application"
  :in-order-to ((test-op (test-op "erlang-echo-test"))))
