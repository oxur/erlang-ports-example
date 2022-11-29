(asdf:defsystem #:erlang-echo-test
  :description "Test system for echo"
  :author "Duncan McGreggor <oubiwann@gmail.com>"
  :license "BSD-2"
  :defsystem-depends-on ("prove-asdf")
  :depends-on ("erlang-echo"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "echo"))))
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
