(defsystem "erlang-echo-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Duncan McGreggor <oubiwann@gmail.com>"
  :license "BSD-2"
  :depends-on ("erlang-echo"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "echo"))))
  :description "Test system for echo"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
