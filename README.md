# port-examples

[![Build Status][gh-actions-badge]][gh-actions] [![LFE Versions][lfe badge]][lfe] [![Erlang Versions][erlang badge]][versions]

[![][logo]][logo-large]

*An LFE/OTP application for running Port examples in various languages*

## Dependencies & Setup

This application assumes that the following are on your system:

* `git`
* GNU `make`
* A modern install of Erlang (v20+)
* [rebar3](https://www.rebar3.org/) (Erlang build tool)
* Golang
* SBCL (Steel Bank Common Lisp)
* [Quicklisp]()

This project's `rebar.config.script` will set the required Go environment
variables, but you will need to link the Common Lisp examples to your local
Quicklisp directory:

```shell
$ cd apps/ports/priv/cl-port-examples/
$ ln -s `pwd` ~/quicklisp/local-projects/
$ cd -
```

Or you can run this convenience `make` target instead:

```shell
$ make quicklisp-link
```

## Build & Run

```shell
$ make
```

This will clone the Go and Common Lisp repos that are used in the examples.
Now you need to tell Quicklisp about the cloned Common Lisp libs:



Now the app is ready to run:

```shell
$ make run
```

This will put you into a release console running the Erlang shell. Switch to LFE with the following:

```erlang
1> lfe_shell:start().
```

See the running `gen_server`s for Go and CL:

```lisp
lfe> (ports-app:children)
```
```lisp
(#(lisp-echo-server #Pid<0.357.0> worker (lisp-echo-server))
 #(go-echo-server #Pid<0.356.0> worker (go-echo-server)))
```

## Echo Examples

```lisp
(go-echo-server:send #(command echo))
```
```lisp
#(result "echo")
```

```lisp
(lisp-echo-server:send #(command echo))
```
```lisp
#(result "echo")
```

<!-- Named page links below: /-->

[logo]: priv/images/project-logo.png
[logo-large]: priv/images/project-logo-large.png
[github]: https://github.com/lfex/port-examples
[gitlab]: https://gitlab.com/lfex/port-examples
[gh-actions-badge]: https://github.com/lfex/port-examples/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/lfex/port-examples/actions
[lfe]: https://github.com/rvirding/lfe
[lfe badge]: https://img.shields.io/badge/lfe-1.3.0-blue.svg
[erlang badge]: https://img.shields.io/badge/erlang-19%20to%2023-blue.svg
[versions]: https://github.com/lfex/port-examples/blob/master/.github/workflows/cicd.yml
