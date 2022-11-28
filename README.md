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
cd apps/ports/priv/cl-port-example/
ln -s `pwd` ~/quicklisp/local-projects/
cd -
```

Or you can run this convenience `make` target instead:

```shell
make quicklisp-link
```

## Build & Run

```shell
make
```

This will build the Go and Common Lisp servers that are used in the examples, compile the LFE, and create an OTP release.

Now the app is ready to run:

```shell
make run
```

This will put you into a release console running the Erlang shell. Switch to LFE with the following:

```erlang
1> lfe_shell:start().
```

See the running `gen_server`s for Go and CL:

```lisp
lfe> (ports.app:children)
```

```lisp
(#(ports.lisp.server #Pid<0.366.0> worker (ports.lisp.server))
 #(ports.go.server #Pid<0.365.0> worker (ports.go.server)))
```

## Echo Examples

```lisp
(ports.go.server:send #(command echo))
```

```lisp
#(result "echo")
```

```lisp
(ports.lisp.server:send #(command echo))
```

```lisp
#(result "echo")
```

## Crashing the Language Servers

You can induce a crash in the Go and Common Lisp echo servers. To easily see that a new port gets started up, let's take a look at the server PIDs and port IDs:

```lisp
(ports@spacemac)lfe> (ports.app:servers)
(#(go #Pid<0.356.0>) #(lisp #Pid<0.357.0>))
(ports@spacemac)lfe> (ports.app:ports)
(#(go #Port<0.7>) #(lisp #Port<0.8>))
```

Now let's crash things:

```lisp
(ports@spacemac)lfe> (ports.go.server:send #(command crash_it))
(ports@spacemac)lfe> (ports.lisp.server:send #(command crash_it))
```

Let's confirm that there are now new servers and ports created:

```lisp
(ports@spacemac)lfe> (ports.app:servers)
(#(go #Pid<0.382.0>) #(lisp #Pid<0.385.0>))
(ports@spacemac)lfe> (ports.app:ports)
(#(go #Port<0.10>) #(lisp #Port<0.11>))
```

<!-- Named page links below: /-->

[logo]: priv/images/project-logo.png
[logo-large]: priv/images/project-logo-large.png
[gh-actions-badge]: https://github.com/lfex/port-examples/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/lfex/port-examples/actions
[lfe]: https://github.com/rvirding/lfe
[lfe badge]: https://img.shields.io/badge/lfe-1.3.0-blue.svg
[erlang badge]: https://img.shields.io/badge/erlang-19%20to%2023-blue.svg
[versions]: https://github.com/lfex/port-examples/blob/master/.github/workflows/cicd.yml
