# port-examples

*An Erlang/OTP application for running Port examples in various languages*

## Dependencies & Setup

This application assumes that the following are on your system:

* `git`
* GNU `make`
* A modern install of Erlang (v20+)
* [rebar3](https://www.rebar3.org/) (Erlang build tool)
* Golang
* SBCL (Steel Bank Common Lisp)
* Quicklisp (CL deps manager)

This project's `rebar.config.script` will sett the required Go environment
variables, but you will need to link the Common Lisp examples to your local
Quicklisp directory (see below for details).

## Build & Run

```shell
$ make
```

This will clone the Go and Common Lisp repos that are used in the examples.
Now you need to tell Quicklisp about the cloned Common Lisp libs:

```shell
$ cd apps/ports/priv/cl-port-examples/
$ ln -s `pwd` ~/quicklisp/local-projects/
$ cd -
```

Now the app is ready to run:

```shell
$ make run
```

See the running port `gen_server`s:

```erlang
> ports_app:children().
[{lisp_echo_server,<0.258.0>,worker,[lisp_echo_server]},
 {go_echo_server,<0.257.0>,worker,[go_echo_server]}]
```

## Echo Examples

```erlang
go_echo_server:send({command, echo}).
```
```erlang
{result,"echo"}
```

```erlang
lisp_echo_server:send({command, echo}).
```
```erlang
{result,"echo"}
```
