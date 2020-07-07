# port-examples

*An Erlang/OTP application for running Port examples in various languages*

## Build

```shell
$ make && make run
```

## Echo Example

```erlang
go_echo_server:send({command, echo}).
```
```erlang
{result,"echo"}
```
