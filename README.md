# erlang-ports-example

[![][build-badge]][build]
[![][crate-badge]][crate]
[![][tag-badge]][tag]
[![][docs-badge]][docs]

[![][logo]][logo-large]

*Example Rust applications that communicate with Erlang/OTP applications via Erlang Ports*

## Usage

This Rust application is intended to be used as part of the [LFE/OTP Port Examples](https://github.com/lfex/port-examples) project (which includes other language integrations such as Common Lisp and Go).

The parent project should:

* have `cargo` in its `PATH`
* ensure this Rust project gets compiled as a release
* calls the compiled executable to start the Rust server

## License

Copyright © 2020-2024, Oxur Group

BSD 3-clause License

<!-- Named page links below: /-->

[logo]: resources/images/project-logo.png
[logo-large]: resources/images/project-logo-large.png
[build]: https://github.com/oxur/erlang-ports-example/actions?query=workflow%3Abuild+
[build-badge]: https://github.com/oxur/erlang-ports-example/workflows/build/badge.svg
[crate]: https://crates.io/crates/twyg
[crate-badge]: https://img.shields.io/crates/v/erlang-ports-example.svg
[docs]: https://docs.rs/twyg/
[docs-badge]: https://img.shields.io/badge/rust-documentation-blue.svg
[tag-badge]: https://img.shields.io/github/tag/oxur/erlang-ports-example.svg
[tag]: https://github.com/oxur/erlang-ports-example/tags
