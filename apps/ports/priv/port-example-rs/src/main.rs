use erlang_ports_example::commands::echo;
use erlang_ports_example::port::protocol;
use log;
use twyg;

fn main() {
    log::info!("Starting Rust port server ...");
    let log_opts = twyg::LoggerOpts {
        coloured: true,
        file: None,
        level: String::from("trace"),
        report_caller: true,
    };

    match twyg::setup_logger(&log_opts) {
        Ok(_) => {}
        Err(error) => panic!("Could not setup logger: {:?}", error),
    };

    protocol::process_port_messages(echo::process_echo_command);
}
