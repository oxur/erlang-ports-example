use erlang_ports_rs::commands::echo;
use erlang_ports_rs::port::protocol;
use log;
use twyg;

fn main() {
    log::info!("Starting Rust port server ...");
    let log_opts = twyg::LoggerOpts {
        colored: true,
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
