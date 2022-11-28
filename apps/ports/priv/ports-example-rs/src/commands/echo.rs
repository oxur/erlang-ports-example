use crate::port::messages;
use eetf::{Atom, Term};
use log;

pub fn process_echo_command(command: &str) {
    match command {
        "echo" => messages::send_result("echo"),
        "stop" => log::info!("Stopping Rust Echo server ..."),
        "crash_it" => log::info!("Intentionally crashing the Rust Echo server ..."),
        _ => messages::send_error("Received unsupported command"),
    }
}
