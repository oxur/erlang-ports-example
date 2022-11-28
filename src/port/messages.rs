use crate::util;
use eetf::{Atom, Term, Tuple};
use log;
use std::io::{self, Write};

pub fn error_message(err_msg: &str) -> Tuple {
    let err = Tuple::from(vec![
        Term::from(Atom::from("error")),
        Term::from(util::str_to_term(err_msg)),
    ]);
    log::debug!("Created error tuple: {:?}", err.to_string());
    return err;
}

pub fn result_message(value: &str) -> Tuple {
    let msg = Tuple::from(vec![
        Term::from(Atom::from("result")),
        Term::from(util::str_to_term(value)),
    ]);
    log::debug!("Created result tuple: {:?}", msg.to_string());
    return msg;
}

pub fn send_message(msg: Tuple) {
    let mut buf = Vec::new();
    let term = Term::from(msg);
    match term.encode(&mut buf) {
        Ok(_) => {
            io::stdout().write_all(&buf).unwrap();
            io::stdout().write_all(b"\n").unwrap()
        }
        Err(e) => log::error!("{:?}", e),
    }
}

pub fn send_result(value: &str) {
    send_message(result_message(value));
}

pub fn send_error(err_msg: &str) {
    log::error!("{:?}", err_msg);
    send_message(error_message(err_msg));
}
