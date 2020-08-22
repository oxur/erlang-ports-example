use eetf::{Atom, List, Term, Tuple};
use log;

fn error_message(errMsg: Vec<u8>) -> Tuple {
    let err = Tuple::from(vec![
        Term::from(Atom::from("error")),
        // Term::from(List::from(errMsg)),
    ]);
    log::debug!("Created error tuple: {:?}", err);
    return err;
}

pub fn send_result(value: &str) {}

pub fn send_error(value: &str) {}
