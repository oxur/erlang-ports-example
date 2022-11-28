use eetf::Binary;
use log;

pub fn str_to_term(s: &str) -> Binary {
    return Binary::from(s.as_bytes());
}

pub fn term_to_str(bin: Binary) -> String {
    match String::from_utf8(bin.bytes) {
        Ok(res) => res,
        Err(e) => {
            log::error!("{:?}", e);
            String::from("")
        }
    }
}
