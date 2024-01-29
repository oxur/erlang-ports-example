use eetf::Binary;
use erlang_ports_example::util;

#[test]
fn test_str_to_term() {
    assert_eq!(
        "<<72,101,108,108,195,182,44,32,228,184,150,231,149,140,33>>",
        util::str_to_term("Hellö, 世界!").to_string()
    );
}

#[test]
fn test_term_to_str() {
    assert_eq!(
        "Hellö, 世界!",
        util::term_to_str(Binary::from(vec![
            72, 101, 108, 108, 195, 182, 44, 32, 228, 184, 150, 231, 149, 140, 33
        ]))
    );
}
