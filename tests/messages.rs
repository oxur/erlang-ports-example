use erlang_ports_example::messages;

#[test]
fn test_error_message() {
    let msg = messages::error_message("oops");
    assert_eq!("{'error',<<111,111,112,115>>}", msg.to_string());
}

#[test]
fn test_result_message() {
    let msg = messages::result_message("froody");
    assert_eq!("{'result',<<102,114,111,111,100,121>>}", msg.to_string());
}
