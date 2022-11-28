use eetf::{Binary, Term};

#[test]
fn test_binary() {
    let b_vec = vec![
        72, 101, 108, 108, 195, 182, 44, 32, 228, 184, 150, 231, 149, 140, 33,
    ];
    let b = Binary::from(b_vec);
    let b_str = b.to_string();
    assert_eq!(
        "<<72,101,108,108,195,182,44,32,228,184,150,231,149,140,33>>",
        b_str
    );
    let b_term = Term::from(b);
    assert_eq!(
        "<<72,101,108,108,195,182,44,32,228,184,150,231,149,140,33>>",
        b_term.to_string()
    );
    let mut buf = Vec::new();
    b_term.encode(&mut buf).unwrap();
    assert_eq!(
        vec![
            131, 109, 0, 0, 0, 15, 72, 101, 108, 108, 195, 182, 44, 32, 228, 184, 150, 231, 149,
            140, 33
        ],
        buf
    );
}
