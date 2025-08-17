use sentience_tokenize::{tokenize_iter, TokenKind};
use std::io::{self, Read};

fn main() {
    let mut src = String::new();
    if io::stdin().read_to_string(&mut src).is_err() {
        eprintln!("failed to read stdin");
        std::process::exit(1);
    }

    for item in tokenize_iter(&src) {
        match item {
            Ok(tok) => match tok.kind {
                TokenKind::Ident(ref s) => {
                    println!("Ident(\"{}\") @{}..{}", s, tok.span.start, tok.span.end)
                }
                TokenKind::Number(ref s) => {
                    println!("Number(\"{}\") @{}..{}", s, tok.span.start, tok.span.end)
                }
                TokenKind::String(ref s) => {
                    println!("String(\"{}\") @{}..{}", s, tok.span.start, tok.span.end)
                }
                ref other => println!("{:?} @{}..{}", other, tok.span.start, tok.span.end),
            },
            Err(e) => {
                eprintln!("error: {}", e);
                std::process::exit(2);
            }
        }
    }
}
