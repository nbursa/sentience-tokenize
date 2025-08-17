use sentience_tokenize::tokenize;

fn main() {
    let code = r#"let rule greet(n) = "hi, " + n"#;
    let toks = tokenize(code).unwrap();
    for t in toks {
        println!("{:?} @{}..{}", t.kind, t.span.start, t.span.end);
    }
}
