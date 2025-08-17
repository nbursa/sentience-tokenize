use sentience_tokenize::{tokenize, LineMap};

fn main() {
    let src = r#"// demo
let rule greet(name) = "hi, " + name
"#;
    let lm = LineMap::new(src);
    match tokenize(src) {
        Ok(toks) => {
            for t in toks {
                let (ls, cs) = lm.to_line_col(t.span.start);
                let (le, ce) = lm.to_line_col(t.span.end);
                println!("{:?} @{}:{}..{}:{}", t.kind, ls, cs, le, ce);
            }
        }
        Err(e) => eprintln!("error: {}", e),
    }
}
