use sentience_tokenize::{tokenize, TokenKind};

fn main() {
    let code = "a.b ..c @d e..f";
    let toks = tokenize(code).unwrap();

    println!("Input: {}", code);
    println!("Tokens:");

    for (i, t) in toks.iter().enumerate() {
        match &t.kind {
            TokenKind::Dot => println!("  {}: Dot @{}..{}", i, t.span.start, t.span.end),
            TokenKind::DoubleDot => {
                println!("  {}: DoubleDot @{}..{}", i, t.span.start, t.span.end)
            }
            TokenKind::At => println!("  {}: At @{}..{}", i, t.span.start, t.span.end),
            TokenKind::Ident(s) => {
                println!("  {}: Ident({}) @{}..{}", i, s, t.span.start, t.span.end)
            }
            other => println!("  {}: {:?} @{}..{}", i, other, t.span.start, t.span.end),
        }
    }

    // Show how dots work with numbers
    println!("\nNumber ranges:");
    let range_toks = tokenize("1..10 5..15").unwrap();
    for t in range_toks {
        match &t.kind {
            TokenKind::Number(s) => println!("  Number: {} @{}..{}", s, t.span.start, t.span.end),
            TokenKind::DoubleDot => println!("  Range operator @{}..{}", t.span.start, t.span.end),
            other => println!("  {:?} @{}..{}", other, t.span.start, t.span.end),
        }
    }

    // Show decimal numbers
    println!("\nDecimal numbers:");
    let decimal_toks = tokenize("3.14 2.5").unwrap();
    for t in decimal_toks {
        match &t.kind {
            TokenKind::Number(s) => println!("  Number: {} @{}..{}", s, t.span.start, t.span.end),
            other => println!("  {:?} @{}..{}", other, t.span.start, t.span.end),
        }
    }
}
