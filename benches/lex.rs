use criterion::{black_box, criterion_group, criterion_main, Criterion};
use sentience_tokenize::tokenize;

fn big_input() -> String {
    let line = "let rule greet(name) = \"hi, \" + name\n";
    let mut s = String::with_capacity(100 * 1024);
    while s.len() < 100 * 1024 {
        s.push_str(line);
    }
    s
}

fn bench_tokenize(c: &mut Criterion) {
    let src = big_input();
    c.bench_function("tokenize 100KB", |b| {
        b.iter(|| {
            let toks = tokenize(black_box(&src)).unwrap();
            black_box(toks);
        })
    });
}

criterion_group!(benches, bench_tokenize);
criterion_main!(benches);
