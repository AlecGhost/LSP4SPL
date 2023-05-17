use criterion::{black_box, criterion_group, criterion_main, Criterion};
use spl_frontend::{lexer, TextChange};
use std::time::Duration;

fn lexer_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("lexer");
    group.measurement_time(Duration::from_secs(10));

    let src = std::fs::read_to_string("tests/programs/bigtest.spl").unwrap();

    // no change
    group.bench_function("no_change", |b| b.iter(|| lexer::lex(black_box(&src))));

    let tokens = lexer::lex(&src);
    let no_change = TextChange {
        range: 0..0,
        text: "".to_string(),
    };
    group.bench_function("inc_no_change", |b| {
        b.iter(|| {
            lexer::update(
                black_box(&src),
                black_box(tokens.clone()),
                black_box(&no_change),
            )
        })
    });

    // single character change
    let mut single_token_src = src.clone();
    single_token_src.replace_range(1500..1504, "type");

    group.bench_function("single_token_change", |b| {
        b.iter(|| lexer::lex(black_box(&single_token_src)))
    });

    let single_token_change = TextChange {
        range: 1500..1504,
        text: "type".to_string(),
    };
    group.bench_function("inc_single_token_change", |b| {
        b.iter(|| {
            lexer::update(
                black_box(&single_token_src),
                black_box(tokens.clone()),
                black_box(&single_token_change),
            )
        })
    });
    // double src
    let double_src = String::new() + &src + &src;
    group.bench_function("double_src", |b| {
        b.iter(|| lexer::lex(black_box(&double_src)))
    });

    let double_change = TextChange {
        range: src.len()..src.len(),
        text: src.clone(),
    };
    group.bench_function("inc_double_src", |b| {
        b.iter(|| {
            lexer::update(
                black_box(&double_src),
                black_box(tokens.clone()),
                black_box(&double_change),
            )
        })
    });
}

criterion_group!(benches, lexer_benchmark);
criterion_main!(benches);
