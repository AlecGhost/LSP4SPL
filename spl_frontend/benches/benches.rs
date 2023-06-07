use criterion::{black_box, criterion_group, criterion_main, BatchSize, Criterion};
use spl_frontend::{
    ast::Program,
    lexer, parser,
    token::{Token, TokenChange, TokenStream},
    AnalyzedSource, TextChange,
};
use std::time::Duration;

struct ChangeTest {
    org_text: String,
    new_text: String,
    org_tokens: Vec<Token>,
    new_tokens: Vec<Token>,
    org_program: Program,
    text_change: TextChange,
    token_change: TokenChange,
}

fn no_change() -> ChangeTest {
    let org_text = std::fs::read_to_string("tests/programs/bigtest.spl").unwrap();
    let org_tokens = lexer::lex(&org_text);
    let org_program = parser::parse(&org_tokens);

    let new_text = org_text.clone();
    let text_change = TextChange {
        range: 0..0,
        text: "".to_string(),
    };
    let (new_tokens, token_change) = lexer::update(&new_text, org_tokens.clone(), &text_change);
    ChangeTest {
        org_text,
        new_text,
        org_tokens,
        new_tokens,
        org_program,
        text_change,
        token_change,
    }
}

fn single_character_change() -> ChangeTest {
    let org_text = std::fs::read_to_string("tests/programs/bigtest.spl").unwrap();
    let org_tokens = lexer::lex(&org_text);
    let org_program = parser::parse(&org_tokens);

    let mut new_text = org_text.clone();
    new_text.replace_range(1500..1504, "type");
    let text_change = TextChange {
        range: 1500..1504,
        text: "type".to_string(),
    };
    let (new_tokens, token_change) = lexer::update(&new_text, org_tokens.clone(), &text_change);
    ChangeTest {
        org_text,
        new_text,
        org_tokens,
        new_tokens,
        org_program,
        text_change,
        token_change,
    }
}

fn double_source() -> ChangeTest {
    let org_text = std::fs::read_to_string("tests/programs/bigtest.spl").unwrap();
    let org_tokens = lexer::lex(&org_text);
    let org_program = parser::parse(&org_tokens);

    let new_text = String::new() + &org_text + &org_text;
    let text_change = TextChange {
        range: org_text.len()..org_text.len(),
        text: org_text.clone(),
    };
    let (new_tokens, token_change) = lexer::update(&new_text, org_tokens.clone(), &text_change);
    ChangeTest {
        org_text,
        new_text,
        org_tokens,
        new_tokens,
        org_program,
        text_change,
        token_change,
    }
}

fn no_change_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("no_change");

    // lexer
    let ChangeTest {
        org_text,
        new_text,
        org_tokens,
        new_tokens,
        org_program,
        text_change,
        token_change,
    } = no_change();
    group.bench_function("lexer", |b| b.iter(|| lexer::lex(black_box(&new_text))));
    group.bench_function("lexer_inc", |b| {
        b.iter_batched(
            || org_tokens.clone(),
            |org_tokens| {
                lexer::update(
                    black_box(&new_text),
                    black_box(org_tokens),
                    black_box(&text_change),
                )
            },
            BatchSize::SmallInput,
        )
    });

    // parser
    let input = TokenStream::new_with_change(&new_tokens, token_change);
    group.bench_function("parser", |b| {
        b.iter(|| parser::parse(black_box(&new_tokens)))
    });
    group.bench_function("parser_inc", |b| {
        b.iter_batched(
            || (org_program.clone(), input.clone()),
            |(org_program, input)| parser::update(black_box(org_program), black_box(input)),
            BatchSize::SmallInput,
        )
    });

    // whole
    group.bench_function("whole", |b| {
        b.iter_batched(
            || new_text.clone(),
            |new_text| AnalyzedSource::new(black_box(new_text)),
            BatchSize::SmallInput,
        )
    });
    let analyzed_source = AnalyzedSource::new(org_text.clone());
    group.bench_function("whole_inc", |b| {
        b.iter_batched(
            || {
                let src = analyzed_source.clone();
                let changes = vec![text_change.clone()];
                (src, changes)
            },
            |(src, changes)| AnalyzedSource::update(black_box(src), black_box(changes)),
            BatchSize::SmallInput,
        )
    });
}

fn single_token_change_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("single_token_change");

    // lexer
    let ChangeTest {
        org_text,
        new_text,
        org_tokens,
        new_tokens,
        org_program,
        text_change,
        token_change,
    } = single_character_change();
    group.bench_function("lexer", |b| b.iter(|| lexer::lex(black_box(&new_text))));
    group.bench_function("lexer_inc", |b| {
        b.iter_batched(
            || org_tokens.clone(),
            |org_tokens| {
                lexer::update(
                    black_box(&new_text),
                    black_box(org_tokens),
                    black_box(&text_change),
                )
            },
            BatchSize::SmallInput,
        )
    });

    // parser
    let input = TokenStream::new_with_change(&new_tokens, token_change);
    group.bench_function("parser", |b| {
        b.iter(|| parser::parse(black_box(&new_tokens)))
    });
    group.bench_function("parser_inc", |b| {
        b.iter_batched(
            || (org_program.clone(), input.clone()),
            |(org_program, input)| parser::update(black_box(org_program), black_box(input)),
            BatchSize::SmallInput,
        )
    });

    // whole
    group.bench_function("whole", |b| {
        b.iter_batched(
            || new_text.clone(),
            |new_text| AnalyzedSource::new(black_box(new_text)),
            BatchSize::SmallInput,
        )
    });
    let analyzed_source = AnalyzedSource::new(org_text.clone());
    group.bench_function("whole_inc", |b| {
        b.iter_batched(
            || {
                let src = analyzed_source.clone();
                let changes = vec![text_change.clone()];
                (src, changes)
            },
            |(src, changes)| AnalyzedSource::update(black_box(src), black_box(changes)),
            BatchSize::SmallInput,
        )
    });
}

fn double_source_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("double_source");

    // lexer
    let ChangeTest {
        org_text,
        new_text,
        org_tokens,
        new_tokens,
        org_program,
        text_change,
        token_change,
    } = double_source();
    group.bench_function("lexer", |b| b.iter(|| lexer::lex(black_box(&new_text))));
    group.bench_function("lexer_inc", |b| {
        b.iter_batched(
            || org_tokens.clone(),
            |org_tokens| {
                lexer::update(
                    black_box(&new_text),
                    black_box(org_tokens),
                    black_box(&text_change),
                )
            },
            BatchSize::SmallInput,
        )
    });

    // parser
    let input = TokenStream::new_with_change(&new_tokens, token_change);
    group.bench_function("parser", |b| {
        b.iter(|| parser::parse(black_box(&new_tokens)))
    });
    group.bench_function("parser_inc", |b| {
        b.iter_batched(
            || (org_program.clone(), input.clone()),
            |(org_program, input)| parser::update(black_box(org_program), black_box(input)),
            BatchSize::SmallInput,
        )
    });

    // whole
    group.bench_function("whole", |b| {
        b.iter_batched(
            || new_text.clone(),
            |new_text| AnalyzedSource::new(black_box(new_text)),
            BatchSize::SmallInput,
        )
    });
    let analyzed_source = AnalyzedSource::new(org_text.clone());
    group.bench_function("whole_inc", move |b| {
        b.iter_batched(
            || {
                let src = analyzed_source.clone();
                let changes = vec![text_change.clone()];
                (src, changes)
            },
            |(src, changes)| AnalyzedSource::update(black_box(src), black_box(changes)),
            BatchSize::SmallInput,
        )
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default().measurement_time(Duration::from_secs(100)).sample_size(500);
    targets = no_change_benchmark, single_token_change_benchmark, double_source_benchmark
}
criterion_main!(benches);
