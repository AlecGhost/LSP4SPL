use criterion::{black_box, criterion_group, criterion_main, BatchSize, Criterion};
use spl_frontend::{
    ast::Program,
    lexer, parser,
    token::{Token, TokenChange, TokenStream},
    AnalyzedSource, TextChange,
};
use std::time::Duration;

fn no_change() -> (String, TextChange, Vec<Token>) {
    let text = std::fs::read_to_string("tests/programs/bigtest.spl").unwrap();
    let tokens = lexer::lex(&text);
    let change = TextChange {
        range: 0..0,
        text: "".to_string(),
    };
    (text, change, tokens)
}

fn single_character_change() -> (String, TextChange, Vec<Token>) {
    let mut text = std::fs::read_to_string("tests/programs/bigtest.spl").unwrap();
    let tokens = lexer::lex(&text);
    text.replace_range(1500..1504, "type");
    let change = TextChange {
        range: 1500..1504,
        text: "type".to_string(),
    };
    (text, change, tokens)
}

fn double_source() -> (String, TextChange, Vec<Token>) {
    let text = std::fs::read_to_string("tests/programs/bigtest.spl").unwrap();
    let tokens = lexer::lex(&text);
    let new_text = String::new() + &text + &text;
    let change = TextChange {
        range: new_text.len()..new_text.len(),
        text: new_text.clone(),
    };
    (new_text, change, tokens)
}

fn prepare_parser(tuple: (String, TextChange, Vec<Token>)) -> (Vec<Token>, TokenChange, Program) {
    let (text, text_change, tokens) = tuple;
    let program = parser::parse(&tokens);
    let (new_tokens, token_change) = lexer::update(&text, tokens, &text_change);
    (new_tokens, token_change, program)
}

fn no_change_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("no_change");

    // lexer
    let (text, text_change, tokens) = no_change();
    group.bench_function("lexer", |b| b.iter(|| lexer::lex(black_box(&text))));
    group.bench_function("lexer_inc", |b| {
        b.iter_batched(
            || tokens.clone(),
            |tokens| lexer::update(black_box(&text), black_box(tokens), black_box(&text_change)),
            BatchSize::SmallInput,
        )
    });

    // parser
    let (tokens, token_change, program) = prepare_parser(no_change());
    let input = TokenStream::new_with_change(&tokens, token_change);
    group.bench_function("parser", |b| b.iter(|| parser::parse(black_box(&tokens))));
    group.bench_function("parser_inc", |b| {
        b.iter_batched(
            || (program.clone(), input.clone()),
            |(program, input)| parser::update(black_box(program), black_box(input)),
            BatchSize::SmallInput,
        )
    });

    // complete
    group.bench_function("complete", |b| {
        b.iter_batched(
            || text.clone(),
            |text| AnalyzedSource::new(black_box(text)),
            BatchSize::SmallInput,
        )
    });
    let analyzed_source = AnalyzedSource::new(text.clone());
    group.bench_function("complete_inc", |b| {
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
    let (text, text_change, tokens) = single_character_change();
    group.bench_function("lexer", |b| b.iter(|| lexer::lex(black_box(&text))));
    group.bench_function("lexer_inc", |b| {
        b.iter_batched(
            || tokens.clone(),
            |tokens| lexer::update(black_box(&text), black_box(tokens), black_box(&text_change)),
            BatchSize::SmallInput,
        )
    });

    // parser
    let (tokens, token_change, program) = prepare_parser(single_character_change());
    let input = TokenStream::new_with_change(&tokens, token_change);
    group.bench_function("parser", |b| b.iter(|| parser::parse(black_box(&tokens))));
    group.bench_function("parser_inc", |b| {
        b.iter_batched(
            || (program.clone(), input.clone()),
            |(program, input)| parser::update(black_box(program), black_box(input)),
            BatchSize::SmallInput,
        )
    });

    // complete
    group.bench_function("complete", |b| {
        b.iter_batched(
            || text.clone(),
            |text| AnalyzedSource::new(black_box(text)),
            BatchSize::SmallInput,
        )
    });
    let analyzed_source = AnalyzedSource::new(text.clone());
    group.bench_function("complete_inc", |b| {
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
    let (text, text_change, tokens) = double_source();
    group.bench_function("lexer", |b| b.iter(|| lexer::lex(black_box(&text))));
    group.bench_function("lexer_inc", |b| {
        b.iter_batched(
            || tokens.clone(),
            |tokens| lexer::update(black_box(&text), black_box(tokens), black_box(&text_change)),
            BatchSize::SmallInput,
        )
    });

    // parser
    let (tokens, token_change, program) = prepare_parser(double_source());
    let input = TokenStream::new_with_change(&tokens, token_change);
    group.bench_function("parser", |b| b.iter(|| parser::parse(black_box(&tokens))));
    group.bench_function("parser_inc", |b| {
        b.iter_batched(
            || (program.clone(), input.clone()),
            |(program, input)| parser::update(black_box(program), black_box(input)),
            BatchSize::SmallInput,
        )
    });

    // complete
    group.bench_function("complete", |b| {
        b.iter_batched(
            || text.clone(),
            |text| AnalyzedSource::new(black_box(text)),
            BatchSize::SmallInput,
        )
    });
    let analyzed_source = AnalyzedSource::new(text.clone());
    group.bench_function("complete_inc", move |b| {
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
    config = Criterion::default().measurement_time(Duration::from_secs(60)).sample_size(500);
    targets = no_change_benchmark, single_token_change_benchmark, double_source_benchmark
}
criterion_main!(benches);
