use criterion::{black_box, criterion_group, criterion_main, Criterion};
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
    group.measurement_time(Duration::from_secs(15));

    // lexer
    let (text, text_change, tokens) = no_change();
    group.bench_function("lexer", |b| b.iter(|| lexer::lex(black_box(&text))));
    group.bench_function("lexer_inc", |b| {
        b.iter(|| {
            lexer::update(
                black_box(&text),
                black_box(tokens.clone()),
                black_box(&text_change),
            )
        })
    });

    // parser
    let (tokens, token_change, program) = prepare_parser(no_change());
    let input = TokenStream::new_with_change(&tokens, token_change);
    group.bench_function("parser", |b| b.iter(|| parser::parse(black_box(&tokens))));
    group.bench_function("parser_inc", |b| {
        b.iter(|| parser::update(black_box(program.clone()), black_box(input.clone())))
    });

    // complete
    group.bench_function("complete", |b| {
        b.iter(|| AnalyzedSource::new(black_box(text.clone())))
    });
    let analyzed_source = AnalyzedSource::new(text.clone());
    group.bench_function("complete_inc", |b| {
        b.iter(|| {
            AnalyzedSource::update(
                black_box(analyzed_source.clone()),
                black_box(vec![text_change.clone()]),
            )
        })
    });
}

fn single_token_change_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("single_token_change");
    group.measurement_time(Duration::from_secs(15));

    // lexer
    let (text, text_change, tokens) = single_character_change();
    group.bench_function("lexer", |b| b.iter(|| lexer::lex(black_box(&text))));
    group.bench_function("lexer_inc", |b| {
        b.iter(|| {
            lexer::update(
                black_box(&text),
                black_box(tokens.clone()),
                black_box(&text_change),
            )
        })
    });

    // parser
    let (tokens, token_change, program) = prepare_parser(single_character_change());
    let input = TokenStream::new_with_change(&tokens, token_change);
    group.bench_function("parser", |b| b.iter(|| parser::parse(black_box(&tokens))));
    group.bench_function("parser_inc", |b| {
        b.iter(|| parser::update(black_box(program.clone()), black_box(input.clone())))
    });

    // complete
    group.bench_function("complete", |b| {
        b.iter(|| AnalyzedSource::new(black_box(text.clone())))
    });
    let analyzed_source = AnalyzedSource::new(text.clone());
    group.bench_function("complete_inc", |b| {
        b.iter(|| {
            AnalyzedSource::update(
                black_box(analyzed_source.clone()),
                black_box(vec![text_change.clone()]),
            )
        })
    });
}

fn double_source_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("double_source");
    group.measurement_time(Duration::from_secs(22));

    // lexer
    let (text, text_change, tokens) = double_source();
    group.bench_function("lexer", |b| b.iter(|| lexer::lex(black_box(&text))));
    group.bench_function("lexer_inc", |b| {
        b.iter(|| {
            lexer::update(
                black_box(&text),
                black_box(tokens.clone()),
                black_box(&text_change),
            )
        })
    });

    // parser
    let (tokens, token_change, program) = prepare_parser(double_source());
    let input = TokenStream::new_with_change(&tokens, token_change);
    group.bench_function("parser", |b| b.iter(|| parser::parse(black_box(&tokens))));
    group.bench_function("parser_inc", |b| {
        b.iter(|| parser::update(black_box(program.clone()), black_box(input.clone())))
    });

    // complete
    group.bench_function("complete", |b| {
        b.iter(|| AnalyzedSource::new(black_box(text.clone())))
    });
    let analyzed_source = AnalyzedSource::new(text.clone());
    group.bench_function("complete_inc", |b| {
        b.iter(|| {
            AnalyzedSource::update(
                black_box(analyzed_source.clone()),
                black_box(vec![text_change.clone()]),
            )
        })
    });
}

criterion_group!(
    benches,
    no_change_benchmark,
    single_token_change_benchmark,
    double_source_benchmark
);
criterion_main!(benches);
