use crate::{
    lexer,
    token::{IntResult, Token, TokenType},
    TextChange,
};
use pretty_assertions::assert_eq;

#[test]
fn no_change() {
    let text = std::fs::read_to_string("tests/programs/acker.spl").unwrap();
    let tokens = lexer::lex(&text);
    let new_tokens = lexer::update(
        &text,
        tokens.clone(),
        &TextChange {
            range: 0..0,
            text: "".to_string(),
        },
    );
    assert_eq!(tokens, new_tokens);
}

#[test]
fn remove_comment() {
    let text = "// a if c 123\n";
    let tokens = lexer::lex(&text);
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Comment(" a if c 123".to_string()), 0..14),
            Token::new(TokenType::Eof, 14..14),
        ],
    );
    let new_text = "a if c 123\n";
    let new_tokens = lexer::update(
        &new_text,
        tokens.clone(),
        &TextChange {
            range: 0..3,
            text: "".to_string(),
        },
    );
    assert_eq!(
        new_tokens,
        vec![
            Token::new(TokenType::Ident("a".to_string()), 0..1),
            Token::new(TokenType::If, 2..4),
            Token::new(TokenType::Ident("c".to_string()), 5..6),
            Token::new(TokenType::Int(IntResult::Int(123)), 7..10),
            Token::new(TokenType::Eof, 11..11),
        ],
    );
}

#[test]
fn insert_middle() {
    let text = "a if else 0x1";
    let tokens = lexer::lex(&text);
    assert_eq!(
        vec![
            Token::new(TokenType::Ident("a".to_string()), 0..1),
            Token::new(TokenType::If, 2..4),
            Token::new(TokenType::Else, 5..9),
            Token::new(TokenType::Hex(IntResult::Int(1)), 10..13),
            Token::new(TokenType::Eof, 13..13),
        ],
        tokens
    );
    let new_text = "a if insertion else 0x1";
    let new_tokens = lexer::update(
        &new_text,
        tokens.clone(),
        &TextChange {
            range: 5..5,
            text: "insertion ".to_string(),
        },
    );
    assert_eq!(
        new_tokens,
        vec![
            Token::new(TokenType::Ident("a".to_string()), 0..1),
            Token::new(TokenType::If, 2..4),
            Token::new(TokenType::Ident("insertion".to_string()), 5..14),
            Token::new(TokenType::Else, 15..19),
            Token::new(TokenType::Hex(IntResult::Int(1)), 20..23),
            Token::new(TokenType::Eof, 23..23),
        ],
    );
}

#[test]
fn merge_keywords() {
    let text = "a if else 0x1";
    let tokens = lexer::lex(&text);
    assert_eq!(
        vec![
            Token::new(TokenType::Ident("a".to_string()), 0..1),
            Token::new(TokenType::If, 2..4),
            Token::new(TokenType::Else, 5..9),
            Token::new(TokenType::Hex(IntResult::Int(1)), 10..13),
            Token::new(TokenType::Eof, 13..13),
        ],
        tokens
    );
    let new_text = "a ifxelse 0x1";
    let new_tokens = lexer::update(
        &new_text,
        tokens.clone(),
        &TextChange {
            range: 4..5,
            text: "x".to_string(),
        },
    );
    assert_eq!(
        new_tokens,
        vec![
            Token::new(TokenType::Ident("a".to_string()), 0..1),
            Token::new(TokenType::Ident("ifxelse".to_string()), 2..9),
            Token::new(TokenType::Hex(IntResult::Int(1)), 10..13),
            Token::new(TokenType::Eof, 13..13),
        ],
    );
}

#[test]
fn replace_token() {
    let text = "a if else 0x1";
    let tokens = lexer::lex(&text);
    assert_eq!(
        vec![
            Token::new(TokenType::Ident("a".to_string()), 0..1),
            Token::new(TokenType::If, 2..4),
            Token::new(TokenType::Else, 5..9),
            Token::new(TokenType::Hex(IntResult::Int(1)), 10..13),
            Token::new(TokenType::Eof, 13..13),
        ],
        tokens
    );
    let new_text = "0 if else 0x1";
    let new_tokens = lexer::update(
        &new_text,
        tokens.clone(),
        &TextChange {
            range: 0..1,
            text: "0".to_string(),
        },
    );
    assert_eq!(
        new_tokens,
        vec![
            Token::new(TokenType::Int(IntResult::Int(0)), 0..1),
            Token::new(TokenType::If, 2..4),
            Token::new(TokenType::Else, 5..9),
            Token::new(TokenType::Hex(IntResult::Int(1)), 10..13),
            Token::new(TokenType::Eof, 13..13),
        ],
    );
}

#[test]
fn delete_from_end() {
    let text = "a if else 0x1";
    let tokens = lexer::lex(&text);
    assert_eq!(
        vec![
            Token::new(TokenType::Ident("a".to_string()), 0..1),
            Token::new(TokenType::If, 2..4),
            Token::new(TokenType::Else, 5..9),
            Token::new(TokenType::Hex(IntResult::Int(1)), 10..13),
            Token::new(TokenType::Eof, 13..13),
        ],
        tokens
    );
    let new_text = "a if else";
    let new_tokens = lexer::update(
        &new_text,
        tokens.clone(),
        &TextChange {
            range: 9..13,
            text: "".to_string(),
        },
    );
    assert_eq!(
        new_tokens,
        vec![
            Token::new(TokenType::Ident("a".to_string()), 0..1),
            Token::new(TokenType::If, 2..4),
            Token::new(TokenType::Else, 5..9),
            Token::new(TokenType::Eof, 9..9),
        ],
    );
}
