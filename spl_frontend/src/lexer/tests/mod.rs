use crate::lexer::{
    lex,
    token::{IntResult, Token, TokenType},
};
use pretty_assertions::assert_eq;

mod incremental;

#[test]
fn type_declaration() {
    let input = "type a = int;";
    let tokens = lex(input);
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Type, 0..4),
            Token::new(TokenType::Ident("a".to_string()), 5..6),
            Token::new(TokenType::Eq, 7..8),
            Token::new(TokenType::Ident("int".to_string()), 9..12),
            Token::new(TokenType::Semic, 12..13),
            Token::new(TokenType::Eof, 13..13),
        ],
        "{}",
        input
    );
}

#[test]
fn call_statement() {
    let input = "a(); ";
    let tokens = lex(input);
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Ident("a".to_string()), 0..1),
            Token::new(TokenType::LParen, 1..2),
            Token::new(TokenType::RParen, 2..3),
            Token::new(TokenType::Semic, 3..4),
            Token::new(TokenType::Eof, 5..5),
        ],
        "{}",
        input
    );
}

#[test]
fn if_statement() {
    let input = "if (1 = 2) {}";
    let tokens = lex(input);
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::If, 0..2),
            Token::new(TokenType::LParen, 3..4),
            Token::new(TokenType::Int(IntResult::Int(1)), 4..5),
            Token::new(TokenType::Eq, 6..7),
            Token::new(TokenType::Int(IntResult::Int(2)), 8..9),
            Token::new(TokenType::RParen, 9..10),
            Token::new(TokenType::LCurly, 11..12),
            Token::new(TokenType::RCurly, 12..13),
            Token::new(TokenType::Eof, 13..13),
        ],
        "{}",
        input
    );
}

#[test]
fn comment() {
    let input = "//\n";
    let tokens = lex(input);
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Comment(String::new()), 0..3),
            Token::new(TokenType::Eof, 3..3),
        ],
        "{}",
        input
    );
}
