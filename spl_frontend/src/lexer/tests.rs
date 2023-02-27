use crate::{
    lexer::{
        lex,
        token::{Token, TokenType},
    },
    LocalBroker,
};
#[cfg(test)]
use pretty_assertions::assert_eq;

#[test]
fn tokens() {
    let input = "type a = int;";
    let broker = LocalBroker::default();
    let tokens = lex(input, broker);
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

    let input = "a(); ";
    let broker = LocalBroker::default();
    let tokens = lex(input, broker);
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

    let input = "if (1 = 2) {}";
    let broker = LocalBroker::default();
    let tokens = lex(input, broker);
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::If, 0..2),
            Token::new(TokenType::LParen, 3..4),
            Token::new(TokenType::Int(Ok(1)), 4..5),
            Token::new(TokenType::Eq, 6..7),
            Token::new(TokenType::Int(Ok(2)), 8..9),
            Token::new(TokenType::RParen, 9..10),
            Token::new(TokenType::LCurly, 11..12),
            Token::new(TokenType::RCurly, 12..13),
            Token::new(TokenType::Eof, 13..13),
        ],
        "{}",
        input
    );

    let input = "//";
    let broker = LocalBroker::default();
    let tokens = lex(input, broker);
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenType::Comment("".to_string()), 0..2),
            Token::new(TokenType::Eof, 2..2),
        ],
        "{}",
        input
    );
}
