use crate::lexer::{lex, token::{Token, TokenType}};

#[test]
fn tokens() {
    let input = "type a = int;";
    let tokens = lex(input);
    assert_eq!(tokens, vec![
        Token::new(TokenType::Type, 0..4),
        Token::new(TokenType::Ident("a".to_string()), 5..6),
        Token::new(TokenType::Eq, 7..8),
        Token::new(TokenType::Ident("int".to_string()), 9..12),
        Token::new(TokenType::Semic, 12..13),
    ])
}
