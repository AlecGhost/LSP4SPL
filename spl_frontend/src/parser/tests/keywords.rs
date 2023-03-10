use super::*;
use crate::parser::keywords;

#[test]
fn array_kw() {
    let kw = "array";
    let broker = LocalBroker::default();
    let tokens = lex(kw, broker);
    assert!(
        all_consuming(terminated(keywords::array, eof))(tokens.to_tokens()).is_ok(),
        "Keyword: {}",
        kw
    );
}

#[test]
fn invalid_trailing_underscore() {
    let kw = "array_";
    let broker = LocalBroker::default();
    let tokens = lex(kw, broker);
    assert!(
        all_consuming(terminated(keywords::array, eof))(tokens.to_tokens()).is_err(),
        "Keyword: {}",
        kw
    );
}

#[test]
fn type_declaration() {
    let kw = "type a=int;";
    let broker = LocalBroker::default();
    let tokens = lex(kw, broker);
    assert!(
        all_consuming(terminated(TypeDeclaration::parse, eof))(tokens.to_tokens()).is_ok(),
        "Keyword: {}",
        kw
    );
}

#[test]
fn missing_whitespace() {
    let kw = "typea=int;";
    let broker = LocalBroker::default();
    let tokens = lex(kw, broker);
    assert!(
        all_consuming(terminated(TypeDeclaration::parse, eof))(tokens.to_tokens()).is_err(),
        "Keyword: {}",
        kw
    );
}
