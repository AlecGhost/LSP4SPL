use super::*;
use crate::parser::keywords;

#[test]
fn array_kw() {
    let kw = "array";
    let tokens = lex(kw);
    assert!(
        all_consuming(terminated(keywords::array, eof))(tokens.to_tokens()).is_ok(),
        "Keyword: {}",
        kw
    );
}

#[test]
fn invalid_trailing_underscore() {
    let kw = "array_";
    let tokens = lex(kw);
    assert!(
        all_consuming(terminated(keywords::array, eof))(tokens.to_tokens()).is_err(),
        "Keyword: {}",
        kw
    );
}

#[test]
fn type_declaration() {
    let kw = "type a=int;";
    let tokens = lex(kw);
    assert!(
        all_consuming(terminated(|input| TypeDeclaration::parse(None, input), eof))(
            tokens.to_tokens()
        )
        .is_ok(),
        "Keyword: {}",
        kw
    );
}

#[test]
fn missing_whitespace() {
    let kw = "typea=int;";
    let tokens = lex(kw);
    assert!(
        all_consuming(terminated(|input| TypeDeclaration::parse(None, input), eof))(
            tokens.to_tokens()
        )
        .is_err(),
        "Keyword: {}",
        kw
    );
}
