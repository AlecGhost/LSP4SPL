use super::*;
use crate::features::format;
use insta;
use lsp_types::{
    DocumentFormattingParams, FormattingOptions, TextDocumentIdentifier, WorkDoneProgressParams,
};

async fn test_formatting(text: &str) -> String {
    let uri = Url::parse("file:///test.spl").unwrap();
    let params = DocumentFormattingParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        options: FormattingOptions::default(),
        work_done_progress_params: WorkDoneProgressParams::default(),
    };
    let result: String = test_feature(format, uri.clone(), text, params)
        .await
        .unwrap()
        .unwrap_or_default()
        .first()
        .map(|text_edit| text_edit.new_text.clone())
        .unwrap_or_default();
    result
}

#[tokio::test]
async fn empty() {
    let result = test_formatting("").await;
    insta::assert_snapshot!(result);
}

#[tokio::test]
async fn type_dec_correct() {
    let result = test_formatting("type a = int;\n").await;
    insta::assert_snapshot!(result);
}

#[tokio::test]
async fn type_dec_incorrect() {
    let result = test_formatting("type\n a=int;").await;
    insta::assert_snapshot!(result);
}

#[tokio::test]
async fn type_dec_missing_semic() {
    let result = test_formatting("type a=int").await;
    insta::assert_snapshot!(result);
}

#[tokio::test]
async fn proc_dec_correct() {
    let result = test_formatting("proc a() {}\n").await;
    insta::assert_snapshot!(result);
}

#[tokio::test]
async fn proc_dec_no_body() {
    let result = test_formatting("proc a() {\n}").await;
    insta::assert_snapshot!(result);
}

#[tokio::test]
async fn acker() {
    let acker = std::fs::read_to_string("tests/features/formatting/acker.spl").unwrap();
    let result = test_formatting(&acker).await;
    insta::assert_snapshot!(result);
}

#[tokio::test]
async fn big_test() {
    let big_test = std::fs::read_to_string("tests/features/formatting/bigtest.spl").unwrap();
    let result = test_formatting(&big_test).await;
    insta::assert_snapshot!(result);
}
