use super::*;
use crate::features::completion::propose;
use insta::assert_debug_snapshot;
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionParams, PartialResultParams,
    TextDocumentIdentifier, TextDocumentPositionParams, WorkDoneProgressParams,
};

async fn test_completion(text: &str, pos: Position) -> Vec<String> {
    let uri = Url::parse("file:///test.spl").unwrap();
    let params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: pos,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
        context: None,
    };
    let result = test_feature(propose, uri, text, params)
        .await
        .unwrap()
        .unwrap_or_default();
    let mut result_strings: Vec<String> = result
        .into_iter()
        .map(|item| {
            let CompletionItem { label, kind, .. } = item;
            let kind = match kind {
                Some(CompletionItemKind::STRUCT) => "type",
                Some(CompletionItemKind::SNIPPET) => "snippet",
                Some(CompletionItemKind::KEYWORD) => "keyword",
                Some(CompletionItemKind::VARIABLE) => "variable",
                Some(CompletionItemKind::FUNCTION) => "proc",
                _ => "unknown",
            };
            format!("{}: {}", kind, label)
        })
        .collect();
    // sort because of hash table
    result_strings.sort();
    result_strings
}

#[tokio::test]
async fn empty() {
    let result = test_completion("", pos(0, 0)).await;
    assert_debug_snapshot!(result);
}

#[tokio::test]
async fn no_main() {
    let result = test_completion("proc main() {}\n", pos(1, 0)).await;
    assert_debug_snapshot!(result);
}

#[tokio::test]
async fn var_and_stmt() {
    let acker = std::fs::read_to_string("tests/programs/acker.spl").unwrap();
    let result = test_completion(&acker, pos(7, 0)).await;
    assert_debug_snapshot!(result);
}

#[tokio::test]
async fn stmt() {
    let acker = std::fs::read_to_string("tests/programs/acker.spl").unwrap();
    let result = test_completion(&acker, pos(29, 0)).await;
    assert_debug_snapshot!(result);
}

#[tokio::test]
async fn expr() {
    let acker = std::fs::read_to_string("tests/programs/acker.spl").unwrap();
    let result = test_completion(&acker, pos(9, 9)).await;
    assert_debug_snapshot!(result);
}

#[tokio::test]
async fn no_else() {
    let acker = std::fs::read_to_string("tests/programs/acker.spl").unwrap();
    let result = test_completion(&acker, pos(10, 3)).await;
    assert_debug_snapshot!(result);
}
