use super::*;
use crate::features::references;
use lsp_types::{
    Location, PartialResultParams, Range, ReferenceContext, ReferenceParams, RenameParams,
    TextDocumentIdentifier, TextDocumentPositionParams, WorkDoneProgressParams,
};
use pretty_assertions::assert_eq;

async fn test_prepare_rename(text: &str, pos: Position, expected: Option<(Position, Position)>) {
    let uri = Url::parse("file:///test.spl").unwrap();
    let params = TextDocumentPositionParams {
        text_document: TextDocumentIdentifier::new(uri.clone()),
        position: pos,
    };
    let result = test_feature(references::prepare_rename, uri, text, params)
        .await
        .unwrap();
    assert_eq!(
        result.map(|range| {
            let Range { start, end } = range;
            (start, end)
        }),
        expected
    );
}

#[tokio::test]
async fn prepare_empty() {
    test_prepare_rename("", pos(0, 0), None).await;
}

#[tokio::test]
async fn prepare_var() {
    let program = std::fs::read_to_string("tests/programs/acker.spl").unwrap();
    test_prepare_rename(&program, pos(6, 6), Some((pos(6, 6), pos(6, 7)))).await;
}

#[tokio::test]
async fn prepare_int() {
    let program = std::fs::read_to_string("tests/programs/acker.spl").unwrap();
    test_prepare_rename(&program, pos(6, 9), None).await;
}

#[tokio::test]
async fn prepare_keywords() {
    let program = std::fs::read_to_string("tests/programs/acker.spl").unwrap();
    test_prepare_rename(&program, pos(5, 0), None).await;
    test_prepare_rename(&program, pos(5, 31), None).await;
    test_prepare_rename(&program, pos(6, 2), None).await;
    test_prepare_rename(&program, pos(10, 4), None).await;
    test_prepare_rename(&program, pos(27, 2), None).await;
}

#[tokio::test]
async fn prepare_type() {
    let program = std::fs::read_to_string("tests/programs/bigtest.spl").unwrap();
    test_prepare_rename(&program, pos(11, 27), Some((pos(11, 27), pos(11, 32)))).await;
}

async fn test_rename(text: &str, pos: Position, expected: Option<Vec<(Position, Position)>>) {
    let uri = Url::parse("file:///test.spl").unwrap();
    let params = RenameParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier::new(uri.clone()),
            position: pos,
        },
        new_name: String::new(),
        work_done_progress_params: WorkDoneProgressParams::default(),
    };
    let result = test_feature(references::rename, uri, text, params)
        .await
        .unwrap();
    let result: Option<Vec<(Position, Position)>> = result.and_then(|edit| {
        edit.changes.map(|change| {
            change
                .values()
                .flat_map(|edits| {
                    edits
                        .iter()
                        .map(|edit| {
                            let Range { start, end } = edit.range;
                            (start, end)
                        })
                        .collect::<Vec<_>>()
                })
                .collect()
        })
    });
    assert_eq!(result, expected);
}

#[tokio::test]
async fn rename_empty() {
    test_rename("", pos(0, 0), None).await;
}

#[tokio::test]
async fn rename_var() {
    let program = std::fs::read_to_string("tests/programs/acker.spl").unwrap();
    test_rename(
        &program,
        pos(6, 6),
        Some(vec![
            (pos(6, 6), pos(6, 7)),
            (pos(14, 26), pos(14, 27)),
            (pos(15, 23), pos(15, 24)),
        ]),
    )
    .await;
}

#[tokio::test]
async fn rename_int() {
    let program = std::fs::read_to_string("tests/programs/acker.spl").unwrap();
    test_rename(&program, pos(6, 9), None).await;
}

#[tokio::test]
async fn rename_keywords() {
    let program = std::fs::read_to_string("tests/programs/acker.spl").unwrap();
    test_rename(&program, pos(5, 0), None).await;
    test_rename(&program, pos(5, 31), None).await;
    test_rename(&program, pos(6, 2), None).await;
    test_rename(&program, pos(10, 4), None).await;
    test_rename(&program, pos(27, 2), None).await;
}

#[tokio::test]
async fn rename_type() {
    let program = std::fs::read_to_string("tests/programs/bigtest.spl").unwrap();
    test_rename(
        &program,
        pos(11, 27),
        Some(vec![
            (pos(8, 5), pos(8, 10)),
            (pos(11, 27), pos(11, 32)),
            (pos(17, 30), pos(17, 35)),
            (pos(20, 35), pos(20, 40)),
            (pos(35, 38), pos(35, 43)),
            (pos(44, 27), pos(44, 32)),
            (pos(44, 56), pos(44, 61)),
            (pos(53, 15), pos(53, 20)),
            (pos(60, 9), pos(60, 14)),
            (pos(61, 23), pos(61, 28)),
        ]),
    )
    .await;
}

async fn test_find(text: &str, pos: Position, expected: Option<Vec<(Position, Position)>>) {
    let uri = Url::parse("file:///test.spl").unwrap();
    let params = ReferenceParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier::new(uri.clone()),
            position: pos,
        },
        partial_result_params: PartialResultParams::default(),
        work_done_progress_params: WorkDoneProgressParams::default(),
        context: ReferenceContext {
            include_declaration: true,
        },
    };
    let result = test_feature(references::find, uri, text, params)
        .await
        .unwrap();
    assert_eq!(
        result.map(|locations| locations
            .into_iter()
            .map(|location| {
                let Location {
                    range: Range { start, end },
                    ..
                } = location;
                (start, end)
            })
            .collect::<Vec<_>>()),
        expected
    );
}

#[tokio::test]
async fn find_empty() {
    test_find("", pos(0, 0), None).await;
}

#[tokio::test]
async fn find_var() {
    let program = std::fs::read_to_string("tests/programs/acker.spl").unwrap();
    test_find(
        &program,
        pos(6, 6),
        Some(vec![
            (pos(14, 26), pos(14, 27)),
            (pos(15, 23), pos(15, 24)),
        ]),
    )
    .await;
}

#[tokio::test]
async fn find_int() {
    let program = std::fs::read_to_string("tests/programs/acker.spl").unwrap();
    test_find(&program, pos(6, 9), Some(vec![
        (pos(5, 18), pos(5, 21)),
        (pos(5, 26), pos(5, 29)),
        (pos(5, 38), pos(5, 41)),
        (pos(22, 9), pos(22, 12)),
        (pos(23, 9), pos(23, 12)),
        (pos(24, 9), pos(24, 12)),
    ])).await;
}

#[tokio::test]
async fn find_keywords() {
    let program = std::fs::read_to_string("tests/programs/acker.spl").unwrap();
    test_find(&program, pos(5, 0), None).await;
    test_find(&program, pos(5, 31), None).await;
    test_find(&program, pos(6, 2), None).await;
    test_find(&program, pos(10, 4), None).await;
    test_find(&program, pos(27, 2), None).await;
}

#[tokio::test]
async fn find_type() {
    let program = std::fs::read_to_string("tests/programs/bigtest.spl").unwrap();
    test_find(
        &program,
        pos(11, 27),
        Some(vec![
            (pos(8, 5), pos(8, 10)),
            (pos(17, 30), pos(17, 35)),
            (pos(20, 35), pos(20, 40)),
            (pos(35, 38), pos(35, 43)),
            (pos(44, 27), pos(44, 32)),
            (pos(44, 56), pos(44, 61)),
            (pos(53, 15), pos(53, 20)),
            (pos(60, 9), pos(60, 14)),
            (pos(61, 23), pos(61, 28)),
        ]),
    )
    .await;
}
