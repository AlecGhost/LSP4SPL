use super::*;
use crate::features::fold;
use lsp_types::{
    FoldingRange, FoldingRangeParams, PartialResultParams, TextDocumentIdentifier,
    WorkDoneProgressParams,
};
use pretty_assertions::assert_eq;

async fn test_fold(text: &str, expected: Vec<(u32, u32)>) {
    let uri = Url::parse("file:///test.spl").unwrap();
    let params = FoldingRangeParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        partial_result_params: PartialResultParams::default(),
        work_done_progress_params: WorkDoneProgressParams::default(),
    };
    let result = test_feature(fold, uri, text, params).await.unwrap();
    assert_eq!(map_ranges(result), expected,);
}

fn map_ranges(ranges: Vec<FoldingRange>) -> Vec<(u32, u32)> {
    ranges
        .into_iter()
        .map(|folding_range| {
            let FoldingRange {
                start_line,
                end_line,
                ..
            } = folding_range;
            (start_line, end_line)
        })
        .collect()
}

#[tokio::test]
async fn empty() {
    test_fold("", Vec::new()).await;
}

#[tokio::test]
async fn proc() {
    test_fold(
        "proc a() {
    var x: int;
}",
        vec![(0, 2)],
    )
    .await;
}

#[tokio::test]
async fn acker() {
    let acker = std::fs::read_to_string("tests/programs/acker.spl").unwrap();
    let folds = vec![(5, 18), (21, 41)];
    test_fold(&acker, folds).await;
}

#[tokio::test]
async fn big_test() {
    let big_test = std::fs::read_to_string("tests/programs/bigtest.spl").unwrap();
    let folds = vec![
        (11, 14),
        (20, 23),
        (26, 28),
        (35, 41),
        (44, 50),
        (52, 55),
        (57, 136),
        (138, 143),
        (145, 150),
        (152, 158),
    ];
    test_fold(&big_test, folds).await;
}
