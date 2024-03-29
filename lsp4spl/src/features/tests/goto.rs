use super::*;
use crate::features::goto;
use lsp_types::{
    GotoDefinitionParams, Location, PartialResultParams, Position, Range, TextDocumentIdentifier,
    TextDocumentPositionParams, Url, WorkDoneProgressParams,
};
use pretty_assertions::assert_eq;

async fn test_goto<F, R>(f: F, text: &str, pos: Position, expected: Option<(Position, Position)>)
where
    F: Fn(Sender<DocumentRequest>, GotoDefinitionParams) -> R,
    R: std::future::Future<Output = Result<Option<Location>>>,
{
    let uri = Url::parse("file:///test.spl").unwrap();
    let result = test_feature(f, uri.clone(), text, goto_def_params(uri.clone(), pos))
        .await
        .unwrap();
    assert_eq!(map_location(result), expected);
}

fn map_location(location: Option<Location>) -> Option<(Position, Position)> {
    let Location {
        range: Range { start, end },
        ..
    } = location?;
    Some((start, end))
}

fn goto_def_params(uri: Url, pos: Position) -> GotoDefinitionParams {
    GotoDefinitionParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier::new(uri),
            position: pos,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    }
}

#[tokio::test]
async fn dec_var() {
    test_goto(
        goto::declaration,
        "
proc a() {
    var x: int;
    x := 0;
}
",
        pos(3, 4),
        Some((pos(2, 8), pos(2, 9))),
    )
    .await;
}

#[tokio::test]
async fn dec_type() {
    test_goto(
        goto::declaration,
        "
type my_int = int;
proc a() {
    var x: my_int;
}
",
        pos(3, 16),
        Some((pos(1, 5), pos(1, 11))),
    )
    .await;
}

#[tokio::test]
async fn dec_proc() {
    test_goto(
        goto::declaration,
        "
proc a() {
    a();
}
",
        pos(2, 4),
        Some((pos(1, 5), pos(1, 6))),
    )
    .await;
}

#[tokio::test]
async fn dec_none() {
    test_goto(
        goto::declaration,
        "
proc a() {}
",
        pos(2, 4),
        None,
    )
    .await;
}

#[tokio::test]
async fn type_dec_redec() {
    test_goto(
        goto::type_definition,
        "
type a = int;
type b = a;
",
        pos(2, 9),
        Some((pos(1, 5), pos(1, 6))),
    )
    .await;
}

#[tokio::test]
async fn type_dec_int() {
    test_goto(
        goto::type_definition,
        "
type a = int;
",
        pos(1, 9),
        None,
    )
    .await;
}

#[tokio::test]
async fn type_dec_int_var() {
    test_goto(
        goto::type_definition,
        "
type a = int;
proc b() {
    var x: a;
}
",
        pos(3, 8),
        None,
    )
    .await;
}

#[tokio::test]
async fn type_dec_var() {
    test_goto(
        goto::type_definition,
        "
type a = array [3] of int;
proc b() {
    var x: a;
}
",
        pos(3, 8),
        Some((pos(1, 5), pos(1, 6))),
    )
    .await;
}

#[tokio::test]
async fn type_dec_type() {
    test_goto(
        goto::type_definition,
        "
type a = array [3] of int;
type b = array [3] of int;
proc c() {
    var x: b;
}
",
        pos(4, 11),
        Some((pos(2, 5), pos(2, 6))),
    )
    .await;
}

#[tokio::test]
async fn impl_proc() {
    test_goto(
        goto::implementation,
        "
proc a() {
    a();
}
",
        pos(2, 4),
        Some((pos(1, 5), pos(1, 6))),
    )
    .await;
}

#[tokio::test]
async fn impl_var() {
    test_goto(
        goto::implementation,
        "
proc a() {
    var x: int;
}
",
        pos(2, 8),
        None,
    )
    .await;
}
