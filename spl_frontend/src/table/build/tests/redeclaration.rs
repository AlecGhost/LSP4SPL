use super::*;

#[test]
fn type_proc() {
    let (_, errors) = test("type a = int; proc a() {}");
    eq!(
        errors,
        vec![
            SplError(0..0, BuildErrorMessage::MainIsMissing.to_string()),
            SplError(
                6..7,
                BuildErrorMessage::RedeclarationAsProcedure("a".to_string()).to_string()
            ),
        ]
    );
}

#[test]
fn proc_type() {
    let (_, errors) = test("proc a() {}\ntype a = int; ");
    eq!(
        errors,
        vec![
            SplError(0..0, BuildErrorMessage::MainIsMissing.to_string()),
            SplError(
                7..8,
                BuildErrorMessage::RedeclarationAsType("a".to_string()).to_string()
            ),
        ]
    );
}

#[test]
fn param() {
    let (_, errors) = test("proc a(i: int, i: int) {}");
    eq!(
        errors,
        vec![
            SplError(0..0, BuildErrorMessage::MainIsMissing.to_string()),
            SplError(
                7..8,
                BuildErrorMessage::RedeclarationAsParameter("i".to_string()).to_string()
            ),
        ]
    );
}

#[test]
fn variable() {
    let (_, errors) = test(
        "proc main() {
            var i: int;
            var i: int;
        }",
    );
    eq!(
        errors,
        vec![SplError(
            11..12,
            BuildErrorMessage::RedeclarationAsVariable("i".to_string()).to_string()
        )]
    );
}
