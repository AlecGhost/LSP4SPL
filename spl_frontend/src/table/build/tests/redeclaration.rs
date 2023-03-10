use super::*;

#[test]
fn type_proc() {
    let (_, _, broker) = test("type a = int; proc a() {}");
    eq!(
        broker.errors(),
        vec![
            SplError(
                19..20,
                BuildErrorMessage::RedeclarationAsProcedure("a".to_string()).to_string()
            ),
            SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())
        ]
    );
}

#[test]
fn proc_type() {
    let (_, _, broker) = test("proc a() {}\ntype a = int; ");
    eq!(
        broker.errors(),
        vec![
            SplError(
                17..18,
                BuildErrorMessage::RedeclarationAsType("a".to_string()).to_string()
            ),
            SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())
        ]
    );
}

#[test]
fn param() {
    let (_, _, broker) = test("proc a(i: int, i: int) {}");
    eq!(
        broker.errors(),
        vec![
            SplError(
                15..16,
                BuildErrorMessage::RedeclarationAsParameter("i".to_string()).to_string()
            ),
            SplError(0..0, BuildErrorMessage::MainIsMissing.to_string())
        ]
    );
}

#[test]
fn variable() {
    let (_, _, broker) = test(
        "proc main() {
            var i: int;
            var i: int;
        }",
    );
    eq!(
        broker.errors(),
        vec![SplError(
            54..55,
            BuildErrorMessage::RedeclarationAsVariable("i".to_string()).to_string()
        )]
    );
}
