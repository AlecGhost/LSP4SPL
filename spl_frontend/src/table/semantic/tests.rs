use crate::{
    error::{SemanticError, SemanticErrorMessage},
    parser, table,
    test::LocalBroker,
};

fn test(src: &str) -> Vec<SemanticError> {
    eprintln!("Testing: {}", src);
    let parse_broker = LocalBroker::new();
    let program = parser::parse(src, parse_broker.clone());
    assert_eq!(parse_broker.errors(), Vec::new(), "parsing failed");
    let build_broker = LocalBroker::new();
    let table = table::build(&program, build_broker.clone());
    assert_eq!(build_broker.errors(), Vec::new(), "building failed");
    let semantic_broker = LocalBroker::new();
    table::semantic::analyze(&program, &table, semantic_broker.clone());
    semantic_broker.errors()
}

#[test]
fn correct() {
    let errors = test("proc main() {}");
    assert_eq!(errors, Vec::new());

    let test1 = std::fs::read_to_string("/Users/alex/dev/compiler/programs/test1.spl").unwrap();
    let errors = test(&test1);
    assert_eq!(errors, Vec::new());

    let test2 = std::fs::read_to_string("/Users/alex/dev/compiler/programs/test2.spl").unwrap();
    let errors = test(&test2);
    assert_eq!(errors, Vec::new());

    let acker = std::fs::read_to_string("/Users/alex/dev/compiler/programs/acker.spl").unwrap();
    let errors = test(&acker);
    assert_eq!(errors, Vec::new());
}

#[test]
fn arrays() {
    let errors = test(
        "
        type matrix = array [3] of array [3] of int;
        proc main() {
            var m: matrix;
            m[0][0] := 1;
        }
        ",
    );
    assert_eq!(errors, Vec::new());

    let errors = test(
        "
        type matrix = array [3] of array [3] of int;
        proc main() {
            var m: matrix;
            m[0] := 1;
        }
        ",
    );
    assert_eq!(
        errors,
        vec![SemanticError(
            0..0,
            SemanticErrorMessage::AssignmentHasDifferentTypes
        ),]
    );

    let errors = test(
        "
        proc main() {
            var m: int;
            m[0] := 1;
        }
        ",
    );
    assert_eq!(
        errors,
        vec![SemanticError(0..0, SemanticErrorMessage::IndexingNonArray)]
    );

    let errors = test(
        "
        type matrix = array [3] of array [3] of int;
        proc main() {
            var m: matrix;
            m[0][0][0] := 1;
        }
        ",
    );
    assert_eq!(
        errors,
        vec![SemanticError(0..0, SemanticErrorMessage::IndexingNonArray)]
    );

    let errors = test(
        "
        type matrix = array [3] of array [3] of int;
        proc main() {
            var m: matrix;
            m[0=1][0] := 1;
        }
        ",
    );
    assert_eq!(
        errors,
        vec![SemanticError(
            0..0,
            SemanticErrorMessage::IndexingWithNonInteger
        )]
    );
}

#[test]
fn call_statements() {
    let errors = test(
        "
        proc a(i: int) {}
        proc main() {
            a(1);
        }
        ",
    );
    assert_eq!(errors, Vec::new());

    let errors = test(
        "
        proc a(i: int) {} proc main() {
            a();
        }
        ",
    );
    assert_eq!(
        errors,
        vec![SemanticError(
            53..54,
            SemanticErrorMessage::TooFewArguments("a".to_string())
        )]
    );

    let errors = test(
        "
        proc a(i: int) {}
        proc main() {
            a(1, 2);
        }
        ",
    );
    assert_eq!(
        errors,
        vec![SemanticError(
            61..62,
            SemanticErrorMessage::TooManyArguments("a".to_string())
        )]
    );

    let errors = test(
        "
        proc a(ref i: int) {}
        proc main() {
            a(1);
        }
        ",
    );
    assert_eq!(
        errors,
        vec![SemanticError(
            65..66,
            SemanticErrorMessage::ArgumentMustBeAVariable("a".to_string(), 1)
        )]
    );

    let errors = test(
        "
        type nonint = array [2] of int;
        proc a(i: int) {}
        proc main() {
            var i: nonint;
            a(i);
        }
        ",
    );
    assert_eq!(
        errors,
        vec![SemanticError(
            128..129,
            SemanticErrorMessage::ArgumentsTypeMismatch("a".to_string(), 1)
        )]
    );
}
