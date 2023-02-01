use crate::{
    error::{SemanticErrorMessage, SplError},
    parser, table, LocalBroker,
};
#[cfg(test)]
use pretty_assertions::assert_eq;

fn test(src: &str) -> Vec<SplError> {
    eprintln!("Testing: {}", src);
    let parse_broker = LocalBroker::default();
    let program = parser::parse(src, parse_broker.clone());
    assert_eq!(parse_broker.errors(), Vec::new(), "parsing failed");
    let build_broker = LocalBroker::default();
    let table = table::build(&program, build_broker.clone());
    assert_eq!(build_broker.errors(), Vec::new(), "building failed");
    let semantic_broker = LocalBroker::default();
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
        vec![SplError(
            115..134,
            SemanticErrorMessage::AssignmentHasDifferentTypes.to_string()
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
        vec![SplError(
            59..60,
            SemanticErrorMessage::IndexingNonArray.to_string()
        )]
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
        vec![SplError(
            115..122,
            SemanticErrorMessage::IndexingNonArray.to_string()
        )]
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
        vec![SplError(
            117..120,
            SemanticErrorMessage::IndexingWithNonInteger.to_string()
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
        proc a(i: int) {} proc main() {a();}
        ",
    );
    assert_eq!(
        errors,
        vec![SplError(
            40..44,
            SemanticErrorMessage::TooFewArguments("a".to_string()).to_string()
        )]
    );

    let errors = test(
        "
        proc a(i: int) {}
        proc main() {a(1, 2);}
        ",
    );
    assert_eq!(
        errors,
        vec![SplError(
            48..56,
            SemanticErrorMessage::TooManyArguments("a".to_string()).to_string()
        )]
    );

    let errors = test(
        "
        proc a(ref i: int) {}
        proc main() {a(1);}
        ",
    );
    assert_eq!(
        errors,
        vec![SplError(
            52..53,
            SemanticErrorMessage::ArgumentMustBeAVariable("a".to_string(), 1).to_string()
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
        vec![SplError(
            128..129,
            SemanticErrorMessage::ArgumentsTypeMismatch("a".to_string(), 1).to_string()
        )]
    );

    let errors = test(
        "
        type a = int;
        proc main() {
            a(1);
        }
        ",
    );
    assert_eq!(
        errors,
        vec![SplError(
            57..71,
            SemanticErrorMessage::CallOfNoneProcedure("a".to_string()).to_string()
        )]
    );

    let errors = test(
        "
        proc main() {
            a(1);
        }
        ",
    );
    assert_eq!(
        errors,
        vec![SplError(
            35..49,
            SemanticErrorMessage::UndefinedProcedure("a".to_string()).to_string()
        )]
    );
}

#[test]
fn comparisons() {
    let errors = test(
        "
        proc main() {
            if (1 = 1);
        }
        ",
    );
    assert_eq!(errors, Vec::new());

    let errors = test("proc main() {if (1);}");
    assert_eq!(
        errors,
        vec![SplError(
            17..18,
            SemanticErrorMessage::IfConditionMustBeBoolean.to_string()
        )]
    );

    let errors = test("proc main() {while (1);}");
    assert_eq!(
        errors,
        vec![SplError(
            20..21,
            SemanticErrorMessage::WhileConditionMustBeBoolean.to_string()
        )]
    );
}

#[test]
fn expressions() {
    let errors = test(
        "
        type matrix = array [3] of array [3] of int;
        proc main() { 
            var i: matrix;
            while (i < i);
        }
        ",
    );
    assert_eq!(
        errors,
        vec![SplError(
            123..128,
            SemanticErrorMessage::ComparisonNonInteger.to_string()
        )]
    );

    let errors = test(
        "
        type matrix = array [3] of array [3] of int;
        proc main() { 
            var i: matrix;
            while (1 < i);
        }
        ",
    );
    assert_eq!(
        errors,
        vec![SplError(
            123..128,
            SemanticErrorMessage::OperatorDifferentTypes.to_string(),
        )]
    );

    let errors = test(
        "
        type matrix = array [3] of array [3] of int;
        proc main() { 
            var i: matrix;
            i := i + i;
        }
        ",
    );
    assert_eq!(
        errors,
        vec![SplError(
            121..126,
            SemanticErrorMessage::ArithmeticOperatorNonInteger.to_string(),
        )]
    );
}
