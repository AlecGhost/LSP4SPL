use super::*;

#[test]
fn two_dim() {
    let errors = test(
        "
        type matrix = array [3] of array [3] of int;
        proc main() {
            var m: matrix;
            m[0][0] := 1;
        }
        ",
    );
    eq!(errors, Vec::new());
}

#[test]
fn different_types() {
    let errors = test(
        "
        type matrix = array [3] of array [3] of int;
        proc main() {
            var m: matrix;
            m[0] := 1;
        }
        ",
    );
    eq!(
        errors,
        vec![SplError(
            25..32,
            SemanticErrorMessage::AssignmentHasDifferentTypes.to_string()
        ),]
    );
}

#[test]
fn non_array_int() {
    let errors = test(
        "
        proc main() {
            var m: int;
            m[0] := 1;
        }
        ",
    );
    eq!(
        errors,
        vec![SplError(
            10..14,
            SemanticErrorMessage::IndexingNonArray.to_string()
        )]
    );
}

#[test]
fn non_array_wrong_dim() {
    let errors = test(
        "
        type matrix = array [3] of array [3] of int;
        proc main() {
            var m: matrix;
            m[0][0][0] := 1;
        }
        ",
    );
    eq!(
        errors,
        vec![SplError(
            25..35,
            SemanticErrorMessage::IndexingNonArray.to_string()
        )]
    );
}

#[test]
fn index_non_integer() {
    let errors = test(
        "
        type matrix = array [3] of array [3] of int;
        proc main() {
            var m: matrix;
            m[0=1][0] := 1;
        }
        ",
    );
    eq!(
        errors,
        vec![SplError(
            27..30,
            SemanticErrorMessage::IndexingWithNonInteger.to_string()
        )]
    );
}
