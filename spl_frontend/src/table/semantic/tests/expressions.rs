use super::*;

#[test]
fn non_int() {
    let errors = test(
        "
        type matrix = array [3] of array [3] of int;
        proc main() { 
            var i: matrix;
            while (i < i);
        }
        ",
    );
    eq!(
        errors,
        vec![SplError(
            27..30,
            SemanticErrorMessage::ComparisonNonInteger.to_string()
        )]
    );
}

#[test]
fn op_different_types() {
    let errors = test(
        "
        type matrix = array [3] of array [3] of int;
        proc main() { 
            var i: matrix;
            while (1 < i);
        }
        ",
    );
    eq!(
        errors,
        vec![SplError(
            27..30,
            SemanticErrorMessage::OperatorDifferentTypes.to_string(),
        )]
    );
}

#[test]
fn arithmetic_op_non_int() {
    let errors = test(
        "
        type matrix = array [3] of array [3] of int;
        proc main() { 
            var i: matrix;
            i := i + i;
        }
        ",
    );
    eq!(
        errors,
        vec![
            SplError(
                25..31,
                SemanticErrorMessage::AssignmentHasDifferentTypes.to_string()
            ),
            SplError(
                27..30,
                SemanticErrorMessage::ArithmeticOperatorNonInteger.to_string(),
            ),
        ]
    );
}
