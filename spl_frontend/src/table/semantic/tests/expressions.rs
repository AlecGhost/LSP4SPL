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
            SemanticErrorMessage::ComparisonNonInteger.into()
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
            SemanticErrorMessage::OperatorDifferentTypes.into(),
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
                SemanticErrorMessage::AssignmentHasDifferentTypes.into()
            ),
            SplError(
                27..30,
                SemanticErrorMessage::ArithmeticOperatorNonInteger.into(),
            ),
        ]
    );
}
