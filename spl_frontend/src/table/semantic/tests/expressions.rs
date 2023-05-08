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
            123..128,
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
            123..128,
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
                121..126,
                SemanticErrorMessage::ArithmeticOperatorNonInteger.to_string(),
            ),
            SplError(
                116..127,
                SemanticErrorMessage::AssignmentHasDifferentTypes.to_string()
            )
        ]
    );
}
