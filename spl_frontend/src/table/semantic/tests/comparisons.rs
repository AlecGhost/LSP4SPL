use super::*;

#[test]
fn simple() {
    let errors = test(
        "
        proc main() {
            if (1 = 1);
        }
        ",
    );
    eq!(errors, Vec::new());
}

#[test]
fn if_not_boolean() {
    let errors = test("proc main() {if (1);}");
    eq!(
        errors,
        vec![SplError(
            7..8,
            SemanticErrorMessage::IfConditionMustBeBoolean.to_string()
        )]
    );
}

#[test]
fn while_not_boolean() {
    let errors = test("proc main() {while (1);}");
    eq!(
        errors,
        vec![SplError(
            7..8,
            SemanticErrorMessage::WhileConditionMustBeBoolean.to_string()
        )]
    );
}
