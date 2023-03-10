use super::*;

#[test]
fn valid() {
    let errors = test(
        "
        proc a(i: int) {}
        proc main() {
            a(1);
        }
        ",
    );
    eq!(errors, Vec::new());
}

#[test]
fn too_few() {
    let errors = test(
        "
        proc a(i: int) {} proc main() {a();}
        ",
    );
    eq!(
        errors,
        vec![SplError(
            40..44,
            SemanticErrorMessage::TooFewArguments("a".to_string()).to_string()
        )]
    );
}

#[test]
fn too_many() {
    let errors = test(
        "
        proc a(i: int) {}
        proc main() {a(1, 2);}
        ",
    );
    eq!(
        errors,
        vec![SplError(
            48..56,
            SemanticErrorMessage::TooManyArguments("a".to_string()).to_string()
        )]
    );
}

#[test]
fn ref_non_variable() {
    let errors = test(
        "
        proc a(ref i: int) {}
        proc main() {a(1);}
        ",
    );
    eq!(
        errors,
        vec![SplError(
            52..53,
            SemanticErrorMessage::ArgumentMustBeAVariable("a".to_string(), 1).to_string()
        )]
    );
}

#[test]
fn arg_type_mismatch() {
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
    eq!(
        errors,
        vec![SplError(
            128..129,
            SemanticErrorMessage::ArgumentsTypeMismatch("a".to_string(), 1).to_string()
        )]
    );
}

#[test]
fn non_proc() {
    let errors = test(
        "
        type a = int;
        proc main() {
            a(1);
        }
        ",
    );
    eq!(
        errors,
        vec![SplError(
            57..62,
            SemanticErrorMessage::CallOfNoneProcedure("a".to_string()).to_string()
        )]
    );
}

#[test]
fn undefined_proc() {
    let errors = test(
        "
        proc main() {
            a(1);
        }
        ",
    );
    eq!(
        errors,
        vec![SplError(
            35..40,
            SemanticErrorMessage::UndefinedProcedure("a".to_string()).to_string()
        )]
    );
}
