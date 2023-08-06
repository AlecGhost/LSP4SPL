use super::*;

#[test]
fn main() {
    let errors = test("proc main() {}");
    eq!(errors, Vec::new());
}

#[test]
fn test1() {
    let file = std::fs::read_to_string("tests/programs/test1.spl").unwrap();
    let errors = test(&file);
    eq!(errors, Vec::new());
}

#[test]
fn test2() {
    let file = std::fs::read_to_string("tests/programs/test2.spl").unwrap();
    let errors = test(&file);
    eq!(errors, Vec::new());
}

#[test]
fn acker() {
    let file = std::fs::read_to_string("tests/programs/acker.spl").unwrap();
    let errors = test(&file);
    eq!(errors, Vec::new());
}

#[test]
fn bigtest() {
    let file = std::fs::read_to_string("tests/programs/bigtest.spl").unwrap();
    let errors = test(&file);
    eq!(errors, Vec::new());
}
