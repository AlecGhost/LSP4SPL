use super::*;

#[test]
fn main() {
    let errors = test("proc main() {}");
    eq!(errors, Vec::new());
}

#[test]
fn test1() {
    let test1 = std::fs::read_to_string("/Users/alex/dev/compiler/programs/test1.spl").unwrap();
    let errors = test(&test1);
    eq!(errors, Vec::new());
}

#[test]
fn test2() {
    let test2 = std::fs::read_to_string("/Users/alex/dev/compiler/programs/test2.spl").unwrap();
    let errors = test(&test2);
    eq!(errors, Vec::new());
}

#[test]
fn acker() {
    let acker = std::fs::read_to_string("/Users/alex/dev/compiler/programs/acker.spl").unwrap();
    let errors = test(&acker);
    eq!(errors, Vec::new());
}
