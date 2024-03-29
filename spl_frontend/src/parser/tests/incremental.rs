use crate::{
    ast::{AstInfo, GlobalDeclaration, Identifier, ProcedureDeclaration, Program, Reference},
    lexer, parser,
    tokens::TokenStream,
    AnalyzedSource, TextChange, ToRange,
};
use insta::assert_debug_snapshot;
use pretty_assertions::assert_eq;

fn test_incremental(text: &str, change: TextChange) -> Program {
    let analyzed_source = AnalyzedSource::new(text.to_string());

    let mut new_text = text.to_string();
    new_text.replace_range(change.to_range(), &change.text);
    let tokens = lexer::lex(&new_text);
    let (inc_tokens, token_change) = lexer::update(&new_text, analyzed_source.tokens, &change);
    assert_eq!(inc_tokens, tokens);

    let ast = parser::parse(&tokens);
    let inc_ast = parser::update(
        analyzed_source.ast,
        TokenStream::new_with_change(&inc_tokens, token_change),
    );

    assert_eq!(inc_ast, ast);
    inc_ast
}

#[test]
fn delete_proc_start() {
    let src = "
proc a() {}
proc b() {}
proc main() {}
";
    let analyzed_source = AnalyzedSource::new(src.to_string());
    let change = TextChange {
        range: 1..13,
        text: String::new(),
    };
    let analyzed_source = analyzed_source.update(vec![change]);
    assert_eq!(
        analyzed_source.ast,
        Program {
            global_declarations: vec![
                Reference::new(
                    GlobalDeclaration::Procedure(ProcedureDeclaration {
                        doc: Vec::new(),
                        name: Some(Identifier::new("b".to_string(), 1..2)),
                        parameters: Vec::new(),
                        variable_declarations: Vec::new(),
                        statements: Vec::new(),
                        info: AstInfo::new(0..6)
                    }),
                    0,
                ),
                Reference::new(
                    GlobalDeclaration::Procedure(ProcedureDeclaration {
                        doc: Vec::new(),
                        name: Some(Identifier::new("main".to_string(), 1..2)),
                        parameters: Vec::new(),
                        variable_declarations: Vec::new(),
                        statements: Vec::new(),
                        info: AstInfo::new(0..6)
                    }),
                    6,
                ),
            ],
            info: AstInfo::new(0..12),
        }
    );
}

#[test]
fn delete_proc_middle() {
    let src = "
proc main() {}
proc b() {}
proc c() {}
";
    let analyzed_source = AnalyzedSource::new(src.to_string());
    let change = TextChange {
        range: 16..28,
        text: String::new(),
    };
    let analyzed_source = analyzed_source.update(vec![change]);
    assert_eq!(
        analyzed_source.ast,
        Program {
            global_declarations: vec![
                Reference::new(
                    GlobalDeclaration::Procedure(ProcedureDeclaration {
                        doc: Vec::new(),
                        name: Some(Identifier::new("main".to_string(), 1..2)),
                        parameters: Vec::new(),
                        variable_declarations: Vec::new(),
                        statements: Vec::new(),
                        info: AstInfo::new(0..6)
                    }),
                    0,
                ),
                Reference::new(
                    GlobalDeclaration::Procedure(ProcedureDeclaration {
                        doc: Vec::new(),
                        name: Some(Identifier::new("c".to_string(), 1..2)),
                        parameters: Vec::new(),
                        variable_declarations: Vec::new(),
                        statements: Vec::new(),
                        info: AstInfo::new(0..6)
                    }),
                    6,
                ),
            ],
            info: AstInfo::new(0..12),
        }
    );
}

#[test]
fn delete_proc_end() {
    let src = "
proc main() {}
proc b() {}
proc c() {}
";
    let analyzed_source = AnalyzedSource::new(src.to_string());
    let change = TextChange {
        range: 28..40,
        text: String::new(),
    };
    let analyzed_source = analyzed_source.update(vec![change]);
    assert_eq!(
        analyzed_source.ast,
        Program {
            global_declarations: vec![
                Reference::new(
                    GlobalDeclaration::Procedure(ProcedureDeclaration {
                        doc: Vec::new(),
                        name: Some(Identifier::new("main".to_string(), 1..2)),
                        parameters: Vec::new(),
                        variable_declarations: Vec::new(),
                        statements: Vec::new(),
                        info: AstInfo::new(0..6)
                    }),
                    0,
                ),
                Reference::new(
                    GlobalDeclaration::Procedure(ProcedureDeclaration {
                        doc: Vec::new(),
                        name: Some(Identifier::new("b".to_string(), 1..2)),
                        parameters: Vec::new(),
                        variable_declarations: Vec::new(),
                        statements: Vec::new(),
                        info: AstInfo::new(0..6)
                    }),
                    6,
                ),
            ],
            info: AstInfo::new(0..12),
        }
    );
}

#[test]
fn insert_proc_start() {
    let text = "
proc a() {}
proc b() {}
";
    let change = TextChange {
        range: 0..0,
        text: "proc main() {}\n".to_string(),
    };
    let ast = test_incremental(text, change);
    assert_debug_snapshot!(ast);
}

#[test]
fn insert_proc_middle() {
    let text = "
proc a() {}
proc b() {}
";
    let change = TextChange {
        range: 13..13,
        text: "proc main() {}\n".to_string(),
    };
    let ast = test_incremental(text, change);
    assert_debug_snapshot!(ast);
}

#[test]
fn insert_proc_end() {
    let text = "
proc a() {}
proc b() {}
";
    let change = TextChange {
        range: 25..25,
        text: "proc main() {}\n".to_string(),
    };
    let ast = test_incremental(text, change);
    assert_debug_snapshot!(ast);
}

#[test]
fn joining_deletion() {
    let text = "
proc a() {
    var x: int;
}
proc b() {
    x := 1;
}
proc main() {}
";
    let change = TextChange {
        range: 28..41,
        text: "".to_string(),
    };
    let ast = test_incremental(text, change);
    assert_debug_snapshot!(ast);
}

#[test]
fn splitting_insertion() {
    let text = "
proc a() {
    var x: int;
    x := 1;
}
proc main() {}
";
    let change = TextChange {
        range: 28..28,
        text: "}
proc b() {
"
        .to_string(),
    };
    let ast = test_incremental(text, change);
    assert_debug_snapshot!(ast);
}

#[test]
fn destroy_proc() {
    let text = "
proc a() {}
proc b() {}
proc main() {}
";
    let change = TextChange {
        range: 13..17,
        text: "".to_string(),
    };
    let ast = test_incremental(text, change);
    assert_debug_snapshot!(ast);
}

#[test]
fn proc_from_error() {
    let text = "
proc a() {}
b() {}
proc main() {}
";
    let change = TextChange {
        range: 13..13,
        text: "proc ".to_string(),
    };
    let ast = test_incremental(text, change);
    assert_debug_snapshot!(ast);
}

#[test]
fn delete_inside_block() {
    let text = "
proc main() {
    if (1=1) {
        x := 1;
    } else {
        main();
    }
}
";
    let change = TextChange {
        range: 30..46,
        text: "".to_string(),
    };
    let ast = test_incremental(text, change);
    assert_debug_snapshot!(ast);
}

#[test]
fn param_deletion() {
    let text = "
proc a(x: int, y: int) {
    x := 1;
}
";
    let change = TextChange {
        range: 8..14,
        text: "".to_string(),
    };
    let ast = test_incremental(text, change);
    assert_debug_snapshot!(ast);
}

#[test]
fn delete_parameter_list() {
    let text = "
proc main() {}
";
    let change = TextChange {
        range: 10..12,
        text: "".to_string(),
    };
    let ast = test_incremental(text, change);
    assert_debug_snapshot!(ast);
}

#[test]
fn param_insertion() {
    let text = "
proc a(, y: int) {
    n := 1;
}
";
    let change = TextChange {
        range: 8..8,
        text: "x: int".to_string(),
    };
    let ast = test_incremental(text, change);
    assert_debug_snapshot!(ast);
}

#[test]
fn arg_deletion() {
    let text = "
proc a() {
    b(1, 2, 3);
}
";
    let change = TextChange {
        range: 20..23,
        text: "".to_string(),
    };
    let ast = test_incremental(text, change);
    assert_debug_snapshot!(ast);
}

#[test]
fn arg_insertion() {
    let text = "
proc a() {
    b(1, 3);
}
";
    let change = TextChange {
        range: 20..20,
        text: "2, ".to_string(),
    };
    let ast = test_incremental(text, change);
    assert_debug_snapshot!(ast);
}

#[test]
fn delete_condition_lhs() {
    let text = "
proc main() {
    if (0 <= 1) {}
}
";
    let change = TextChange {
        range: 23..24,
        text: "".to_string(),
    };
    let ast = test_incremental(text, change);
    assert_debug_snapshot!(ast);
}

#[test]
fn insert_condition_lhs() {
    let text = "
proc main() {
    if ( <= 1) {}
}
";
    let change = TextChange {
        range: 23..23,
        text: "0".to_string(),
    };
    let ast = test_incremental(text, change);
    assert_debug_snapshot!(ast);
}

#[test]
fn delete_condition_rhs() {
    let text = "
proc main() {
    if (0 <= 1) {}
}
";
    let change = TextChange {
        range: 28..29,
        text: "".to_string(),
    };
    let ast = test_incremental(text, change);
    assert_debug_snapshot!(ast);
}

#[test]
fn insert_condition_rhs() {
    let text = "
proc main() {
    if (0 <= ) {}
}
";
    let change = TextChange {
        range: 28..28,
        text: "1".to_string(),
    };
    let ast = test_incremental(text, change);
    assert_debug_snapshot!(ast);
}

#[test]
fn if_block_disruption() {
    let text = "
proc main() {
    if (0 = 0) {
        y := 2;
    } else {
        x := 1;
    }
}
";
    let change = TextChange {
        range: 30..31,
        text: "".to_string(),
    };
    let ast = test_incremental(text, change);
    assert_debug_snapshot!(ast);
}

#[test]
fn if_block_restoration() {
    let text = "
proc main() {
    if (0 = 0) 
        y := 2;
    }
    else {
        x := 1;
    }
}
";
    let change = TextChange {
        range: 30..30,
        text: "{".to_string(),
    };
    let ast = test_incremental(text, change);
    assert_debug_snapshot!(ast);
}
