use strawman::parser::{parse, parse_all, Expr};

#[test]
fn single_atom_number() {
    let result = parse("42").unwrap();
    assert_eq!(result, Expr::Number(42.0));
}

#[test]
fn single_atom_symbol() {
    let result = parse("foo").unwrap();
    assert_eq!(result, Expr::Symbol("foo".to_string()));
}

#[test]
fn single_atom_string() {
    let result = parse("\"hi\"").unwrap();
    assert_eq!(result, Expr::StringLit("hi".to_string()));
}

#[test]
fn single_atom_bool() {
    let result = parse("#t").unwrap();
    assert_eq!(result, Expr::Boolean(true));
}

#[test]
fn simple_list() {
    let result = parse("(+ 1 2)").unwrap();
    assert_eq!(
        result,
        Expr::List(vec![
            Expr::Symbol("+".to_string()),
            Expr::Number(1.0),
            Expr::Number(2.0),
        ])
    );
}

#[test]
fn nested_list() {
    let result = parse("(+ (* 2 3) 4)").unwrap();
    assert_eq!(
        result,
        Expr::List(vec![
            Expr::Symbol("+".to_string()),
            Expr::List(vec![
                Expr::Symbol("*".to_string()),
                Expr::Number(2.0),
                Expr::Number(3.0),
            ]),
            Expr::Number(4.0),
        ])
    );
}

#[test]
fn empty_list() {
    let result = parse("()").unwrap();
    assert_eq!(result, Expr::List(vec![]));
}

#[test]
fn deeply_nested() {
    let result = parse("(a (b (c d)))").unwrap();
    assert_eq!(
        result,
        Expr::List(vec![
            Expr::Symbol("a".to_string()),
            Expr::List(vec![
                Expr::Symbol("b".to_string()),
                Expr::List(vec![
                    Expr::Symbol("c".to_string()),
                    Expr::Symbol("d".to_string()),
                ]),
            ]),
        ])
    );
}

#[test]
fn quote_sugar_atom() {
    let result = parse("'x").unwrap();
    assert_eq!(
        result,
        Expr::List(vec![
            Expr::Symbol("quote".to_string()),
            Expr::Symbol("x".to_string()),
        ])
    );
}

#[test]
fn quote_sugar_list() {
    let result = parse("'(1 2 3)").unwrap();
    assert_eq!(
        result,
        Expr::List(vec![
            Expr::Symbol("quote".to_string()),
            Expr::List(vec![
                Expr::Number(1.0),
                Expr::Number(2.0),
                Expr::Number(3.0),
            ]),
        ])
    );
}

#[test]
fn multiple_exprs() {
    let result = parse_all("1 2 3").unwrap();
    assert_eq!(
        result,
        vec![
            Expr::Number(1.0),
            Expr::Number(2.0),
            Expr::Number(3.0),
        ]
    );
}

#[test]
fn unmatched_open_paren() {
    let result = parse("(+ 1");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.contains("unexpected end of input"),
        "expected error containing 'unexpected end of input', got: {}",
        err
    );
}

#[test]
fn unmatched_close_paren() {
    let result = parse(")");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.contains("unexpected closing paren"),
        "expected error containing 'unexpected closing paren', got: {}",
        err
    );
}

#[test]
fn empty_input() {
    let result = parse("");
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.contains("empty input"),
        "expected error containing 'empty input', got: {}",
        err
    );
}
