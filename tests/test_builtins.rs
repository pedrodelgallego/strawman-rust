use strawman::builtins::default_env;
use strawman::eval::straw_eval;
use strawman::parser::parse;

// ── E1.11 — Arithmetic builtins ──

#[test]
fn e1_11_add_zero_args() {
    // (+) → 0
    let env = default_env();
    let expr = parse("(+)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Number(0.0));
}

#[test]
fn e1_11_add_one() {
    // (+ 5) → 5
    let env = default_env();
    let expr = parse("(+ 5)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Number(5.0));
}

#[test]
fn e1_11_add_many() {
    // (+ 1 2 3 4) → 10
    let env = default_env();
    let expr = parse("(+ 1 2 3 4)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Number(10.0));
}

#[test]
fn e1_11_add_floats() {
    // (+ 1.5 2.5) → 4.0
    let env = default_env();
    let expr = parse("(+ 1.5 2.5)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Number(4.0));
}

#[test]
fn e1_11_sub_negate() {
    // (- 5) → -5
    let env = default_env();
    let expr = parse("(- 5)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Number(-5.0));
}

#[test]
fn e1_11_sub_two() {
    // (- 10 3) → 7
    let env = default_env();
    let expr = parse("(- 10 3)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Number(7.0));
}

#[test]
fn e1_11_sub_many() {
    // (- 10 3 2) → 5
    let env = default_env();
    let expr = parse("(- 10 3 2)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Number(5.0));
}

#[test]
fn e1_11_mul_zero_args() {
    // (*) → 1
    let env = default_env();
    let expr = parse("(*)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Number(1.0));
}

#[test]
fn e1_11_mul() {
    // (* 3 4) → 12
    let env = default_env();
    let expr = parse("(* 3 4)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Number(12.0));
}

#[test]
fn e1_11_div_int() {
    // (/ 10 2) → 5
    let env = default_env();
    let expr = parse("(/ 10 2)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Number(5.0));
}

#[test]
fn e1_11_div_float() {
    // (/ 7 2) → 3.5
    let env = default_env();
    let expr = parse("(/ 7 2)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Number(3.5));
}

#[test]
fn e1_11_div_zero() {
    // (/ 1 0) → Error: division by zero
    let env = default_env();
    let expr = parse("(/ 1 0)").unwrap();
    let result = straw_eval(&expr, &env);
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), "division by zero");
}

#[test]
fn e1_11_mod() {
    // (mod 10 3) → 1
    let env = default_env();
    let expr = parse("(mod 10 3)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Number(1.0));
}

#[test]
fn e1_11_mod_zero() {
    // (mod 10 0) → Error: division by zero
    let env = default_env();
    let expr = parse("(mod 10 0)").unwrap();
    let result = straw_eval(&expr, &env);
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), "division by zero");
}

#[test]
fn e1_11_non_number() {
    // (+ 1 "a") → Error: expected number
    let env = default_env();
    let expr = parse("(+ 1 \"a\")").unwrap();
    let result = straw_eval(&expr, &env);
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), "expected number");
}

// ── E1.12 — Comparison operators ──

#[test]
fn e1_12_less_true() {
    // (< 1 2) → #t
    let env = default_env();
    let expr = parse("(< 1 2)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(true));
}

#[test]
fn e1_12_less_false() {
    // (< 2 1) → #f
    let env = default_env();
    let expr = parse("(< 2 1)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(false));
}

#[test]
fn e1_12_less_equal_args() {
    // (< 2 2) → #f
    let env = default_env();
    let expr = parse("(< 2 2)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(false));
}

#[test]
fn e1_12_greater() {
    // (> 3 1) → #t
    let env = default_env();
    let expr = parse("(> 3 1)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(true));
}

#[test]
fn e1_12_leq_true() {
    // (<= 2 2) → #t
    let env = default_env();
    let expr = parse("(<= 2 2)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(true));
}

#[test]
fn e1_12_geq_false() {
    // (>= 1 2) → #f
    let env = default_env();
    let expr = parse("(>= 1 2)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(false));
}

#[test]
fn e1_12_num_eq() {
    // (= 5 5) → #t
    let env = default_env();
    let expr = parse("(= 5 5)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(true));
}

#[test]
fn e1_12_num_neq() {
    // (= 5 6) → #f
    let env = default_env();
    let expr = parse("(= 5 6)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(false));
}

#[test]
fn e1_12_equal_atoms() {
    // (equal? 42 42) → #t
    let env = default_env();
    let expr = parse("(equal? 42 42)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(true));
}

#[test]
fn e1_12_equal_strings() {
    // (equal? "ab" "ab") → #t
    let env = default_env();
    let expr = parse(r#"(equal? "ab" "ab")"#).unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(true));
}

#[test]
fn e1_12_equal_lists() {
    // (equal? '(1 2) '(1 2)) → #t
    let env = default_env();
    let expr = parse("(equal? '(1 2) '(1 2))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(true));
}

#[test]
fn e1_12_equal_diff() {
    // (equal? '(1 2) '(1 3)) → #f
    let env = default_env();
    let expr = parse("(equal? '(1 2) '(1 3))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(false));
}

#[test]
fn e1_12_equal_nested_lists() {
    // (equal? (list 1 (list 2 3)) (list 1 (list 2 3))) → #t
    // Acceptance criterion: nested list equality
    let env = default_env();
    let expr = parse("(equal? (list 1 (list 2 3)) (list 1 (list 2 3)))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(true));
}

#[test]
fn e1_12_non_number_comparison_error() {
    // (< "a" "b") → Error: expected number
    let env = default_env();
    let expr = parse(r#"(< "a" "b")"#).unwrap();
    let result = straw_eval(&expr, &env);
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), "expected number");
}

// ── E1.13 — List operations ──

#[test]
fn e1_13_cons_onto_list() {
    // (cons 1 '(2 3)) → '(1 2 3)
    let env = default_env();
    let expr = parse("(cons 1 '(2 3))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(
        result,
        strawman::env::Value::List(vec![
            strawman::env::Value::Number(1.0),
            strawman::env::Value::Number(2.0),
            strawman::env::Value::Number(3.0),
        ])
    );
}

#[test]
fn e1_13_cons_dotted() {
    // (cons 1 2) → '(1 . 2)
    let env = default_env();
    let expr = parse("(cons 1 2)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(
        result,
        strawman::env::Value::Pair(
            Box::new(strawman::env::Value::Number(1.0)),
            Box::new(strawman::env::Value::Number(2.0)),
        )
    );
}

// ── E1.13 — car / cdr ──

#[test]
fn e1_13_car() {
    // (car '(a b c)) → 'a
    let env = default_env();
    let expr = parse("(car '(a b c))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Symbol("a".to_string()));
}

#[test]
fn e1_13_cdr() {
    // (cdr '(a b c)) → '(b c)
    let env = default_env();
    let expr = parse("(cdr '(a b c))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(
        result,
        strawman::env::Value::List(vec![
            strawman::env::Value::Symbol("b".to_string()),
            strawman::env::Value::Symbol("c".to_string()),
        ])
    );
}

#[test]
fn e1_13_car_of_cons() {
    // (car (cons 1 2)) → 1
    let env = default_env();
    let expr = parse("(car (cons 1 2))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Number(1.0));
}

#[test]
fn e1_13_cdr_of_cons() {
    // (cdr (cons 1 2)) → 2
    let env = default_env();
    let expr = parse("(cdr (cons 1 2))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Number(2.0));
}

#[test]
fn e1_13_car_of_empty_errors() {
    // (car '()) → Error: car: expected pair
    let env = default_env();
    let expr = parse("(car '())").unwrap();
    let result = straw_eval(&expr, &env);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("car"), "error should mention 'car': {}", err);
    assert!(err.contains("pair"), "error should mention 'pair': {}", err);
}

#[test]
fn e1_13_cdr_of_atom_errors() {
    // (cdr 42) → Error: cdr: expected pair
    let env = default_env();
    let expr = parse("(cdr 42)").unwrap();
    let result = straw_eval(&expr, &env);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("cdr"), "error should mention 'cdr': {}", err);
    assert!(err.contains("pair"), "error should mention 'pair': {}", err);
}

#[test]
fn e1_13_car_cdr_composition() {
    // Acceptance criterion: (car (cdr (list 1 2 3))) → 2
    let env = default_env();
    let expr = parse("(car (cdr (list 1 2 3)))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Number(2.0));
}

#[test]
fn e1_13_car_of_atom_errors() {
    // Acceptance criterion: (car 42) → Error mentioning "car" and "pair"
    let env = default_env();
    let expr = parse("(car 42)").unwrap();
    let result = straw_eval(&expr, &env);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.contains("car"), "error should mention 'car': {}", err);
    assert!(err.contains("pair"), "error should mention 'pair': {}", err);
}

// ── E1.13 — list builtin ──

#[test]
fn e1_13_list_empty() {
    // (list) → '()
    let env = default_env();
    let expr = parse("(list)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::List(vec![]));
}

#[test]
fn e1_13_list_many() {
    // (list 1 2 3) → '(1 2 3)
    let env = default_env();
    let expr = parse("(list 1 2 3)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(
        result,
        strawman::env::Value::List(vec![
            strawman::env::Value::Number(1.0),
            strawman::env::Value::Number(2.0),
            strawman::env::Value::Number(3.0),
        ])
    );
}

// ── E1.13 — null? ──

#[test]
fn e1_13_null_empty() {
    // (null? '()) → #t
    let env = default_env();
    let expr = parse("(null? '())").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(true));
}

#[test]
fn e1_13_null_non_empty() {
    // (null? '(1)) → #f
    let env = default_env();
    let expr = parse("(null? '(1))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(false));
}

#[test]
fn e1_13_null_non_list() {
    // (null? 42) → #f
    let env = default_env();
    let expr = parse("(null? 42)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(false));
}

// ── E1.13 — pair? ──

#[test]
fn e1_13_pair_pair() {
    // (pair? '(1 2)) → #t
    let env = default_env();
    let expr = parse("(pair? '(1 2))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(true));
}

#[test]
fn e1_13_pair_empty() {
    // (pair? '()) → #f
    let env = default_env();
    let expr = parse("(pair? '())").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(false));
}

#[test]
fn e1_13_pair_atom() {
    // (pair? 42) → #f
    let env = default_env();
    let expr = parse("(pair? 42)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(false));
}

// ── E1.14 — not builtin ──

#[test]
fn e1_14_not_true() {
    // (not #t) → #f
    let env = default_env();
    let expr = parse("(not #t)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(false));
}

#[test]
fn e1_14_not_false() {
    // (not #f) → #t
    let env = default_env();
    let expr = parse("(not #f)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(true));
}

#[test]
fn e1_14_not_truthy() {
    // (not 42) → #f
    let env = default_env();
    let expr = parse("(not 42)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(false));
}

// ── E1.15 — Type predicates ──

#[test]
fn e1_15_number_yes() {
    // (number? 42) → #t
    let env = default_env();
    let expr = parse("(number? 42)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(true));
}

#[test]
fn e1_15_number_no() {
    // (number? "x") → #f
    let env = default_env();
    let expr = parse(r#"(number? "x")"#).unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(false));
}

#[test]
fn e1_15_string_yes() {
    // (string? "hi") → #t
    let env = default_env();
    let expr = parse(r#"(string? "hi")"#).unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(true));
}

#[test]
fn e1_15_string_no() {
    // (string? 42) → #f
    let env = default_env();
    let expr = parse("(string? 42)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(false));
}

#[test]
fn e1_15_symbol_yes() {
    // (symbol? 'foo) → #t
    let env = default_env();
    let expr = parse("(symbol? 'foo)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(true));
}

#[test]
fn e1_15_symbol_no() {
    // (symbol? 42) → #f
    let env = default_env();
    let expr = parse("(symbol? 42)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(false));
}

#[test]
fn e1_15_boolean_yes() {
    // (boolean? #t) → #t
    let env = default_env();
    let expr = parse("(boolean? #t)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(true));
}

#[test]
fn e1_15_boolean_no() {
    // (boolean? 0) → #f
    let env = default_env();
    let expr = parse("(boolean? 0)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(false));
}

#[test]
fn e1_15_procedure_lambda() {
    // (procedure? (lambda (x) x)) → #t
    let env = default_env();
    let expr = parse("(procedure? (lambda (x) x))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(true));
}

#[test]
fn e1_15_procedure_builtin() {
    // (procedure? +) → #t
    let env = default_env();
    let expr = parse("(procedure? +)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(true));
}

#[test]
fn e1_15_procedure_no() {
    // (procedure? 42) → #f
    let env = default_env();
    let expr = parse("(procedure? 42)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(false));
}

// ── E1.15 — Acceptance criteria ──

#[test]
fn e1_15_number_computed() {
    // Acceptance: (number? (+ 1 2)) → #t
    let env = default_env();
    let expr = parse("(number? (+ 1 2))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(true));
}

#[test]
fn e1_15_procedure_car() {
    // Acceptance: (procedure? car) → #t
    let env = default_env();
    let expr = parse("(procedure? car)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, strawman::env::Value::Boolean(true));
}

// ── E1.15 — display ──

#[test]
fn e1_15_display_string() {
    // (display "hi") → stdout: hi, returns void
    use std::io::Read;
    let env = default_env();
    let expr = parse(r#"(display "hi")"#).unwrap();

    // Capture stdout using gag crate alternative: redirect via pipe
    let mut buf = gag::BufferRedirect::stdout().unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    let mut output = String::new();
    buf.read_to_string(&mut output).unwrap();
    drop(buf);

    assert_eq!(result, strawman::env::Value::Void);
    assert_eq!(output, "hi");
}

#[test]
fn e1_15_display_number() {
    // (display 42) → stdout: 42, returns void
    use std::io::Read;
    let env = default_env();
    let expr = parse("(display 42)").unwrap();

    let mut buf = gag::BufferRedirect::stdout().unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    let mut output = String::new();
    buf.read_to_string(&mut output).unwrap();
    drop(buf);

    assert_eq!(result, strawman::env::Value::Void);
    assert_eq!(output, "42");
}

// ── E1.15 — newline ──

#[test]
fn e1_15_newline() {
    // (newline) → stdout: \n, returns void
    use std::io::Read;
    let env = default_env();
    let expr = parse("(newline)").unwrap();

    let mut buf = gag::BufferRedirect::stdout().unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    let mut output = String::new();
    buf.read_to_string(&mut output).unwrap();
    drop(buf);

    assert_eq!(result, strawman::env::Value::Void);
    assert_eq!(output, "\n");
}
