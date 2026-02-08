use std::rc::Rc;
use strawman::env::{Env, Value};
use strawman::eval::straw_eval;
use strawman::parser::Expr;

#[test]
fn eval_integer_returns_itself() {
    let env = Rc::new(Env::new());
    let result = straw_eval(&Expr::Number(42.0), &env).unwrap();
    assert_eq!(result, Value::Number(42.0));
}

#[test]
fn eval_float_returns_itself() {
    let env = Rc::new(Env::new());
    let result = straw_eval(&Expr::Number(3.14), &env).unwrap();
    assert_eq!(result, Value::Number(3.14));
}

#[test]
fn eval_negative_returns_itself() {
    let env = Rc::new(Env::new());
    let result = straw_eval(&Expr::Number(-7.0), &env).unwrap();
    assert_eq!(result, Value::Number(-7.0));
}

#[test]
fn eval_string_returns_itself() {
    let env = Rc::new(Env::new());
    let result = straw_eval(&Expr::StringLit("hello".to_string()), &env).unwrap();
    assert_eq!(result, Value::StringLit("hello".to_string()));
}

#[test]
fn eval_boolean_true_returns_itself() {
    let env = Rc::new(Env::new());
    let result = straw_eval(&Expr::Boolean(true), &env).unwrap();
    assert_eq!(result, Value::Boolean(true));
}

#[test]
fn eval_boolean_false_returns_itself() {
    let env = Rc::new(Env::new());
    let result = straw_eval(&Expr::Boolean(false), &env).unwrap();
    assert_eq!(result, Value::Boolean(false));
}

#[test]
fn eval_bound_symbol_returns_value() {
    let env = Rc::new(Env::new());
    env.set("x", Value::Number(5.0));
    let result = straw_eval(&Expr::Symbol("x".to_string()), &env).unwrap();
    assert_eq!(result, Value::Number(5.0));
}

#[test]
fn eval_unbound_symbol_returns_error() {
    let env = Rc::new(Env::new());
    let result = straw_eval(&Expr::Symbol("y".to_string()), &env);
    assert_eq!(result, Err("unbound variable: y".to_string()));
}

#[test]
fn eval_quote_symbol() {
    let env = Rc::new(Env::new());
    // (quote foo) → symbol foo — even though foo is unbound
    let expr = Expr::List(vec![
        Expr::Symbol("quote".to_string()),
        Expr::Symbol("foo".to_string()),
    ]);
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Symbol("foo".to_string()));
}

#[test]
fn eval_quote_number() {
    let env = Rc::new(Env::new());
    // (quote 42) → 42
    let expr = Expr::List(vec![
        Expr::Symbol("quote".to_string()),
        Expr::Number(42.0),
    ]);
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(42.0));
}

#[test]
fn eval_quote_list() {
    let env = Rc::new(Env::new());
    // (quote (1 2 3)) → list of 1, 2, 3
    let expr = Expr::List(vec![
        Expr::Symbol("quote".to_string()),
        Expr::List(vec![
            Expr::Number(1.0),
            Expr::Number(2.0),
            Expr::Number(3.0),
        ]),
    ]);
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(
        result,
        Value::List(vec![
            Value::Number(1.0),
            Value::Number(2.0),
            Value::Number(3.0),
        ])
    );
}

#[test]
fn eval_quote_nested() {
    let env = Rc::new(Env::new());
    // (quote (a (b c))) → nested list with symbols
    let expr = Expr::List(vec![
        Expr::Symbol("quote".to_string()),
        Expr::List(vec![
            Expr::Symbol("a".to_string()),
            Expr::List(vec![
                Expr::Symbol("b".to_string()),
                Expr::Symbol("c".to_string()),
            ]),
        ]),
    ]);
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(
        result,
        Value::List(vec![
            Value::Symbol("a".to_string()),
            Value::List(vec![
                Value::Symbol("b".to_string()),
                Value::Symbol("c".to_string()),
            ]),
        ])
    );
}

#[test]
fn eval_quote_empty_list() {
    let env = Rc::new(Env::new());
    // (quote ()) → empty list
    let expr = Expr::List(vec![
        Expr::Symbol("quote".to_string()),
        Expr::List(vec![]),
    ]);
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::List(vec![]));
}

#[test]
fn eval_quote_too_many_args_error() {
    let env = Rc::new(Env::new());
    // (quote a b) → error: quote expects exactly one argument
    let expr = Expr::List(vec![
        Expr::Symbol("quote".to_string()),
        Expr::Symbol("a".to_string()),
        Expr::Symbol("b".to_string()),
    ]);
    let result = straw_eval(&expr, &env);
    assert_eq!(
        result,
        Err("quote expects exactly one argument".to_string())
    );
}

#[test]
fn eval_quote_no_args_error() {
    let env = Rc::new(Env::new());
    // (quote) → error: quote expects exactly one argument
    let expr = Expr::List(vec![
        Expr::Symbol("quote".to_string()),
    ]);
    let result = straw_eval(&expr, &env);
    assert_eq!(
        result,
        Err("quote expects exactly one argument".to_string())
    );
}

#[test]
fn eval_if_true_branch() {
    let env = Rc::new(Env::new());
    // (if #t 1 2) → 1
    let expr = Expr::List(vec![
        Expr::Symbol("if".to_string()),
        Expr::Boolean(true),
        Expr::Number(1.0),
        Expr::Number(2.0),
    ]);
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(1.0));
}

#[test]
fn eval_if_false_branch() {
    let env = Rc::new(Env::new());
    // (if #f 1 2) → 2
    let expr = Expr::List(vec![
        Expr::Symbol("if".to_string()),
        Expr::Boolean(false),
        Expr::Number(1.0),
        Expr::Number(2.0),
    ]);
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(2.0));
}

#[test]
fn eval_if_truthy_zero() {
    let env = Rc::new(Env::new());
    // (if 0 "yes" "no") → "yes" — 0 is truthy in Strawman Lisp
    let expr = Expr::List(vec![
        Expr::Symbol("if".to_string()),
        Expr::Number(0.0),
        Expr::StringLit("yes".to_string()),
        Expr::StringLit("no".to_string()),
    ]);
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::StringLit("yes".to_string()));
}

#[test]
fn eval_if_truthy_empty_list() {
    let env = Rc::new(Env::new());
    // (if '() "yes" "no") → "yes" — empty list is truthy in Strawman Lisp
    // '() is (quote ()), which evaluates to Value::List(vec![])
    let expr = Expr::List(vec![
        Expr::Symbol("if".to_string()),
        Expr::List(vec![
            Expr::Symbol("quote".to_string()),
            Expr::List(vec![]),
        ]),
        Expr::StringLit("yes".to_string()),
        Expr::StringLit("no".to_string()),
    ]);
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::StringLit("yes".to_string()));
}

#[test]
fn eval_if_no_alternative_true() {
    let env = Rc::new(Env::new());
    // (if #t 42) → 42
    let expr = Expr::List(vec![
        Expr::Symbol("if".to_string()),
        Expr::Boolean(true),
        Expr::Number(42.0),
    ]);
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(42.0));
}

#[test]
fn eval_if_no_alternative_false() {
    let env = Rc::new(Env::new());
    // (if #f 42) → void — no alternative branch with false condition
    let expr = Expr::List(vec![
        Expr::Symbol("if".to_string()),
        Expr::Boolean(false),
        Expr::Number(42.0),
    ]);
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Void);
}

#[test]
fn eval_if_non_taken_branch_not_evaluated() {
    let env = Rc::new(Env::new());
    // (if #t 1 (error "boom")) → 1 (no error)
    // We simulate (error "boom") with an unbound variable reference
    // that would error if evaluated. The else branch must NOT be evaluated.
    let expr = Expr::List(vec![
        Expr::Symbol("if".to_string()),
        Expr::Boolean(true),
        Expr::Number(1.0),
        Expr::List(vec![
            Expr::Symbol("error".to_string()),
            Expr::StringLit("boom".to_string()),
        ]),
    ]);
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(1.0));

    // Verify the else branch WOULD fail if evaluated directly
    let else_branch = Expr::List(vec![
        Expr::Symbol("error".to_string()),
        Expr::StringLit("boom".to_string()),
    ]);
    assert!(straw_eval(&else_branch, &env).is_err());
}

#[test]
fn eval_if_no_args_error() {
    let env = Rc::new(Env::new());
    // (if) → error: if expects 2 or 3 arguments
    let expr = Expr::List(vec![
        Expr::Symbol("if".to_string()),
    ]);
    let result = straw_eval(&expr, &env);
    assert_eq!(result, Err("if expects 2 or 3 arguments".to_string()));
}

#[test]
fn eval_begin_single_expr() {
    let env = Rc::new(Env::new());
    // (begin 42) → 42
    let expr = Expr::List(vec![
        Expr::Symbol("begin".to_string()),
        Expr::Number(42.0),
    ]);
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(42.0));
}

#[test]
fn eval_begin_two_exprs_returns_last() {
    let env = Rc::new(Env::new());
    // (begin 1 2) → 2
    let expr = Expr::List(vec![
        Expr::Symbol("begin".to_string()),
        Expr::Number(1.0),
        Expr::Number(2.0),
    ]);
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(2.0));
}

#[test]
fn eval_begin_three_exprs_returns_last() {
    let env = Rc::new(Env::new());
    // (begin 1 2 3) → 3
    let expr = Expr::List(vec![
        Expr::Symbol("begin".to_string()),
        Expr::Number(1.0),
        Expr::Number(2.0),
        Expr::Number(3.0),
    ]);
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(3.0));
}

#[test]
fn eval_begin_empty_returns_void() {
    let env = Rc::new(Env::new());
    // (begin) → void
    let expr = Expr::List(vec![
        Expr::Symbol("begin".to_string()),
    ]);
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Void);
}

#[test]
fn eval_begin_nested() {
    let env = Rc::new(Env::new());
    // (begin (begin 1 2) 3) → 3
    let expr = Expr::List(vec![
        Expr::Symbol("begin".to_string()),
        Expr::List(vec![
            Expr::Symbol("begin".to_string()),
            Expr::Number(1.0),
            Expr::Number(2.0),
        ]),
        Expr::Number(3.0),
    ]);
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(3.0));
}

#[test]
fn eval_simple_define() {
    let env = Rc::new(Env::new());
    // (begin (define x 42) x) → 42
    let expr = Expr::List(vec![
        Expr::Symbol("begin".to_string()),
        Expr::List(vec![
            Expr::Symbol("define".to_string()),
            Expr::Symbol("x".to_string()),
            Expr::Number(42.0),
        ]),
        Expr::Symbol("x".to_string()),
    ]);
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(42.0));
}

#[test]
fn eval_begin_side_effect_order() {
    let env = Rc::new(Env::new());
    // (begin (define x 1) (define x 2) x) → 2
    let expr = Expr::List(vec![
        Expr::Symbol("begin".to_string()),
        Expr::List(vec![
            Expr::Symbol("define".to_string()),
            Expr::Symbol("x".to_string()),
            Expr::Number(1.0),
        ]),
        Expr::List(vec![
            Expr::Symbol("define".to_string()),
            Expr::Symbol("x".to_string()),
            Expr::Number(2.0),
        ]),
        Expr::Symbol("x".to_string()),
    ]);
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(2.0));
}

#[test]
fn eval_define_overwrites() {
    let env = Rc::new(Env::new());
    // E1.8 Test Matrix: Define overwrites
    // (begin (define x 1) (define x 2) x) → 2
    let expr = Expr::List(vec![
        Expr::Symbol("begin".to_string()),
        Expr::List(vec![
            Expr::Symbol("define".to_string()),
            Expr::Symbol("x".to_string()),
            Expr::Number(1.0),
        ]),
        Expr::List(vec![
            Expr::Symbol("define".to_string()),
            Expr::Symbol("x".to_string()),
            Expr::Number(2.0),
        ]),
        Expr::Symbol("x".to_string()),
    ]);
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(2.0));
}

#[test]
fn eval_set_existing() {
    use strawman::parser::parse;
    // E1.8 Test Matrix: Set! existing
    // (begin (define x 1) (set! x 99) x) → 99
    let env = Rc::new(Env::new());
    let expr = parse("(begin (define x 1) (set! x 99) x)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(99.0));
}

#[test]
fn eval_define_expr() {
    use strawman::parser::parse;
    // E1.8 Test Matrix: Define expr
    // (begin (define x (+ 1 2)) x) → 3
    let env = Rc::new(Env::new());
    // Set up + as a builtin in the environment
    env.set("+", Value::Builtin("+".to_string(), |args| {
        let mut sum = 0.0;
        for arg in args {
            match arg {
                Value::Number(n) => sum += n,
                _ => return Err("expected number".to_string()),
            }
        }
        Ok(Value::Number(sum))
    }));
    let expr = parse("(begin (define x (+ 1 2)) x)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(3.0));
}

#[test]
fn eval_set_parent_binding() {
    use strawman::parser::parse;
    // E1.8 Test Matrix: Set! parent binding
    // (begin (define x 1) ((lambda () (set! x 5))) x) → 5
    let env = Rc::new(Env::new());
    let expr = parse("(begin (define x 1) ((lambda () (set! x 5))) x)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(5.0));
}

#[test]
fn eval_set_unbound_error() {
    use strawman::parser::parse;
    // E1.8 Test Matrix: Set! unbound → error
    // (set! z 1) → Error: cannot set! unbound variable
    let env = Rc::new(Env::new());
    let expr = parse("(set! z 1)").unwrap();
    let result = straw_eval(&expr, &env);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.contains("cannot set! unbound variable"),
        "expected error about unbound variable, got: {err}"
    );
}

#[test]
fn eval_define_returns_void() {
    use strawman::parser::parse;
    // E1.8 Test Matrix: Define returns void
    // (define x 1) → (void)
    let env = Rc::new(Env::new());
    let expr = parse("(define x 1)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Void);
    // Also verify the binding was actually created
    assert_eq!(env.lookup("x").unwrap(), Value::Number(1.0));
}

#[test]
fn eval_lambda_identity() {
    use strawman::parser::parse;
    // E1.9 Test Matrix: Identity
    // ((lambda (x) x) 42) → 42
    let env = Rc::new(Env::new());
    let expr = parse("((lambda (x) x) 42)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(42.0));
}

#[test]
fn eval_lambda_multi_param() {
    use strawman::parser::parse;
    // E1.9 Test Matrix: Multi param
    // ((lambda (x y) (+ x y)) 3 4) → 7
    let env = Rc::new(Env::new());
    env.set("+", Value::Builtin("+".to_string(), |args| {
        let mut sum = 0.0;
        for arg in args {
            match arg {
                Value::Number(n) => sum += n,
                _ => return Err("expected number".to_string()),
            }
        }
        Ok(Value::Number(sum))
    }));
    let expr = parse("((lambda (x y) (+ x y)) 3 4)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(7.0));
}

#[test]
fn eval_lambda_closure_captures_env() {
    use strawman::parser::parse;
    // E1.9 Test Matrix: Closure captures
    // (begin (define a 10) (define f (lambda (x) (+ x a))) (f 5)) → 15
    let env = Rc::new(Env::new());
    env.set("+", Value::Builtin("+".to_string(), |args| {
        let mut sum = 0.0;
        for arg in args {
            match arg {
                Value::Number(n) => sum += n,
                _ => return Err("expected number".to_string()),
            }
        }
        Ok(Value::Number(sum))
    }));
    let expr = parse("(begin (define a 10) (define f (lambda (x) (+ x a))) (f 5))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(15.0));
}

#[test]
fn eval_lambda_closure_over_closure() {
    use strawman::parser::parse;
    // E1.9 Test Matrix: Closure over closure (make-adder)
    // (begin (define make-adder (lambda (n) (lambda (x) (+ n x)))) ((make-adder 3) 7)) → 10
    let env = Rc::new(Env::new());
    env.set("+", Value::Builtin("+".to_string(), |args| {
        let mut sum = 0.0;
        for arg in args {
            match arg {
                Value::Number(n) => sum += n,
                _ => return Err("expected number".to_string()),
            }
        }
        Ok(Value::Number(sum))
    }));
    let expr = parse("(begin (define make-adder (lambda (n) (lambda (x) (+ n x)))) ((make-adder 3) 7))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(10.0));
}

#[test]
fn eval_lambda_implicit_begin_in_body() {
    use strawman::parser::parse;
    // E1.9 Test Matrix: Implicit begin
    // ((lambda (x) (define y 1) (+ x y)) 5) → 6
    let env = Rc::new(Env::new());
    env.set("+", Value::Builtin("+".to_string(), |args| {
        let mut sum = 0.0;
        for arg in args {
            match arg {
                Value::Number(n) => sum += n,
                _ => return Err("expected number".to_string()),
            }
        }
        Ok(Value::Number(sum))
    }));
    let expr = parse("((lambda (x) (define y 1) (+ x y)) 5)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(6.0));
}

#[test]
fn eval_lambda_no_params() {
    use strawman::parser::parse;
    // E1.9 Test Matrix: No params
    // ((lambda () 42)) → 42
    let env = Rc::new(Env::new());
    let expr = parse("((lambda () 42))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(42.0));
}

#[test]
fn eval_lambda_wrong_arity_error() {
    use strawman::parser::parse;
    // E1.9 Test Matrix: Wrong arity
    // ((lambda (x) x) 1 2) → Error: arity mismatch
    let env = Rc::new(Env::new());
    let expr = parse("((lambda (x) x) 1 2)").unwrap();
    let result = straw_eval(&expr, &env);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.contains("arity mismatch"),
        "expected arity mismatch error, got: {err}"
    );
    assert!(
        err.contains("expected 1") && err.contains("got 2"),
        "expected 'expected 1, got 2' in error, got: {err}"
    );
}

#[test]
fn eval_lambda_bad_param_list_error() {
    use strawman::parser::parse;
    // E1.9 Test Matrix: Bad param list
    // (lambda 42 x) → Error: expected parameter list
    let env = Rc::new(Env::new());
    let expr = parse("(lambda 42 x)").unwrap();
    let result = straw_eval(&expr, &env);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.contains("expected parameter list"),
        "expected 'expected parameter list' error, got: {err}"
    );
}

// ── E1.10 — Function application ──

fn make_test_env() -> Rc<Env> {
    let env = Rc::new(Env::new());
    env.set("+", Value::Builtin("+".to_string(), |args| {
        let mut sum = 0.0;
        for arg in &args {
            match arg {
                Value::Number(n) => sum += n,
                _ => return Err("expected number".to_string()),
            }
        }
        Ok(Value::Number(sum))
    }));
    env.set("-", Value::Builtin("-".to_string(), |args| {
        if args.is_empty() {
            return Err("expected at least 1 argument".to_string());
        }
        match &args[0] {
            Value::Number(first) => {
                if args.len() == 1 {
                    return Ok(Value::Number(-first));
                }
                let mut result = *first;
                for arg in &args[1..] {
                    match arg {
                        Value::Number(n) => result -= n,
                        _ => return Err("expected number".to_string()),
                    }
                }
                Ok(Value::Number(result))
            }
            _ => Err("expected number".to_string()),
        }
    }));
    env.set("*", Value::Builtin("*".to_string(), |args| {
        let mut product = 1.0;
        for arg in &args {
            match arg {
                Value::Number(n) => product *= n,
                _ => return Err("expected number".to_string()),
            }
        }
        Ok(Value::Number(product))
    }));
    env
}

#[test]
fn eval_e1_10_builtin_call() {
    use strawman::parser::parse;
    // E1.10 Test Matrix: Builtin call
    // (+ 1 2) → 3
    let env = make_test_env();
    let expr = parse("(+ 1 2)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(3.0));
}

#[test]
fn eval_e1_10_closure_call() {
    use strawman::parser::parse;
    // E1.10 Test Matrix: Closure call
    // ((lambda (x) (* x x)) 5) → 25
    let env = make_test_env();
    let expr = parse("((lambda (x) (* x x)) 5)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(25.0));
}

#[test]
fn eval_e1_10_nested_call() {
    use strawman::parser::parse;
    // E1.10 Test Matrix: Nested call
    // (+ (* 2 3) (- 10 4)) → 12
    let env = make_test_env();
    let expr = parse("(+ (* 2 3) (- 10 4))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(12.0));
}

#[test]
fn eval_e1_10_higher_order() {
    use strawman::parser::parse;
    // E1.10 Test Matrix: Higher-order
    // ((lambda (f x) (f x)) (lambda (n) (* n 2)) 5) → 10
    let env = make_test_env();
    let expr = parse("((lambda (f x) (f x)) (lambda (n) (* n 2)) 5)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(10.0));
}

#[test]
fn eval_e1_10_non_procedure_number() {
    use strawman::parser::parse;
    // E1.10 Test Matrix: Non-procedure
    // (42) → Error: not a procedure
    let env = make_test_env();
    let expr = parse("(42)").unwrap();
    let result = straw_eval(&expr, &env);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.contains("not a procedure"),
        "expected 'not a procedure' error, got: {err}"
    );
}

#[test]
fn eval_e1_10_non_procedure_string() {
    use strawman::parser::parse;
    // E1.10 Test Matrix: Non-procedure string
    // ("hello" 1) → Error: not a procedure
    let env = make_test_env();
    let expr = parse("(\"hello\" 1)").unwrap();
    let result = straw_eval(&expr, &env);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.contains("not a procedure"),
        "expected 'not a procedure' error, got: {err}"
    );
}
