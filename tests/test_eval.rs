extern crate gag;

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

// ── E1.14 — and special form ──

#[test]
fn eval_e1_14_and_all_true() {
    use strawman::parser::parse;
    // (and 1 2 3) → 3
    let env = Rc::new(Env::new());
    let expr = parse("(and 1 2 3)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(3.0));
}

#[test]
fn eval_e1_14_and_short_circuit() {
    use strawman::parser::parse;
    // (and #f (error "boom")) → #f
    // The second expression must NOT be evaluated
    let env = Rc::new(Env::new());
    let expr = parse("(and #f (error \"boom\"))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Boolean(false));
}

#[test]
fn eval_e1_14_and_empty() {
    use strawman::parser::parse;
    // (and) → #t
    let env = Rc::new(Env::new());
    let expr = parse("(and)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Boolean(true));
}

#[test]
fn eval_e1_14_and_one_false() {
    use strawman::parser::parse;
    // (and 1 #f 3) → #f
    let env = Rc::new(Env::new());
    let expr = parse("(and 1 #f 3)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Boolean(false));
}

// ── E1.14 — or special form ──

#[test]
fn eval_e1_14_or_first_true() {
    use strawman::parser::parse;
    // (or 1 2) → 1
    let env = Rc::new(Env::new());
    let expr = parse("(or 1 2)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(1.0));
}

#[test]
fn eval_e1_14_or_all_false() {
    use strawman::parser::parse;
    // (or #f #f) → #f
    let env = Rc::new(Env::new());
    let expr = parse("(or #f #f)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Boolean(false));
}

#[test]
fn eval_e1_14_or_short_circuit() {
    use strawman::parser::parse;
    // (or 1 (error "boom")) → 1
    // The second expression must NOT be evaluated
    let env = Rc::new(Env::new());
    let expr = parse("(or 1 (error \"boom\"))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(1.0));
}

#[test]
fn eval_e1_14_or_empty() {
    use strawman::parser::parse;
    // (or) → #f
    let env = Rc::new(Env::new());
    let expr = parse("(or)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Boolean(false));
}

// ── E2.1 — let ──

#[test]
fn eval_e2_1_let_simple() {
    use strawman::parser::parse;
    // (let ((x 1)) x) → 1
    let env = Rc::new(Env::new());
    let expr = parse("(let ((x 1)) x)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(1.0));
}

#[test]
fn eval_e2_1_let_two_bindings() {
    use strawman::parser::parse;
    // (let ((x 1) (y 2)) (+ x y)) → 3
    let env = make_test_env();
    let expr = parse("(let ((x 1) (y 2)) (+ x y))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(3.0));
}

#[test]
fn eval_e2_1_let_parallel_semantics() {
    use strawman::parser::parse;
    // (let ((x 1) (y x)) y) with no outer x → Error: unbound variable
    let env = Rc::new(Env::new());
    let expr = parse("(let ((x 1) (y x)) y)").unwrap();
    let result = straw_eval(&expr, &env);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.contains("unbound variable"),
        "expected 'unbound variable' error, got: {err}"
    );
}

#[test]
fn eval_e2_1_let_shadowing() {
    use strawman::parser::parse;
    // (begin (define x 10) (let ((x 20)) x)) → 20
    let env = make_test_env();
    let expr = parse("(begin (define x 10) (let ((x 20)) x))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(20.0));
}

#[test]
fn eval_e2_1_let_outer_unchanged() {
    use strawman::parser::parse;
    // (begin (define x 10) (let ((x 20)) x) x) → 10
    let env = make_test_env();
    let expr = parse("(begin (define x 10) (let ((x 20)) x) x)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(10.0));
}

#[test]
fn eval_e2_1_let_body_implicit_begin() {
    use strawman::parser::parse;
    // (let ((x 1)) (define y 2) (+ x y)) → 3
    let env = make_test_env();
    let expr = parse("(let ((x 1)) (define y 2) (+ x y))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(3.0));
}

#[test]
fn eval_e2_1_let_nested() {
    use strawman::parser::parse;
    // (let ((x 1)) (let ((y 2)) (+ x y))) → 3
    let env = make_test_env();
    let expr = parse("(let ((x 1)) (let ((y 2)) (+ x y)))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(3.0));
}

#[test]
fn eval_e2_1_let_empty_bindings() {
    use strawman::parser::parse;
    // (let () 42) → 42
    let env = Rc::new(Env::new());
    let expr = parse("(let () 42)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(42.0));
}

#[test]
fn eval_e2_1_let_malformed_binding() {
    use strawman::parser::parse;
    // (let (x 1) x) → Error: malformed binding
    let env = Rc::new(Env::new());
    let expr = parse("(let (x 1) x)").unwrap();
    let result = straw_eval(&expr, &env);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.contains("malformed binding"),
        "expected 'malformed binding' error, got: {err}"
    );
}

// ── E2.2 — let* ──

#[test]
fn eval_e2_2_let_star_sequential() {
    use strawman::parser::parse;
    // (let* ((x 1) (y (+ x 1))) y) → 2
    let env = make_test_env();
    let expr = parse("(let* ((x 1) (y (+ x 1))) y)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(2.0));
}

#[test]
fn eval_e2_2_let_star_three_deps() {
    use strawman::parser::parse;
    // (let* ((a 1) (b (+ a 1)) (c (+ b 1))) c) → 3
    let env = make_test_env();
    let expr = parse("(let* ((a 1) (b (+ a 1)) (c (+ b 1))) c)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(3.0));
}

#[test]
fn eval_e2_2_let_star_shadow_across() {
    use strawman::parser::parse;
    // (let* ((x 1) (x (+ x 1))) x) → 2
    let env = make_test_env();
    let expr = parse("(let* ((x 1) (x (+ x 1))) x)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(2.0));
}

#[test]
fn eval_e2_2_let_star_empty() {
    use strawman::parser::parse;
    // (let* () 42) → 42
    let env = Rc::new(Env::new());
    let expr = parse("(let* () 42)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(42.0));
}

#[test]
fn eval_e2_2_let_star_acceptance() {
    use strawman::parser::parse;
    // Acceptance criteria: (let* ((x 10) (y (* x 2))) y) → 20
    let env = make_test_env();
    let expr = parse("(let* ((x 10) (y (* x 2))) y)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(20.0));
}

// ── E2.3 — letrec ──

#[test]
fn eval_e2_3_letrec_self_recursive_factorial() {
    use strawman::parser::parse;
    use strawman::builtins::default_env;
    // E2.3 Test Matrix: Self-recursive (factorial)
    // (letrec ((f (lambda (n) (if (<= n 0) 1 (* n (f (- n 1))))))) (f 5)) → 120
    let env = default_env();
    let expr = parse("(letrec ((f (lambda (n) (if (<= n 0) 1 (* n (f (- n 1))))))) (f 5))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(120.0));
}

#[test]
fn eval_e2_3_letrec_mutual_recursion_even() {
    use strawman::parser::parse;
    use strawman::builtins::default_env;
    // E2.3 Test Matrix: Mutual recursion (even?/odd?)
    // (letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
    //          (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
    //   (even? 4)) → #t
    let env = default_env();
    let expr = parse("(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (even? 4))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Boolean(true));
}

#[test]
fn eval_e2_3_letrec_mutual_recursion_even_10() {
    use strawman::parser::parse;
    use strawman::builtins::default_env;
    // E2.3 Acceptance Criterion: (even? 10) → #t
    let env = default_env();
    let expr = parse("(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (even? 10))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Boolean(true));
}

#[test]
fn eval_e2_3_letrec_non_lambda_value() {
    use strawman::parser::parse;
    use strawman::builtins::default_env;
    // E2.3 Test Matrix: Non-lambda value
    // (letrec ((x 42)) x) → 42
    let env = default_env();
    let expr = parse("(letrec ((x 42)) x)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(42.0));
}

// ── E2.4 — Define shorthand ──

#[test]
fn eval_e2_4_define_shorthand_simple() {
    use strawman::parser::parse;
    // E2.4 Test Matrix: Simple
    // (begin (define (f x) x) (f 42)) → 42
    let env = Rc::new(Env::new());
    let expr = parse("(begin (define (f x) x) (f 42))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(42.0));
}

#[test]
fn eval_e2_4_define_shorthand_multi_param() {
    use strawman::parser::parse;
    // E2.4 Test Matrix: Multi-param
    // (begin (define (add a b) (+ a b)) (add 3 4)) → 7
    let env = make_test_env();
    let expr = parse("(begin (define (add a b) (+ a b)) (add 3 4))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(7.0));
}

#[test]
fn eval_e2_4_define_shorthand_with_body() {
    use strawman::parser::parse;
    // E2.4 Test Matrix: With body
    // (begin (define (g x) (define y 1) (+ x y)) (g 5)) → 6
    let env = make_test_env();
    let expr = parse("(begin (define (g x) (define y 1) (+ x y)) (g 5))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(6.0));
}

#[test]
fn eval_e2_4_define_shorthand_recursive() {
    use strawman::parser::parse;
    use strawman::builtins::default_env;
    // E2.4 Test Matrix: Recursive
    // (begin (define (fact n) (if (<= n 1) 1 (* n (fact (- n 1))))) (fact 5)) → 120
    let env = default_env();
    let expr = parse("(begin (define (fact n) (if (<= n 1) 1 (* n (fact (- n 1))))) (fact 5))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(120.0));
}

#[test]
fn eval_e2_4_define_shorthand_no_params() {
    use strawman::parser::parse;
    // E2.4 Test Matrix: No params
    // (begin (define (f) 42) (f)) → 42
    let env = Rc::new(Env::new());
    let expr = parse("(begin (define (f) 42) (f))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(42.0));
}

#[test]
fn eval_e2_4_define_shorthand_acceptance() {
    use strawman::parser::parse;
    use strawman::builtins::default_env;
    // E2.4 Acceptance Criterion:
    // (begin (define (square x) (* x x)) (square 9)) → 81
    let env = default_env();
    let expr = parse("(begin (define (square x) (* x x)) (square 9))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(81.0));
}

// ── E2.5 — Integration ──

#[test]
fn eval_e2_5_factorial_0() {
    use strawman::parser::parse;
    use strawman::builtins::default_env;
    // E2.5 Test Matrix: Factorial 0
    // (begin (define (fact n) (if (<= n 1) 1 (* n (fact (- n 1))))) (fact 0)) → 1
    let env = default_env();
    let expr = parse("(begin (define (fact n) (if (<= n 1) 1 (* n (fact (- n 1))))) (fact 0))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(1.0));
}

#[test]
fn eval_e2_5_factorial_10() {
    use strawman::parser::parse;
    use strawman::builtins::default_env;
    // E2.5 Test Matrix: Factorial 10
    // Define fact in the env, then call (fact 10) → 3628800
    let env = default_env();
    let define_expr = parse("(define (fact n) (if (<= n 1) 1 (* n (fact (- n 1)))))").unwrap();
    straw_eval(&define_expr, &env).unwrap();
    let expr = parse("(fact 10)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(3628800.0));
}

#[test]
fn eval_e2_5_fibonacci_10() {
    use strawman::parser::parse;
    use strawman::builtins::default_env;
    // E2.5 Test Matrix: Fibonacci
    // (begin (define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)) → 55
    let env = default_env();
    let expr = parse("(begin (define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(55.0));
}

#[test]
fn eval_e2_5_map_with_lambda() {
    use strawman::parser::parse;
    use strawman::builtins::default_env;
    // E2.5 Test Matrix: Map
    // (begin (define (map f lst) (if (null? lst) '() (cons (f (car lst)) (map f (cdr lst))))) (map (lambda (x) (* x x)) '(1 2 3 4))) → '(1 4 9 16)
    let env = default_env();
    let expr = parse("(begin (define (map f lst) (if (null? lst) (quote ()) (cons (f (car lst)) (map f (cdr lst))))) (map (lambda (x) (* x x)) (quote (1 2 3 4))))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    // Expected: (1 4 9 16) — cons onto List([]) produces List
    let expected = Value::List(vec![
        Value::Number(1.0),
        Value::Number(4.0),
        Value::Number(9.0),
        Value::Number(16.0),
    ]);
    assert_eq!(result, expected);
}

#[test]
fn eval_e2_5_closure_scope_lexical() {
    use strawman::parser::parse;
    use strawman::builtins::default_env;
    // E2.5 Test Matrix: Closure scope (lexical vs dynamic)
    // (begin (define x 10) (define (f) x) (define (g) (define x 20) (f)) (g)) → 10
    let env = default_env();
    let expr = parse("(begin (define x 10) (define (f) x) (define (g) (define x 20) (f)) (g))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(10.0));
}

#[test]
fn eval_e2_5_filter_with_lambda() {
    use strawman::parser::parse;
    use strawman::builtins::default_env;
    // E2.5 Test Matrix: Filter
    // (begin (define (filter p lst) (if (null? lst) '() (if (p (car lst)) (cons (car lst) (filter p (cdr lst))) (filter p (cdr lst))))) (filter (lambda (x) (> x 2)) '(1 2 3 4 5))) → '(3 4 5)
    let env = default_env();
    let expr = parse("(begin (define (filter p lst) (if (null? lst) (quote ()) (if (p (car lst)) (cons (car lst) (filter p (cdr lst))) (filter p (cdr lst))))) (filter (lambda (x) (> x 2)) (quote (1 2 3 4 5))))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    // Expected: (3 4 5)
    let expected = Value::List(vec![
        Value::Number(3.0),
        Value::Number(4.0),
        Value::Number(5.0),
    ]);
    assert_eq!(result, expected);
}

#[test]
fn eval_e2_5_accumulator_mutable_closure() {
    use strawman::parser::parse;
    use strawman::builtins::default_env;
    // E2.5 Test Matrix: Accumulator (mutable closure)
    // (begin (define (make-acc init) (define n init) (lambda (x) (set! n (+ n x)) n))
    //        (define a (make-acc 0)) (a 5) (a 3) (a 2)) → 10
    // Intermediate results: (a 5) → 5, (a 3) → 8, (a 2) → 10
    let env = default_env();
    let expr = parse("(begin (define (make-acc init) (define n init) (lambda (x) (set! n (+ n x)) n)) (define a (make-acc 0)) (a 5) (a 3) (a 2))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(10.0));
}

#[test]
fn eval_e2_5_accumulator_intermediate_values() {
    use strawman::parser::parse;
    use strawman::builtins::default_env;
    // Verify all intermediate accumulator values: 5, 8, 10
    let env = default_env();
    // Define make-acc and create accumulator
    let setup = parse("(begin (define (make-acc init) (define n init) (lambda (x) (set! n (+ n x)) n)) (define a (make-acc 0)))").unwrap();
    straw_eval(&setup, &env).unwrap();

    // (a 5) → 5
    let expr1 = parse("(a 5)").unwrap();
    let result1 = straw_eval(&expr1, &env).unwrap();
    assert_eq!(result1, Value::Number(5.0));

    // (a 3) → 8
    let expr2 = parse("(a 3)").unwrap();
    let result2 = straw_eval(&expr2, &env).unwrap();
    assert_eq!(result2, Value::Number(8.0));

    // (a 2) → 10
    let expr3 = parse("(a 2)").unwrap();
    let result3 = straw_eval(&expr3, &env).unwrap();
    assert_eq!(result3, Value::Number(10.0));
}

// ── E3.2 — call/cc ──

#[test]
fn eval_e3_2_callcc_normal_return() {
    use strawman::parser::parse;
    // (call/cc (lambda (k) 42)) → 42
    let env = make_test_env();
    let expr = parse("(call/cc (lambda (k) 42))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(42.0));
}

#[test]
fn eval_e3_2_callcc_early_exit() {
    use strawman::parser::parse;
    // (call/cc (lambda (k) (k 10) 20)) → 10
    let env = make_test_env();
    let expr = parse("(call/cc (lambda (k) (k 10) 20))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(10.0));
}

#[test]
fn eval_e3_2_callcc_in_expression() {
    use strawman::parser::parse;
    // (+ 1 (call/cc (lambda (k) (k 5)))) → 6
    let env = make_test_env();
    let expr = parse("(+ 1 (call/cc (lambda (k) (k 5))))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(6.0));
}

#[test]
fn eval_e3_2_callcc_saved_continuation() {
    use strawman::parser::parse;
    // (begin (define saved #f) (+ 1 (call/cc (lambda (k) (set! saved k) 10)))) → 11
    // Then (saved 20) → 21
    let env = make_test_env();
    let expr1 = parse("(begin (define saved #f) (+ 1 (call/cc (lambda (k) (set! saved k) 10))))").unwrap();
    let result1 = straw_eval(&expr1, &env).unwrap();
    assert_eq!(result1, Value::Number(11.0));

    let expr2 = parse("(saved 20)").unwrap();
    let result2 = straw_eval(&expr2, &env).unwrap();
    assert_eq!(result2, Value::Number(21.0));
}

#[test]
fn eval_e3_2_callcc_non_procedure_error() {
    use strawman::parser::parse;
    // (call/cc 42) → Error: expected procedure
    let env = make_test_env();
    let expr = parse("(call/cc 42)").unwrap();
    let result = straw_eval(&expr, &env);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.contains("expected procedure"),
        "expected 'expected procedure' error, got: {err}"
    );
}

#[test]
fn eval_e3_2_callcc_acceptance_abandon_computation() {
    use strawman::parser::parse;
    // Acceptance Criterion 1:
    // (+ 1 (call/cc (lambda (k) (+ 2 (k 3))))) → 4
    // The (+ 2 ...) is abandoned when k is invoked
    let env = make_test_env();
    let expr = parse("(+ 1 (call/cc (lambda (k) (+ 2 (k 3)))))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(4.0));
}

#[test]
fn eval_e3_2_callcc_acceptance_saved_resume() {
    use strawman::parser::parse;
    // Acceptance Criterion 2:
    // A saved continuation called later resumes the computation as if call/cc returned that value
    let env = make_test_env();
    let setup = parse("(begin (define saved #f) (+ 1 (call/cc (lambda (k) (set! saved k) 10))))").unwrap();
    let result1 = straw_eval(&setup, &env).unwrap();
    assert_eq!(result1, Value::Number(11.0));

    // (saved 100) → 101
    let expr = parse("(saved 100)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(101.0));
}

// ── E3.1 — CPS evaluator: identity continuation ──

#[test]
fn eval_e3_1_identity_continuation() {
    use strawman::parser::parse;
    use strawman::eval::straw_eval_k;
    // E3.1 Test Matrix: Identity continuation
    // (+ 1 2) with identity k → 3
    let env = make_test_env();
    let expr = parse("(+ 1 2)").unwrap();
    let result = straw_eval_k(&expr, &env, &|v| Ok(v)).unwrap();
    assert_eq!(result, Value::Number(3.0));
}

// ── E3.3 — catch/throw ──

#[test]
fn eval_e3_3_catch_no_throw() {
    use strawman::parser::parse;
    // (catch 'x 42) → 42
    let env = make_test_env();
    let expr = parse("(catch (quote x) 42)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(42.0));
}

#[test]
fn eval_e3_3_catch_simple_throw() {
    use strawman::parser::parse;
    // (catch 'x (begin (throw 'x 10) 20)) → 10
    let env = make_test_env();
    let expr = parse("(catch (quote x) (begin (throw (quote x) 10) 20))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(10.0));
}

#[test]
fn eval_e3_3_catch_nested() {
    use strawman::parser::parse;
    // (catch 'a (catch 'b (throw 'a 1))) → 1
    let env = make_test_env();
    let expr = parse("(catch (quote a) (catch (quote b) (throw (quote a) 1)))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(1.0));
}

#[test]
fn eval_e3_3_catch_wrong_tag() {
    use strawman::parser::parse;
    // (catch 'a (throw 'b 1)) → Error: no matching catch
    let env = make_test_env();
    let expr = parse("(catch (quote a) (throw (quote b) 1))").unwrap();
    let result = straw_eval(&expr, &env);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.contains("no matching catch"),
        "expected 'no matching catch' error, got: {err}"
    );
}

#[test]
fn eval_e3_3_catch_throw_in_function() {
    use strawman::parser::parse;
    // (catch 'x ((lambda () (throw 'x 99)))) → 99
    let env = make_test_env();
    let expr = parse("(catch (quote x) ((lambda () (throw (quote x) 99))))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(99.0));
}

#[test]
fn eval_e3_3_catch_acceptance_criterion() {
    use strawman::parser::parse;
    // Acceptance Criterion:
    // (catch 'err (begin (throw 'err "oops") "unreachable")) → "oops"
    let env = make_test_env();
    let expr = parse("(catch (quote err) (begin (throw (quote err) \"oops\") \"unreachable\"))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::StringLit("oops".to_string()));
}

// ── E3.4 — block / return-from ──

#[test]
fn eval_e3_4_block_no_return() {
    use strawman::parser::parse;
    // (block b 42) → 42
    let env = make_test_env();
    let expr = parse("(block b 42)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(42.0));
}

#[test]
fn eval_e3_4_block_early_return() {
    use strawman::parser::parse;
    // (block b (return-from b 10) 20) → 10
    let env = make_test_env();
    let expr = parse("(block b (return-from b 10) 20)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(10.0));
}

#[test]
fn eval_e3_4_block_nested_return_outer() {
    use strawman::parser::parse;
    // (block a (block b (return-from a 1))) → 1
    let env = make_test_env();
    let expr = parse("(block a (block b (return-from a 1)))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(1.0));
}

#[test]
fn eval_e3_4_block_return_from_inner() {
    use strawman::parser::parse;
    // (block a (block b (return-from b 2)) 3) → 3
    // return-from b exits block b with 2, then block a continues to evaluate 3
    let env = make_test_env();
    let expr = parse("(block a (block b (return-from b 2)) 3)").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(3.0));
}

#[test]
fn eval_e3_4_return_from_unknown_block() {
    use strawman::parser::parse;
    // (return-from z 1) → Error
    let env = make_test_env();
    let expr = parse("(return-from z 1)").unwrap();
    let result = straw_eval(&expr, &env);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.contains("unknown block"),
        "expected 'unknown block' error, got: {err}"
    );
}

#[test]
fn eval_e3_4_block_acceptance_criterion() {
    use strawman::parser::parse;
    use strawman::builtins::default_env;
    // Acceptance Criterion:
    // (block done (return-from done 42) (error "unreachable")) → 42
    let env = default_env();
    let expr = parse("(block done (return-from done 42) (error \"unreachable\"))").unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    assert_eq!(result, Value::Number(42.0));
}

// ── E3.5 — unwind-protect ──

#[test]
fn eval_e3_5_unwind_protect_normal() {
    use strawman::parser::parse;
    use strawman::builtins::default_env;
    use std::io::Read;
    // (unwind-protect 42 (display "clean")) → 42, stdout: "clean"
    let env = default_env();
    let expr = parse(r#"(unwind-protect 42 (display "clean"))"#).unwrap();

    let mut buf = gag::BufferRedirect::stdout().unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    let mut output = String::new();
    buf.read_to_string(&mut output).unwrap();
    drop(buf);

    assert_eq!(result, Value::Number(42.0));
    assert_eq!(output, "clean");
}

#[test]
fn eval_e3_5_unwind_protect_with_throw() {
    use strawman::parser::parse;
    use strawman::builtins::default_env;
    use std::io::Read;
    // (catch 'x (unwind-protect (throw 'x 1) (display "clean"))) → 1, stdout: "clean"
    let env = default_env();
    let expr = parse(r#"(catch (quote x) (unwind-protect (throw (quote x) 1) (display "clean")))"#).unwrap();

    let mut buf = gag::BufferRedirect::stdout().unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    let mut output = String::new();
    buf.read_to_string(&mut output).unwrap();
    drop(buf);

    assert_eq!(result, Value::Number(1.0));
    assert_eq!(output, "clean");
}

#[test]
fn eval_e3_5_unwind_protect_cleanup_order() {
    use strawman::parser::parse;
    use strawman::builtins::default_env;
    use std::io::Read;
    // (unwind-protect 10 (display "a") (display "b")) → 10, stdout: "ab"
    let env = default_env();
    let expr = parse(r#"(unwind-protect 10 (display "a") (display "b"))"#).unwrap();

    let mut buf = gag::BufferRedirect::stdout().unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    let mut output = String::new();
    buf.read_to_string(&mut output).unwrap();
    drop(buf);

    assert_eq!(result, Value::Number(10.0));
    assert_eq!(output, "ab");
}

#[test]
fn eval_e3_5_unwind_protect_acceptance() {
    use strawman::parser::parse;
    use strawman::builtins::default_env;
    use std::io::Read;
    // Acceptance Criterion:
    // (catch 'e (unwind-protect (throw 'e "err") (display "cleaned")))
    // → "err", stdout: "cleaned"
    let env = default_env();
    let expr = parse(r#"(catch (quote e) (unwind-protect (throw (quote e) "err") (display "cleaned")))"#).unwrap();

    let mut buf = gag::BufferRedirect::stdout().unwrap();
    let result = straw_eval(&expr, &env).unwrap();
    let mut output = String::new();
    buf.read_to_string(&mut output).unwrap();
    drop(buf);

    assert_eq!(result, Value::StringLit("err".to_string()));
    assert_eq!(output, "cleaned");
}
