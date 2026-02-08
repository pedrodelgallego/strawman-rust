use std::rc::Rc;
use strawman::builtins::default_env;
use strawman::env::{Env, Value};
use strawman::eval::straw_eval;
use strawman::fast_eval::fast_eval;
use strawman::parser::{parse, parse_all};
use strawman::pretreat::pretreat;

/// Helper: evaluate an expression with both straw_eval and fast_eval,
/// assert they produce the same result.
fn eval_both(input: &str, env: &Rc<Env>) -> Value {
    let expr = parse(input).unwrap();
    let expected = straw_eval(&expr, env).unwrap();
    let treated = pretreat(&expr);
    let actual = fast_eval(&treated, env).unwrap();
    assert_eq!(
        format!("{:?}", expected),
        format!("{:?}", actual),
        "Mismatch for input: {}",
        input
    );
    actual
}

/// Helper: evaluate an expression expecting an error from both evaluators.
fn eval_both_err(input: &str, env: &Rc<Env>) -> String {
    let expr = parse(input).unwrap();
    let expected_err = straw_eval(&expr, env).unwrap_err();
    let treated = pretreat(&expr);
    let actual_err = fast_eval(&treated, env).unwrap_err();
    assert_eq!(
        expected_err, actual_err,
        "Error mismatch for input: {}",
        input
    );
    actual_err
}

fn make_test_env() -> Rc<Env> {
    let env = Rc::new(Env::new());
    env.set(
        "+",
        Value::Builtin("+".to_string(), |args| {
            let mut sum = 0.0;
            for arg in &args {
                match arg {
                    Value::Number(n) => sum += n,
                    _ => return Err("expected number".to_string()),
                }
            }
            Ok(Value::Number(sum))
        }),
    );
    env.set(
        "-",
        Value::Builtin("-".to_string(), |args| {
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
        }),
    );
    env.set(
        "*",
        Value::Builtin("*".to_string(), |args| {
            let mut product = 1.0;
            for arg in &args {
                match arg {
                    Value::Number(n) => product *= n,
                    _ => return Err("expected number".to_string()),
                }
            }
            Ok(Value::Number(product))
        }),
    );
    env
}

// === Self-evaluating forms ===

#[test]
fn fast_eval_number() {
    let env = make_test_env();
    let val = eval_both("42", &env);
    assert_eq!(val, Value::Number(42.0));
}

#[test]
fn fast_eval_float() {
    let env = make_test_env();
    eval_both("3.14", &env);
}

#[test]
fn fast_eval_string() {
    let env = make_test_env();
    eval_both("\"hello\"", &env);
}

#[test]
fn fast_eval_boolean() {
    let env = make_test_env();
    eval_both("#t", &env);
    eval_both("#f", &env);
}

// === Symbol lookup ===

#[test]
fn fast_eval_symbol_lookup() {
    let env = make_test_env();
    env.set("x", Value::Number(10.0));
    eval_both("x", &env);
}

#[test]
fn fast_eval_unbound_symbol_error() {
    let env = make_test_env();
    eval_both_err("unknown", &env);
}

// === Quote ===

#[test]
fn fast_eval_quote_symbol() {
    let env = make_test_env();
    eval_both("(quote foo)", &env);
}

#[test]
fn fast_eval_quote_list() {
    let env = make_test_env();
    eval_both("(quote (1 2 3))", &env);
}

// === If ===

#[test]
fn fast_eval_if_true_branch() {
    let env = make_test_env();
    eval_both("(if #t 1 2)", &env);
}

#[test]
fn fast_eval_if_false_branch() {
    let env = make_test_env();
    eval_both("(if #f 1 2)", &env);
}

#[test]
fn fast_eval_if_no_alternative() {
    let env = make_test_env();
    eval_both("(if #f 1)", &env);
}

// === Begin ===

#[test]
fn fast_eval_begin() {
    let env = make_test_env();
    eval_both("(begin 1 2 3)", &env);
}

#[test]
fn fast_eval_begin_empty() {
    let env = make_test_env();
    eval_both("(begin)", &env);
}

// === Define & Set! ===

#[test]
fn fast_eval_define_and_lookup() {
    let env = make_test_env();
    let expr1 = parse("(define x 42)").unwrap();
    straw_eval(&expr1, &env).unwrap();
    let treated1 = pretreat(&expr1);
    // Use a fresh env for fast_eval to avoid contamination
    let env2 = make_test_env();
    fast_eval(&treated1, &env2).unwrap();
    // Both envs should now have x = 42
    let expr2 = parse("x").unwrap();
    let treated2 = pretreat(&expr2);
    let v1 = straw_eval(&expr2, &env).unwrap();
    let v2 = fast_eval(&treated2, &env2).unwrap();
    assert_eq!(format!("{:?}", v1), format!("{:?}", v2));
}

#[test]
fn fast_eval_set_bang() {
    let env = make_test_env();
    env.set("x", Value::Number(1.0));
    let env2 = make_test_env();
    env2.set("x", Value::Number(1.0));

    let set_expr = parse("(set! x 99)").unwrap();
    straw_eval(&set_expr, &env).unwrap();
    let treated = pretreat(&set_expr);
    fast_eval(&treated, &env2).unwrap();

    let lookup = parse("x").unwrap();
    let treated_lookup = pretreat(&lookup);
    let v1 = straw_eval(&lookup, &env).unwrap();
    let v2 = fast_eval(&treated_lookup, &env2).unwrap();
    assert_eq!(format!("{:?}", v1), format!("{:?}", v2));
}

// === Lambda & application ===

#[test]
fn fast_eval_lambda_call() {
    let env = make_test_env();
    eval_both("((lambda (x) x) 42)", &env);
}

#[test]
fn fast_eval_lambda_closure() {
    let env = make_test_env();
    eval_both("((lambda (x) (+ x 1)) 10)", &env);
}

#[test]
fn fast_eval_higher_order() {
    let env = make_test_env();
    eval_both("(((lambda (x) (lambda (y) (+ x y))) 3) 4)", &env);
}

// === Let / Let* / Letrec ===

#[test]
fn fast_eval_let() {
    let env = make_test_env();
    eval_both("(let ((x 1) (y 2)) (+ x y))", &env);
}

#[test]
fn fast_eval_let_star() {
    let env = make_test_env();
    eval_both("(let* ((x 1) (y (+ x 1))) (+ x y))", &env);
}

#[test]
fn fast_eval_letrec() {
    let env = make_test_env();
    eval_both(
        "(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))",
        &default_env(),
    );
}

// === And / Or ===

#[test]
fn fast_eval_and() {
    let env = make_test_env();
    eval_both("(and #t #t)", &env);
    eval_both("(and #t #f)", &env);
    eval_both("(and)", &env);
}

#[test]
fn fast_eval_or() {
    let env = make_test_env();
    eval_both("(or #f #t)", &env);
    eval_both("(or #f #f)", &env);
    eval_both("(or)", &env);
}

// === Builtins ===

#[test]
fn fast_eval_arithmetic() {
    let env = default_env();
    eval_both("(+ 1 2 3)", &env);
    eval_both("(- 10 3)", &env);
    eval_both("(* 3 4)", &env);
    eval_both("(/ 10 2)", &env);
}

#[test]
fn fast_eval_comparison() {
    let env = default_env();
    eval_both("(< 1 2)", &env);
    eval_both("(> 2 1)", &env);
    eval_both("(= 5 5)", &env);
}

#[test]
fn fast_eval_list_ops() {
    let env = default_env();
    eval_both("(cons 1 (list 2 3))", &env);
    eval_both("(car (list 1 2 3))", &env);
    eval_both("(cdr (list 1 2 3))", &env);
    eval_both("(null? (list))", &env);
    eval_both("(pair? (cons 1 2))", &env);
}

// === Catch / Throw ===

#[test]
fn fast_eval_catch_throw() {
    let env = default_env();
    eval_both("(catch (quote done) (+ 1 2))", &env);
    eval_both("(catch (quote done) (throw (quote done) 42))", &env);
}

// === Block / Return-from ===

#[test]
fn fast_eval_block_return_from() {
    let env = default_env();
    eval_both("(block myblock (+ 1 2))", &env);
    eval_both("(block myblock (return-from myblock 99) 0)", &env);
}

// === Call/cc ===

#[test]
fn fast_eval_call_cc_normal() {
    let env = default_env();
    eval_both("(call/cc (lambda (k) 42))", &env);
}

#[test]
fn fast_eval_call_cc_escape() {
    let env = default_env();
    eval_both("(+ 1 (call/cc (lambda (k) (+ 2 (k 10)))))", &env);
}

// === Define shorthand ===

#[test]
fn fast_eval_define_shorthand() {
    let env = default_env();
    let exprs = parse_all("(define (square x) (* x x)) (square 5)").unwrap();
    // straw_eval both
    let env1 = default_env();
    for expr in &exprs[..exprs.len() - 1] {
        straw_eval(expr, &env1).unwrap();
    }
    let expected = straw_eval(&exprs[exprs.len() - 1], &env1).unwrap();
    // fast_eval both
    let env2 = default_env();
    for expr in &exprs[..exprs.len() - 1] {
        let treated = pretreat(expr);
        fast_eval(&treated, &env2).unwrap();
    }
    let treated_last = pretreat(&exprs[exprs.len() - 1]);
    let actual = fast_eval(&treated_last, &env2).unwrap();
    assert_eq!(format!("{:?}", expected), format!("{:?}", actual));
}

// === Vectors ===

#[test]
fn fast_eval_vectors() {
    let env = default_env();
    eval_both("(vector-length (make-vector 3 0))", &env);
    eval_both("(vector-ref (make-vector 3 0) 1)", &env);
}

// === Set-car! / Set-cdr! ===

#[test]
fn fast_eval_set_car_cdr() {
    let env1 = default_env();
    let env2 = default_env();
    let program = "(begin (define p (cons 1 2)) (set-car! p 10) (car p))";
    let exprs = parse(program).unwrap();
    let expected = straw_eval(&exprs, &env1).unwrap();
    let treated = pretreat(&exprs);
    let actual = fast_eval(&treated, &env2).unwrap();
    assert_eq!(format!("{:?}", expected), format!("{:?}", actual));
}

// === Unwind-protect ===

#[test]
fn fast_eval_unwind_protect() {
    let env = default_env();
    eval_both("(unwind-protect (+ 1 2) (+ 3 4))", &env);
}

// === Integration: factorial ===

#[test]
fn fast_eval_factorial() {
    // CPS eval_both for factorial(10) needs extra stack in debug mode
    let builder = std::thread::Builder::new().stack_size(8 * 1024 * 1024);
    let handler = builder.spawn(|| {
        let env = default_env();
        let program = "(begin (define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 10))";
        eval_both(program, &env);
    }).unwrap();
    handler.join().unwrap();
}

// === Integration: fibonacci ===

#[test]
fn fast_eval_fibonacci() {
    let env = default_env();
    let program = "(begin (define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10))";
    eval_both(program, &env);
}

// === Error cases produce identical errors ===

#[test]
fn fast_eval_division_by_zero() {
    let env = default_env();
    eval_both_err("(/ 1 0)", &env);
}

#[test]
fn fast_eval_not_a_procedure() {
    let env = default_env();
    eval_both_err("(42)", &env);
}

#[test]
fn fast_eval_arity_mismatch() {
    let env = default_env();
    eval_both_err("((lambda (x) x) 1 2)", &env);
}

// === Type predicates ===

#[test]
fn fast_eval_type_predicates() {
    let env = default_env();
    eval_both("(number? 42)", &env);
    eval_both("(string? \"hello\")", &env);
    eval_both("(boolean? #t)", &env);
    eval_both("(symbol? (quote foo))", &env);
    eval_both("(procedure? +)", &env);
    eval_both("(null? (list))", &env);
}

// === Not ===

#[test]
fn fast_eval_not() {
    let env = default_env();
    eval_both("(not #f)", &env);
    eval_both("(not #t)", &env);
    eval_both("(not 0)", &env);
}

// === Equal? ===

#[test]
fn fast_eval_equal() {
    let env = default_env();
    eval_both("(equal? (list 1 2 3) (list 1 2 3))", &env);
    eval_both("(equal? 1 1)", &env);
    eval_both("(equal? 1 2)", &env);
}

// === E6.2: Lexical addressing ===

use strawman::pretreat::TreatedExpr;

/// Helper: parse and pretreat an expression, return the TreatedExpr.
fn pretreat_input(input: &str) -> TreatedExpr {
    let expr = parse(input).unwrap();
    pretreat(&expr)
}

#[test]
fn lexical_addressing_local_var() {
    // (lambda (x) x) — x should be LexicalRef at depth 0, offset 0
    let treated = pretreat_input("(lambda (x) x)");
    match treated {
        TreatedExpr::Lambda(params, body) => {
            assert_eq!(params, vec!["x".to_string()]);
            assert_eq!(body.len(), 1);
            match &body[0] {
                TreatedExpr::LexicalRef(depth, offset) => {
                    assert_eq!(*depth, 0, "expected depth 0 for local var x");
                    assert_eq!(*offset, 0, "expected offset 0 for first param x");
                }
                other => panic!("expected LexicalRef, got {:?}", other),
            }
        }
        other => panic!("expected Lambda, got {:?}", other),
    }
}

#[test]
fn lexical_addressing_free_var_one_level() {
    // (lambda (x) (lambda (y) x)) — inner x should be LexicalRef at depth 1, offset 0
    let treated = pretreat_input("(lambda (x) (lambda (y) x))");
    match treated {
        TreatedExpr::Lambda(params, body) => {
            assert_eq!(params, vec!["x".to_string()]);
            assert_eq!(body.len(), 1);
            match &body[0] {
                TreatedExpr::Lambda(inner_params, inner_body) => {
                    assert_eq!(inner_params, &vec!["y".to_string()]);
                    assert_eq!(inner_body.len(), 1);
                    match &inner_body[0] {
                        TreatedExpr::LexicalRef(depth, offset) => {
                            assert_eq!(*depth, 1, "expected depth 1 for free var x");
                            assert_eq!(*offset, 0, "expected offset 0 for x in outer scope");
                        }
                        other => panic!("expected LexicalRef for x, got {:?}", other),
                    }
                }
                other => panic!("expected inner Lambda, got {:?}", other),
            }
        }
        other => panic!("expected Lambda, got {:?}", other),
    }
}

#[test]
fn lexical_addressing_multiple_params() {
    // (lambda (a b c) b) — b should be LexicalRef at depth 0, offset 1
    let treated = pretreat_input("(lambda (a b c) b)");
    match treated {
        TreatedExpr::Lambda(params, body) => {
            assert_eq!(params, vec!["a".to_string(), "b".to_string(), "c".to_string()]);
            assert_eq!(body.len(), 1);
            match &body[0] {
                TreatedExpr::LexicalRef(depth, offset) => {
                    assert_eq!(*depth, 0, "expected depth 0 for local var b");
                    assert_eq!(*offset, 1, "expected offset 1 for second param b");
                }
                other => panic!("expected LexicalRef for b, got {:?}", other),
            }
        }
        other => panic!("expected Lambda, got {:?}", other),
    }
}

#[test]
fn lexical_addressing_acceptance_criteria() {
    // (lambda (x) (lambda (y) (+ x y)))
    // x in inner body: depth=1, offset=0
    // y in inner body: depth=0, offset=0
    let treated = pretreat_input("(lambda (x) (lambda (y) (+ x y)))");
    match treated {
        TreatedExpr::Lambda(_, body) => {
            match &body[0] {
                TreatedExpr::Lambda(_, inner_body) => {
                    // inner body is (+ x y) which is an Application
                    match &inner_body[0] {
                        TreatedExpr::Application(func, args) => {
                            // func should be VarRef("+") since + is a global/builtin
                            match func.as_ref() {
                                TreatedExpr::VarRef(name) => assert_eq!(name, "+"),
                                other => panic!("expected VarRef for +, got {:?}", other),
                            }
                            assert_eq!(args.len(), 2);
                            // x at depth 1, offset 0
                            match &args[0] {
                                TreatedExpr::LexicalRef(depth, offset) => {
                                    assert_eq!(*depth, 1, "x should have depth 1");
                                    assert_eq!(*offset, 0, "x should have offset 0");
                                }
                                other => panic!("expected LexicalRef for x, got {:?}", other),
                            }
                            // y at depth 0, offset 0
                            match &args[1] {
                                TreatedExpr::LexicalRef(depth, offset) => {
                                    assert_eq!(*depth, 0, "y should have depth 0");
                                    assert_eq!(*offset, 0, "y should have offset 0");
                                }
                                other => panic!("expected LexicalRef for y, got {:?}", other),
                            }
                        }
                        other => panic!("expected Application for (+ x y), got {:?}", other),
                    }
                }
                other => panic!("expected inner Lambda, got {:?}", other),
            }
        }
        other => panic!("expected Lambda, got {:?}", other),
    }
}

#[test]
fn lexical_addressing_eval_local_var() {
    // End-to-end: ((lambda (x) x) 42) should still evaluate to 42
    let env = make_test_env();
    eval_both("((lambda (x) x) 42)", &env);
}

#[test]
fn lexical_addressing_eval_free_var_one_level() {
    // End-to-end: (((lambda (x) (lambda (y) x)) 10) 20) should evaluate to 10
    let env = make_test_env();
    eval_both("(((lambda (x) (lambda (y) x)) 10) 20)", &env);
}

#[test]
fn lexical_addressing_eval_multiple_params() {
    // End-to-end: ((lambda (a b c) b) 1 2 3) should evaluate to 2
    let env = make_test_env();
    eval_both("((lambda (a b c) b) 1 2 3)", &env);
}

#[test]
fn lexical_addressing_eval_acceptance() {
    // End-to-end: (((lambda (x) (lambda (y) (+ x y))) 3) 4) should evaluate to 7
    let env = make_test_env();
    eval_both("(((lambda (x) (lambda (y) (+ x y))) 3) 4)", &env);
}
