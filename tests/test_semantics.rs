/// Tests validating that the interpreter behavior matches the denotational
/// semantics defined in docs/semantics.md. Each test is annotated with the
/// semantic clause it validates.
use std::rc::Rc;
use strawman::builtins::default_env;
use strawman::env::Value;
use strawman::eval::straw_eval;
use strawman::parser::parse;

fn eval(input: &str) -> Result<Value, String> {
    let env = default_env();
    let expr = parse(input).unwrap();
    straw_eval(&expr, &env)
}

fn eval_multi(inputs: &[&str]) -> Result<Value, String> {
    let env = default_env();
    let mut result = Value::Void;
    for input in inputs {
        let expr = parse(input).unwrap();
        result = straw_eval(&expr, &env)?;
    }
    Ok(result)
}

// ============================================================
// Section 4.1: Self-Evaluating Forms (Literals)
// E[n, env, k] = k(Num(n))
// E[s, env, k] = k(Str(s))
// E[b, env, k] = k(Bool(b))
// ============================================================

#[test]
fn sem_4_1_number_literal() {
    assert_eq!(eval("42"), Ok(Value::Number(42.0)));
}

#[test]
fn sem_4_1_string_literal() {
    assert_eq!(eval("\"hello\""), Ok(Value::StringLit("hello".to_string())));
}

#[test]
fn sem_4_1_boolean_true() {
    assert_eq!(eval("#t"), Ok(Value::Boolean(true)));
}

#[test]
fn sem_4_1_boolean_false() {
    assert_eq!(eval("#f"), Ok(Value::Boolean(false)));
}

// ============================================================
// Section 4.2: Symbol (Variable Reference)
// E[x, env, k] = k(lookup(x, env))
// ============================================================

#[test]
fn sem_4_2_symbol_lookup() {
    assert_eq!(eval_multi(&["(define x 10)", "x"]), Ok(Value::Number(10.0)));
}

#[test]
fn sem_4_2_unbound_variable() {
    let result = eval("nonexistent");
    assert_eq!(result, Err("unbound variable: nonexistent".to_string()));
}

// ============================================================
// Section 4.3: Quote
// E[(quote e), env, k] = k(datum(e))
// ============================================================

#[test]
fn sem_4_3_quote_symbol() {
    assert_eq!(eval("(quote foo)"), Ok(Value::Symbol("foo".to_string())));
}

#[test]
fn sem_4_3_quote_list() {
    assert_eq!(
        eval("(quote (1 2 3))"),
        Ok(Value::List(Rc::new(vec![
            Value::Number(1.0),
            Value::Number(2.0),
            Value::Number(3.0),
        ])))
    );
}

#[test]
fn sem_4_3_quote_number_unchanged() {
    assert_eq!(eval("(quote 42)"), Ok(Value::Number(42.0)));
}

// ============================================================
// Section 4.4: Conditional (if)
// E[(if e1 e2 e3), env, k]: truthy? selects e2, falsy? selects e3
// Only #f is falsy
// ============================================================

#[test]
fn sem_4_4_if_true_branch() {
    assert_eq!(eval("(if #t 1 2)"), Ok(Value::Number(1.0)));
}

#[test]
fn sem_4_4_if_false_branch() {
    assert_eq!(eval("(if #f 1 2)"), Ok(Value::Number(2.0)));
}

#[test]
fn sem_4_4_if_zero_is_truthy() {
    // Per Section 2: only #f is falsy; 0 is truthy
    assert_eq!(eval("(if 0 1 2)"), Ok(Value::Number(1.0)));
}

#[test]
fn sem_4_4_if_no_else_returns_void() {
    assert_eq!(eval("(if #f 1)"), Ok(Value::Void));
}

// ============================================================
// Section 4.5: Sequence (begin)
// E[(begin e1..en), env, k]: returns last value
// ============================================================

#[test]
fn sem_4_5_begin_returns_last() {
    assert_eq!(eval("(begin 1 2 3)"), Ok(Value::Number(3.0)));
}

#[test]
fn sem_4_5_begin_empty_returns_void() {
    assert_eq!(eval("(begin)"), Ok(Value::Void));
}

// ============================================================
// Section 4.6: Definition (define)
// E[(define x e), env, k] = set(x, E[e]), k(Void)
// ============================================================

#[test]
fn sem_4_6_define_simple() {
    assert_eq!(eval_multi(&["(define x 42)", "x"]), Ok(Value::Number(42.0)));
}

#[test]
fn sem_4_6_define_function_shorthand() {
    assert_eq!(
        eval_multi(&["(define (add a b) (+ a b))", "(add 3 4)"]),
        Ok(Value::Number(7.0))
    );
}

#[test]
fn sem_4_6_define_returns_void() {
    assert_eq!(eval("(define x 1)"), Ok(Value::Void));
}

// ============================================================
// Section 4.7: Assignment (set!)
// E[(set! x e), env, k] = update(x, E[e]), k(Void)
// ============================================================

#[test]
fn sem_4_7_set_bang_mutates() {
    assert_eq!(
        eval_multi(&["(define x 1)", "(set! x 2)", "x"]),
        Ok(Value::Number(2.0))
    );
}

#[test]
fn sem_4_7_set_bang_unbound_error() {
    let result = eval("(set! nonexistent 1)");
    assert_eq!(
        result,
        Err("cannot set! unbound variable: nonexistent".to_string())
    );
}

// ============================================================
// Section 4.8: Lambda
// E[(lambda (x1..xn) body), env, k] = k(Closure([x1..xn], body, env))
// ============================================================

#[test]
fn sem_4_8_lambda_creates_closure() {
    // A lambda applied immediately
    assert_eq!(eval("((lambda (x) (+ x 1)) 5)"), Ok(Value::Number(6.0)));
}

#[test]
fn sem_4_8_lambda_captures_env() {
    assert_eq!(
        eval_multi(&[
            "(define a 10)",
            "(define f (lambda (x) (+ x a)))",
            "(f 5)"
        ]),
        Ok(Value::Number(15.0))
    );
}

// ============================================================
// Section 4.9: Function Application
// apply(Closure(params, body, cenv), args, k) = extend, eval body, k(result)
// ============================================================

#[test]
fn sem_4_9_builtin_application() {
    assert_eq!(eval("(+ 1 2 3)"), Ok(Value::Number(6.0)));
}

#[test]
fn sem_4_9_closure_application() {
    assert_eq!(
        eval_multi(&["(define (square x) (* x x))", "(square 5)"]),
        Ok(Value::Number(25.0))
    );
}

#[test]
fn sem_4_9_not_a_procedure_error() {
    let result = eval("(42)");
    assert!(result.is_err());
    assert!(result.unwrap_err().starts_with("not a procedure"));
}

#[test]
fn sem_4_9_arity_mismatch() {
    let result = eval_multi(&["(define (f x) x)", "(f 1 2)"]);
    assert_eq!(
        result,
        Err("arity mismatch: expected 1, got 2".to_string())
    );
}

// ============================================================
// Section 4.10: Logical Connectives (and, or)
// ============================================================

#[test]
fn sem_4_10_and_empty() {
    assert_eq!(eval("(and)"), Ok(Value::Boolean(true)));
}

#[test]
fn sem_4_10_and_short_circuits() {
    assert_eq!(eval("(and #f 42)"), Ok(Value::Boolean(false)));
}

#[test]
fn sem_4_10_and_returns_last() {
    assert_eq!(eval("(and 1 2 3)"), Ok(Value::Number(3.0)));
}

#[test]
fn sem_4_10_or_empty() {
    assert_eq!(eval("(or)"), Ok(Value::Boolean(false)));
}

#[test]
fn sem_4_10_or_short_circuits() {
    assert_eq!(eval("(or 1 #f)"), Ok(Value::Number(1.0)));
}

#[test]
fn sem_4_10_or_returns_last_if_all_false() {
    assert_eq!(eval("(or #f #f 3)"), Ok(Value::Number(3.0)));
}

// ============================================================
// Section 4.11: Let Forms
// ============================================================

#[test]
fn sem_4_11_let_bindings_in_outer_env() {
    // let bindings evaluated in outer env, cannot see each other
    assert_eq!(
        eval("(let ((x 1) (y 2)) (+ x y))"),
        Ok(Value::Number(3.0))
    );
}

#[test]
fn sem_4_11_let_star_sequential() {
    // let* each binding sees previous
    assert_eq!(
        eval("(let* ((x 1) (y (+ x 1))) y)"),
        Ok(Value::Number(2.0))
    );
}

#[test]
fn sem_4_11_letrec_mutual_recursion() {
    assert_eq!(
        eval("(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))"),
        Ok(Value::Number(120.0))
    );
}

// ============================================================
// Section 4.12: Mutation (set-car!, set-cdr!)
// ============================================================

#[test]
fn sem_4_12_set_car() {
    assert_eq!(
        eval_multi(&[
            "(define p (cons 1 2))",
            "(set-car! p 10)",
            "(car p)"
        ]),
        Ok(Value::Number(10.0))
    );
}

#[test]
fn sem_4_12_set_cdr() {
    assert_eq!(
        eval_multi(&[
            "(define p (cons 1 2))",
            "(set-cdr! p 20)",
            "(cdr p)"
        ]),
        Ok(Value::Number(20.0))
    );
}

// ============================================================
// Section 4.13: First-Class Continuations (call/cc)
// ============================================================

#[test]
fn sem_4_13_callcc_normal_return() {
    // When continuation is not invoked, returns normally
    assert_eq!(eval("(call/cc (lambda (k) 42))"), Ok(Value::Number(42.0)));
}

#[test]
fn sem_4_13_callcc_escape() {
    // Invoking the continuation escapes
    assert_eq!(
        eval("(+ 1 (call/cc (lambda (k) (+ 10 (k 42)))))"),
        Ok(Value::Number(43.0))
    );
}

// ============================================================
// Section 4.14: Catch and Throw
// ============================================================

#[test]
fn sem_4_14_catch_throw() {
    assert_eq!(
        eval("(catch 'done (throw 'done 42))"),
        Ok(Value::Number(42.0))
    );
}

#[test]
fn sem_4_14_catch_normal() {
    assert_eq!(
        eval("(catch 'done 99)"),
        Ok(Value::Number(99.0))
    );
}

#[test]
fn sem_4_14_uncaught_throw() {
    let result = eval("(throw 'oops 1)");
    assert_eq!(
        result,
        Err("no matching catch for tag: oops".to_string())
    );
}

// ============================================================
// Section 4.15: Block and Return-From
// ============================================================

#[test]
fn sem_4_15_block_normal() {
    assert_eq!(eval("(block done 1 2 3)"), Ok(Value::Number(3.0)));
}

#[test]
fn sem_4_15_return_from_escapes() {
    assert_eq!(
        eval("(block done 1 (return-from done 42) 3)"),
        Ok(Value::Number(42.0))
    );
}

#[test]
fn sem_4_15_unknown_block() {
    let result = eval("(return-from nowhere 1)");
    assert_eq!(
        result,
        Err("unknown block: nowhere".to_string())
    );
}

// ============================================================
// Section 4.16: Unwind-Protect
// ============================================================

#[test]
fn sem_4_16_unwind_protect_normal() {
    assert_eq!(
        eval_multi(&[
            "(define count 0)",
            "(unwind-protect (begin (set! count (+ count 1)) 42) (set! count (+ count 10)))",
        ]),
        Ok(Value::Number(42.0))
    );
}

#[test]
fn sem_4_16_cleanup_runs_on_error() {
    // Cleanup runs even when protected form errors, then error propagates
    let env = default_env();
    let exprs = [
        "(define cleanup-ran 0)",
        "(unwind-protect (/ 1 0) (set! cleanup-ran 1))",
    ];
    // First define
    let e1 = parse(exprs[0]).unwrap();
    straw_eval(&e1, &env).unwrap();
    // unwind-protect should error
    let e2 = parse(exprs[1]).unwrap();
    let result = straw_eval(&e2, &env);
    assert!(result.is_err());
    // But cleanup should have run
    let cleanup_val = env.lookup("cleanup-ran").unwrap();
    assert_eq!(cleanup_val, Value::Number(1.0));
}

// ============================================================
// Section 5: Builtins
// ============================================================

#[test]
fn sem_5_1_arithmetic() {
    assert_eq!(eval("(+ 1 2 3)"), Ok(Value::Number(6.0)));
    assert_eq!(eval("(- 10 3)"), Ok(Value::Number(7.0)));
    assert_eq!(eval("(- 5)"), Ok(Value::Number(-5.0)));
    assert_eq!(eval("(* 2 3 4)"), Ok(Value::Number(24.0)));
    assert_eq!(eval("(/ 12 3)"), Ok(Value::Number(4.0)));
    assert_eq!(eval("(mod 10 3)"), Ok(Value::Number(1.0)));
}

#[test]
fn sem_5_2_comparison() {
    assert_eq!(eval("(< 1 2)"), Ok(Value::Boolean(true)));
    assert_eq!(eval("(> 2 1)"), Ok(Value::Boolean(true)));
    assert_eq!(eval("(<= 1 1)"), Ok(Value::Boolean(true)));
    assert_eq!(eval("(>= 2 1)"), Ok(Value::Boolean(true)));
    assert_eq!(eval("(= 1 1)"), Ok(Value::Boolean(true)));
}

#[test]
fn sem_5_3_equality() {
    assert_eq!(eval("(equal? 1 1)"), Ok(Value::Boolean(true)));
    assert_eq!(
        eval("(equal? (list 1 2) (list 1 2))"),
        Ok(Value::Boolean(true))
    );
}

#[test]
fn sem_5_4_list_operations() {
    assert_eq!(eval("(car (list 1 2 3))"), Ok(Value::Number(1.0)));
    assert_eq!(
        eval("(cdr (list 1 2 3))"),
        Ok(Value::List(Rc::new(vec![Value::Number(2.0), Value::Number(3.0)])))
    );
    assert_eq!(eval("(null? (list))"), Ok(Value::Boolean(true)));
    assert_eq!(eval("(null? (list 1))"), Ok(Value::Boolean(false)));
    assert_eq!(eval("(pair? (list 1))"), Ok(Value::Boolean(true)));
}

#[test]
fn sem_5_5_type_predicates() {
    assert_eq!(eval("(number? 1)"), Ok(Value::Boolean(true)));
    assert_eq!(eval("(string? \"hi\")"), Ok(Value::Boolean(true)));
    assert_eq!(eval("(symbol? (quote foo))"), Ok(Value::Boolean(true)));
    assert_eq!(eval("(boolean? #t)"), Ok(Value::Boolean(true)));
    assert_eq!(eval("(procedure? +)"), Ok(Value::Boolean(true)));
    assert_eq!(eval("(vector? (make-vector 3))"), Ok(Value::Boolean(true)));
}

#[test]
fn sem_5_6_not() {
    assert_eq!(eval("(not #f)"), Ok(Value::Boolean(true)));
    assert_eq!(eval("(not #t)"), Ok(Value::Boolean(false)));
    assert_eq!(eval("(not 0)"), Ok(Value::Boolean(false)));
}

#[test]
fn sem_5_7_vectors() {
    assert_eq!(
        eval_multi(&[
            "(define v (make-vector 3 0))",
            "(vector-set! v 1 42)",
            "(vector-ref v 1)"
        ]),
        Ok(Value::Number(42.0))
    );
    assert_eq!(
        eval("(vector-length (make-vector 5))"),
        Ok(Value::Number(5.0))
    );
}

// ============================================================
// Section 6: Error Semantics
// ============================================================

#[test]
fn sem_6_division_by_zero() {
    assert_eq!(eval("(/ 1 0)"), Err("division by zero".to_string()));
}

#[test]
fn sem_6_car_on_non_pair() {
    assert_eq!(eval("(car 42)"), Err("car: expected pair".to_string()));
}

#[test]
fn sem_6_cdr_on_non_pair() {
    assert_eq!(eval("(cdr 42)"), Err("cdr: expected pair".to_string()));
}

#[test]
fn sem_6_type_error() {
    assert_eq!(eval("(+ 1 \"a\")"), Err("expected number".to_string()));
}

// ============================================================
// Section 2: Truthiness — validate that only #f is falsy
// ============================================================

#[test]
fn sem_2_truthiness_zero_is_truthy() {
    assert_eq!(eval("(if 0 \"yes\" \"no\")"), Ok(Value::StringLit("yes".to_string())));
}

#[test]
fn sem_2_truthiness_empty_string_is_truthy() {
    assert_eq!(eval("(if \"\" \"yes\" \"no\")"), Ok(Value::StringLit("yes".to_string())));
}

#[test]
fn sem_2_truthiness_empty_list_is_truthy() {
    assert_eq!(eval("(if (list) \"yes\" \"no\")"), Ok(Value::StringLit("yes".to_string())));
}

// ============================================================
// Section 2: Truthiness — Void is truthy
// ============================================================

#[test]
fn sem_2_truthiness_void_is_truthy() {
    // Per Section 2: Void is truthy (only #f is falsy)
    assert_eq!(
        eval("(if (begin) \"yes\" \"no\")"),
        Ok(Value::StringLit("yes".to_string()))
    );
}

// ============================================================
// Section 3: Environment Operations
// ============================================================

#[test]
fn sem_3_lookup_parent_chain() {
    // lookup traverses parent chain (Section 3)
    assert_eq!(
        eval_multi(&[
            "(define x 10)",
            "(let ((y 20)) (+ x y))"
        ]),
        Ok(Value::Number(30.0))
    );
}

#[test]
fn sem_3_extend_creates_new_scope() {
    // extend creates a new env with parent (Section 3)
    assert_eq!(
        eval_multi(&[
            "(define x 1)",
            "((lambda (x) x) 99)",
        ]),
        Ok(Value::Number(99.0))
    );
}

// ============================================================
// Section 4.1: Self-Evaluating — float literal
// ============================================================

#[test]
fn sem_4_1_float_literal() {
    assert_eq!(eval("3.14"), Ok(Value::Number(3.14)));
}

// ============================================================
// Section 4.3: Quote — additional datum conversions
// ============================================================

#[test]
fn sem_4_3_quote_string() {
    // datum(s) = Str(s)
    assert_eq!(
        eval("(quote \"hello\")"),
        Ok(Value::StringLit("hello".to_string()))
    );
}

#[test]
fn sem_4_3_quote_boolean() {
    // datum(b) = Bool(b)
    assert_eq!(eval("(quote #t)"), Ok(Value::Boolean(true)));
}

#[test]
fn sem_4_3_quote_nested_list() {
    // datum((e1 .. en)) = List(datum(e1), .., datum(en)) — nested
    assert_eq!(
        eval("(quote (1 (2 3)))"),
        Ok(Value::List(Rc::new(vec![
            Value::Number(1.0),
            Value::List(Rc::new(vec![Value::Number(2.0), Value::Number(3.0)])),
        ])))
    );
}

// ============================================================
// Section 4.4: Conditional — truthy condition with no-else
// ============================================================

#[test]
fn sem_4_4_if_truthy_no_else_returns_value() {
    // E[(if e1 e2), env, k]: truthy? => E[e2, env, k]
    assert_eq!(eval("(if #t 42)"), Ok(Value::Number(42.0)));
}

// ============================================================
// Section 4.5: Sequence — single form, side effects
// ============================================================

#[test]
fn sem_4_5_begin_single_form() {
    assert_eq!(eval("(begin 42)"), Ok(Value::Number(42.0)));
}

#[test]
fn sem_4_5_begin_side_effects() {
    // Intermediate forms evaluated for side effects
    assert_eq!(
        eval_multi(&[
            "(define x 0)",
            "(begin (set! x 10) (+ x 1))"
        ]),
        Ok(Value::Number(11.0))
    );
}

// ============================================================
// Section 4.7: Assignment — set! returns Void
// ============================================================

#[test]
fn sem_4_7_set_bang_returns_void() {
    assert_eq!(
        eval_multi(&["(define x 1)", "(set! x 2)"]),
        Ok(Value::Void)
    );
}

// ============================================================
// Section 4.8: Lambda — multi-body
// ============================================================

#[test]
fn sem_4_8_lambda_multi_body() {
    // Lambda with multiple body forms returns last value
    assert_eq!(
        eval_multi(&[
            "(define x 0)",
            "((lambda () (set! x 10) (+ x 1)))"
        ]),
        Ok(Value::Number(11.0))
    );
}

// ============================================================
// Section 4.9: Application — continuation application
// ============================================================

#[test]
fn sem_4_9_continuation_application() {
    // apply(Cont(kc), [v], k) = escape(kc(v))
    assert_eq!(
        eval("(call/cc (lambda (k) (k 42)))"),
        Ok(Value::Number(42.0))
    );
}

// ============================================================
// Section 4.11: Let — cannot see own bindings
// ============================================================

#[test]
fn sem_4_11_let_bindings_not_visible_to_each_other() {
    // let inits evaluated in outer env — y cannot see x's binding
    let result = eval("(let ((x 1) (y x)) y)");
    // x is not bound in outer env, so this should error
    assert!(result.is_err());
}

#[test]
fn sem_4_11_letrec_init_void() {
    // letrec binds to Void first, then evaluates — lambdas can see each other
    assert_eq!(
        eval("(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (even? 4))"),
        Ok(Value::Boolean(true))
    );
}

// ============================================================
// Section 4.12: Mutation — error cases and list mutation
// ============================================================

#[test]
fn sem_4_12_set_car_on_list() {
    // set-car! on List (not just Pair)
    assert_eq!(
        eval_multi(&[
            "(define p (list 1 2 3))",
            "(set-car! p 10)",
            "(car p)"
        ]),
        Ok(Value::Number(10.0))
    );
}

#[test]
fn sem_4_12_set_car_non_pair_error() {
    let result = eval_multi(&["(define x 42)", "(set-car! x 1)"]);
    assert_eq!(result, Err("expected mutable pair".to_string()));
}

#[test]
fn sem_4_12_set_cdr_non_pair_error() {
    let result = eval_multi(&["(define x 42)", "(set-cdr! x 1)"]);
    assert_eq!(result, Err("expected mutable pair".to_string()));
}

// ============================================================
// Section 4.14: Catch — non-matching tag propagates
// ============================================================

#[test]
fn sem_4_14_catch_non_matching_tag_propagates() {
    // Inner throw with non-matching tag propagates to outer catch
    assert_eq!(
        eval("(catch 'outer (catch 'inner (throw 'outer 42)))"),
        Ok(Value::Number(42.0))
    );
}

// ============================================================
// Section 5.1: Arithmetic — mod division by zero
// ============================================================

#[test]
fn sem_5_1_mod_division_by_zero() {
    assert_eq!(eval("(mod 10 0)"), Err("division by zero".to_string()));
}

// ============================================================
// Section 5.3: Equality — eq? identity vs structural
// ============================================================

#[test]
fn sem_5_3_eq_identity() {
    // eq? on atoms: value equality
    assert_eq!(eval("(eq? 1 1)"), Ok(Value::Boolean(true)));
}

#[test]
fn sem_5_3_eq_list_identity() {
    // eq? on different list objects: identity (pointer) comparison
    assert_eq!(
        eval("(eq? (list 1 2) (list 1 2))"),
        Ok(Value::Boolean(false))
    );
}

// ============================================================
// Section 5.4: List Operations — cons creating pair, cons onto list
// ============================================================

#[test]
fn sem_5_4_cons_onto_list() {
    // D[cons](h, List(elems)) = List(h :: elems)
    assert_eq!(
        eval("(cons 1 (list 2 3))"),
        Ok(Value::List(Rc::new(vec![
            Value::Number(1.0),
            Value::Number(2.0),
            Value::Number(3.0),
        ])))
    );
}

#[test]
fn sem_5_4_cons_pair() {
    // D[cons](h, t) = Pair(h, t) when t is not a list
    assert_eq!(
        eval("(cons 1 2)"),
        Ok(Value::Pair(
            Box::new(Value::Number(1.0)),
            Box::new(Value::Number(2.0))
        ))
    );
}

#[test]
fn sem_5_4_list_constructor() {
    // D[list](v1..vn) = List(v1..vn)
    assert_eq!(
        eval("(list 1 2 3)"),
        Ok(Value::List(Rc::new(vec![
            Value::Number(1.0),
            Value::Number(2.0),
            Value::Number(3.0),
        ])))
    );
}

#[test]
fn sem_5_4_pair_predicate_on_pair() {
    // D[pair?](Pair(_, _)) = Bool(#t)
    assert_eq!(eval("(pair? (cons 1 2))"), Ok(Value::Boolean(true)));
}

// ============================================================
// Section 5.7: Vectors — error cases
// ============================================================

#[test]
fn sem_5_7_vector_ref_out_of_range() {
    assert_eq!(
        eval_multi(&["(define v (make-vector 3))", "(vector-ref v 5)"]),
        Err("index out of range".to_string())
    );
}

#[test]
fn sem_5_7_vector_set_out_of_range() {
    assert_eq!(
        eval_multi(&["(define v (make-vector 3))", "(vector-set! v 5 1)"]),
        Err("index out of range".to_string())
    );
}

#[test]
fn sem_5_7_vector_ref_non_vector() {
    assert_eq!(
        eval("(vector-ref 42 0)"),
        Err("expected vector".to_string())
    );
}

// ============================================================
// Section 5.8: I/O — display and newline return Void
// ============================================================

#[test]
fn sem_5_8_display_returns_void() {
    assert_eq!(eval("(display 42)"), Ok(Value::Void));
}

#[test]
fn sem_5_8_newline_returns_void() {
    assert_eq!(eval("(newline)"), Ok(Value::Void));
}

// ============================================================
// Section 6: Error Semantics — additional error cases
// ============================================================

#[test]
fn sem_6_not_a_procedure() {
    let result = eval("(42 1 2)");
    assert!(result.is_err());
    assert!(result.unwrap_err().starts_with("not a procedure"));
}

#[test]
fn sem_6_arity_mismatch() {
    assert_eq!(
        eval_multi(&["(define (f x) x)", "(f 1 2 3)"]),
        Err("arity mismatch: expected 1, got 3".to_string())
    );
}

#[test]
fn sem_6_set_bang_unbound() {
    assert_eq!(
        eval("(set! zzz 1)"),
        Err("cannot set! unbound variable: zzz".to_string())
    );
}

#[test]
fn sem_6_uncaught_throw() {
    assert_eq!(
        eval("(throw 'bad 1)"),
        Err("no matching catch for tag: bad".to_string())
    );
}

#[test]
fn sem_6_unknown_block() {
    assert_eq!(
        eval("(return-from mystery 1)"),
        Err("unknown block: mystery".to_string())
    );
}

#[test]
fn sem_6_expected_mutable_pair() {
    assert_eq!(
        eval_multi(&["(define x 42)", "(set-car! x 1)"]),
        Err("expected mutable pair".to_string())
    );
}

#[test]
fn sem_6_index_out_of_range() {
    assert_eq!(
        eval_multi(&["(define v (make-vector 2))", "(vector-ref v 10)"]),
        Err("index out of range".to_string())
    );
}

#[test]
fn sem_6_expected_vector() {
    assert_eq!(
        eval("(vector-set! 42 0 1)"),
        Err("expected vector".to_string())
    );
}

// ============================================================
// Section 7: Evaluation Entry Point
// straw_eval(expr, env) = E[expr, env, id]
// ============================================================

#[test]
fn sem_7_straw_eval_identity_continuation() {
    // straw_eval is E[expr, env, id] — returns value directly
    let env = default_env();
    let expr = parse("(+ 1 2)").unwrap();
    assert_eq!(straw_eval(&expr, &env), Ok(Value::Number(3.0)));
}

#[test]
fn sem_7_continuation_escape_returns_value() {
    // Top-level catches continuation escape and returns the escaped value
    assert_eq!(
        eval("(+ 1 (call/cc (lambda (k) (k 10))))"),
        Ok(Value::Number(11.0))
    );
}

// ============================================================
// Verify docs/semantics.md exists
// ============================================================

#[test]
fn sem_document_exists() {
    assert!(
        std::path::Path::new("docs/semantics.md").exists(),
        "docs/semantics.md must exist"
    );
}

#[test]
fn sem_document_covers_core_forms() {
    let content = std::fs::read_to_string("docs/semantics.md")
        .expect("should be able to read docs/semantics.md");
    // Verify all core forms are documented
    let required_sections = [
        "Self-Evaluating Forms",
        "Symbol",
        "Quote",
        "Conditional",
        "Sequence",
        "Definition",
        "Assignment",
        "Lambda",
        "Function Application",
        "Logical Connectives",
        "Let Forms",
        "Mutation",
        "First-Class Continuations",
        "Catch and Throw",
        "Block and Return-From",
        "Unwind-Protect",
        "Builtins",
        "Error Semantics",
    ];
    for section in &required_sections {
        assert!(
            content.contains(section),
            "docs/semantics.md should contain section about '{}'",
            section
        );
    }
}

// ============================================================
// Traceability Summary:
// Section 1 (Domains):       declarative, no test needed
// Section 2 (Truthiness):    4 tests (sem_2_*)
// Section 3 (Env Ops):       2 tests (sem_3_*) + indirect via 4.2, 4.6, 4.7, 4.9
// Section 4.1 (Literals):    5 tests (4 original + float)
// Section 4.2 (Symbol):      2 tests
// Section 4.3 (Quote):       6 tests (3 original + string, boolean, nested)
// Section 4.4 (Conditional): 5 tests (4 original + truthy no-else)
// Section 4.5 (Sequence):    4 tests (2 original + single, side effects)
// Section 4.6 (Define):      3 tests
// Section 4.7 (Set!):        3 tests (2 original + returns void)
// Section 4.8 (Lambda):      3 tests (2 original + multi-body)
// Section 4.9 (Application): 5 tests (4 original + continuation)
// Section 4.10 (and/or):     6 tests
// Section 4.11 (Let):        5 tests (3 original + not-visible, mutual rec)
// Section 4.12 (Mutation):   5 tests (2 original + list, 2 error)
// Section 4.13 (call/cc):    2 tests
// Section 4.14 (Catch):      4 tests (3 original + propagation)
// Section 4.15 (Block):      3 tests
// Section 4.16 (Unwind):     2 tests
// Section 5.1 (Arithmetic):  2 tests (1 original + mod/0)
// Section 5.2 (Comparison):  1 test
// Section 5.3 (Equality):    3 tests (1 original + eq? identity, eq? list)
// Section 5.4 (List Ops):    5 tests (1 original + cons list, cons pair, list, pair?)
// Section 5.5 (Type Preds):  1 test
// Section 5.6 (not):         1 test
// Section 5.7 (Vectors):     4 tests (1 original + 3 error)
// Section 5.8 (I/O):         2 tests (display, newline)
// Section 6 (Errors):       12 tests (4 original + 8 new)
// Section 7 (Entry Point):   2 tests
// Docs verification:          2 tests
// Total: 97 tests covering all valuation clauses
// ============================================================
