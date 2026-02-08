use std::cell::RefCell;
use std::rc::Rc;
use crate::env::{Env, Value};
use crate::parser::Expr;

/// Sentinel for continuation escape errors.
const CONT_ESCAPE: &str = "\0cont-escape\0";

/// Sentinel for throw escape errors.
const THROW_ESCAPE: &str = "\0throw-escape\0";

/// Sentinel for block/return-from escape errors.
const BLOCK_ESCAPE: &str = "\0block-escape\0";

thread_local! {
    static ESCAPED_VALUE: RefCell<Option<Value>> = const { RefCell::new(None) };
    static THROWN_TAG: RefCell<Option<String>> = const { RefCell::new(None) };
    static THROWN_VALUE: RefCell<Option<Value>> = const { RefCell::new(None) };
    static BLOCK_NAME: RefCell<Option<String>> = const { RefCell::new(None) };
    static BLOCK_VALUE: RefCell<Option<Value>> = const { RefCell::new(None) };
}

type Cont = Rc<dyn Fn(Value) -> Result<Value, String>>;

/// CPS evaluator: evaluates expr in env, passing the result to continuation k.
/// This is the public API. Internally delegates to eval_cps with Rc continuations.
/// Note: call/cc may capture k. If k is just the identity, this is fine.
/// For proper call/cc semantics, callers should use straw_eval (identity k).
pub fn straw_eval_k(
    expr: &Expr,
    env: &Rc<Env>,
    k: &dyn Fn(Value) -> Result<Value, String>,
) -> Result<Value, String> {
    // Evaluate with identity continuation, then apply k to the result.
    // This ensures call/cc never captures a dangling reference.
    let id: Cont = Rc::new(|v: Value| Ok(v));
    let result = eval_cps(expr, env, &id)?;
    k(result)
}

/// Internal CPS evaluator using Rc<dyn Fn> continuations.
fn eval_cps(
    expr: &Expr,
    env: &Rc<Env>,
    k: &Cont,
) -> Result<Value, String> {
    match expr {
        Expr::Number(n) => k(Value::Number(*n)),
        Expr::StringLit(s) => k(Value::StringLit(s.clone())),
        Expr::Boolean(b) => k(Value::Boolean(*b)),
        Expr::Symbol(name) => {
            let val = env.lookup(name)?;
            k(val)
        }
        Expr::List(elements) => {
            if elements.is_empty() {
                return Err("not implemented".to_string());
            }
            match &elements[0] {
                Expr::Symbol(name) if name == "quote" => {
                    if elements.len() != 2 {
                        return Err("quote expects exactly one argument".to_string());
                    }
                    let val = expr_to_value(&elements[1])?;
                    k(val)
                }
                Expr::Symbol(name) if name == "begin" => {
                    let body = &elements[1..];
                    if body.is_empty() {
                        return k(Value::Void);
                    }
                    let id: Cont = Rc::new(|v| Ok(v));
                    let mut result = Value::Void;
                    for expr in body {
                        result = eval_cps(expr, env, &id)?;
                    }
                    k(result)
                }
                Expr::Symbol(name) if name == "define" => {
                    match &elements[1] {
                        Expr::Symbol(var_name) => {
                            if elements.len() != 3 {
                                return Err("define expects exactly 2 arguments".to_string());
                            }
                            let id: Cont = Rc::new(|v| Ok(v));
                            let val = eval_cps(&elements[2], env, &id)?;
                            env.set(var_name, val);
                            k(Value::Void)
                        }
                        Expr::List(name_and_params) => {
                            if name_and_params.is_empty() {
                                return Err("define: missing function name".to_string());
                            }
                            if elements.len() < 3 {
                                return Err("define: missing body".to_string());
                            }
                            let func_name = match &name_and_params[0] {
                                Expr::Symbol(s) => s,
                                _ => return Err("define: function name must be a symbol".to_string()),
                            };
                            let mut params = Vec::new();
                            for p in &name_and_params[1..] {
                                match p {
                                    Expr::Symbol(s) => params.push(s.clone()),
                                    _ => return Err("define: parameter must be a symbol".to_string()),
                                }
                            }
                            let body = elements[2..].to_vec();
                            let closure = Value::Closure(params, body, Rc::clone(env));
                            env.set(func_name, closure);
                            k(Value::Void)
                        }
                        _ => Err("define: first argument must be a symbol or list".to_string()),
                    }
                }
                Expr::Symbol(name) if name == "set!" => {
                    if elements.len() != 3 {
                        return Err("set! expects exactly 2 arguments".to_string());
                    }
                    match &elements[1] {
                        Expr::Symbol(var_name) => {
                            let id: Cont = Rc::new(|v| Ok(v));
                            let val = eval_cps(&elements[2], env, &id)?;
                            env.update(var_name, val)?;
                            k(Value::Void)
                        }
                        _ => Err("set!: first argument must be a symbol".to_string()),
                    }
                }
                Expr::Symbol(name) if name == "lambda" => {
                    if elements.len() < 3 {
                        return Err("lambda expects at least 2 arguments".to_string());
                    }
                    let params = match &elements[1] {
                        Expr::List(param_exprs) => {
                            let mut params = Vec::new();
                            for p in param_exprs {
                                match p {
                                    Expr::Symbol(s) => params.push(s.clone()),
                                    _ => return Err("lambda: parameter must be a symbol".to_string()),
                                }
                            }
                            params
                        }
                        _ => return Err("lambda: expected parameter list".to_string()),
                    };
                    let body = elements[2..].to_vec();
                    k(Value::Closure(params, body, Rc::clone(env)))
                }
                Expr::Symbol(name) if name == "and" => {
                    let args = &elements[1..];
                    if args.is_empty() {
                        return k(Value::Boolean(true));
                    }
                    let id: Cont = Rc::new(|v| Ok(v));
                    for arg in &args[..args.len() - 1] {
                        let val = eval_cps(arg, env, &id)?;
                        if matches!(val, Value::Boolean(false)) {
                            return k(Value::Boolean(false));
                        }
                    }
                    eval_cps(&args[args.len() - 1], env, k)
                }
                Expr::Symbol(name) if name == "or" => {
                    let args = &elements[1..];
                    if args.is_empty() {
                        return k(Value::Boolean(false));
                    }
                    let id: Cont = Rc::new(|v| Ok(v));
                    for arg in &args[..args.len() - 1] {
                        let val = eval_cps(arg, env, &id)?;
                        if !matches!(val, Value::Boolean(false)) {
                            return k(val);
                        }
                    }
                    eval_cps(&args[args.len() - 1], env, k)
                }
                Expr::Symbol(name) if name == "let" => {
                    if elements.len() < 3 {
                        return Err("let expects at least 2 arguments".to_string());
                    }
                    let bindings_expr = match &elements[1] {
                        Expr::List(b) => b,
                        _ => return Err("let: expected bindings list".to_string()),
                    };
                    let id: Cont = Rc::new(|v| Ok(v));
                    let mut names = Vec::new();
                    let mut vals = Vec::new();
                    for binding in bindings_expr {
                        match binding {
                            Expr::List(pair) if pair.len() == 2 => {
                                match &pair[0] {
                                    Expr::Symbol(s) => {
                                        let val = eval_cps(&pair[1], env, &id)?;
                                        names.push(s.clone());
                                        vals.push(val);
                                    }
                                    _ => return Err("let: binding name must be a symbol".to_string()),
                                }
                            }
                            _ => return Err("malformed binding".to_string()),
                        }
                    }
                    let child = Env::with_parent(Rc::clone(env));
                    for (name, val) in names.into_iter().zip(vals) {
                        child.set(&name, val);
                    }
                    let child = Rc::new(child);
                    let body = &elements[2..];
                    let mut result = Value::Void;
                    for expr in body {
                        result = eval_cps(expr, &child, &id)?;
                    }
                    k(result)
                }
                Expr::Symbol(name) if name == "let*" => {
                    if elements.len() < 3 {
                        return Err("let* expects at least 2 arguments".to_string());
                    }
                    let bindings_expr = match &elements[1] {
                        Expr::List(b) => b,
                        _ => return Err("let*: expected bindings list".to_string()),
                    };
                    let id: Cont = Rc::new(|v| Ok(v));
                    let mut current_env = Rc::clone(env);
                    for binding in bindings_expr {
                        match binding {
                            Expr::List(pair) if pair.len() == 2 => {
                                match &pair[0] {
                                    Expr::Symbol(s) => {
                                        let val = eval_cps(&pair[1], &current_env, &id)?;
                                        let new_env = Env::with_parent(current_env);
                                        new_env.set(s, val);
                                        current_env = Rc::new(new_env);
                                    }
                                    _ => return Err("let*: binding name must be a symbol".to_string()),
                                }
                            }
                            _ => return Err("malformed binding".to_string()),
                        }
                    }
                    let body = &elements[2..];
                    let mut result = Value::Void;
                    for expr in body {
                        result = eval_cps(expr, &current_env, &id)?;
                    }
                    k(result)
                }
                Expr::Symbol(name) if name == "letrec" => {
                    if elements.len() < 3 {
                        return Err("letrec expects at least 2 arguments".to_string());
                    }
                    let bindings_expr = match &elements[1] {
                        Expr::List(b) => b,
                        _ => return Err("letrec: expected bindings list".to_string()),
                    };
                    let letrec_env = Rc::new(Env::with_parent(Rc::clone(env)));
                    let id: Cont = Rc::new(|v| Ok(v));
                    let mut names = Vec::new();
                    for binding in bindings_expr {
                        match binding {
                            Expr::List(pair) if pair.len() == 2 => {
                                match &pair[0] {
                                    Expr::Symbol(s) => {
                                        letrec_env.set(s, Value::Void);
                                        names.push(s.clone());
                                    }
                                    _ => return Err("letrec: binding name must be a symbol".to_string()),
                                }
                            }
                            _ => return Err("malformed binding".to_string()),
                        }
                    }
                    let mut vals = Vec::new();
                    for binding in bindings_expr {
                        if let Expr::List(pair) = binding {
                            let val = eval_cps(&pair[1], &letrec_env, &id)?;
                            vals.push(val);
                        }
                    }
                    for (name, val) in names.iter().zip(vals) {
                        letrec_env.set(name, val);
                    }
                    let body = &elements[2..];
                    let mut result = Value::Void;
                    for expr in body {
                        result = eval_cps(expr, &letrec_env, &id)?;
                    }
                    k(result)
                }
                Expr::Symbol(name) if name == "if" => {
                    if elements.len() < 3 || elements.len() > 4 {
                        return Err("if expects 2 or 3 arguments".to_string());
                    }
                    let id: Cont = Rc::new(|v| Ok(v));
                    let condition = eval_cps(&elements[1], env, &id)?;
                    let is_false = matches!(condition, Value::Boolean(false));
                    if !is_false {
                        eval_cps(&elements[2], env, k)
                    } else if elements.len() == 4 {
                        eval_cps(&elements[3], env, k)
                    } else {
                        k(Value::Void)
                    }
                }
                Expr::Symbol(name) if name == "catch" => {
                    if elements.len() != 3 {
                        return Err("catch expects exactly 2 arguments".to_string());
                    }
                    let id: Cont = Rc::new(|v| Ok(v));
                    let tag = eval_cps(&elements[1], env, &id)?;
                    let tag_name = match tag {
                        Value::Symbol(s) => s,
                        _ => return Err("catch: tag must be a symbol".to_string()),
                    };
                    // Evaluate the body. If a throw escape occurs, check the tag.
                    match eval_cps(&elements[2], env, &id) {
                        Ok(val) => k(val),
                        Err(e) if e == THROW_ESCAPE => {
                            let thrown_tag = THROWN_TAG.with(|cell| cell.borrow_mut().take());
                            let thrown_val = THROWN_VALUE.with(|cell| cell.borrow_mut().take());
                            match (thrown_tag, thrown_val) {
                                (Some(t), Some(v)) if t == tag_name => k(v),
                                (Some(t), Some(v)) => {
                                    // Not our tag — re-propagate
                                    THROWN_TAG.with(|cell| *cell.borrow_mut() = Some(t));
                                    THROWN_VALUE.with(|cell| *cell.borrow_mut() = Some(v));
                                    Err(THROW_ESCAPE.to_string())
                                }
                                _ => Err("internal error: throw escape without tag/value".to_string()),
                            }
                        }
                        Err(e) => Err(e),
                    }
                }
                Expr::Symbol(name) if name == "throw" => {
                    if elements.len() != 3 {
                        return Err("throw expects exactly 2 arguments".to_string());
                    }
                    let id: Cont = Rc::new(|v| Ok(v));
                    let tag = eval_cps(&elements[1], env, &id)?;
                    let tag_name = match tag {
                        Value::Symbol(s) => s,
                        _ => return Err("throw: tag must be a symbol".to_string()),
                    };
                    let val = eval_cps(&elements[2], env, &id)?;
                    THROWN_TAG.with(|cell| *cell.borrow_mut() = Some(tag_name));
                    THROWN_VALUE.with(|cell| *cell.borrow_mut() = Some(val));
                    Err(THROW_ESCAPE.to_string())
                }
                Expr::Symbol(name) if name == "block" => {
                    if elements.len() < 3 {
                        return Err("block expects at least 2 arguments".to_string());
                    }
                    let block_name = match &elements[1] {
                        Expr::Symbol(s) => s.clone(),
                        _ => return Err("block: name must be a symbol".to_string()),
                    };
                    let id: Cont = Rc::new(|v| Ok(v));
                    let body = &elements[2..];
                    let mut result = Value::Void;
                    for expr in body {
                        match eval_cps(expr, env, &id) {
                            Ok(v) => result = v,
                            Err(e) if e == BLOCK_ESCAPE => {
                                let escaped_name = BLOCK_NAME.with(|cell| cell.borrow_mut().take());
                                let escaped_val = BLOCK_VALUE.with(|cell| cell.borrow_mut().take());
                                match (escaped_name, escaped_val) {
                                    (Some(n), Some(v)) if n == block_name => return k(v),
                                    (Some(n), Some(v)) => {
                                        // Not our block — re-propagate
                                        BLOCK_NAME.with(|cell| *cell.borrow_mut() = Some(n));
                                        BLOCK_VALUE.with(|cell| *cell.borrow_mut() = Some(v));
                                        return Err(BLOCK_ESCAPE.to_string());
                                    }
                                    _ => return Err("internal error: block escape without name/value".to_string()),
                                }
                            }
                            Err(e) => return Err(e),
                        }
                    }
                    k(result)
                }
                Expr::Symbol(name) if name == "return-from" => {
                    if elements.len() != 3 {
                        return Err("return-from expects exactly 2 arguments".to_string());
                    }
                    let block_name = match &elements[1] {
                        Expr::Symbol(s) => s.clone(),
                        _ => return Err("return-from: name must be a symbol".to_string()),
                    };
                    let id: Cont = Rc::new(|v| Ok(v));
                    let val = eval_cps(&elements[2], env, &id)?;
                    BLOCK_NAME.with(|cell| *cell.borrow_mut() = Some(block_name));
                    BLOCK_VALUE.with(|cell| *cell.borrow_mut() = Some(val));
                    Err(BLOCK_ESCAPE.to_string())
                }
                Expr::Symbol(name) if name == "unwind-protect" => {
                    if elements.len() < 2 {
                        return Err("unwind-protect expects at least 1 argument".to_string());
                    }
                    let id: Cont = Rc::new(|v| Ok(v));
                    // Evaluate the protected expression, capturing any error
                    let protected_result = eval_cps(&elements[1], env, &id);
                    // Always evaluate cleanup forms
                    let cleanup = &elements[2..];
                    let cleanup_id: Cont = Rc::new(|v| Ok(v));
                    for expr in cleanup {
                        eval_cps(expr, env, &cleanup_id)?;
                    }
                    // Now return the protected result (or re-propagate error)
                    k(protected_result?)
                }
                Expr::Symbol(name) if name == "call/cc" => {
                    if elements.len() != 2 {
                        return Err("call/cc expects exactly 1 argument".to_string());
                    }
                    let id: Cont = Rc::new(|v| Ok(v));
                    let proc = eval_cps(&elements[1], env, &id)?;
                    match proc {
                        Value::Closure(params, body, closure_env) => {
                            if params.len() != 1 {
                                return Err(format!(
                                    "arity mismatch: expected 1, got {}",
                                    params.len()
                                ));
                            }
                            // Reify k as a first-class continuation value.
                            // k is Rc<dyn Fn>, so we can clone it into the Continuation.
                            let captured_k = Rc::clone(k);
                            let cont_val = Value::Continuation(Rc::new(move |val: Value| {
                                captured_k(val)
                            }));

                            // Bind the continuation to the lambda's parameter
                            let child_env = Rc::new(Env::with_parent(Rc::clone(&closure_env)));
                            child_env.set(&params[0], cont_val);

                            // Evaluate the lambda body with identity continuation.
                            // If k is invoked inside the body, it will escape via CONT_ESCAPE.
                            // If not invoked, the body returns normally and we pass to k.
                            let mut result = Value::Void;
                            for expr in &body {
                                result = eval_cps(expr, &child_env, &id)?;
                            }
                            // Normal return: lambda body completed without invoking k
                            k(result)
                        }
                        _ => Err("expected procedure".to_string()),
                    }
                }
                _ => {
                    // Function application: evaluate func then args in CPS
                    let env = Rc::clone(env);
                    let arg_exprs = elements[1..].to_vec();
                    let k = Rc::clone(k);
                    let id: Cont = Rc::new(|v| Ok(v));
                    let func = eval_cps(&elements[0], &env, &id)?;
                    let then_fn: Rc<dyn Fn(Vec<Value>) -> Result<Value, String>> =
                        Rc::new(move |args: Vec<Value>| {
                            apply_function(func.clone(), args, &k)
                        });
                    eval_args_cps(&arg_exprs, &env, Vec::new(), &then_fn)
                }
            }
        }
    }
}

/// Evaluate argument expressions left-to-right in CPS, then call `then`.
fn eval_args_cps(
    arg_exprs: &[Expr],
    env: &Rc<Env>,
    accumulated: Vec<Value>,
    then: &Rc<dyn Fn(Vec<Value>) -> Result<Value, String>>,
) -> Result<Value, String> {
    if arg_exprs.is_empty() {
        return then(accumulated);
    }
    let rest = arg_exprs[1..].to_vec();
    let env2 = Rc::clone(env);
    let then2 = Rc::clone(then);
    let arg_k: Cont = Rc::new(move |val: Value| {
        let mut acc = accumulated.clone();
        acc.push(val);
        eval_args_cps(&rest, &env2, acc, &then2)
    });
    eval_cps(&arg_exprs[0], env, &arg_k)
}

/// Apply a function value to arguments, passing result to k.
fn apply_function(
    func: Value,
    args: Vec<Value>,
    k: &Cont,
) -> Result<Value, String> {
    match func {
        Value::Builtin(_, f) => {
            let result = f(args)?;
            k(result)
        }
        Value::Closure(params, body, closure_env) => {
            let child_env = Env::extend(Rc::clone(&closure_env), &params, args)?;
            let child_env = Rc::new(child_env);
            let id: Cont = Rc::new(|v| Ok(v));
            let mut result = Value::Void;
            for expr in &body {
                result = eval_cps(expr, &child_env, &id)?;
            }
            k(result)
        }
        Value::Continuation(cont_fn) => {
            if args.len() != 1 {
                return Err(format!(
                    "arity mismatch: expected 1, got {}",
                    args.len()
                ));
            }
            let val = args.into_iter().next().unwrap();
            // Apply the captured continuation to the value.
            // This produces the final result. We escape with it.
            let result = cont_fn(val)?;
            ESCAPED_VALUE.with(|cell| {
                *cell.borrow_mut() = Some(result);
            });
            Err(CONT_ESCAPE.to_string())
        }
        _ => Err(format!("not a procedure: {:?}", func)),
    }
}

/// Convenience wrapper: evaluates with the identity continuation.
/// Also catches continuation escapes from saved continuations.
pub fn straw_eval(expr: &Expr, env: &Rc<Env>) -> Result<Value, String> {
    match straw_eval_k(expr, env, &|v| Ok(v)) {
        Ok(v) => Ok(v),
        Err(e) if e == CONT_ESCAPE => {
            ESCAPED_VALUE.with(|cell| {
                cell.borrow_mut()
                    .take()
                    .ok_or_else(|| "internal error: continuation escape without value".to_string())
            })
        }
        Err(e) if e == THROW_ESCAPE => {
            // Uncaught throw — clean up thread-local state and report error
            let tag = THROWN_TAG.with(|cell| cell.borrow_mut().take());
            THROWN_VALUE.with(|cell| cell.borrow_mut().take());
            match tag {
                Some(t) => Err(format!("no matching catch for tag: {}", t)),
                None => Err("no matching catch".to_string()),
            }
        }
        Err(e) if e == BLOCK_ESCAPE => {
            // Uncaught return-from — clean up thread-local state and report error
            let name = BLOCK_NAME.with(|cell| cell.borrow_mut().take());
            BLOCK_VALUE.with(|cell| cell.borrow_mut().take());
            match name {
                Some(n) => Err(format!("unknown block: {}", n)),
                None => Err("unknown block".to_string()),
            }
        }
        Err(e) => Err(e),
    }
}

fn expr_to_value(expr: &Expr) -> Result<Value, String> {
    match expr {
        Expr::Number(n) => Ok(Value::Number(*n)),
        Expr::StringLit(s) => Ok(Value::StringLit(s.clone())),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Symbol(s) => Ok(Value::Symbol(s.clone())),
        Expr::List(elements) => {
            let values: Result<Vec<Value>, String> = elements.iter().map(expr_to_value).collect();
            Ok(Value::List(values?))
        }
    }
}
