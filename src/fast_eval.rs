use std::cell::RefCell;
use std::rc::Rc;
use crate::env::{Env, Value};
use crate::parser::Expr;
use crate::pretreat::TreatedExpr;

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

/// Public API: evaluate a pretreated expression in an environment.
/// Catches continuation, throw, and block escapes at the top level.
pub fn fast_eval(expr: &TreatedExpr, env: &Rc<Env>) -> Result<Value, String> {
    let id: Cont = Rc::new(|v| Ok(v));
    match fast_eval_cps(expr, env, &id) {
        Ok(v) => Ok(v),
        Err(e) if e == CONT_ESCAPE => {
            ESCAPED_VALUE.with(|cell| {
                cell.borrow_mut()
                    .take()
                    .ok_or_else(|| "internal error: continuation escape without value".to_string())
            })
        }
        Err(e) if e == THROW_ESCAPE => {
            let tag = THROWN_TAG.with(|cell| cell.borrow_mut().take());
            THROWN_VALUE.with(|cell| cell.borrow_mut().take());
            match tag {
                Some(t) => Err(format!("no matching catch for tag: {}", t)),
                None => Err("no matching catch".to_string()),
            }
        }
        Err(e) if e == BLOCK_ESCAPE => {
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

/// CPS evaluator for pretreated expressions.
fn fast_eval_cps(
    expr: &TreatedExpr,
    env: &Rc<Env>,
    k: &Cont,
) -> Result<Value, String> {
    match expr {
        TreatedExpr::Number(n) => k(Value::Number(*n)),
        TreatedExpr::StringLit(s) => k(Value::StringLit(s.clone())),
        TreatedExpr::Boolean(b) => k(Value::Boolean(*b)),
        TreatedExpr::VarRef(name) => {
            let val = env.lookup(name)?;
            k(val)
        }
        TreatedExpr::LexicalRef(depth, offset) => {
            let val = env.lexical_lookup(*depth, *offset)?;
            k(val)
        }
        TreatedExpr::Quote(datum) => {
            let val = expr_to_value(datum)?;
            k(val)
        }
        TreatedExpr::If(cond, then, els) => {
            let id: Cont = Rc::new(|v| Ok(v));
            let condition = fast_eval_cps(cond, env, &id)?;
            let is_false = matches!(condition, Value::Boolean(false));
            if !is_false {
                fast_eval_cps(then, env, k)
            } else if let Some(alt) = els {
                fast_eval_cps(alt, env, k)
            } else {
                k(Value::Void)
            }
        }
        TreatedExpr::Begin(body) => {
            if body.is_empty() {
                return k(Value::Void);
            }
            let id: Cont = Rc::new(|v| Ok(v));
            let mut result = Value::Void;
            for expr in body {
                result = fast_eval_cps(expr, env, &id)?;
            }
            k(result)
        }
        TreatedExpr::Define(name, val_expr) => {
            let id: Cont = Rc::new(|v| Ok(v));
            let val = fast_eval_cps(val_expr, env, &id)?;
            env.set(name, val);
            k(Value::Void)
        }
        TreatedExpr::DefineFunc(name, params, body) => {
            let closure = Value::FastClosure(params.clone(), body.clone(), Rc::clone(env));
            env.set(name, closure);
            k(Value::Void)
        }
        TreatedExpr::SetBang(name, val_expr) => {
            let id: Cont = Rc::new(|v| Ok(v));
            let val = fast_eval_cps(val_expr, env, &id)?;
            env.update(name, val)?;
            k(Value::Void)
        }
        TreatedExpr::Lambda(params, body) => {
            k(Value::FastClosure(params.clone(), body.clone(), Rc::clone(env)))
        }
        TreatedExpr::And(args) => {
            if args.is_empty() {
                return k(Value::Boolean(true));
            }
            let id: Cont = Rc::new(|v| Ok(v));
            for arg in &args[..args.len() - 1] {
                let val = fast_eval_cps(arg, env, &id)?;
                if matches!(val, Value::Boolean(false)) {
                    return k(Value::Boolean(false));
                }
            }
            fast_eval_cps(&args[args.len() - 1], env, k)
        }
        TreatedExpr::Or(args) => {
            if args.is_empty() {
                return k(Value::Boolean(false));
            }
            let id: Cont = Rc::new(|v| Ok(v));
            for arg in &args[..args.len() - 1] {
                let val = fast_eval_cps(arg, env, &id)?;
                if !matches!(val, Value::Boolean(false)) {
                    return k(val);
                }
            }
            fast_eval_cps(&args[args.len() - 1], env, k)
        }
        TreatedExpr::Let(bindings, body) => {
            let id: Cont = Rc::new(|v| Ok(v));
            let mut names = Vec::new();
            let mut vals = Vec::new();
            for (name, val_expr) in bindings {
                let val = fast_eval_cps(val_expr, env, &id)?;
                names.push(name.clone());
                vals.push(val);
            }
            let child = Env::extend(Rc::clone(env), &names, vals)?;
            let child = Rc::new(child);
            let mut result = Value::Void;
            for expr in body {
                result = fast_eval_cps(expr, &child, &id)?;
            }
            k(result)
        }
        TreatedExpr::LetStar(bindings, body) => {
            let id: Cont = Rc::new(|v| Ok(v));
            let mut current_env = Rc::clone(env);
            for (name, val_expr) in bindings {
                let val = fast_eval_cps(val_expr, &current_env, &id)?;
                let new_env = Env::extend(current_env, &[name.clone()], vec![val])?;
                current_env = Rc::new(new_env);
            }
            let mut result = Value::Void;
            for expr in body {
                result = fast_eval_cps(expr, &current_env, &id)?;
            }
            k(result)
        }
        TreatedExpr::Letrec(bindings, body) => {
            let id: Cont = Rc::new(|v| Ok(v));
            let mut names = Vec::new();
            let mut void_vals = Vec::new();
            for (name, _) in bindings {
                names.push(name.clone());
                void_vals.push(Value::Void);
            }
            let letrec_env = Rc::new(Env::extend(Rc::clone(env), &names, void_vals)?);
            let mut vals = Vec::new();
            for (_, val_expr) in bindings {
                let val = fast_eval_cps(val_expr, &letrec_env, &id)?;
                vals.push(val);
            }
            // Update both the hash bindings and the rib
            for (i, (name, val)) in names.iter().zip(vals).enumerate() {
                letrec_env.set(name, val.clone());
                letrec_env.rib_set(i, val);
            }
            let mut result = Value::Void;
            for expr in body {
                result = fast_eval_cps(expr, &letrec_env, &id)?;
            }
            k(result)
        }
        TreatedExpr::Catch(tag_expr, body_expr) => {
            let id: Cont = Rc::new(|v| Ok(v));
            let tag = fast_eval_cps(tag_expr, env, &id)?;
            let tag_name = match tag {
                Value::Symbol(s) => s,
                _ => return Err("catch: tag must be a symbol".to_string()),
            };
            match fast_eval_cps(body_expr, env, &id) {
                Ok(val) => k(val),
                Err(e) if e == THROW_ESCAPE => {
                    let thrown_tag = THROWN_TAG.with(|cell| cell.borrow_mut().take());
                    let thrown_val = THROWN_VALUE.with(|cell| cell.borrow_mut().take());
                    match (thrown_tag, thrown_val) {
                        (Some(t), Some(v)) if t == tag_name => k(v),
                        (Some(t), Some(v)) => {
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
        TreatedExpr::Throw(tag_expr, val_expr) => {
            let id: Cont = Rc::new(|v| Ok(v));
            let tag = fast_eval_cps(tag_expr, env, &id)?;
            let tag_name = match tag {
                Value::Symbol(s) => s,
                _ => return Err("throw: tag must be a symbol".to_string()),
            };
            let val = fast_eval_cps(val_expr, env, &id)?;
            THROWN_TAG.with(|cell| *cell.borrow_mut() = Some(tag_name));
            THROWN_VALUE.with(|cell| *cell.borrow_mut() = Some(val));
            Err(THROW_ESCAPE.to_string())
        }
        TreatedExpr::Block(block_name, body) => {
            let id: Cont = Rc::new(|v| Ok(v));
            let mut result = Value::Void;
            for expr in body {
                match fast_eval_cps(expr, env, &id) {
                    Ok(v) => result = v,
                    Err(e) if e == BLOCK_ESCAPE => {
                        let escaped_name = BLOCK_NAME.with(|cell| cell.borrow_mut().take());
                        let escaped_val = BLOCK_VALUE.with(|cell| cell.borrow_mut().take());
                        match (escaped_name, escaped_val) {
                            (Some(n), Some(v)) if n == *block_name => return k(v),
                            (Some(n), Some(v)) => {
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
        TreatedExpr::ReturnFrom(block_name, val_expr) => {
            let id: Cont = Rc::new(|v| Ok(v));
            let val = fast_eval_cps(val_expr, env, &id)?;
            BLOCK_NAME.with(|cell| *cell.borrow_mut() = Some(block_name.clone()));
            BLOCK_VALUE.with(|cell| *cell.borrow_mut() = Some(val));
            Err(BLOCK_ESCAPE.to_string())
        }
        TreatedExpr::UnwindProtect(protected, cleanup) => {
            let id: Cont = Rc::new(|v| Ok(v));
            let protected_result = fast_eval_cps(protected, env, &id);
            let cleanup_id: Cont = Rc::new(|v| Ok(v));
            for expr in cleanup {
                fast_eval_cps(expr, env, &cleanup_id)?;
            }
            k(protected_result?)
        }
        TreatedExpr::CallCC(proc_expr) => {
            let id: Cont = Rc::new(|v| Ok(v));
            let proc = fast_eval_cps(proc_expr, env, &id)?;
            match proc {
                Value::FastClosure(params, body, closure_env) => {
                    if params.len() != 1 {
                        return Err(format!(
                            "arity mismatch: expected 1, got {}",
                            params.len()
                        ));
                    }
                    let captured_k = Rc::clone(k);
                    let cont_val = Value::Continuation(Rc::new(move |val: Value| {
                        captured_k(val)
                    }));
                    let child_env = Rc::new(Env::extend(
                        Rc::clone(&closure_env),
                        &params,
                        vec![cont_val],
                    )?);
                    let mut result = Value::Void;
                    for expr in &body {
                        result = fast_eval_cps(expr, &child_env, &id)?;
                    }
                    k(result)
                }
                Value::Closure(params, body, closure_env) => {
                    // Handle regular closures passed to call/cc too
                    if params.len() != 1 {
                        return Err(format!(
                            "arity mismatch: expected 1, got {}",
                            params.len()
                        ));
                    }
                    let captured_k = Rc::clone(k);
                    let cont_val = Value::Continuation(Rc::new(move |val: Value| {
                        captured_k(val)
                    }));
                    let child_env = Rc::new(Env::extend(
                        Rc::clone(&closure_env),
                        &params,
                        vec![cont_val],
                    )?);
                    // Regular closure body is Vec<Expr>, need to use straw_eval semantics
                    // But since fast_eval operates on TreatedExpr, pretreat each body expr
                    let id: Cont = Rc::new(|v| Ok(v));
                    let mut result = Value::Void;
                    for expr in &body {
                        let treated = crate::pretreat::pretreat(expr);
                        result = fast_eval_cps(&treated, &child_env, &id)?;
                    }
                    k(result)
                }
                _ => Err("expected procedure".to_string()),
            }
        }
        TreatedExpr::SetCar(target_expr, val_expr, var_name) => {
            let id: Cont = Rc::new(|v| Ok(v));
            let target = fast_eval_cps(target_expr, env, &id)?;
            let new_val = fast_eval_cps(val_expr, env, &id)?;
            let updated = match target {
                Value::Pair(_, cdr) => Value::Pair(Box::new(new_val), cdr),
                Value::List(elems) if !elems.is_empty() => {
                    let mut new_elems = (*elems).clone();
                    new_elems[0] = new_val;
                    Value::List(Rc::new(new_elems))
                }
                _ => return Err("expected mutable pair".to_string()),
            };
            if let Some(name) = var_name {
                env.update(name, updated)?;
            }
            k(Value::Void)
        }
        TreatedExpr::SetCdr(target_expr, val_expr, var_name) => {
            let id: Cont = Rc::new(|v| Ok(v));
            let target = fast_eval_cps(target_expr, env, &id)?;
            let new_val = fast_eval_cps(val_expr, env, &id)?;
            let updated = match target {
                Value::Pair(car, _) => Value::Pair(car, Box::new(new_val)),
                Value::List(elems) if !elems.is_empty() => {
                    Value::Pair(Box::new(elems[0].clone()), Box::new(new_val))
                }
                _ => return Err("expected mutable pair".to_string()),
            };
            if let Some(name) = var_name {
                env.update(name, updated)?;
            }
            k(Value::Void)
        }
        TreatedExpr::Application(func_expr, arg_exprs) => {
            let env = Rc::clone(env);
            let arg_exprs = arg_exprs.clone();
            let k = Rc::clone(k);
            let id: Cont = Rc::new(|v| Ok(v));
            let func = fast_eval_cps(func_expr, &env, &id)?;
            let then_fn: Rc<dyn Fn(Vec<Value>) -> Result<Value, String>> =
                Rc::new(move |args: Vec<Value>| {
                    apply_function(func.clone(), args, &k)
                });
            eval_args_cps(&arg_exprs, &env, Vec::new(), &then_fn)
        }
    }
}

/// Evaluate argument expressions left-to-right in CPS, then call `then`.
fn eval_args_cps(
    arg_exprs: &[TreatedExpr],
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
    fast_eval_cps(&arg_exprs[0], env, &arg_k)
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
        Value::FastClosure(params, body, closure_env) => {
            let child_env = Env::extend(Rc::clone(&closure_env), &params, args)?;
            let child_env = Rc::new(child_env);
            let id: Cont = Rc::new(|v| Ok(v));
            let mut result = Value::Void;
            for expr in &body {
                result = fast_eval_cps(expr, &child_env, &id)?;
            }
            k(result)
        }
        Value::Closure(params, body, closure_env) => {
            // Handle regular closures too (e.g. from builtins or mixed eval)
            let child_env = Env::extend(Rc::clone(&closure_env), &params, args)?;
            let child_env = Rc::new(child_env);
            let id: Cont = Rc::new(|v| Ok(v));
            let mut result = Value::Void;
            for expr in &body {
                let treated = crate::pretreat::pretreat(expr);
                result = fast_eval_cps(&treated, &child_env, &id)?;
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
            let result = cont_fn(val)?;
            ESCAPED_VALUE.with(|cell| {
                *cell.borrow_mut() = Some(result);
            });
            Err(CONT_ESCAPE.to_string())
        }
        _ => Err(format!("not a procedure: {:?}", func)),
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
            Ok(Value::List(Rc::new(values?)))
        }
    }
}
