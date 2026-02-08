use std::rc::Rc;
use crate::env::{Env, Value};
use crate::parser::Expr;

pub fn straw_eval(expr: &Expr, env: &Rc<Env>) -> Result<Value, String> {
    match expr {
        Expr::Number(n) => Ok(Value::Number(*n)),
        Expr::StringLit(s) => Ok(Value::StringLit(s.clone())),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Symbol(name) => env.lookup(name),
        Expr::List(elements) => {
            if elements.is_empty() {
                return Err("not implemented".to_string());
            }
            match &elements[0] {
                Expr::Symbol(name) if name == "quote" => {
                    if elements.len() != 2 {
                        return Err("quote expects exactly one argument".to_string());
                    }
                    expr_to_value(&elements[1])
                }
                Expr::Symbol(name) if name == "begin" => {
                    let body = &elements[1..];
                    if body.is_empty() {
                        return Ok(Value::Void);
                    }
                    let mut result = Value::Void;
                    for expr in body {
                        result = straw_eval(expr, env)?;
                    }
                    Ok(result)
                }
                Expr::Symbol(name) if name == "define" => {
                    match &elements[1] {
                        Expr::Symbol(var_name) => {
                            if elements.len() != 3 {
                                return Err("define expects exactly 2 arguments".to_string());
                            }
                            let val = straw_eval(&elements[2], env)?;
                            env.set(var_name, val);
                            Ok(Value::Void)
                        }
                        Expr::List(name_and_params) => {
                            // (define (f params...) body...) shorthand
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
                            Ok(Value::Void)
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
                            let val = straw_eval(&elements[2], env)?;
                            env.update(var_name, val)?;
                            Ok(Value::Void)
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
                    Ok(Value::Closure(params, body, Rc::clone(env)))
                }
                Expr::Symbol(name) if name == "and" => {
                    let args = &elements[1..];
                    if args.is_empty() {
                        return Ok(Value::Boolean(true));
                    }
                    for arg in &args[..args.len() - 1] {
                        let val = straw_eval(arg, env)?;
                        if matches!(val, Value::Boolean(false)) {
                            return Ok(Value::Boolean(false));
                        }
                    }
                    straw_eval(&args[args.len() - 1], env)
                }
                Expr::Symbol(name) if name == "or" => {
                    let args = &elements[1..];
                    if args.is_empty() {
                        return Ok(Value::Boolean(false));
                    }
                    for arg in &args[..args.len() - 1] {
                        let val = straw_eval(arg, env)?;
                        if !matches!(val, Value::Boolean(false)) {
                            return Ok(val);
                        }
                    }
                    straw_eval(&args[args.len() - 1], env)
                }
                Expr::Symbol(name) if name == "let" => {
                    if elements.len() < 3 {
                        return Err("let expects at least 2 arguments".to_string());
                    }
                    let bindings_expr = match &elements[1] {
                        Expr::List(b) => b,
                        _ => return Err("let: expected bindings list".to_string()),
                    };
                    // Evaluate all init expressions in the OUTER env (parallel semantics)
                    let mut names = Vec::new();
                    let mut vals = Vec::new();
                    for binding in bindings_expr {
                        match binding {
                            Expr::List(pair) if pair.len() == 2 => {
                                match &pair[0] {
                                    Expr::Symbol(s) => {
                                        let val = straw_eval(&pair[1], env)?;
                                        names.push(s.clone());
                                        vals.push(val);
                                    }
                                    _ => return Err("let: binding name must be a symbol".to_string()),
                                }
                            }
                            _ => return Err("malformed binding".to_string()),
                        }
                    }
                    // Create child env with all bindings
                    let child = Env::with_parent(Rc::clone(env));
                    for (name, val) in names.into_iter().zip(vals) {
                        child.set(&name, val);
                    }
                    let child = Rc::new(child);
                    // Evaluate body (implicit begin)
                    let body = &elements[2..];
                    let mut result = Value::Void;
                    for expr in body {
                        result = straw_eval(expr, &child)?;
                    }
                    Ok(result)
                }
                Expr::Symbol(name) if name == "let*" => {
                    if elements.len() < 3 {
                        return Err("let* expects at least 2 arguments".to_string());
                    }
                    let bindings_expr = match &elements[1] {
                        Expr::List(b) => b,
                        _ => return Err("let*: expected bindings list".to_string()),
                    };
                    // Sequential semantics: each binding sees all previous ones
                    let mut current_env = Rc::clone(env);
                    for binding in bindings_expr {
                        match binding {
                            Expr::List(pair) if pair.len() == 2 => {
                                match &pair[0] {
                                    Expr::Symbol(s) => {
                                        let val = straw_eval(&pair[1], &current_env)?;
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
                    // Evaluate body (implicit begin) in final env
                    let body = &elements[2..];
                    let mut result = Value::Void;
                    for expr in body {
                        result = straw_eval(expr, &current_env)?;
                    }
                    Ok(result)
                }
                Expr::Symbol(name) if name == "letrec" => {
                    if elements.len() < 3 {
                        return Err("letrec expects at least 2 arguments".to_string());
                    }
                    let bindings_expr = match &elements[1] {
                        Expr::List(b) => b,
                        _ => return Err("letrec: expected bindings list".to_string()),
                    };
                    // Create a new env with all names bound to Void (placeholder)
                    let letrec_env = Rc::new(Env::with_parent(Rc::clone(env)));
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
                    // Evaluate init expressions in the letrec env (so lambdas close over it)
                    let mut vals = Vec::new();
                    for binding in bindings_expr {
                        if let Expr::List(pair) = binding {
                            let val = straw_eval(&pair[1], &letrec_env)?;
                            vals.push(val);
                        }
                    }
                    // Update bindings with actual values
                    for (name, val) in names.iter().zip(vals) {
                        letrec_env.set(name, val);
                    }
                    // Evaluate body (implicit begin)
                    let body = &elements[2..];
                    let mut result = Value::Void;
                    for expr in body {
                        result = straw_eval(expr, &letrec_env)?;
                    }
                    Ok(result)
                }
                Expr::Symbol(name) if name == "if" => {
                    if elements.len() < 3 || elements.len() > 4 {
                        return Err("if expects 2 or 3 arguments".to_string());
                    }
                    let condition = straw_eval(&elements[1], env)?;
                    let is_false = matches!(condition, Value::Boolean(false));
                    if !is_false {
                        straw_eval(&elements[2], env)
                    } else if elements.len() == 4 {
                        straw_eval(&elements[3], env)
                    } else {
                        Ok(Value::Void)
                    }
                }
                _ => {
                    // Function application
                    let func = straw_eval(&elements[0], env)?;
                    let args: Result<Vec<Value>, String> = elements[1..]
                        .iter()
                        .map(|arg| straw_eval(arg, env))
                        .collect();
                    let args = args?;
                    match func {
                        Value::Builtin(_, f) => f(args),
                        Value::Closure(params, body, closure_env) => {
                            let child_env = Env::extend(Rc::clone(&closure_env), &params, args)?;
                            let child_env = Rc::new(child_env);
                            let mut result = Value::Void;
                            for expr in &body {
                                result = straw_eval(expr, &child_env)?;
                            }
                            Ok(result)
                        }
                        _ => Err(format!("not a procedure: {:?}", func)),
                    }
                }
            }
        }
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
