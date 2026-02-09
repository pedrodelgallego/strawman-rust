use crate::parser::Expr;

/// A pretreated expression. Special forms are recognized at pretreat time
/// so the fast evaluator doesn't need to match on symbol names.
#[derive(Debug, Clone, PartialEq)]
pub enum TreatedExpr {
    /// Self-evaluating number.
    Number(f64),
    /// Self-evaluating string.
    StringLit(String),
    /// Self-evaluating boolean.
    Boolean(bool),
    /// Variable reference by name (for globals/builtins not in lexical scope).
    VarRef(String),
    /// Lexical variable reference: (depth, offset) for O(1)-ish lookup.
    LexicalRef(usize, usize),
    /// (quote <datum>)
    Quote(Expr),
    /// (if <cond> <then> <else?>)
    If(Box<TreatedExpr>, Box<TreatedExpr>, Option<Box<TreatedExpr>>),
    /// (begin <body...>)
    Begin(Vec<TreatedExpr>),
    /// (define <name> <expr>)
    Define(String, Box<TreatedExpr>),
    /// (define (<name> <params...>) <body...>) — shorthand
    DefineFunc(String, Vec<String>, Vec<TreatedExpr>),
    /// (set! <name> <expr>)
    SetBang(String, Box<TreatedExpr>),
    /// (lambda (<params...>) <body...>)
    Lambda(Vec<String>, Vec<TreatedExpr>),
    /// (and <args...>)
    And(Vec<TreatedExpr>),
    /// (or <args...>)
    Or(Vec<TreatedExpr>),
    /// (let ((<name> <val>)...) <body...>)
    Let(Vec<(String, TreatedExpr)>, Vec<TreatedExpr>),
    /// (let* ((<name> <val>)...) <body...>)
    LetStar(Vec<(String, TreatedExpr)>, Vec<TreatedExpr>),
    /// (letrec ((<name> <val>)...) <body...>)
    Letrec(Vec<(String, TreatedExpr)>, Vec<TreatedExpr>),
    /// (catch <tag-expr> <body>)
    Catch(Box<TreatedExpr>, Box<TreatedExpr>),
    /// (throw <tag-expr> <value-expr>)
    Throw(Box<TreatedExpr>, Box<TreatedExpr>),
    /// (block <name> <body...>)
    Block(String, Vec<TreatedExpr>),
    /// (return-from <name> <expr>)
    ReturnFrom(String, Box<TreatedExpr>),
    /// (unwind-protect <protected> <cleanup...>)
    UnwindProtect(Box<TreatedExpr>, Vec<TreatedExpr>),
    /// (call/cc <proc-expr>)
    CallCC(Box<TreatedExpr>),
    /// (set-car! <target> <value>) — also stores the original target expr for env update
    SetCar(Box<TreatedExpr>, Box<TreatedExpr>, Option<String>),
    /// (set-cdr! <target> <value>) — also stores the original target expr for env update
    SetCdr(Box<TreatedExpr>, Box<TreatedExpr>, Option<String>),
    /// Function application: (<func> <args...>)
    Application(Box<TreatedExpr>, Vec<TreatedExpr>),
}

/// Static lexical environment: a stack of ribs (innermost first).
/// Each rib is a list of parameter names from a lambda/let/etc.
type StaticEnv<'a> = &'a [Vec<String>];

/// Pretreat an expression: analyze it into a TreatedExpr that the fast evaluator
/// can process without needing to match on symbol names.
pub fn pretreat(expr: &Expr) -> TreatedExpr {
    pretreat_inner(expr, &[])
}

fn pretreat_inner(expr: &Expr, senv: StaticEnv) -> TreatedExpr {
    match expr {
        Expr::Number(n) => TreatedExpr::Number(*n),
        Expr::StringLit(s) => TreatedExpr::StringLit(s.clone()),
        Expr::Boolean(b) => TreatedExpr::Boolean(*b),
        Expr::Symbol(name) => lookup_lexical(name, senv),
        Expr::List(elements) => pretreat_list(elements, senv),
    }
}

/// Look up a variable in the static environment. If found, return LexicalRef;
/// otherwise return VarRef for globals/builtins.
fn lookup_lexical(name: &str, senv: StaticEnv) -> TreatedExpr {
    for (depth, rib) in senv.iter().rev().enumerate() {
        if let Some(offset) = rib.iter().position(|n| n == name) {
            return TreatedExpr::LexicalRef(depth, offset);
        }
    }
    TreatedExpr::VarRef(name.to_string())
}

fn pretreat_list(elements: &[Expr], senv: StaticEnv) -> TreatedExpr {
    if elements.is_empty() {
        // Empty list application — will error at runtime, same as eval
        return TreatedExpr::Application(
            Box::new(TreatedExpr::VarRef("()".to_string())),
            vec![],
        );
    }

    match &elements[0] {
        Expr::Symbol(name) => match name.as_str() {
            "quote" => {
                if elements.len() != 2 {
                    // Let fast_eval produce the same error
                    return TreatedExpr::Quote(Expr::List(elements.to_vec()));
                }
                TreatedExpr::Quote(elements[1].clone())
            }
            "if" => {
                if elements.len() < 3 || elements.len() > 4 {
                    // Will error at runtime
                    return TreatedExpr::If(
                        Box::new(TreatedExpr::Boolean(false)),
                        Box::new(TreatedExpr::Boolean(false)),
                        None,
                    );
                }
                let cond = pretreat_inner(&elements[1], senv);
                let then = pretreat_inner(&elements[2], senv);
                let els = if elements.len() == 4 {
                    Some(Box::new(pretreat_inner(&elements[3], senv)))
                } else {
                    None
                };
                TreatedExpr::If(Box::new(cond), Box::new(then), els)
            }
            "begin" => {
                let body: Vec<TreatedExpr> = elements[1..].iter().map(|e| pretreat_inner(e, senv)).collect();
                TreatedExpr::Begin(body)
            }
            "define" => pretreat_define(elements, senv),
            "set!" => {
                if elements.len() != 3 {
                    // Fall through to application which will error
                    return pretreat_application(elements, senv);
                }
                match &elements[1] {
                    Expr::Symbol(var_name) => {
                        TreatedExpr::SetBang(var_name.clone(), Box::new(pretreat_inner(&elements[2], senv)))
                    }
                    _ => pretreat_application(elements, senv),
                }
            }
            "lambda" => pretreat_lambda(elements, senv),
            "and" => {
                let args: Vec<TreatedExpr> = elements[1..].iter().map(|e| pretreat_inner(e, senv)).collect();
                TreatedExpr::And(args)
            }
            "or" => {
                let args: Vec<TreatedExpr> = elements[1..].iter().map(|e| pretreat_inner(e, senv)).collect();
                TreatedExpr::Or(args)
            }
            "let" => pretreat_let(elements, senv),
            "let*" => pretreat_let_star(elements, senv),
            "letrec" => pretreat_letrec(elements, senv),
            "catch" => {
                if elements.len() != 3 {
                    return pretreat_application(elements, senv);
                }
                TreatedExpr::Catch(
                    Box::new(pretreat_inner(&elements[1], senv)),
                    Box::new(pretreat_inner(&elements[2], senv)),
                )
            }
            "throw" => {
                if elements.len() != 3 {
                    return pretreat_application(elements, senv);
                }
                TreatedExpr::Throw(
                    Box::new(pretreat_inner(&elements[1], senv)),
                    Box::new(pretreat_inner(&elements[2], senv)),
                )
            }
            "block" => {
                if elements.len() < 3 {
                    return pretreat_application(elements, senv);
                }
                match &elements[1] {
                    Expr::Symbol(name) => {
                        let body: Vec<TreatedExpr> = elements[2..].iter().map(|e| pretreat_inner(e, senv)).collect();
                        TreatedExpr::Block(name.clone(), body)
                    }
                    _ => pretreat_application(elements, senv),
                }
            }
            "return-from" => {
                if elements.len() != 3 {
                    return pretreat_application(elements, senv);
                }
                match &elements[1] {
                    Expr::Symbol(name) => {
                        TreatedExpr::ReturnFrom(name.clone(), Box::new(pretreat_inner(&elements[2], senv)))
                    }
                    _ => pretreat_application(elements, senv),
                }
            }
            "unwind-protect" => {
                if elements.len() < 2 {
                    return pretreat_application(elements, senv);
                }
                let protected = pretreat_inner(&elements[1], senv);
                let cleanup: Vec<TreatedExpr> = elements[2..].iter().map(|e| pretreat_inner(e, senv)).collect();
                TreatedExpr::UnwindProtect(Box::new(protected), cleanup)
            }
            "call/cc" => {
                if elements.len() != 2 {
                    return pretreat_application(elements, senv);
                }
                TreatedExpr::CallCC(Box::new(pretreat_inner(&elements[1], senv)))
            }
            "set-car!" => pretreat_set_pair(elements, true, senv),
            "set-cdr!" => pretreat_set_pair(elements, false, senv),
            _ => pretreat_application(elements, senv),
        },
        _ => pretreat_application(elements, senv),
    }
}

fn pretreat_define(elements: &[Expr], senv: StaticEnv) -> TreatedExpr {
    if elements.len() < 2 {
        return pretreat_application(elements, senv);
    }
    match &elements[1] {
        Expr::Symbol(var_name) => {
            if elements.len() != 3 {
                return pretreat_application(elements, senv);
            }
            TreatedExpr::Define(var_name.clone(), Box::new(pretreat_inner(&elements[2], senv)))
        }
        Expr::List(name_and_params) => {
            if name_and_params.is_empty() || elements.len() < 3 {
                return pretreat_application(elements, senv);
            }
            match &name_and_params[0] {
                Expr::Symbol(func_name) => {
                    let mut params = Vec::new();
                    for p in &name_and_params[1..] {
                        match p {
                            Expr::Symbol(s) => params.push(s.clone()),
                            _ => return pretreat_application(elements, senv),
                        }
                    }
                    // DefineFunc body is like a lambda body — extend senv with params
                    let mut new_senv = senv.to_vec();
                    new_senv.push(params.clone());
                    let body: Vec<TreatedExpr> = elements[2..].iter().map(|e| pretreat_inner(e, &new_senv)).collect();
                    TreatedExpr::DefineFunc(func_name.clone(), params, body)
                }
                _ => pretreat_application(elements, senv),
            }
        }
        _ => pretreat_application(elements, senv),
    }
}

fn pretreat_lambda(elements: &[Expr], senv: StaticEnv) -> TreatedExpr {
    if elements.len() < 3 {
        return pretreat_application(elements, senv);
    }
    match &elements[1] {
        Expr::List(param_exprs) => {
            let mut params = Vec::new();
            for p in param_exprs {
                match p {
                    Expr::Symbol(s) => params.push(s.clone()),
                    _ => return pretreat_application(elements, senv),
                }
            }
            // Extend static env with this lambda's params
            let mut new_senv = senv.to_vec();
            new_senv.push(params.clone());
            let body: Vec<TreatedExpr> = elements[2..].iter().map(|e| pretreat_inner(e, &new_senv)).collect();
            TreatedExpr::Lambda(params, body)
        }
        _ => pretreat_application(elements, senv),
    }
}

fn pretreat_let(elements: &[Expr], senv: StaticEnv) -> TreatedExpr {
    if elements.len() < 3 {
        return pretreat_application(elements, senv);
    }
    match &elements[1] {
        Expr::List(binding_exprs) => {
            let mut bindings = Vec::new();
            let mut binding_names = Vec::new();
            for b in binding_exprs {
                match b {
                    Expr::List(pair) if pair.len() == 2 => match &pair[0] {
                        Expr::Symbol(name) => {
                            // let binding values are evaluated in the outer senv
                            bindings.push((name.clone(), pretreat_inner(&pair[1], senv)));
                            binding_names.push(name.clone());
                        }
                        _ => return pretreat_application(elements, senv),
                    },
                    _ => return pretreat_application(elements, senv),
                }
            }
            // let body sees all bindings as a new rib
            let mut new_senv = senv.to_vec();
            new_senv.push(binding_names);
            let body: Vec<TreatedExpr> = elements[2..].iter().map(|e| pretreat_inner(e, &new_senv)).collect();
            TreatedExpr::Let(bindings, body)
        }
        _ => pretreat_application(elements, senv),
    }
}

fn pretreat_let_star(elements: &[Expr], senv: StaticEnv) -> TreatedExpr {
    if elements.len() < 3 {
        return pretreat_application(elements, senv);
    }
    match &elements[1] {
        Expr::List(binding_exprs) => {
            let mut bindings = Vec::new();
            let mut current_senv = senv.to_vec();
            for b in binding_exprs {
                match b {
                    Expr::List(pair) if pair.len() == 2 => match &pair[0] {
                        Expr::Symbol(name) => {
                            // let* evaluates each value in current (progressively extended) senv
                            bindings.push((name.clone(), pretreat_inner(&pair[1], &current_senv)));
                            // Each binding creates a new rib for subsequent bindings
                            current_senv.push(vec![name.clone()]);
                        }
                        _ => return pretreat_application(elements, senv),
                    },
                    _ => return pretreat_application(elements, senv),
                }
            }
            // Body sees all let* bindings
            let body: Vec<TreatedExpr> = elements[2..].iter().map(|e| pretreat_inner(e, &current_senv)).collect();
            TreatedExpr::LetStar(bindings, body)
        }
        _ => pretreat_application(elements, senv),
    }
}

fn pretreat_letrec(elements: &[Expr], senv: StaticEnv) -> TreatedExpr {
    if elements.len() < 3 {
        return pretreat_application(elements, senv);
    }
    match &elements[1] {
        Expr::List(binding_exprs) => {
            let mut bindings = Vec::new();
            let mut binding_names = Vec::new();
            for b in binding_exprs {
                match b {
                    Expr::List(pair) if pair.len() == 2 => match &pair[0] {
                        Expr::Symbol(name) => {
                            binding_names.push(name.clone());
                        }
                        _ => return pretreat_application(elements, senv),
                    },
                    _ => return pretreat_application(elements, senv),
                }
            }
            // letrec: all bindings are visible to all init exprs and body
            let mut new_senv = senv.to_vec();
            new_senv.push(binding_names);
            for b in binding_exprs {
                match b {
                    Expr::List(pair) if pair.len() == 2 => match &pair[0] {
                        Expr::Symbol(name) => {
                            bindings.push((name.clone(), pretreat_inner(&pair[1], &new_senv)));
                        }
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }
            }
            let body: Vec<TreatedExpr> = elements[2..].iter().map(|e| pretreat_inner(e, &new_senv)).collect();
            TreatedExpr::Letrec(bindings, body)
        }
        _ => pretreat_application(elements, senv),
    }
}

fn pretreat_set_pair(elements: &[Expr], is_car: bool, senv: StaticEnv) -> TreatedExpr {
    if elements.len() != 3 {
        return pretreat_application(elements, senv);
    }
    let var_name = match &elements[1] {
        Expr::Symbol(s) => Some(s.clone()),
        _ => None,
    };
    let target = pretreat_inner(&elements[1], senv);
    let value = pretreat_inner(&elements[2], senv);
    if is_car {
        TreatedExpr::SetCar(Box::new(target), Box::new(value), var_name)
    } else {
        TreatedExpr::SetCdr(Box::new(target), Box::new(value), var_name)
    }
}

fn pretreat_application(elements: &[Expr], senv: StaticEnv) -> TreatedExpr {
    let func = pretreat_inner(&elements[0], senv);
    let args: Vec<TreatedExpr> = elements[1..].iter().map(|e| pretreat_inner(e, senv)).collect();
    TreatedExpr::Application(Box::new(func), args)
}
