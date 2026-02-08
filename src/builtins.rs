use std::cell::RefCell;
use std::rc::Rc;
use crate::env::{Env, Value};

pub fn default_env() -> Rc<Env> {
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
            return Err("arity mismatch: expected at least 1, got 0".to_string());
        }
        let first = match &args[0] {
            Value::Number(n) => *n,
            _ => return Err("expected number".to_string()),
        };
        if args.len() == 1 {
            return Ok(Value::Number(-first));
        }
        let mut result = first;
        for arg in &args[1..] {
            match arg {
                Value::Number(n) => result -= n,
                _ => return Err("expected number".to_string()),
            }
        }
        Ok(Value::Number(result))
    }));

    env.set("/", Value::Builtin("/".to_string(), |args| {
        if args.len() < 2 {
            return Err("arity mismatch: expected at least 2, got ".to_string() + &args.len().to_string());
        }
        let first = match &args[0] {
            Value::Number(n) => *n,
            _ => return Err("expected number".to_string()),
        };
        let mut result = first;
        for arg in &args[1..] {
            match arg {
                Value::Number(n) => {
                    if *n == 0.0 {
                        return Err("division by zero".to_string());
                    }
                    result /= n;
                }
                _ => return Err("expected number".to_string()),
            }
        }
        Ok(Value::Number(result))
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

    env.set("<", Value::Builtin("<".to_string(), |args| {
        if args.len() != 2 {
            return Err(format!("arity mismatch: expected 2, got {}", args.len()));
        }
        let a = match &args[0] {
            Value::Number(n) => *n,
            _ => return Err("expected number".to_string()),
        };
        let b = match &args[1] {
            Value::Number(n) => *n,
            _ => return Err("expected number".to_string()),
        };
        Ok(Value::Boolean(a < b))
    }));

    env.set(">", Value::Builtin(">".to_string(), |args| {
        if args.len() != 2 {
            return Err(format!("arity mismatch: expected 2, got {}", args.len()));
        }
        let a = match &args[0] {
            Value::Number(n) => *n,
            _ => return Err("expected number".to_string()),
        };
        let b = match &args[1] {
            Value::Number(n) => *n,
            _ => return Err("expected number".to_string()),
        };
        Ok(Value::Boolean(a > b))
    }));

    env.set("<=", Value::Builtin("<=".to_string(), |args| {
        if args.len() != 2 {
            return Err(format!("arity mismatch: expected 2, got {}", args.len()));
        }
        let a = match &args[0] {
            Value::Number(n) => *n,
            _ => return Err("expected number".to_string()),
        };
        let b = match &args[1] {
            Value::Number(n) => *n,
            _ => return Err("expected number".to_string()),
        };
        Ok(Value::Boolean(a <= b))
    }));

    env.set(">=", Value::Builtin(">=".to_string(), |args| {
        if args.len() != 2 {
            return Err(format!("arity mismatch: expected 2, got {}", args.len()));
        }
        let a = match &args[0] {
            Value::Number(n) => *n,
            _ => return Err("expected number".to_string()),
        };
        let b = match &args[1] {
            Value::Number(n) => *n,
            _ => return Err("expected number".to_string()),
        };
        Ok(Value::Boolean(a >= b))
    }));

    env.set("=", Value::Builtin("=".to_string(), |args| {
        if args.len() != 2 {
            return Err(format!("arity mismatch: expected 2, got {}", args.len()));
        }
        let a = match &args[0] {
            Value::Number(n) => *n,
            _ => return Err("expected number".to_string()),
        };
        let b = match &args[1] {
            Value::Number(n) => *n,
            _ => return Err("expected number".to_string()),
        };
        Ok(Value::Boolean(a == b))
    }));

    env.set("eq?", Value::Builtin("eq?".to_string(), |args| {
        if args.len() != 2 {
            return Err(format!("arity mismatch: expected 2, got {}", args.len()));
        }
        let result = match (&args[0], &args[1]) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::StringLit(a), Value::StringLit(b)) => a == b,
            (Value::List(a), Value::List(b)) => Rc::ptr_eq(a, b),
            (Value::Void, Value::Void) => true,
            _ => false,
        };
        Ok(Value::Boolean(result))
    }));

    env.set("equal?", Value::Builtin("equal?".to_string(), |args| {
        if args.len() != 2 {
            return Err(format!("arity mismatch: expected 2, got {}", args.len()));
        }
        Ok(Value::Boolean(args[0] == args[1]))
    }));

    env.set("list", Value::Builtin("list".to_string(), |args| {
        Ok(Value::List(Rc::new(args)))
    }));

    env.set("cons", Value::Builtin("cons".to_string(), |args| {
        if args.len() != 2 {
            return Err(format!("arity mismatch: expected 2, got {}", args.len()));
        }
        let head = args[0].clone();
        let tail = args[1].clone();
        match tail {
            Value::List(elems) => {
                let mut new_elems = (*elems).clone();
                new_elems.insert(0, head);
                Ok(Value::List(Rc::new(new_elems)))
            }
            _ => Ok(Value::Pair(Box::new(head), Box::new(tail))),
        }
    }));

    env.set("car", Value::Builtin("car".to_string(), |args| {
        if args.len() != 1 {
            return Err(format!("arity mismatch: expected 1, got {}", args.len()));
        }
        match &args[0] {
            Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
            Value::Pair(head, _) => Ok(*head.clone()),
            _ => Err("car: expected pair".to_string()),
        }
    }));

    env.set("cdr", Value::Builtin("cdr".to_string(), |args| {
        if args.len() != 1 {
            return Err(format!("arity mismatch: expected 1, got {}", args.len()));
        }
        match &args[0] {
            Value::List(elems) if !elems.is_empty() => Ok(Value::List(Rc::new(elems[1..].to_vec()))),
            Value::Pair(_, tail) => Ok(*tail.clone()),
            _ => Err("cdr: expected pair".to_string()),
        }
    }));

    env.set("null?", Value::Builtin("null?".to_string(), |args| {
        if args.len() != 1 {
            return Err(format!("arity mismatch: expected 1, got {}", args.len()));
        }
        match &args[0] {
            Value::List(elems) if elems.is_empty() => Ok(Value::Boolean(true)),
            _ => Ok(Value::Boolean(false)),
        }
    }));

    env.set("pair?", Value::Builtin("pair?".to_string(), |args| {
        if args.len() != 1 {
            return Err(format!("arity mismatch: expected 1, got {}", args.len()));
        }
        match &args[0] {
            Value::List(elems) if !elems.is_empty() => Ok(Value::Boolean(true)),
            Value::Pair(_, _) => Ok(Value::Boolean(true)),
            _ => Ok(Value::Boolean(false)),
        }
    }));

    env.set("not", Value::Builtin("not".to_string(), |args| {
        if args.len() != 1 {
            return Err(format!("arity mismatch: expected 1, got {}", args.len()));
        }
        match &args[0] {
            Value::Boolean(false) => Ok(Value::Boolean(true)),
            _ => Ok(Value::Boolean(false)),
        }
    }));

    env.set("number?", Value::Builtin("number?".to_string(), |args| {
        if args.len() != 1 {
            return Err(format!("arity mismatch: expected 1, got {}", args.len()));
        }
        Ok(Value::Boolean(matches!(&args[0], Value::Number(_))))
    }));

    env.set("string?", Value::Builtin("string?".to_string(), |args| {
        if args.len() != 1 {
            return Err(format!("arity mismatch: expected 1, got {}", args.len()));
        }
        Ok(Value::Boolean(matches!(&args[0], Value::StringLit(_))))
    }));

    env.set("symbol?", Value::Builtin("symbol?".to_string(), |args| {
        if args.len() != 1 {
            return Err(format!("arity mismatch: expected 1, got {}", args.len()));
        }
        Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
    }));

    env.set("boolean?", Value::Builtin("boolean?".to_string(), |args| {
        if args.len() != 1 {
            return Err(format!("arity mismatch: expected 1, got {}", args.len()));
        }
        Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
    }));

    env.set("procedure?", Value::Builtin("procedure?".to_string(), |args| {
        if args.len() != 1 {
            return Err(format!("arity mismatch: expected 1, got {}", args.len()));
        }
        Ok(Value::Boolean(matches!(&args[0], Value::Builtin(_, _) | Value::Closure(_, _, _) | Value::FastClosure(_, _, _))))
    }));

    env.set("display", Value::Builtin("display".to_string(), |args| {
        use std::io::Write;
        if args.len() != 1 {
            return Err(format!("arity mismatch: expected 1, got {}", args.len()));
        }
        let mut out = std::io::stdout();
        match &args[0] {
            Value::StringLit(s) => { let _ = write!(out, "{}", s); }
            Value::Number(n) => {
                if *n == n.floor() && n.is_finite() {
                    let _ = write!(out, "{}", *n as i64);
                } else {
                    let _ = write!(out, "{}", n);
                }
            }
            Value::Boolean(b) => { let _ = write!(out, "{}", if *b { "#t" } else { "#f" }); }
            Value::Symbol(s) => { let _ = write!(out, "{}", s); }
            Value::List(elems) if elems.is_empty() => { let _ = write!(out, "()"); }
            _ => { let _ = write!(out, "{:?}", args[0]); }
        }
        let _ = out.flush();
        Ok(Value::Void)
    }));

    env.set("newline", Value::Builtin("newline".to_string(), |args| {
        use std::io::Write;
        if !args.is_empty() {
            return Err(format!("arity mismatch: expected 0, got {}", args.len()));
        }
        let mut out = std::io::stdout();
        let _ = write!(out, "\n");
        let _ = out.flush();
        Ok(Value::Void)
    }));

    env.set("mod", Value::Builtin("mod".to_string(), |args| {
        if args.len() != 2 {
            return Err(format!("arity mismatch: expected 2, got {}", args.len()));
        }
        let a = match &args[0] {
            Value::Number(n) => *n,
            _ => return Err("expected number".to_string()),
        };
        let b = match &args[1] {
            Value::Number(n) => *n,
            _ => return Err("expected number".to_string()),
        };
        if b == 0.0 {
            return Err("division by zero".to_string());
        }
        Ok(Value::Number(a % b))
    }));

    env.set("make-vector", Value::Builtin("make-vector".to_string(), |args| {
        if args.is_empty() || args.len() > 2 {
            return Err(format!("arity mismatch: expected 1 or 2, got {}", args.len()));
        }
        let size = match &args[0] {
            Value::Number(n) => *n as usize,
            _ => return Err("expected number".to_string()),
        };
        let fill = if args.len() == 2 {
            args[1].clone()
        } else {
            Value::Number(0.0)
        };
        Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; size]))))
    }));

    env.set("vector-ref", Value::Builtin("vector-ref".to_string(), |args| {
        if args.len() != 2 {
            return Err(format!("arity mismatch: expected 2, got {}", args.len()));
        }
        let vec = match &args[0] {
            Value::Vector(v) => v.clone(),
            _ => return Err("expected vector".to_string()),
        };
        let idx = match &args[1] {
            Value::Number(n) => *n as usize,
            _ => return Err("expected number".to_string()),
        };
        let borrowed = vec.borrow();
        if idx >= borrowed.len() {
            return Err("index out of range".to_string());
        }
        Ok(borrowed[idx].clone())
    }));

    env.set("vector-set!", Value::Builtin("vector-set!".to_string(), |args| {
        if args.len() != 3 {
            return Err(format!("arity mismatch: expected 3, got {}", args.len()));
        }
        let vec = match &args[0] {
            Value::Vector(v) => v.clone(),
            _ => return Err("expected vector".to_string()),
        };
        let idx = match &args[1] {
            Value::Number(n) => *n as usize,
            _ => return Err("expected number".to_string()),
        };
        let val = args[2].clone();
        let mut borrowed = vec.borrow_mut();
        if idx >= borrowed.len() {
            return Err("index out of range".to_string());
        }
        borrowed[idx] = val;
        Ok(Value::Void)
    }));

    env.set("vector-length", Value::Builtin("vector-length".to_string(), |args| {
        if args.len() != 1 {
            return Err(format!("arity mismatch: expected 1, got {}", args.len()));
        }
        match &args[0] {
            Value::Vector(v) => Ok(Value::Number(v.borrow().len() as f64)),
            _ => Err("expected vector".to_string()),
        }
    }));

    env.set("vector?", Value::Builtin("vector?".to_string(), |args| {
        if args.len() != 1 {
            return Err(format!("arity mismatch: expected 1, got {}", args.len()));
        }
        Ok(Value::Boolean(matches!(&args[0], Value::Vector(_))))
    }));

    env
}
