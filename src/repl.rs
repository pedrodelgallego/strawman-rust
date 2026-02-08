use std::io::{BufRead, Write};

use crate::builtins::default_env;
use crate::env::Value;
use crate::eval::straw_eval;
use crate::parser::{parse, parse_all, Expr};

/// Format a Value for REPL display.
fn format_value(val: &Value) -> String {
    match val {
        Value::Number(n) => {
            if *n == n.floor() && n.is_finite() {
                format!("{}", *n as i64)
            } else {
                format!("{}", n)
            }
        }
        Value::StringLit(s) => format!("{:?}", s),
        Value::Boolean(b) => {
            if *b { "#t".to_string() } else { "#f".to_string() }
        }
        Value::Symbol(s) => s.clone(),
        Value::List(elems) => {
            if elems.is_empty() {
                "()".to_string()
            } else {
                let inner: Vec<String> = elems.iter().map(format_value).collect();
                format!("({})", inner.join(" "))
            }
        }
        Value::Pair(a, b) => format!("({} . {})", format_value(a), format_value(b)),
        Value::Vector(v) => {
            let borrowed = v.borrow();
            let inner: Vec<String> = borrowed.iter().map(format_value).collect();
            format!("#({})", inner.join(" "))
        }
        Value::Builtin(name, _) => format!("#<procedure:{}>", name),
        Value::Closure(_, _, _) | Value::FastClosure(_, _, _) => "#<closure>".to_string(),
        Value::Continuation(_) => "#<continuation>".to_string(),
        Value::Void => String::new(),
    }
}

/// Count net open parentheses in a string (open minus close).
fn paren_depth(s: &str) -> i32 {
    let mut depth: i32 = 0;
    let mut in_string = false;
    let mut escape = false;
    for ch in s.chars() {
        if escape {
            escape = false;
            continue;
        }
        if in_string {
            if ch == '\\' {
                escape = true;
            } else if ch == '"' {
                in_string = false;
            }
            continue;
        }
        match ch {
            '"' => in_string = true,
            ';' => break, // rest of line is comment
            '(' => depth += 1,
            ')' => depth -= 1,
            _ => {}
        }
    }
    depth
}

/// Check if an expression is an exit/quit command: (exit) or (quit).
fn is_exit_command(expr: &Expr) -> bool {
    if let Expr::List(elems) = expr {
        if elems.len() == 1 {
            if let Expr::Symbol(name) = &elems[0] {
                return name == "exit" || name == "quit";
            }
        }
    }
    false
}

/// Execute a .straw file: read, parse all expressions, evaluate in order, print non-void results.
pub fn run_file<W: Write>(path: &str, output: &mut W) -> Result<(), String> {
    let source = std::fs::read_to_string(path).map_err(|e| e.to_string())?;
    let exprs = parse_all(&source)?;
    let env = default_env();
    for expr in &exprs {
        let val = straw_eval(expr, &env)?;
        if !matches!(val, Value::Void) {
            let _ = writeln!(output, "{}", format_value(&val));
        }
    }
    Ok(())
}

/// Run the REPL, reading from `input` and writing to `output`.
pub fn run_repl<R: BufRead, W: Write>(input: &mut R, output: &mut W) {
    let env = default_env();
    let prompt = "strawman> ";
    let continuation = "...    ";

    loop {
        let _ = write!(output, "{}", prompt);
        let _ = output.flush();

        let mut line = String::new();
        match input.read_line(&mut line) {
            Ok(0) => break, // EOF
            Ok(_) => {}
            Err(_) => break,
        }

        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        // Accumulate lines until parens are balanced
        let mut buffer = line.clone();
        let mut depth = paren_depth(&buffer);

        while depth > 0 {
            let _ = write!(output, "{}", continuation);
            let _ = output.flush();

            let mut next_line = String::new();
            match input.read_line(&mut next_line) {
                Ok(0) => break, // EOF mid-expression
                Ok(_) => {}
                Err(_) => break,
            }
            depth += paren_depth(&next_line);
            buffer.push_str(&next_line);
        }

        let input_str = buffer.trim();
        if input_str.is_empty() {
            continue;
        }

        match parse(input_str) {
            Ok(ref expr) if is_exit_command(expr) => break,
            Ok(expr) => match straw_eval(&expr, &env) {
                Ok(Value::Void) => {}
                Ok(val) => {
                    let _ = writeln!(output, "{}", format_value(&val));
                }
                Err(e) => {
                    let _ = writeln!(output, "Error: {}", e);
                }
            },
            Err(e) => {
                let _ = writeln!(output, "Error: {}", e);
            }
        }
    }
}
