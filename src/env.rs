use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Value represents a Strawman Lisp value.
#[derive(Clone)]
pub enum Value {
    Number(f64),
    StringLit(String),
    Boolean(bool),
    Symbol(String),
    List(Vec<Value>),
    Pair(Box<Value>, Box<Value>),
    Builtin(String, fn(Vec<Value>) -> Result<Value, String>),
    Closure(Vec<String>, Vec<crate::parser::Expr>, Rc<Env>),
    Void,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::StringLit(a), Value::StringLit(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Pair(a1, a2), Value::Pair(b1, b2)) => a1 == b1 && a2 == b2,
            (Value::Builtin(a, _), Value::Builtin(b, _)) => a == b,
            (Value::Closure(_, _, _), Value::Closure(_, _, _)) => false,
            (Value::Void, Value::Void) => true,
            _ => false,
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "Number({n})"),
            Value::StringLit(s) => write!(f, "StringLit({s:?})"),
            Value::Boolean(b) => write!(f, "Boolean({b})"),
            Value::Symbol(s) => write!(f, "Symbol({s:?})"),
            Value::List(l) => write!(f, "List({l:?})"),
            Value::Pair(a, b) => write!(f, "Pair({a:?}, {b:?})"),
            Value::Builtin(name, _) => write!(f, "Builtin({name:?})"),
            Value::Closure(params, _, _) => write!(f, "Closure({params:?})"),
            Value::Void => write!(f, "Void"),
        }
    }
}

impl From<f64> for Value {
    fn from(n: f64) -> Self {
        Value::Number(n)
    }
}

/// Environment for variable bindings with lexical scoping.
pub struct Env {
    bindings: RefCell<HashMap<String, Value>>,
    parent: Option<Rc<Env>>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            bindings: RefCell::new(HashMap::new()),
            parent: None,
        }
    }

    pub fn with_parent(parent: Rc<Env>) -> Self {
        Env {
            bindings: RefCell::new(HashMap::new()),
            parent: Some(parent),
        }
    }

    pub fn set(&self, name: &str, value: Value) {
        self.bindings.borrow_mut().insert(name.to_string(), value);
    }

    pub fn update(&self, name: &str, value: Value) -> Result<(), String> {
        if self.bindings.borrow().contains_key(name) {
            self.bindings.borrow_mut().insert(name.to_string(), value);
            return Ok(());
        }
        match &self.parent {
            Some(parent) => parent.update(name, value),
            None => Err(format!("cannot set! unbound variable: {}", name)),
        }
    }

    pub fn extend(parent: Rc<Env>, params: &[String], args: Vec<Value>) -> Result<Self, String> {
        if params.len() != args.len() {
            return Err(format!(
                "arity mismatch: expected {}, got {}",
                params.len(),
                args.len()
            ));
        }
        let child = Env::with_parent(parent);
        for (name, value) in params.iter().zip(args) {
            child.set(name, value);
        }
        Ok(child)
    }

    pub fn lookup(&self, name: &str) -> Result<Value, String> {
        match self.bindings.borrow().get(name) {
            Some(value) => Ok(value.clone()),
            None => match &self.parent {
                Some(parent) => parent.lookup(name),
                None => Err(format!("unbound variable: {}", name)),
            },
        }
    }
}
