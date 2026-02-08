use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Value represents a Strawman Lisp value.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
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
