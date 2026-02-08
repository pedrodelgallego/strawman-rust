use std::rc::Rc;
use strawman::env::Env;

#[test]
fn set_and_lookup() {
    let env = Env::new();
    env.set("x", 1.0.into());
    let result = env.lookup("x").unwrap();
    assert_eq!(result, 1.0.into());
}

#[test]
fn unbound_lookup_errors() {
    let env = Env::new();
    let result = env.lookup("y");
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), "unbound variable: y");
}

#[test]
fn parent_chain_lookup() {
    let parent = Rc::new(Env::new());
    parent.set("x", 1.0.into());
    let child = Env::with_parent(Rc::clone(&parent));
    let result = child.lookup("x").unwrap();
    assert_eq!(result, 1.0.into());
}

#[test]
fn shadowing() {
    let parent = Rc::new(Env::new());
    parent.set("x", 1.0.into());
    let child = Env::with_parent(Rc::clone(&parent));
    child.set("x", 2.0.into());
    // Child sees its own binding
    assert_eq!(child.lookup("x").unwrap(), 2.0.into());
    // Parent is not affected
    assert_eq!(parent.lookup("x").unwrap(), 1.0.into());
}

#[test]
fn update_existing_binding() {
    let env = Env::new();
    env.set("x", 1.0.into());
    env.update("x", 2.0.into()).unwrap();
    assert_eq!(env.lookup("x").unwrap(), 2.0.into());
}

#[test]
fn update_unbound_errors() {
    let env = Env::new();
    let result = env.update("y", 42.0.into());
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), "cannot set! unbound variable: y");
}

#[test]
fn extend_with_params_and_args() {
    let parent = Rc::new(Env::new());
    let child = Env::extend(
        Rc::clone(&parent),
        &["a".to_string(), "b".to_string()],
        vec![1.0.into(), 2.0.into()],
    )
    .unwrap();
    assert_eq!(child.lookup("a").unwrap(), 1.0.into());
    assert_eq!(child.lookup("b").unwrap(), 2.0.into());
}

#[test]
fn extend_arity_mismatch_errors() {
    let parent = Rc::new(Env::new());
    let result = Env::extend(
        Rc::clone(&parent),
        &["a".to_string(), "b".to_string()],
        vec![1.0.into()],
    );
    assert!(result.is_err());
    let err = result.err().unwrap();
    assert!(err.contains("arity mismatch"));
    assert!(err.contains("expected 2"));
    assert!(err.contains("got 1"));
}

#[test]
fn parent_not_mutated_by_child() {
    let parent = Rc::new(Env::new());
    let child = Env::extend(
        Rc::clone(&parent),
        &["x".to_string()],
        vec![42.0.into()],
    )
    .unwrap();
    // Child has x bound
    assert_eq!(child.lookup("x").unwrap(), 42.0.into());
    // Parent does NOT have x — extend should not mutate the parent
    let result = parent.lookup("x");
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), "unbound variable: x");
}
