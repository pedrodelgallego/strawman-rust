use strawman::parser::parse;
use strawman::pretreat::{pretreat, TreatedExpr};

/// Helper: parse and pretreat an expression.
fn pt(input: &str) -> TreatedExpr {
    let expr = parse(input).unwrap();
    pretreat(&expr)
}

// === E6.2 Test Matrix ===

#[test]
fn pretreat_local_var_depth0_offset0() {
    // (lambda (x) x) — x should be LexicalRef(0, 0)
    let treated = pt("(lambda (x) x)");
    match treated {
        TreatedExpr::Lambda(params, body) => {
            assert_eq!(params, vec!["x"]);
            assert_eq!(body.len(), 1);
            assert_eq!(body[0], TreatedExpr::LexicalRef(0, 0));
        }
        other => panic!("expected Lambda, got {:?}", other),
    }
}

#[test]
fn pretreat_free_var_one_level_depth1_offset0() {
    // (lambda (x) (lambda (y) x)) — inner x should be LexicalRef(1, 0)
    let treated = pt("(lambda (x) (lambda (y) x))");
    match treated {
        TreatedExpr::Lambda(outer_params, outer_body) => {
            assert_eq!(outer_params, vec!["x"]);
            assert_eq!(outer_body.len(), 1);
            match &outer_body[0] {
                TreatedExpr::Lambda(inner_params, inner_body) => {
                    assert_eq!(inner_params, &vec!["y".to_string()]);
                    assert_eq!(inner_body.len(), 1);
                    assert_eq!(inner_body[0], TreatedExpr::LexicalRef(1, 0));
                }
                other => panic!("expected inner Lambda, got {:?}", other),
            }
        }
        other => panic!("expected Lambda, got {:?}", other),
    }
}

#[test]
fn pretreat_multiple_params_offset1() {
    // (lambda (a b c) b) — b should be LexicalRef(0, 1)
    let treated = pt("(lambda (a b c) b)");
    match treated {
        TreatedExpr::Lambda(params, body) => {
            assert_eq!(params, vec!["a", "b", "c"]);
            assert_eq!(body.len(), 1);
            assert_eq!(body[0], TreatedExpr::LexicalRef(0, 1));
        }
        other => panic!("expected Lambda, got {:?}", other),
    }
}

// === E6.2 Acceptance Criteria ===

#[test]
fn pretreat_acceptance_inner_lambda_lexical_refs() {
    // (lambda (x) (lambda (y) (+ x y)))
    // x in inner body: depth=1, offset=0
    // y in inner body: depth=0, offset=0
    // + is a global: VarRef("+")
    let treated = pt("(lambda (x) (lambda (y) (+ x y)))");
    match treated {
        TreatedExpr::Lambda(_, outer_body) => match &outer_body[0] {
            TreatedExpr::Lambda(_, inner_body) => match &inner_body[0] {
                TreatedExpr::Application(func, args) => {
                    assert_eq!(**func, TreatedExpr::VarRef("+".to_string()));
                    assert_eq!(args.len(), 2);
                    assert_eq!(args[0], TreatedExpr::LexicalRef(1, 0), "x: depth=1, offset=0");
                    assert_eq!(args[1], TreatedExpr::LexicalRef(0, 0), "y: depth=0, offset=0");
                }
                other => panic!("expected Application, got {:?}", other),
            },
            other => panic!("expected inner Lambda, got {:?}", other),
        },
        other => panic!("expected Lambda, got {:?}", other),
    }
}

// === Additional coverage: globals stay as VarRef ===

#[test]
fn pretreat_global_stays_varref() {
    // (lambda (x) +) — + is not in static env, should remain VarRef
    let treated = pt("(lambda (x) +)");
    match treated {
        TreatedExpr::Lambda(_, body) => {
            assert_eq!(body[0], TreatedExpr::VarRef("+".to_string()));
        }
        other => panic!("expected Lambda, got {:?}", other),
    }
}

#[test]
fn pretreat_top_level_var_is_varref() {
    // x at top level (no enclosing lambda) → VarRef
    let treated = pt("x");
    assert_eq!(treated, TreatedExpr::VarRef("x".to_string()));
}
