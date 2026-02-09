use std::rc::Rc;
use strawman::builtins::default_env;
use strawman::env::Env;
use strawman::eval::straw_eval;
use strawman::parser::{parse, parse_all};

#[test]
fn smoke_ack_3_6_naive() {
    let builder = std::thread::Builder::new().stack_size(256 * 1024 * 1024);
    let handler = builder
        .spawn(|| {
            let env = default_env();
            let child = Rc::new(Env::with_parent(env.clone()));
            let setup = parse_all("(define (ack m n) (if (= m 0) (+ n 1) (if (= n 0) (ack (- m 1) 1) (ack (- m 1) (ack m (- n 1))))))").unwrap();
            for e in &setup { straw_eval(e, &child).unwrap(); }
            let expr = parse("(ack 3 6)").unwrap();
            let val = straw_eval(&expr, &child).unwrap();
            eprintln!("ack(3,6) naive = {:?}", val);
        })
        .unwrap();
    handler.join().unwrap();
}
