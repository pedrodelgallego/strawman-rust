use std::rc::Rc;
use std::time::Instant;

use crate::env::Env;
use crate::eval::straw_eval;
use crate::fast_eval::fast_eval;
use crate::parser::{parse, parse_all};
use crate::pretreat::pretreat;
use crate::repl::format_value;

pub struct BenchResult {
    pub name: String,
    pub naive_time_ms: f64,
    pub fast_time_ms: f64,
    pub naive_result: String,
    pub fast_result: String,
}

struct BenchProgram {
    name: &'static str,
    setup: &'static str,
    expr: &'static str,
}

const BENCHMARKS: &[BenchProgram] = &[
    BenchProgram {
        name: "factorial(20)",
        setup: "(define (fact n) (if (<= n 1) 1 (* n (fact (- n 1)))))",
        expr: "(fact 20)",
    },
    BenchProgram {
        name: "fibonacci(25)",
        setup: "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))",
        expr: "(fib 25)",
    },
    BenchProgram {
        name: "ackermann(3,5)",
        setup: "(define (ack m n) (if (= m 0) (+ n 1) (if (= n 0) (ack (- m 1) 1) (ack (- m 1) (ack m (- n 1))))))",
        expr: "(ack 3 5)",
    },
    BenchProgram {
        name: "map over list",
        setup: "(define (my-map f lst) (if (null? lst) (quote ()) (cons (f (car lst)) (my-map f (cdr lst)))))",
        expr: "(my-map (lambda (x) (* x x)) (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))",
    },
];

/// Stack size for benchmark threads (128 MB) — CPS evaluators with
/// `Rc<dyn Fn>` closures are extremely stack-hungry in debug builds.
const BENCH_STACK_SIZE: usize = 128 * 1024 * 1024;

/// Run all benchmarks, comparing naive eval vs fast eval.
/// Caller should ensure sufficient stack size (see `BENCH_STACK_SIZE`).
pub fn run_benchmarks(env: &Rc<Env>) -> Vec<BenchResult> {
    let mut results = Vec::new();

    for bench in BENCHMARKS {
        let naive_env = Rc::new(Env::with_parent(env.clone()));
        let fast_env = Rc::new(Env::with_parent(env.clone()));

        // Run setup in both envs
        let setup_exprs = parse_all(bench.setup).unwrap();
        for expr in &setup_exprs {
            straw_eval(expr, &naive_env).unwrap();
            let treated = pretreat(expr);
            fast_eval(&treated, &fast_env).unwrap();
        }

        let expr = parse(bench.expr).unwrap();
        let treated = pretreat(&expr);

        // Time naive eval
        let start = Instant::now();
        let naive_val = straw_eval(&expr, &naive_env).unwrap();
        let naive_time = start.elapsed();

        // Time fast eval
        let start = Instant::now();
        let fast_val = fast_eval(&treated, &fast_env).unwrap();
        let fast_time = start.elapsed();

        results.push(BenchResult {
            name: bench.name.to_string(),
            naive_time_ms: naive_time.as_secs_f64() * 1000.0,
            fast_time_ms: fast_time.as_secs_f64() * 1000.0,
            naive_result: format_value(&naive_val),
            fast_result: format_value(&fast_val),
        });
    }

    results
}

pub fn format_table(results: &[BenchResult]) -> String {
    let mut out = String::new();
    out.push_str(&format!(
        "{:<20} {:>12} {:>12} {:>10}\n",
        "Benchmark", "Naive (ms)", "Fast (ms)", "Speedup"
    ));
    out.push_str(&format!("{:-<20} {:->12} {:->12} {:->10}\n", "", "", "", ""));
    for r in results {
        let speedup = if r.fast_time_ms > 0.0 {
            r.naive_time_ms / r.fast_time_ms
        } else {
            f64::INFINITY
        };
        out.push_str(&format!(
            "{:<20} {:>12.3} {:>12.3} {:>9.2}x\n",
            r.name, r.naive_time_ms, r.fast_time_ms, speedup
        ));
    }
    out
}
