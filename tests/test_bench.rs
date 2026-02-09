use strawman::bench::{run_benchmarks, format_table, run_bench_to_writer};
use strawman::builtins::default_env;

/// All bench tests need a large stack because CPS evaluators with
/// Rc<dyn Fn> closures are extremely stack-hungry in debug builds.
/// fibonacci(25) and ackermann(3,5) require 256+ MB.
const BENCH_STACK: usize = 256 * 1024 * 1024;

#[test]
fn benchmark_harness_runs_all_programs() {
    let builder = std::thread::Builder::new().stack_size(BENCH_STACK);
    let handler = builder
        .spawn(|| {
            let env = default_env();
            let results = run_benchmarks(&env);

            // Should have results for all 4 benchmark programs
            assert_eq!(results.len(), 4, "Expected 4 benchmark results");

            // Check that all expected benchmarks are present
            let names: Vec<&str> = results.iter().map(|r| r.name.as_str()).collect();
            assert!(names.contains(&"factorial(20)"), "Missing factorial benchmark");
            assert!(names.contains(&"fibonacci(25)"), "Missing fibonacci benchmark");
            assert!(names.contains(&"ackermann(3,5)"), "Missing ackermann benchmark");
            assert!(
                names.contains(&"map over list"),
                "Missing map benchmark"
            );

            // Check that both evaluators produced the same result for each benchmark
            for result in &results {
                assert_eq!(
                    result.naive_result, result.fast_result,
                    "Result mismatch for {}: naive={}, fast={}",
                    result.name, result.naive_result, result.fast_result
                );
            }

            // Check that times are positive
            for result in &results {
                assert!(
                    result.naive_time_ms > 0.0,
                    "Naive time should be positive for {}",
                    result.name
                );
                assert!(
                    result.fast_time_ms > 0.0,
                    "Fast time should be positive for {}",
                    result.name
                );
            }
        })
        .unwrap();
    handler.join().unwrap();
}

#[test]
fn benchmark_factorial_correctness() {
    let builder = std::thread::Builder::new().stack_size(BENCH_STACK);
    let handler = builder
        .spawn(|| {
            let env = default_env();
            let results = run_benchmarks(&env);
            let fact = results.iter().find(|r| r.name == "factorial(20)").unwrap();
            assert_eq!(fact.naive_result, fact.fast_result);
        })
        .unwrap();
    handler.join().unwrap();
}

#[test]
fn benchmark_table_output() {
    let builder = std::thread::Builder::new().stack_size(BENCH_STACK);
    let handler = builder
        .spawn(|| {
            let env = default_env();
            let results = run_benchmarks(&env);
            let table = format_table(&results);

            // Table should contain header and all benchmark names
            assert!(table.contains("Benchmark"), "Table missing header");
            assert!(table.contains("Naive"), "Table missing Naive column");
            assert!(table.contains("Fast"), "Table missing Fast column");
            assert!(table.contains("factorial(20)"), "Table missing factorial row");
            assert!(table.contains("fibonacci(25)"), "Table missing fibonacci row");
            assert!(table.contains("ackermann(3,5)"), "Table missing ackermann row");
            assert!(table.contains("map over list"), "Table missing map row");
        })
        .unwrap();
    handler.join().unwrap();
}

#[test]
fn run_bench_to_writer_prints_comparison_table() {
    let builder = std::thread::Builder::new().stack_size(BENCH_STACK);
    let handler = builder
        .spawn(|| {
            let mut output = Vec::new();
            run_bench_to_writer(&mut output);
            let table = String::from_utf8(output).unwrap();

            // Should contain the header row with column names
            assert!(table.contains("Benchmark"), "Output missing Benchmark header");
            assert!(table.contains("Naive (ms)"), "Output missing Naive column");
            assert!(table.contains("Fast (ms)"), "Output missing Fast column");
            assert!(table.contains("Speedup"), "Output missing Speedup column");

            // Should contain all four benchmark rows
            assert!(table.contains("factorial(20)"), "Output missing factorial row");
            assert!(table.contains("fibonacci(25)"), "Output missing fibonacci row");
            assert!(table.contains("ackermann(3,5)"), "Output missing ackermann row");
            assert!(table.contains("map over list"), "Output missing map row");

            // Each row should have a speedup value (ends with 'x')
            for line in table.lines().skip(2) {
                // skip header + separator
                if !line.trim().is_empty() {
                    assert!(line.contains('x'), "Row missing speedup: {}", line);
                }
            }
        })
        .unwrap();
    handler.join().unwrap();
}
