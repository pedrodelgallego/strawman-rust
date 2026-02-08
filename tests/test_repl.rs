use std::io::Write;
use strawman::repl::{run_file, run_repl};

#[test]
fn repl_simple_expression_prints_result() {
    let input = b"42\n";
    let mut output = Vec::new();
    run_repl(&mut &input[..], &mut output);
    let output_str = String::from_utf8(output).unwrap();
    // REPL prints: "strawman> 42\nstrawman> " (prompt, result, next prompt, then EOF)
    assert_eq!(output_str, "strawman> 42\nstrawman> ");
}

#[test]
fn repl_computation_prints_result() {
    let input = b"(+ 1 2)\n";
    let mut output = Vec::new();
    run_repl(&mut &input[..], &mut output);
    let output_str = String::from_utf8(output).unwrap();
    assert_eq!(output_str, "strawman> 3\nstrawman> ");
}

#[test]
fn repl_define_then_use_across_inputs() {
    let input = b"(define x 5)\nx\n";
    let mut output = Vec::new();
    run_repl(&mut &input[..], &mut output);
    let output_str = String::from_utf8(output).unwrap();
    // First input: (define x 5) → prints nothing (Void), just prompt
    // Second input: x → prints 5
    // Then final prompt before EOF
    assert_eq!(output_str, "strawman> strawman> 5\nstrawman> ");
}

#[test]
fn repl_define_then_use_in_expression() {
    // Acceptance criteria: (define x 10) then (+ x 5) → first prints nothing, second prints 15
    let input = b"(define x 10)\n(+ x 5)\n";
    let mut output = Vec::new();
    run_repl(&mut &input[..], &mut output);
    let output_str = String::from_utf8(output).unwrap();
    assert_eq!(output_str, "strawman> strawman> 15\nstrawman> ");
}

#[test]
fn repl_error_recovery_continues_after_error() {
    // Error recovery: (/ 1 0) causes "Error: division by zero", then 42 prints "42"
    let input = b"(/ 1 0)\n42\n";
    let mut output = Vec::new();
    run_repl(&mut &input[..], &mut output);
    let output_str = String::from_utf8(output).unwrap();
    // First input: (/ 1 0) → "Error: division by zero"
    // Second input: 42 → "42"
    // Then final prompt before EOF
    assert_eq!(
        output_str,
        "strawman> Error: division by zero\nstrawman> 42\nstrawman> "
    );
}

#[test]
fn repl_unbound_variable_error() {
    // Unbound error: `foo` → prints "Error: unbound variable: foo"
    let input = b"foo\n";
    let mut output = Vec::new();
    run_repl(&mut &input[..], &mut output);
    let output_str = String::from_utf8(output).unwrap();
    assert_eq!(
        output_str,
        "strawman> Error: unbound variable: foo\nstrawman> "
    );
}

#[test]
fn repl_exit_terminates() {
    // (exit) should terminate the REPL — no further input is processed
    let input = b"(exit)\n42\n";
    let mut output = Vec::new();
    run_repl(&mut &input[..], &mut output);
    let output_str = String::from_utf8(output).unwrap();
    // REPL prints prompt, then exits. The "42" line should never be processed.
    assert_eq!(output_str, "strawman> ");
}

#[test]
fn repl_quit_terminates() {
    // (quit) should terminate the REPL — no further input is processed
    let input = b"(quit)\n42\n";
    let mut output = Vec::new();
    run_repl(&mut &input[..], &mut output);
    let output_str = String::from_utf8(output).unwrap();
    // REPL prints prompt, then exits. The "42" line should never be processed.
    assert_eq!(output_str, "strawman> ");
}

#[test]
fn repl_eof_terminates() {
    // EOF (Ctrl-D) should terminate the REPL cleanly
    let input = b"";
    let mut output = Vec::new();
    run_repl(&mut &input[..], &mut output);
    let output_str = String::from_utf8(output).unwrap();
    // REPL prints the initial prompt, then hits EOF and exits
    assert_eq!(output_str, "strawman> ");
}

#[test]
fn repl_multiline_input_balanced_parens() {
    // Multi-line: (+ 1\n   2) should accumulate lines until parens are balanced, then print 3
    let input = b"(+ 1\n   2)\n";
    let mut output = Vec::new();
    run_repl(&mut &input[..], &mut output);
    let output_str = String::from_utf8(output).unwrap();
    // First line "(+ 1" has unbalanced parens → REPL shows continuation prompt
    // Second line "   2)" completes the expression → evaluates and prints 3
    assert_eq!(output_str, "strawman> ...    3\nstrawman> ");
}

#[test]
fn run_file_executes_expressions_and_prints_results() {
    // A .straw file with simple expressions should be evaluated and results printed
    let dir = std::env::temp_dir();
    let file_path = dir.join("test_run_file.straw");
    {
        let mut f = std::fs::File::create(&file_path).unwrap();
        writeln!(f, "42").unwrap();
        writeln!(f, "(+ 1 2)").unwrap();
    }
    let mut output = Vec::new();
    run_file(file_path.to_str().unwrap(), &mut output).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    // File execution prints each non-void result on its own line
    assert_eq!(output_str, "42\n3\n");
    std::fs::remove_file(&file_path).ok();
}

#[test]
fn run_file_define_then_use() {
    // Define a variable in a file, then use it — define prints nothing, use prints value
    let dir = std::env::temp_dir();
    let file_path = dir.join("test_run_file_define.straw");
    {
        let mut f = std::fs::File::create(&file_path).unwrap();
        writeln!(f, "(define x 5)").unwrap();
        writeln!(f, "(+ x 10)").unwrap();
    }
    let mut output = Vec::new();
    run_file(file_path.to_str().unwrap(), &mut output).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    // define returns Void (not printed), (+ x 10) returns 15
    assert_eq!(output_str, "15\n");
    std::fs::remove_file(&file_path).ok();
}

#[test]
fn run_file_error_reports_and_stops() {
    // An error in a file should be reported
    let dir = std::env::temp_dir();
    let file_path = dir.join("test_run_file_error.straw");
    {
        let mut f = std::fs::File::create(&file_path).unwrap();
        writeln!(f, "(/ 1 0)").unwrap();
    }
    let mut output = Vec::new();
    let result = run_file(file_path.to_str().unwrap(), &mut output);
    // File execution should return an error on evaluation failure
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("division by zero"));
    std::fs::remove_file(&file_path).ok();
}

#[test]
fn run_file_nonexistent_file_returns_error() {
    let mut output = Vec::new();
    let result = run_file("/tmp/nonexistent_file_12345.straw", &mut output);
    assert!(result.is_err());
}
