# Context

Rolling working memory for the Strawman build. Updated by Claude after each task.
Read by Claude at the start of the next task. Keep under 100 lines.

## Current State

- `src/main.rs` — Entry point (binary). Parses CLI args: `--bench` → runs benchmark harness on 256MB stack, file arg → `run_file(path, stdout)`, no args → `run_repl(stdin, stdout)`.
- `src/lib.rs` — Library root. Declares `pub mod bench;`, `pub mod builtins;`, `pub mod env;`, `pub mod eval;`, `pub mod fast_eval;`, `pub mod lexer;`, `pub mod parser;`, `pub mod pretreat;`, `pub mod repl;`.
- `src/lexer.rs` — Lexer module. Exports `Token` enum and `tokenize` function.
- `src/parser.rs` — Parser module. Exports `Expr` enum, `parse`, and `parse_all`.
- `src/env.rs` — Environment module. Exports `Value` enum and `Env` struct.
  - `Value` enum: `Number(f64)`, `StringLit(String)`, `Boolean(bool)`, `Symbol(String)`,
    `List(Rc<Vec<Value>>)`, `Pair(Box<Value>, Box<Value>)`,
    `Vector(Rc<RefCell<Vec<Value>>>)`,
    `Builtin(String, fn(Vec<Value>) -> Result<Value, String>)`,
    `Closure(Vec<String>, Vec<Expr>, Rc<Env>)`,
    `FastClosure(Vec<String>, Vec<TreatedExpr>, Rc<Env>)`,
    `Continuation(Rc<dyn Fn(Value) -> Result<Value, String>>)`, `Void`.
  - `Env` struct with `bindings: RefCell<HashMap<String, Value>>`, `rib: RefCell<Vec<Value>>`, `parent: Option<Rc<Env>>`.
  - Methods: `new`, `with_parent`, `set`, `update`, `extend`, `lookup`, `lexical_lookup`, `rib_set`.
  - `extend` populates both `bindings` (HashMap) and `rib` (Vec) for dual-path lookup.
  - `lexical_lookup(depth, offset)` — O(1)-ish lookup for lexically addressed vars.
  - `rib_set(offset, value)` — updates rib at given offset (used by letrec init).
- `src/eval.rs` — CPS evaluator module. Exports `straw_eval` and `straw_eval_k`.
- `src/pretreat.rs` — Pretreatment module. Exports `TreatedExpr` enum and `pretreat` function.
  - `pretreat` resolves lambda/let-bound vars to `LexicalRef(depth, offset)`.
  - Globals/builtins remain `VarRef(name)`.
- `src/fast_eval.rs` — Fast CPS evaluator. Exports `fast_eval` function.
  - Handles `LexicalRef` via `env.lexical_lookup`, `VarRef` via `env.lookup`.
  - Creates `FastClosure` values; handles both `FastClosure` and `Closure` in apply/call-cc.
- `src/bench.rs` — Benchmark harness. Exports `BenchResult` struct, `run_benchmarks`, `format_table`, `run_bench_to_writer`.
  - 4 benchmarks: factorial(20), fibonacci(25), ackermann(3,5), map over 20-element list.
  - Runs each under both `straw_eval` and `fast_eval`, measures wall-clock time.
  - `format_table` produces a comparison table with Naive, Fast, and Speedup columns.
  - `run_bench_to_writer<W: Write>(output)` — creates default env, runs all benchmarks, writes table.
- `src/builtins.rs` — Builtins module. `procedure?` recognizes `FastClosure` too.
- `src/repl.rs` — REPL module. `format_value` handles `FastClosure` as `#<closure>`.
- `tests/test_bench.rs` — 4 tests (harness runs all, factorial correctness, table output, run_bench_to_writer).
- `tests/test_bench_smoke.rs` — 1 test (smoke test for ackermann(3,6) with naive eval).
- `tests/test_fast_eval.rs` — 50 tests. `tests/test_pretreat.rs` — 6 tests.
- `tests/test_eval.rs` — 136. `tests/test_repl.rs` — 14. `tests/test_lexer.rs` — 17.
- `tests/test_parser.rs` — 14. `tests/test_env.rs` — 9. `tests/test_builtins.rs` — 66.
- `tests/test_semantics.rs` — 104.
- `Cargo.toml` — Crate name is `strawman`, edition 2024. Dev-dependency: `gag = "1.0"`.
- **All 421 tests pass.**

## Conventions & Decisions

- Integration tests: `tests/test_{module}.rs`. Builtins tests use `default_env()`.
- Eval tests use inline `make_test_env()` helper with `+`, `-`, `*` builtins.
- Fast eval tests use `eval_both(input, env)` helper that runs both evaluators.
- Pretreat tests use `pt(input)` helper that parses+pretreats.
- Bench tests use 256MB stack via `std::thread::Builder` for CPS stack depth.
- `Value::FastClosure` — closures created by `fast_eval`. Debug format matches `Closure`.
- Error messages identical between `straw_eval` and `fast_eval`.
- CLI: `--bench` flag runs benchmark harness; file arg runs file; no args starts REPL.

## Gotchas & Notes for Next Task

- **E6.1–E6.3 COMPLETE**: Phase 11 (Fast Interpretation) is done.
- **Next up: Phase 12 — Bytecode Compilation** (E7.1–E7.4).
- CPS evaluators are stack-hungry in debug mode; bench/smoke tests need 256MB stack.
- `TreatedExpr::Quote` stores original `Expr` (not `TreatedExpr`) for `expr_to_value`.
- `let*` creates one rib per binding in static env; `letrec` creates a single rib.
- `SetCar`/`SetCdr` store optional `var_name: Option<String>` for env update.
- `BENCH_STACK_SIZE` constant in bench.rs is unused (only used in main.rs directly) — could clean up.
