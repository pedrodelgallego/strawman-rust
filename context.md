# Context

Rolling working memory for the Strawman build. Updated by Claude after each task.
Read by Claude at the start of the next task. Keep under 100 lines.

## Current State

- `src/main.rs` — Entry point (binary). Parses CLI args: no args → `run_repl(stdin, stdout)`, file arg → `run_file(path, stdout)`.
- `src/lib.rs` — Library root. Declares `pub mod builtins;`, `pub mod env;`, `pub mod eval;`, `pub mod lexer;`, `pub mod parser;`, `pub mod repl;`.
- `src/lexer.rs` — Lexer module. Exports `Token` enum and `tokenize` function.
  - `Token` enum: `Number(f64)`, `StringLit(String)`, `Boolean(bool)`, `Symbol(String)`,
    `LParen`, `RParen`, `Quote`. Derives `Debug, PartialEq`.
  - `pub fn tokenize(input: &str) -> Result<Vec<Token>, String>`.
- `src/parser.rs` — Parser module. Exports `Expr` enum, `parse`, and `parse_all`.
  - `Expr` enum: `Number(f64)`, `Symbol(String)`, `StringLit(String)`, `Boolean(bool)`,
    `List(Vec<Expr>)`. Derives `Debug, PartialEq, Clone`.
  - `pub fn parse(input: &str) -> Result<Expr, String>` — returns first expression.
  - `pub fn parse_all(input: &str) -> Result<Vec<Expr>, String>` — returns all expressions.
- `src/env.rs` — Environment module. Exports `Value` enum and `Env` struct.
  - `Value` enum: `Number(f64)`, `StringLit(String)`, `Boolean(bool)`, `Symbol(String)`,
    `List(Vec<Value>)`, `Pair(Box<Value>, Box<Value>)`,
    `Builtin(String, fn(Vec<Value>) -> Result<Value, String>)`,
    `Closure(Vec<String>, Vec<Expr>, Rc<Env>)`, `Void`.
  - `Env` struct with `bindings: RefCell<HashMap<String, Value>>`, `parent: Option<Rc<Env>>`.
  - Methods: `new`, `with_parent`, `set`, `update`, `extend`, `lookup`.
- `src/eval.rs` — Evaluator module. Exports `straw_eval`.
  - `pub fn straw_eval(expr: &Expr, env: &Rc<Env>) -> Result<Value, String>`.
  - Handles: self-eval, symbol lookup, quote, begin, define, set!, lambda, and, or, if, function application.
- `src/builtins.rs` — Builtins module. Exports `default_env`.
  - Registers: `+`, `-`, `*`, `/`, `mod`, `<`, `>`, `<=`, `>=`, `=`, `equal?`, `list`, `cons`, `car`, `cdr`, `null?`, `pair?`, `not`, `number?`, `string?`, `symbol?`, `boolean?`, `procedure?`, `display`, `newline`.
- `src/repl.rs` — REPL module. Exports `run_repl`, `run_file`, and private `format_value`.
  - `pub fn run_repl<R: BufRead, W: Write>(input: &mut R, output: &mut W)` — generic over I/O for testability.
  - `pub fn run_file<W: Write>(path: &str, output: &mut W) -> Result<(), String>` — reads file, parse_all, eval each, print non-void results, returns Err on failure.
  - `fn format_value(val: &Value) -> String` — formats values for REPL output.
  - `fn paren_depth(s: &str) -> i32` — counts net open parens (handles strings, escapes, comments).
  - `fn is_exit_command(expr: &Expr) -> bool` — checks if expr is `(exit)` or `(quit)`.
- `tests/test_repl.rs` — 14 passing REPL tests (10 REPL + 4 run_file).
- `tests/test_lexer.rs` — 17 passing. `tests/test_parser.rs` — 14 passing.
- `tests/test_env.rs` — 9 passing. `tests/test_eval.rs` — 58 passing.
- `tests/test_builtins.rs` — 66 passing.
- `Cargo.toml` — Crate name is `strawman`, edition 2024. Dev-dependency: `gag = "1.0"`.

## Conventions & Decisions

- Integration tests: `tests/test_{module}.rs`. Builtins tests use `default_env()`.
- Eval tests use inline `make_test_env()` helper with `+`, `-`, `*` builtins.
- TDD discipline: write failing test first, then minimal impl.
- `Value::Builtin(name, fn_ptr)` uses `fn(Vec<Value>) -> Result<Value, String>`.
- Error messages: `"unbound variable: <name>"`, `"not a procedure: <debug>"`,
  `"cannot set! unbound variable: <name>"`, `"expected number"`, `"division by zero"`.
- `straw_eval` signature: takes `&Expr` and `&Rc<Env>`, returns `Result<Value, String>`.
- REPL uses generic `BufRead`/`Write` for testability. Tests pass byte slices as input and `Vec<u8>` as output.
- REPL prompt: `"strawman> "`. Value formatting: integers as `i64`, floats as `f64`, bools as `#t`/`#f`, strings quoted, symbols bare, lists with parens, pairs with dot notation, closures as `#<closure>`, builtins as `#<procedure:name>`.
- `run_file` reads file, uses `parse_all`, evaluates in shared env, stops on first error (returns `Err`).
- `main.rs` uses `std::env::args()`: `args.len() > 1` → file mode, else REPL mode.

## Gotchas & Notes for Next Task

- **Phase 6 complete**: All E1.16 tasks done. Entry point wired: no args → REPL, file arg → execute.
- `run_file` stops on first error and returns `Err(msg)`. The REPL recovers from errors and continues.
- `format_value` is private in `repl.rs` — may need to be made `pub` if needed elsewhere.
- `display` builtin prints to actual stdout (not the REPL's writer) — this is correct per spec since `display` is a side effect.
- Stdout capture tests (gag) MUST run with `--test-threads=1`.
- Empty `Expr::List` returns `Err("not implemented")` — not yet needed.
- Only `Value::Boolean(false)` is falsy; everything else is truthy.
- Next: Phase 7 — Namespaces & Recursion (E2.1–E2.5: let, let*, letrec, define shorthand).
