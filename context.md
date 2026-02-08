# Context

Rolling working memory for the Strawman build. Updated by Claude after each task.
Read by Claude at the start of the next task. Keep under 100 lines.

## Current State

- `src/main.rs` — Entry point (binary). Just `fn main()` with a placeholder print.
- `src/lib.rs` — Library root. Declares `pub mod env;`, `pub mod eval;`, `pub mod lexer;`, `pub mod parser;`.
- `src/lexer.rs` — Lexer module. Exports `Token` enum and `tokenize` function.
  - `Token` enum: `Number(f64)`, `StringLit(String)`, `Boolean(bool)`, `Symbol(String)`,
    `LParen`, `RParen`, `Quote`. Derives `Debug, PartialEq`.
  - `pub fn tokenize(input: &str) -> Result<Vec<Token>, String>`.
- `src/parser.rs` — Parser module. Exports `Expr` enum, `parse`, and `parse_all`.
  - `Expr` enum: `Number(f64)`, `Symbol(String)`, `StringLit(String)`, `Boolean(bool)`,
    `List(Vec<Expr>)`. Derives `Debug, PartialEq`.
  - `pub fn parse(input: &str) -> Result<Expr, String>` — returns first expression.
  - `pub fn parse_all(input: &str) -> Result<Vec<Expr>, String>` — returns all expressions.
- `src/env.rs` — Environment module. Exports `Value` enum and `Env` struct.
  - `Value` enum: `Number(f64)`, `StringLit(String)`, `Boolean(bool)`, `Symbol(String)`,
    `List(Vec<Value>)`, `Builtin(String, fn(Vec<Value>) -> Result<Value, String>)`,
    `Closure(Vec<String>, Vec<Expr>, Rc<Env>)`, `Void`.
    Derives `Clone`. Has manual `PartialEq` (Builtin by name, Closure always !=) and manual `Debug`.
  - `Env` struct: contains `bindings: RefCell<HashMap<String, Value>>`,
    `parent: Option<Rc<Env>>`.
  - Methods: `new`, `with_parent`, `set`, `update`, `extend`, `lookup`.
- `src/eval.rs` — Evaluator module. Exports `straw_eval`.
  - `pub fn straw_eval(expr: &Expr, env: &Rc<Env>) -> Result<Value, String>`.
  - Handles: self-evaluating forms (Number, StringLit, Boolean),
    `Expr::Symbol(name)` → `env.lookup(name)`,
    `Expr::List` with special forms: `(quote ...)`, `(define var expr)`, `(set! var expr)`,
    `(begin ...)`, `(if ...)`, `(lambda (params...) body...)`.
  - Function application: non-special-form lists evaluate head + args, then call.
    Supports `Value::Builtin` and `Value::Closure`. Closures create child env via `Env::extend`,
    evaluate body sequentially, return last value. Non-procedure head → `"not a procedure: ..."` error.
  - Helper `expr_to_value` converts `Expr` to `Value` for quote.
- `tests/test_lexer.rs` — 17 passing lexer tests.
- `tests/test_parser.rs` — 14 passing parser tests.
- `tests/test_env.rs` — 9 passing env tests.
- `tests/test_eval.rs` — 50 passing eval tests (8 E1.4 + 7 quote E1.5 + 8 if E1.6 + 6 begin E1.7 + 7 define/set! E1.8 + 8 lambda E1.9 + 6 E1.10 builtin/closure/nested/higher-order/non-procedure/non-procedure-string).
- `Cargo.toml` — Crate name is `strawman`, edition 2024.

## Conventions & Decisions

- Integration tests: `tests/test_{module}.rs`. Eval tests import with
  `use strawman::eval::straw_eval;`, `use strawman::env::{Env, Value};`,
  `use strawman::parser::Expr;`.
- TDD discipline: write failing test first, then minimal impl.
- `Value::Builtin(name, fn_ptr)` uses `fn(Vec<Value>) -> Result<Value, String>` (fn pointer, not closure).
  `PartialEq` compares by name only. `Debug` shows `Builtin("name")`.
- Error messages: `"unbound variable: <name>"`, `"not a procedure: <debug>"`,
  `"cannot set! unbound variable: <name>"`.
- `straw_eval` signature: takes `&Expr` and `&Rc<Env>`, returns `Result<Value, String>`.

## Gotchas & Notes for Next Task

- **E1.10 complete.** All 6 test matrix rows pass: builtin call, closure call, nested call,
  higher-order, non-procedure number error, non-procedure string error.
- Test file has a `make_test_env()` helper that sets up `+`, `-`, `*` builtins for E1.10 tests.
- Non-procedure error format: `"not a procedure: Number(42)"`, `"not a procedure: StringLit(\"hello\")"`.
- Arity mismatch errors come from `Env::extend` — format: `"arity mismatch: expected <n>, got <m>"`.
- `define` handles only `(define symbol expr)` — no function shorthand yet.
- Empty `Expr::List` currently returns `Err("not implemented")` — will need to handle as nil or error.
- Builtins module (`src/builtins.rs`) doesn't exist yet — will be needed for E1.11 (default env).
