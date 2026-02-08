# Context

Rolling working memory for the Strawman build. Updated by Claude after each task.
Read by Claude at the start of the next task. Keep under 100 lines.

## Current State

- `src/main.rs` — Entry point (binary). Just `fn main()` with a placeholder print.
- `src/lib.rs` — Library root. Declares `pub mod builtins;`, `pub mod env;`, `pub mod eval;`, `pub mod lexer;`, `pub mod parser;`.
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
    `List(Vec<Value>)`, `Pair(Box<Value>, Box<Value>)`,
    `Builtin(String, fn(Vec<Value>) -> Result<Value, String>)`,
    `Closure(Vec<String>, Vec<Expr>, Rc<Env>)`, `Void`.
    Derives `Clone`. Has manual `PartialEq` (Builtin by name, Closure always !=, Pair structural) and manual `Debug`.
  - `Env` struct: contains `bindings: RefCell<HashMap<String, Value>>`,
    `parent: Option<Rc<Env>>`.
  - Methods: `new`, `with_parent`, `set`, `update`, `extend`, `lookup`.
- `src/eval.rs` — Evaluator module. Exports `straw_eval`.
  - `pub fn straw_eval(expr: &Expr, env: &Rc<Env>) -> Result<Value, String>`.
  - Handles: self-evaluating forms (Number, StringLit, Boolean),
    `Expr::Symbol(name)` → `env.lookup(name)`,
    `Expr::List` with special forms: `(quote ...)`, `(define var expr)`, `(set! var expr)`,
    `(begin ...)`, `(if ...)`, `(lambda (params...) body...)`, `(and ...)`, `(or ...)`.
  - `and` is a special form (short-circuit): returns `#t` for empty, `#f` on first false, last value if all truthy.
  - `or` is a special form (short-circuit): returns `#f` for empty, first truthy value, last value if all false.
  - Function application for `Value::Builtin` and `Value::Closure`.
  - Helper `expr_to_value` converts `Expr` to `Value` for quote.
- `src/builtins.rs` — Builtins module. Exports `default_env`.
  - `pub fn default_env() -> Rc<Env>` — creates env with builtin operators.
  - Registers: `+`, `-`, `*`, `/`, `mod`, `<`, `>`, `<=`, `>=`, `=`, `equal?`, `list`, `cons`, `car`, `cdr`, `null?`, `pair?`, `not`, `number?`, `string?`, `symbol?`, `boolean?`, `procedure?`, `display`, `newline`.
- `tests/test_lexer.rs` — 17 passing lexer tests.
- `tests/test_parser.rs` — 14 passing parser tests.
- `tests/test_env.rs` — 9 passing env tests.
- `tests/test_eval.rs` — 58 passing eval tests (includes E1.14 `and`: 4 tests, `or`: 4 tests).
- `tests/test_builtins.rs` — 66 passing builtin tests (E1.11: 15 arithmetic; E1.12: 14 comparison/equality; E1.13: 18 list ops — 2 cons, 8 car/cdr, 2 list, 3 null?, 3 pair?; E1.14: 3 not; E1.15: 16 — 13 type predicates, 2 display, 1 newline).
- `Cargo.toml` — Crate name is `strawman`, edition 2024. Dev-dependency: `gag = "1.0"` for stdout capture in tests.

## Conventions & Decisions

- Integration tests: `tests/test_{module}.rs`. Builtins tests use `default_env()`.
- Eval tests use inline `make_test_env()` helper with `+`, `-`, `*` builtins.
- TDD discipline: write failing test first, then minimal impl.
- `Value::Builtin(name, fn_ptr)` uses `fn(Vec<Value>) -> Result<Value, String>` (fn pointer, not closure).
  `PartialEq` compares by name only. `Debug` shows `Builtin("name")`.
- `Value::Pair(Box<Value>, Box<Value>)` represents dotted pairs / cons cells.
  `cons` onto a `Value::List` prepends and returns a `Value::List`.
  `cons` with a non-list tail returns `Value::Pair`.
- Error messages: `"unbound variable: <name>"`, `"not a procedure: <debug>"`,
  `"cannot set! unbound variable: <name>"`, `"expected number"`, `"division by zero"`.
- `straw_eval` signature: takes `&Expr` and `&Rc<Env>`, returns `Result<Value, String>`.

## Gotchas & Notes for Next Task

- **E1.15 complete**: All type predicates, `display`, and `newline` implemented.
- `display` prints to stdout via `write!` to `std::io::stdout()` + flush, returns `Value::Void`.
- `newline` writes `"\n"` to stdout via `write!` + flush, takes 0 args, returns `Value::Void`.
- **Stdout capture in tests**: uses `gag::BufferRedirect::stdout()`. Tests using gag MUST run with `--test-threads=1` because gag can only have one active stdout redirect. The `gag` crate works with `write!(std::io::stdout(), ...)` but NOT with `print!` macro.
- `tests/test_builtins.rs` imports: `strawman::builtins::default_env`, `strawman::eval::straw_eval`, `strawman::parser::parse`.
- Empty `Expr::List` still returns `Err("not implemented")` — not yet needed.
- Only `Value::Boolean(false)` is falsy; everything else (0, empty list, etc.) is truthy.
- Next up: E1.16 (REPL) or Phase 6+ stories.
