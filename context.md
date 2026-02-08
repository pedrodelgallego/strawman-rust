# Context

Rolling working memory for the Strawman build. Updated by Claude after each task.
Read by Claude at the start of the next task. Keep under 100 lines.

## Current State

- `src/main.rs` — Entry point (binary). Just `fn main()` with a placeholder print.
- `src/lib.rs` — Library root. Declares `pub mod env;`, `pub mod lexer;`, `pub mod parser;`.
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
  - `Value` enum: `Number(f64)`. Derives `Debug, Clone, PartialEq`.
    Has `impl From<f64> for Value`.
  - `Env` struct: contains `bindings: RefCell<HashMap<String, Value>>`,
    `parent: Option<Rc<Env>>`.
  - `Env::new()` — creates empty environment (no parent).
  - `Env::with_parent(parent: Rc<Env>)` — creates child environment with parent link.
  - `Env::set(&self, name: &str, value: Value)` — inserts binding in current frame.
  - `Env::update(&self, name: &str, value: Value) -> Result<(), String>` — updates
    existing binding, walking parent chain. Returns
    `Err("cannot set! unbound variable: <name>")` if not found anywhere.
  - `Env::extend(parent: Rc<Env>, params: &[String], args: Vec<Value>) -> Result<Self, String>`
    — creates a child env binding each param to the corresponding arg.
    Returns `Err("arity mismatch: expected N, got M")` if lengths differ.
  - `Env::lookup(&self, name: &str) -> Result<Value, String>` — checks local bindings,
    then walks parent chain. Returns `Err("unbound variable: <name>")` if not found.
- `tests/test_lexer.rs` — 17 passing lexer tests.
- `tests/test_parser.rs` — 14 passing parser tests.
- `tests/test_env.rs` — 9 passing env tests: `set_and_lookup`, `unbound_lookup_errors`,
  `parent_chain_lookup`, `shadowing`, `update_existing_binding`, `update_unbound_errors`,
  `extend_with_params_and_args`, `extend_arity_mismatch_errors`,
  `parent_not_mutated_by_child`.
- `Cargo.toml` — Crate name is `strawman`, edition 2024.

## Conventions & Decisions

- Lexer: `src/lexer.rs`, declared via `pub mod lexer;` in `lib.rs`.
- Parser: `src/parser.rs`, declared via `pub mod parser;` in `lib.rs`.
- Env: `src/env.rs`, declared via `pub mod env;` in `lib.rs`.
- `main.rs` is the binary entry point; `lib.rs` is the library root.
- Integration tests: `tests/test_{module}.rs`. Env tests import with
  `use strawman::env::Env;`.
- TDD discipline: write failing test first, then minimal impl.
- `Token` enum uses `f64` for all numbers. `Expr::Number(f64)` mirrors this.
- `Value::Number(f64)` mirrors the same convention.
- Env uses `RefCell<HashMap<String, Value>>` for interior mutability so `set`
  can take `&self` instead of `&mut self` (needed for shared parent refs later).
- Error messages: `"unbound variable: <name>"` for missing lookups.
- `Value` has `From<f64>` for ergonomic test construction (e.g., `1.0.into()`).

## Gotchas & Notes for Next Task

- **E1.3 complete:** All 9 test matrix rows passing (9/9). Environment module is done.
- Next up: E1.4 — Evaluator (`straw-eval`). Will need `Value` variants beyond
  `Number(f64)`: `Boolean(bool)`, `StringLit(String)`, `Symbol(String)`,
  `List(Vec<Value>)`, `Nil`, `Closure{params, body, env}`, `Builtin(fn)`.
- `Expr` has no `Clone` derive yet — add it when the evaluator needs to clone AST nodes.
- `Env::set` inserts only into the current frame (child), so parent isolation
  is guaranteed by construction. Tested in `parent_not_mutated_by_child`.
- `Env::update` walks the parent chain to find and mutate an existing binding.
  Uses `contains_key` then `insert` to avoid double-borrow on `RefCell`.
