# Context

Rolling working memory for the Strawman build. Updated by Claude after each task.
Read by Claude at the start of the next task. Keep under 100 lines.

## Current State

- `src/main.rs` — Entry point (binary). Parses CLI args: no args → `run_repl(stdin, stdout)`, file arg → `run_file(path, stdout)`.
- `src/lib.rs` — Library root. Declares `pub mod builtins;`, `pub mod env;`, `pub mod eval;`, `pub mod lexer;`, `pub mod parser;`, `pub mod repl;`.
- `src/lexer.rs` — Lexer module. Exports `Token` enum and `tokenize` function.
- `src/parser.rs` — Parser module. Exports `Expr` enum, `parse`, and `parse_all`.
- `src/env.rs` — Environment module. Exports `Value` enum and `Env` struct.
  - `Value` enum: `Number(f64)`, `StringLit(String)`, `Boolean(bool)`, `Symbol(String)`,
    `List(Vec<Value>)`, `Pair(Box<Value>, Box<Value>)`,
    `Builtin(String, fn(Vec<Value>) -> Result<Value, String>)`,
    `Closure(Vec<String>, Vec<Expr>, Rc<Env>)`,
    `Continuation(Rc<dyn Fn(Value) -> Result<Value, String>>)`, `Void`.
  - `Env` struct with `bindings: RefCell<HashMap<String, Value>>`, `parent: Option<Rc<Env>>`.
  - Methods: `new`, `with_parent`, `set`, `update`, `extend`, `lookup`.
- `src/eval.rs` — CPS evaluator module. Exports `straw_eval` and `straw_eval_k`.
  - Internal `eval_cps` uses `Cont = Rc<dyn Fn(Value) -> Result<Value, String>>` for proper continuation capture.
  - `straw_eval_k` is public API wrapper: evaluates with identity Rc continuation, then applies caller's `&dyn Fn` k.
  - `straw_eval` catches continuation escapes (CONT_ESCAPE), throw escapes (THROW_ESCAPE), and block escapes (BLOCK_ESCAPE) at top level.
  - `call/cc` special form: reifies k as `Value::Continuation`, captures full computation via Rc clone.
  - `catch` special form: `(catch tag-expr body-expr)` — evaluates tag to symbol, evaluates body; intercepts THROW_ESCAPE if tag matches.
  - `throw` special form: `(throw tag-expr value-expr)` — evaluates both, stores tag+value in thread-locals, returns THROW_ESCAPE sentinel.
  - `block` special form: `(block name body ...)` — evaluates body; intercepts BLOCK_ESCAPE if name matches.
  - `return-from` special form: `(return-from name value-expr)` — evaluates value, stores name+value in thread-locals, returns BLOCK_ESCAPE sentinel.
  - `unwind-protect` special form: `(unwind-protect protected cleanup ...)` — evaluates protected, always runs cleanup forms, re-propagates error if any.
  - `eval_args_cps`: evaluates arguments left-to-right in CPS, threading k through for proper continuation capture.
  - `apply_function`: handles `Builtin`, `Closure`, and `Continuation` application.
- `src/builtins.rs` — Builtins module. Exports `default_env`.
- `src/repl.rs` — REPL module. Exports `run_repl`, `run_file`, and private `format_value`.
- `tests/test_eval.rs` — 114 passing tests (91 from Epics 1-2/E3.1 + 7 call/cc + 6 catch/throw + 6 block/return-from + 4 unwind-protect).
- `tests/test_repl.rs` — 14 passing. `tests/test_lexer.rs` — 17 passing.
- `tests/test_parser.rs` — 14 passing. `tests/test_env.rs` — 9 passing.
- `tests/test_builtins.rs` — 66 passing.
- `Cargo.toml` — Crate name is `strawman`, edition 2024. Dev-dependency: `gag = "1.0"`.

## Conventions & Decisions

- Integration tests: `tests/test_{module}.rs`. Builtins tests use `default_env()`.
- Eval tests use inline `make_test_env()` helper with `+`, `-`, `*` builtins.
- TDD discipline: write failing test first, then minimal impl.
- `Value::Builtin(name, fn_ptr)` uses `fn(Vec<Value>) -> Result<Value, String>`.
- `Value::Continuation(Rc<dyn Fn(Value) -> Result<Value, String>>)` — reified first-class continuation.
- Error messages: `"unbound variable: <name>"`, `"not a procedure: <debug>"`,
  `"cannot set! unbound variable: <name>"`, `"expected number"`, `"division by zero"`,
  `"expected procedure"` (for call/cc with non-procedure),
  `"no matching catch for tag: <tag>"` (uncaught throw),
  `"unknown block: <name>"` (uncaught return-from).
- Continuation escape: thread-local `ESCAPED_VALUE` + sentinel `CONT_ESCAPE` error string.
- Throw escape: thread-locals `THROWN_TAG` + `THROWN_VALUE` + sentinel `THROW_ESCAPE` error string.
- Block escape: thread-locals `BLOCK_NAME` + `BLOCK_VALUE` + sentinel `BLOCK_ESCAPE` error string.
- `catch` intercepts THROW_ESCAPE, checks tag match; re-propagates if tag doesn't match.
- `block` intercepts BLOCK_ESCAPE, checks name match; re-propagates if name doesn't match.
- `straw_eval` catches CONT_ESCAPE, THROW_ESCAPE, and BLOCK_ESCAPE at top level.
- REPL uses generic `BufRead`/`Write` for testability.
- REPL prompt: `"strawman> "`. Value formatting: integers as `i64`, floats as `f64`, bools as `#t`/`#f`.
- Only `Value::Boolean(false)` is falsy; everything else is truthy.
- Stdout capture tests (gag) MUST run with `--test-threads=1`.
- Empty `Expr::List` returns `Err("not implemented")`.

## Gotchas & Notes for Next Task

- **E3.5 unwind-protect COMPLETE**: All 4 tests pass (normal, with throw, cleanup order, acceptance criterion).
- `unwind-protect` captures the protected expression result (Ok or Err), always runs cleanup forms, then re-propagates.
- Cleanup forms run with `?` — if a cleanup form errors, it replaces the original error (matches Common Lisp behavior).
- Thread-local state (THROWN_TAG/VALUE, BLOCK_NAME/VALUE) survives through cleanup forms as long as cleanup doesn't touch them.
- **Epic 3 COMPLETE**: All stories E3.1-E3.5 done (CPS evaluator, call/cc, catch/throw, block/return-from, unwind-protect).
- Epics 1-2 fully complete. Epic 3 fully complete.
- All 234 tests pass across all modules (114 eval + 66 builtins + 17 lexer + 14 parser + 14 repl + 9 env).
