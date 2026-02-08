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
    `List(Rc<Vec<Value>>)`, `Pair(Box<Value>, Box<Value>)`,
    `Vector(Rc<RefCell<Vec<Value>>>)`,
    `Builtin(String, fn(Vec<Value>) -> Result<Value, String>)`,
    `Closure(Vec<String>, Vec<Expr>, Rc<Env>)`,
    `Continuation(Rc<dyn Fn(Value) -> Result<Value, String>>)`, `Void`.
  - `Env` struct with `bindings: RefCell<HashMap<String, Value>>`, `parent: Option<Rc<Env>>`.
  - Methods: `new`, `with_parent`, `set`, `update`, `extend`, `lookup`.
- `src/eval.rs` — CPS evaluator module. Exports `straw_eval` and `straw_eval_k`.
  - Special forms: `quote`, `begin`, `define`, `set!`, `lambda`, `and`, `or`,
    `let`, `let*`, `letrec`, `if`, `catch`, `throw`, `block`, `return-from`,
    `unwind-protect`, `call/cc`, `set-car!`, `set-cdr!`.
- `src/builtins.rs` — Builtins module. Exports `default_env`.
  - Includes `eq?`, `equal?`, `make-vector`, `vector-ref`, `vector-set!`, `vector-length`, `vector?`.
- `src/repl.rs` — REPL module. Exports `run_repl`, `run_file`, and private `format_value`.
- `tests/test_eval.rs` — 136 passing tests (129 prior + 7 E4.4).
- `tests/test_repl.rs` — 14 passing. `tests/test_lexer.rs` — 17 passing.
- `tests/test_parser.rs` — 14 passing. `tests/test_env.rs` — 9 passing.
- `tests/test_builtins.rs` — 66 passing.
- `Cargo.toml` — Crate name is `strawman`, edition 2024. Dev-dependency: `gag = "1.0"`.

## Conventions & Decisions

- Integration tests: `tests/test_{module}.rs`. Builtins tests use `default_env()`.
- Eval tests use inline `make_test_env()` helper with `+`, `-`, `*` builtins.
- TDD discipline: write failing test first, then minimal impl.
- `Value::List(Rc<Vec<Value>>)` — lists use `Rc` for identity semantics (`eq?` uses `Rc::ptr_eq`).
- `Value::Vector(Rc<RefCell<Vec<Value>>>)` — vectors use `Rc<RefCell<>>` for mutable in-place access.
  `vector-set!` is a regular builtin (not a special form) because it mutates via `RefCell`.
- `Value::Builtin(name, fn_ptr)` uses `fn(Vec<Value>) -> Result<Value, String>`.
- `Value::Continuation(Rc<dyn Fn(Value) -> Result<Value, String>>)` — reified first-class continuation.
- `PartialEq` for `Value` compares structurally (used by `equal?`). `eq?` uses `Rc::ptr_eq` for lists.
- Error messages: `"unbound variable: <name>"`, `"not a procedure: <debug>"`,
  `"cannot set! unbound variable: <name>"`, `"expected number"`, `"division by zero"`,
  `"expected procedure"`, `"no matching catch for tag: <tag>"`,
  `"unknown block: <name>"`, `"expected mutable pair"`, `"index out of range"`,
  `"expected vector"`.
- Continuation escape: thread-local `ESCAPED_VALUE` + sentinel `CONT_ESCAPE` error string.
- Throw escape: thread-locals `THROWN_TAG` + `THROWN_VALUE` + sentinel `THROW_ESCAPE` error string.
- Block escape: thread-locals `BLOCK_NAME` + `BLOCK_VALUE` + sentinel `BLOCK_ESCAPE` error string.
- REPL uses generic `BufRead`/`Write` for testability.
- REPL prompt: `"strawman> "`. Value formatting: integers as `i64`, floats as `f64`, bools as `#t`/`#f`.
- Vectors display as `#(elem1 elem2 ...)`.
- Only `Value::Boolean(false)` is falsy; everything else is truthy.
- Stdout capture tests (gag) MUST run with `--test-threads=1`.
- Constructing lists: always `Value::List(Rc::new(vec![...]))`. In tests, import `std::rc::Rc`.

## Gotchas & Notes for Next Task

- **E4.4 Vectors COMPLETE**: `make-vector`, `vector-ref`, `vector-set!`, `vector-length`, `vector?` added.
- `Value::Vector(Rc<RefCell<Vec<Value>>>)` uses `RefCell` for interior mutability.
  `vector-set!` works as a regular builtin — cloning a `Value::Vector` clones the `Rc`,
  so the mutation is visible through all references to the same vector.
- `make-vector` accepts 1 or 2 args: `(make-vector size)` fills with `0`, `(make-vector size fill)`.
- Out-of-bounds access returns `"index out of range"` error.
- **Epics 1-3 COMPLETE**. E4.1-E4.4 complete.
- All 256 tests pass across all modules (136 eval + 66 builtins + 17 lexer + 14 parser + 14 repl + 9 env).
