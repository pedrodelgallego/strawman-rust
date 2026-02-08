# Context

Rolling working memory for the Strawman build. Updated by Claude after each task.
Read by Claude at the start of the next task. Keep under 100 lines.

## Current State

- `src/main.rs` — Entry point (binary). Parses CLI args: no args → `run_repl(stdin, stdout)`, file arg → `run_file(path, stdout)`.
- `src/lib.rs` — Library root. Declares `pub mod builtins;`, `pub mod env;`, `pub mod eval;`, `pub mod fast_eval;`, `pub mod lexer;`, `pub mod parser;`, `pub mod pretreat;`, `pub mod repl;`.
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
  - `Env` struct with `bindings: RefCell<HashMap<String, Value>>`, `parent: Option<Rc<Env>>`.
  - Methods: `new`, `with_parent`, `set`, `update`, `extend`, `lookup`.
- `src/eval.rs` — CPS evaluator module. Exports `straw_eval` and `straw_eval_k`.
- `src/pretreat.rs` — Pretreatment module. Exports `TreatedExpr` enum and `pretreat` function.
  - `TreatedExpr` variants: `Number`, `StringLit`, `Boolean`, `VarRef`, `Quote`, `If`, `Begin`,
    `Define`, `DefineFunc`, `SetBang`, `Lambda`, `And`, `Or`, `Let`, `LetStar`, `Letrec`,
    `Catch`, `Throw`, `Block`, `ReturnFrom`, `UnwindProtect`, `CallCC`, `SetCar`, `SetCdr`,
    `Application`.
  - `pretreat(expr: &Expr) -> TreatedExpr` — converts `Expr` to `TreatedExpr`, recognizing
    all special forms at pretreat time (no string matching needed at eval time).
- `src/fast_eval.rs` — Fast CPS evaluator. Exports `fast_eval` function.
  - `fast_eval(expr: &TreatedExpr, env: &Rc<Env>) -> Result<Value, String>`
  - Mirrors `straw_eval` but operates on `TreatedExpr` — dispatches by enum variant, not string.
  - Uses CPS arg evaluation (`eval_args_cps`) for proper continuation threading.
  - Creates `FastClosure` values instead of `Closure` for lambda/define.
  - Handles both `FastClosure` and `Closure` in apply/call/cc (for builtins interop).
- `src/builtins.rs` — Builtins module. `procedure?` recognizes `FastClosure` too.
- `src/repl.rs` — REPL module. `format_value` handles `FastClosure` as `#<closure>`.
- `tests/test_fast_eval.rs` — 42 tests validating `pretreat` + `fast_eval` produces identical results to `straw_eval`.
- `tests/test_eval.rs` — 136 passing. `tests/test_repl.rs` — 14 passing.
- `tests/test_lexer.rs` — 17 passing. `tests/test_parser.rs` — 14 passing.
- `tests/test_env.rs` — 9 passing. `tests/test_builtins.rs` — 66 passing.
- `tests/test_semantics.rs` — 104 passing.
- `Cargo.toml` — Crate name is `strawman`, edition 2024. Dev-dependency: `gag = "1.0"`.

## Conventions & Decisions

- Integration tests: `tests/test_{module}.rs`. Builtins tests use `default_env()`.
- Eval tests use inline `make_test_env()` helper with `+`, `-`, `*` builtins.
- Fast eval tests use `eval_both(input, env)` helper that runs both `straw_eval` and `fast_eval`, asserting identical Debug output.
- TDD discipline: write failing test first, then minimal impl.
- `Value::FastClosure(Vec<String>, Vec<TreatedExpr>, Rc<Env>)` — closures created by `fast_eval`.
  Debug format: `Closure({params:?})` (same as regular Closure for comparison).
- `fast_eval` handles both `Closure` and `FastClosure` in `apply_function` and `call/cc`.
  Regular `Closure` bodies are pretreated on-the-fly when encountered.
- Error messages identical between `straw_eval` and `fast_eval`.
- All existing conventions from prior epics remain (see Conventions above for Value types, error formats, etc.).

## Gotchas & Notes for Next Task

- **E6.1 COMPLETE**: `pretreat` + `fast_eval` produce identical results to `straw_eval` on all 42 tests.
- **All 402 tests pass** (360 existing + 42 new fast_eval tests).
- **Next up: E6.2** — Lexical addressing: replace `VarRef(String)` with `LexicalRef(depth, offset)`.
  Need to add a static environment to `pretreat` that tracks variable positions.
  Runtime environment should use vector-of-vectors (ribs) instead of hash maps.
- **E6.3** after that: benchmark harness comparing `straw_eval` vs `fast_eval`.
- The `Application` handler in `fast_eval` uses proper CPS arg evaluation (not eager) to ensure
  `call/cc` captures the correct continuation context. This is critical.
- `TreatedExpr::Quote` stores the original `Expr` (not `TreatedExpr`) since quoted data
  is not evaluated and needs `expr_to_value` conversion.
- `SetCar`/`SetCdr` store an optional `var_name: Option<String>` for env update.
