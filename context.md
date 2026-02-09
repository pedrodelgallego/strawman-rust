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
  - `Env` struct with `bindings: RefCell<HashMap<String, Value>>`, `rib: RefCell<Vec<Value>>`, `parent: Option<Rc<Env>>`.
  - Methods: `new`, `with_parent`, `set`, `update`, `extend`, `lookup`, `lexical_lookup`, `rib_set`.
  - `extend` populates both `bindings` (HashMap) and `rib` (Vec) for dual-path lookup.
  - `lexical_lookup(depth, offset)` — O(1)-ish lookup: walks `parent` chain `depth` times, indexes `rib[offset]`.
  - `rib_set(offset, value)` — updates rib at given offset (used by letrec init).
- `src/eval.rs` — CPS evaluator module. Exports `straw_eval` and `straw_eval_k`.
- `src/pretreat.rs` — Pretreatment module. Exports `TreatedExpr` enum and `pretreat` function.
  - `TreatedExpr` derives `Debug, Clone, PartialEq` — enables `assert_eq!` in tests.
  - `TreatedExpr` variants: `Number`, `StringLit`, `Boolean`, `VarRef`, **`LexicalRef(usize, usize)`**,
    `Quote`, `If`, `Begin`, `Define`, `DefineFunc`, `SetBang`, `Lambda`, `And`, `Or`,
    `Let`, `LetStar`, `Letrec`, `Catch`, `Throw`, `Block`, `ReturnFrom`, `UnwindProtect`,
    `CallCC`, `SetCar`, `SetCdr`, `Application`.
  - `pretreat(expr: &Expr) -> TreatedExpr` — calls `pretreat_inner(expr, &[])` with empty static env.
  - `pretreat_inner(expr, senv)` — tracks a `StaticEnv` (stack of ribs = `&[Vec<String>]`).
    Lambda, DefineFunc, Let, LetStar, Letrec all extend the static env with their parameter/binding names.
  - `lookup_lexical(name, senv)` — scans ribs innermost-first; if found returns `LexicalRef(depth, offset)`,
    otherwise returns `VarRef(name)` for globals/builtins.
- `src/fast_eval.rs` — Fast CPS evaluator. Exports `fast_eval` function.
  - Handles `LexicalRef(depth, offset)` via `env.lexical_lookup(depth, offset)`.
  - Handles `VarRef(name)` via `env.lookup(name)` (for globals/builtins not in lexical scope).
  - Creates `FastClosure` values; handles both `FastClosure` and `Closure` in apply/call-cc.
- `src/builtins.rs` — Builtins module. `procedure?` recognizes `FastClosure` too.
- `src/repl.rs` — REPL module. `format_value` handles `FastClosure` as `#<closure>`.
- `tests/test_fast_eval.rs` — 50 tests (42 E6.1 + 8 E6.2 lexical addressing).
- `tests/test_pretreat.rs` — 6 tests (E6.2 pretreat unit tests: local var, free var, multi-params, acceptance, globals).
- `tests/test_eval.rs` — 136 passing. `tests/test_repl.rs` — 14 passing.
- `tests/test_lexer.rs` — 17 passing. `tests/test_parser.rs` — 14 passing.
- `tests/test_env.rs` — 9 passing. `tests/test_builtins.rs` — 66 passing.
- `tests/test_semantics.rs` — 104 passing.
- `Cargo.toml` — Crate name is `strawman`, edition 2024. Dev-dependency: `gag = "1.0"`.

## Conventions & Decisions

- Integration tests: `tests/test_{module}.rs`. Builtins tests use `default_env()`.
- Eval tests use inline `make_test_env()` helper with `+`, `-`, `*` builtins.
- Fast eval tests use `eval_both(input, env)` helper that runs both `straw_eval` and `fast_eval`, asserting identical Debug output.
- Pretreat tests use `pt(input)` helper that parses+pretreats, then `assert_eq!` on `TreatedExpr` variants.
- TDD discipline: write failing test first, then minimal impl.
- `Value::FastClosure(Vec<String>, Vec<TreatedExpr>, Rc<Env>)` — closures created by `fast_eval`.
  Debug format: `Closure({params:?})` (same as regular Closure for comparison).
- `fast_eval` handles both `Closure` and `FastClosure` in `apply_function` and `call/cc`.
  Regular `Closure` bodies are pretreated on-the-fly when encountered.
- Error messages identical between `straw_eval` and `fast_eval`.
- Lexical addressing: `pretreat` resolves lambda/let-bound vars to `LexicalRef(depth, offset)`.
  Globals/builtins remain `VarRef(name)`. `Env::extend` stores args in both HashMap and Vec rib.

## Gotchas & Notes for Next Task

- **E6.1 COMPLETE**: `pretreat` + `fast_eval` produce identical results to `straw_eval`.
- **E6.2 COMPLETE**: Lexical addressing with `LexicalRef(depth, offset)` in pretreat, `lexical_lookup` in Env.
  Static env tracks ribs for lambda, define-func, let, let*, letrec. All 14 E6.2 tests pass (8 in test_fast_eval + 6 in test_pretreat).
- **All 416 tests pass** (360 existing + 50 fast_eval + 6 pretreat).
- **Next up: E6.3** — Benchmark harness comparing `straw_eval` vs `fast_eval`.
- The `Application` handler in `fast_eval` uses proper CPS arg evaluation (not eager) to ensure
  `call/cc` captures the correct continuation context. This is critical.
- `TreatedExpr::Quote` stores the original `Expr` (not `TreatedExpr`) since quoted data
  is not evaluated and needs `expr_to_value` conversion.
- `SetCar`/`SetCdr` store an optional `var_name: Option<String>` for env update.
- `let*` creates one rib per binding in the static env (progressive extension).
  `letrec` creates a single rib with all names visible to all init exprs.
