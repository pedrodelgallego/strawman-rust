# Context

Rolling working memory for the Strawman build. Updated by Claude after each task.
Read by Claude at the start of the next task. Keep under 100 lines.

## Current State

- `src/main.rs` — Entry point (binary). Just `fn main()` with a placeholder print.
- `src/lib.rs` — Library root. Declares `pub mod lexer;` and `pub mod parser;`.
- `src/lexer.rs` — Lexer module. Exports `Token` enum and `tokenize` function.
  - `Token` enum: `Number(f64)`, `StringLit(String)`, `Boolean(bool)`, `Symbol(String)`,
    `LParen`, `RParen`, `Quote`. Derives `Debug, PartialEq`.
  - `pub fn tokenize(input: &str) -> Result<Vec<Token>, String>` — character-by-character
    scanner. Handles comments, whitespace, parens, quote (`'`), numbers, strings,
    booleans, symbols. `'` is excluded from symbol characters.
- `src/parser.rs` — Parser module. Exports `Expr` enum, `parse`, and `parse_all`.
  - `Expr` enum: `Number(f64)`, `Symbol(String)`, `StringLit(String)`, `Boolean(bool)`,
    `List(Vec<Expr>)`. Derives `Debug, PartialEq`.
  - `pub fn parse(input: &str) -> Result<Expr, String>` — tokenizes input, then
    calls `parse_expr` on the token stream. Returns first parsed expression.
  - `pub fn parse_all(input: &str) -> Result<Vec<Expr>, String>` — tokenizes input,
    then parses all expressions in the token stream. Returns `Vec<Expr>`.
    Empty input → `Err("empty input")`.
  - `fn parse_expr(tokens: &[Token], pos: &mut usize) -> Result<Expr, String>` —
    recursive-descent parser. Handles all Token variants including `Token::Quote`,
    which desugars `'expr` into `Expr::List(vec![Expr::Symbol("quote"), expr])`.
- `tests/test_lexer.rs` — 17 passing lexer tests.
- `tests/test_parser.rs` — 14 passing parser tests: `single_atom_number`,
  `single_atom_symbol`, `single_atom_string`, `single_atom_bool`, `simple_list`,
  `nested_list`, `empty_list`, `deeply_nested`, `quote_sugar_atom`, `quote_sugar_list`,
  `multiple_exprs`, `unmatched_open_paren`, `unmatched_close_paren`, `empty_input`.
- `Cargo.toml` — Crate name is `strawman`, edition 2024.

## Conventions & Decisions

- Lexer: `src/lexer.rs`, declared via `pub mod lexer;` in `lib.rs`.
- Parser: `src/parser.rs`, declared via `pub mod parser;` in `lib.rs`.
- `main.rs` is the binary entry point; `lib.rs` is the library root.
- Integration tests: `tests/test_{module}.rs`. Parser tests import with
  `use strawman::parser::{parse, parse_all, Expr};`.
- TDD discipline: write failing test first, then minimal impl.
- `Token` enum uses `f64` for all numbers. `Expr::Number(f64)` mirrors this.
- Parser uses recursive-descent with `&[Token]` slice + `&mut usize` position.
- `parse(input)` returns first expression. `parse_all(input)` returns all expressions.
- Both return `Result<..., String>`. Happy-path tests use `.unwrap()`.
- Error messages: `"empty input"`, `"unexpected end of input"`,
  `"unexpected token"`, `"unexpected closing paren"`.
- Quote sugar: lexer emits `Token::Quote` for `'`, parser desugars to
  `(quote <expr>)`. No special `Quote` variant in `Expr` — it's just a `List`.

## Gotchas & Notes for Next Task

- **Parser E1.2 is complete.** All 14 test matrix rows are covered.
- Next up: E1.3 (Environment) or E1.4 (Evaluator) per plan.md.
- `parse` returns only the first expression and ignores trailing tokens.
  `parse_all` consumes all tokens and returns `Vec<Expr>`.
- Both `parse` and `parse_all` return `Err("empty input")` for empty/whitespace input.
- Unmatched paren errors: missing `)` → `"unexpected end of input"`,
  stray `)` → `"unexpected closing paren"`.
- Quote desugaring is recursive — `''x` produces `(quote (quote x))`.
- `Expr` has no `Clone` derive yet — add it when the evaluator needs to clone AST nodes.
