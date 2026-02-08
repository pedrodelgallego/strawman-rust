# Context

Rolling working memory for the Strawman build. Updated by Claude after each task.
Read by Claude at the start of the next task. Keep under 100 lines.

## Current State

- `src/main.rs` — Entry point (binary). Just `fn main()` with a placeholder print.
- `src/lib.rs` — Library root. Declares `pub mod lexer;`.
- `src/lexer.rs` — Lexer module. Exports `Token` enum and `tokenize` function.
  - `Token` enum: `Number(f64)`, `StringLit(String)`, `Boolean(bool)`, `Symbol(String)`,
    `LParen`, `RParen`. Derives `Debug, PartialEq`.
  - `pub fn tokenize(input: &str) -> Result<Vec<Token>, String>` — character-by-character
    scanner. Returns `Ok(tokens)` on success, `Err(message)` on error.
    Handles: comments (`;` to end of line, skipped),
    whitespace (skipped), parentheses (`(` → `LParen`, `)` → `RParen`),
    numbers (integers, negatives, floats),
    string literals (with backslash escape sequences),
    booleans (`#t` → `Boolean(true)`, `#f` → `Boolean(false)`),
    symbols (identifiers like `foo` and operators like `+`, `*`, `-`).
    Unterminated strings return `Err("unterminated string")`.
  - `fn is_symbol_char(ch: char) -> bool` — helper; returns true for any char that
    isn't whitespace, `(`, `)`, `"`, or `;`.
- `tests/test_lexer.rs` — Integration tests for lexer.
  Contains 17 tests (all passing): `empty_input_returns_empty_list`, `single_number`,
  `negative_number`, `float_number`, `string_literal`, `string_with_escape`, `symbol`,
  `operator_symbol`, `boolean_true`, `boolean_false`, `parens`, `mixed_expression`,
  `comment_skipped`, `comment_skipped_with_newline`, `whitespace_only_returns_empty_list`,
  `unterminated_string_returns_error`, `nested_parens`.
- `Cargo.toml` — Crate name is `strawman`, edition 2024.

## Conventions & Decisions

- Lexer module lives at `src/lexer.rs`, declared via `pub mod lexer;` in `lib.rs`.
- `main.rs` is the binary entry point; `lib.rs` is the library root that exposes
  modules for integration tests.
- Integration tests live in `tests/test_lexer.rs` (matching `test_{module}` pattern
  from config.json). Import with `use strawman::lexer::{tokenize, Token};`.
- Test names describe behavior: `single_number`, `negative_number`, `symbol`, etc.
- TDD discipline: write failing test first, then minimal impl.
- `Token` enum uses `f64` for all numbers (integers stored as `42.0` etc.).
- `Token::StringLit(String)` for string literals (named `StringLit` to avoid
  collision with Rust's `String` type).
- `Token::Symbol(String)` for identifiers and operator symbols.
- Lexer uses `Vec<char>` + index-based scanning (not iterator-based).
- String escape handling: backslash causes next char to be taken literally.
- `tokenize` returns `Result<Vec<Token>, String>`. Happy-path tests call `.unwrap()`.
  Error tests assert on `is_err()` and check the error message with `unwrap_err()`.
- Negative numbers: `-` followed by digit is parsed as a negative number.
  Standalone `-` (not followed by digit) is parsed as a Symbol.
- Symbol parsing: any sequence of `is_symbol_char` characters. This helper returns
  true for anything that isn't whitespace, `(`, `)`, `"`, or `;`.

## Gotchas & Notes for Next Task

- All 17 test matrix rows for E1.1 (Lexer) are now covered and passing.
  The lexer story E1.1 is complete. Next phase: Parser (E1.2).
- `tokenize` returns `Result<Vec<Token>, String>`. Any new callers (parser,
  REPL, etc.) must handle the Result. All existing tests use `.unwrap()`.
- The negative number heuristic (`-` + digit = number) may conflict with expressions
  like `(-3)` (no space). This is acceptable — Lisp convention uses spaces.
- `is_symbol_char` allows digits inside symbols (e.g., `foo123`), which is correct.
  `#` is still a valid symbol char, but `#t`/`#f` are caught before the symbol
  branch. If other `#`-prefixed tokens are needed later, the boolean branch or
  a general `#`-dispatch may need refinement.
- Parser will need to consume `Vec<Token>` and produce an AST (s-expressions).
  Consider a recursive-descent parser with `parse` as the public entry point.
