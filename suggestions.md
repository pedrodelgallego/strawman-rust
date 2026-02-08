# Suggestions

Improvements, new features, and future directions surfaced by Claude during
development. The human decides what to pull into spec.md.

## Active

- [direction] Add a `Quote` token to the lexer before building the parser — the spec requires `'x` → `(quote x)` sugar (E1.2 row 9–10), and handling it as parser-only lookahead on the raw token stream is awkward since `'` currently falls through to the symbol branch and gets absorbed into a multi-char symbol like `'foo` → `Symbol("'foo")`. Emitting a dedicated `Quote` token in the lexer keeps the parser clean and matches how most Lisp readers work. (surfaced after *Stories: E1.1*)
- [improvement] The lexer silently skips unrecognized characters (line 105: bare `i += 1` fallthrough) — `#z`, stray `#` without `t`/`f`, or non-ASCII garbage won't produce an error or a token, they just vanish. Before the evaluator is wired up this is harmless, but once real programs are parsed it will mask typos. Consider returning `Err("unexpected character: ...")` from the catch-all branch. (surfaced after *Stories: E1.1*)
- [improvement] `Token` derives `PartialEq` but not `Clone` — the parser will almost certainly need to clone or copy tokens when building the AST (e.g., peeking ahead without consuming). Adding `#[derive(Clone)]` now avoids a cascade of borrow-checker friction during E1.2. (surfaced after *Stories: E1.1*)

## Deprecated

_No deprecated suggestions yet._
