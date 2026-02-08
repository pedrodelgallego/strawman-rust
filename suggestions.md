# Suggestions

Improvements, new features, and future directions surfaced by Claude during
development. The human decides what to pull into spec.md.

## Active

- [improvement] The lexer silently skips unrecognized characters (line 113: bare `i += 1` fallthrough) — `#z`, stray `#` without `t`/`f`, or non-ASCII garbage won't produce an error or a token, they just vanish. Once the evaluator is wired up this will mask typos in real programs. Consider returning `Err("unexpected character: ...")` from the catch-all branch. (surfaced after *Stories: E1.1*)
- [direction] `Expr` needs `Clone` (and likely `Expr` ≠ `Value`) before E1.4 — the evaluator will store lambda bodies inside closures, and `quote` must return AST subtrees as values. More fundamentally, `Expr` conflates syntax and runtime values; the evaluator will need a separate `Value` enum with variants like `Closure { params, body, env }`, `Builtin(fn)`, `Pair(Box, Box)`, and `Void`. Designing `Value` as a distinct type before writing `straw-eval` avoids a painful refactor mid-epic. (surfaced after *Stories: E1.2*)
- [direction] Environment ownership model requires `Rc<RefCell<Env>>` — closures capture their defining environment, multiple closures can share the same env, and `set!` mutates through that shared reference. A plain `HashMap` with parent `Box` won't work because closures need shared ownership of env frames. This is the central Rust-specific design decision for E1.3 and should be settled before writing tests. (surfaced after *Stories: E1.2*)

## Deprecated

- [~~direction~~] Add a `Quote` token to the lexer before building the parser — done: `Token::Quote` variant exists and the parser desugars `'x` into `(quote x)` correctly. (deprecated after *Stories: E1.2*)
- [~~improvement~~] `Token` derives `PartialEq` but not `Clone` — turned out the parser only borrows tokens via `&[Token]` and never needs to clone them. The slice+index approach sidestepped the issue entirely. `Clone` can still be added later if needed but is no longer blocking. (deprecated after *Stories: E1.2*)
