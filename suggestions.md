# Suggestions

Improvements, new features, and future directions surfaced by Claude during
development. The human decides what to pull into spec.md.

## Active

- [improvement] The lexer silently skips unrecognized characters (line 113: bare `i += 1` fallthrough) — `#z`, stray `#` without `t`/`f`, or non-ASCII garbage won't produce an error or a token, they just vanish. Once the evaluator is wired up this will mask typos in real programs. Consider returning `Err("unexpected character: ...")` from the catch-all branch. (surfaced after *Stories: E1.1*)
- [direction] `Value::List(Vec<Value>)` cannot represent dotted pairs — `cons` (E1.13) must handle `(cons 1 2)` producing a pair, not a list. The current design forces a choice: add a `Value::Pair(Box<Value>, Box<Value>)` variant now, or treat `Value::List` as always-proper and defer dotted pairs. Deciding before Phase 5 avoids rewriting `car`/`cdr`/`null?`/`pair?` later. If dotted pairs are deferred, document that `(cons a b)` where `b` is not a list is an error. (surfaced after **E1.10 — Function application**)
- [improvement] Error messages use `Debug` formatting (`{:?}`) for values — `eval.rs:112` produces `not a procedure: Number(42.0)` instead of the spec's `not a procedure: 42`. A `Display` impl for `Value` is needed before the REPL (E1.16) anyway; adding it early keeps error messages consistent with the spec across all of Phase 5. (surfaced after **E1.10 — Function application**)
- [direction] `and`/`or` must be special forms in the evaluator, not builtins — the spec lists them under "Special forms" because they short-circuit (`(and #f (error))` must not evaluate the second argument). Phase 5 (E1.14) should add them as `Expr::Symbol` match arms in `straw_eval`, alongside `if`/`begin`/`define`, not as entries in `default_env`. (surfaced after **E1.10 — Function application**)

## Deprecated

- [~~direction~~] Add a `Quote` token to the lexer before building the parser — done: `Token::Quote` variant exists and the parser desugars `'x` into `(quote x)` correctly. (deprecated after *Stories: E1.2*)
- [~~improvement~~] `Token` derives `PartialEq` but not `Clone` — turned out the parser only borrows tokens via `&[Token]` and never needs to clone them. The slice+index approach sidestepped the issue entirely. `Clone` can still be added later if needed but is no longer blocking. (deprecated after *Stories: E1.2*)
- [~~direction~~] `Expr` needs `Clone` (and likely `Expr` ≠ `Value`) before E1.4 — resolved: `Expr` derives `Clone`, and `Value` is a separate enum in `env.rs` with `Closure`, `Builtin`, and `Void` variants. The syntax/runtime split is clean. (deprecated after **E1.10 — Function application**)
- [~~direction~~] Environment ownership model requires `Rc<RefCell<Env>>` — resolved with a better design: `Rc<Env>` with `RefCell` only on the inner `HashMap<String, Value>`, not the whole `Env`. This is more granular and avoids broad interior mutability. Closures capture `Rc<Env>`, `set!` mutates through `RefCell<HashMap>`. (deprecated after **E1.10 — Function application**)
