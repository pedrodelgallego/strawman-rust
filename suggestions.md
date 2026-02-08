# Suggestions

Improvements, new features, and future directions surfaced by Claude during
development. The human decides what to pull into spec.md.

## Active

- [improvement] The lexer silently skips unrecognized characters (line 113: bare `i += 1` fallthrough) — `#z`, stray `#` without `t`/`f`, or non-ASCII garbage won't produce an error or a token, they just vanish. Once the evaluator is wired up this will mask typos in real programs. Consider returning `Err("unexpected character: ...")` from the catch-all branch. (surfaced after *Stories: E1.1*)
- [improvement] Error messages use `Debug` formatting (`{:?}`) for values — `eval.rs:138` produces `not a procedure: Number(42.0)` instead of the spec's `not a procedure: 42`, and `display` in `builtins.rs:280` falls back to `{:?}` for lists and pairs. Implementing `Display` for `Value` before the REPL (E1.16) fixes both: error messages match the spec and `display` can print any value in Lisp-readable form. (surfaced after **E1.10 — Function application**, updated after **E1.15 — Type predicates & I/O**)
- [improvement] `cons` onto a `Value::List` uses `Vec::insert(0, head)` which is O(n) — every `cons` onto a list copies the entire vector. This is invisible for small programs but will become a bottleneck in Phase 7 (E2.5 integration tests with `map`/`filter` building lists via recursive `cons`). Consider making `List` a linked structure (e.g. always use `Pair` chains with a `Nil` sentinel) or accept the cost and document it. The hybrid `List`/`Pair` representation also complicates every function that matches on both variants. (surfaced after **E1.15 — Type predicates & I/O**)
- [direction] The lexer's string escape handling (`lexer.rs:71-74`) treats `\` followed by any character as that literal character — `\n` becomes `n`, `\t` becomes `t`. Real Lisps interpret these as newline and tab. This will surface as a bug in Phase 6 when users write `(display "hello\nworld")` and get `hellonworld`. Fix the escape table before the REPL. (surfaced after **E1.15 — Type predicates & I/O**)

## Deprecated

- [~~direction~~] Add a `Quote` token to the lexer before building the parser — done: `Token::Quote` variant exists and the parser desugars `'x` into `(quote x)` correctly. (deprecated after *Stories: E1.2*)
- [~~improvement~~] `Token` derives `PartialEq` but not `Clone` — turned out the parser only borrows tokens via `&[Token]` and never needs to clone them. The slice+index approach sidestepped the issue entirely. `Clone` can still be added later if needed but is no longer blocking. (deprecated after *Stories: E1.2*)
- [~~direction~~] `Expr` needs `Clone` (and likely `Expr` ≠ `Value`) before E1.4 — resolved: `Expr` derives `Clone`, and `Value` is a separate enum in `env.rs` with `Closure`, `Builtin`, and `Void` variants. The syntax/runtime split is clean. (deprecated after **E1.10 — Function application**)
- [~~direction~~] Environment ownership model requires `Rc<RefCell<Env>>` — resolved with a better design: `Rc<Env>` with `RefCell` only on the inner `HashMap<String, Value>`, not the whole `Env`. This is more granular and avoids broad interior mutability. Closures capture `Rc<Env>`, `set!` mutates through `RefCell<HashMap>`. (deprecated after **E1.10 — Function application**)
- [~~direction~~] `Value::List(Vec<Value>)` cannot represent dotted pairs — resolved: `Value::Pair(Box<Value>, Box<Value>)` was added alongside `Value::List`. `cons` produces `Pair` when the tail is not a list, and `car`/`cdr`/`null?`/`pair?` all handle both variants. (deprecated after **E1.15 — Type predicates & I/O**)
- [~~direction~~] `and`/`or` must be special forms in the evaluator, not builtins — done: both are match arms in `straw_eval` (`eval.rs:79-103`) with proper short-circuit semantics. They are not in `default_env`. (deprecated after **E1.15 — Type predicates & I/O**)
