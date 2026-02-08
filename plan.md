# Strawman Implementation Plan

Step-by-step build order using strict TDD. Each phase produces a working,
testable system. Phases map to user stories in `spec.md`.

---

## TDD Cycle

Every task within every phase follows this loop:

```
1. RED    — Write a failing test for the next behavior
2. RED    — Run the test, confirm it fails (for the right reason)
3. GREEN  — Write the minimum code to make the test pass
4. GREEN  — Run the test suite, confirm it passes
5. FIX    — If any test fails, fix code until all tests pass
6. REFACTOR — Clean up code while keeping all tests green
7. REPEAT — Pick the next test case from the Test Matrix and go to step 1
```

**Rules:**
- Never write production code without a failing test first
- Never skip step 2 — seeing the failure confirms the test tests what you think
- Each RED→GREEN cycle should be small (one row from the Test Matrix)
- Run the *full* test suite (`test_dir_command` from config.json) after each GREEN, not just the new test
- Commit after completing each story (all its Test Matrix rows are green)

---

## Phase 1 — Lexer
*Stories: E1.1*

### TDD Steps
1. **RED** — Create `tests/test-lexer.rkt`, write test: `(tokenize "") → '()`
2. **RED** — Run `raco test tests/test-lexer.rkt` → fails (no `tokenize`)
3. **GREEN** — Create `src/lexer.rkt` with minimal `tokenize` that returns `'()`
4. **GREEN** — Run tests → passes
5. **RED** — Write test: `(tokenize "42")` → `(list (token 'NUMBER 42))`
6. **RED** — Run → fails (no token struct, no number parsing)
7. **GREEN** — Define `token` struct, add number lexing
8. **GREEN** — Run tests → both pass
9. Continue for each row in E1.1 Test Matrix:
   - [x] Empty input
   - [x] Single number / negative / float
   - [x] String literal / string with escape
   - [x] Symbol / operator symbol
   - [x] Boolean `#t` / `#f`
   - [x] Parentheses
   - [x] Mixed expression `(+ 1 2)`
   - [x] Comment skipping
   - [x] Whitespace only
   - [x] Unterminated string → error
   - [x] Nested parens
10. **REFACTOR** — Clean up lexer code, ensure no duplication
11. **COMMIT** — All E1.1 tests green

## Phase 2 — Parser
*Stories: E1.2*

### TDD Steps
For each row in E1.2 Test Matrix:
1. **RED** — Write test in `tests/test-parser.rkt`
2. **RED** — Run → fails
3. **GREEN** — Implement just enough in `src/parser.rkt`
4. **GREEN** — Run full suite → all pass

Rows to cover (one RED→GREEN cycle each):
- [x] Single atom: number
- [x] Single atom: symbol
- [x] Single atom: string
- [x] Single atom: boolean
- [x] Simple list `(+ 1 2)`
- [x] Nested list `(+ (* 2 3) 4)`
- [x] Empty list `()`
- [x] Deeply nested `(a (b (c d)))`
- [x] Quote sugar `'x` → `(quote x)`
- [x] Quote list `'(1 2 3)`
- [x] Multiple expressions via `read-all-from-string`
- [x] Error: unmatched open paren
- [x] Error: unmatched close paren
- [x] Error: empty input
- **REFACTOR** then **COMMIT**

## Phase 3 — Environment
*Stories: E1.3*

### TDD Steps
For each row in E1.3 Test Matrix:
- [x] Set and lookup
- [x] Unbound lookup → error
- [x] Parent chain lookup
- [x] Shadowing
- [x] Update existing binding
- [x] Update unbound → error
- [x] Extend with params/args
- [x] Extend arity mismatch → error
- [x] Parent not mutated by child
- **REFACTOR** then **COMMIT**

## Phase 4 — Core Evaluator
*Stories: E1.4, E1.5, E1.6, E1.7, E1.8, E1.9, E1.10*

### TDD Steps
Work through stories in order. For each story, work through its Test Matrix row by row:

**E1.4 — Self-evaluating & symbol lookup:**
- [x] Integer → itself
- [x] Float → itself
- [x] Negative → itself
- [x] String → itself
- [x] Boolean true/false → itself
- [x] Bound symbol → value
- [x] Unbound symbol → error

**E1.5 — Quote:**
- [x] `(quote foo)` → symbol
- [x] `(quote 42)` → number
- [x] `(quote (1 2 3))` → list
- [x] `(quote (a (b c)))` → nested
- [x] `(quote ())` → empty list
- [x] `(quote)` → arity error
- [x] `(quote a b)` → arity error

**E1.6 — If:**
- [x] True branch
- [x] False branch
- [x] Truthy zero
- [x] Truthy empty list
- [x] No alternative (true)
- [x] No alternative (false) → void
- [x] Non-taken branch not evaluated
- [x] No args → error

**E1.7 — Begin:**
- [x] Single expr
- [x] Two/three exprs → last
- [x] Empty begin → void
- [x] Side effect ordering
- [x] Nested begin

**E1.8 — Define & Set!:**
- [x] Simple define
- [x] Define overwrites
- [x] Define with expression
- [x] Set! existing
- [x] Set! parent binding
- [x] Set! unbound → error
- [x] Define returns void

**E1.9 — Lambda & closures:**
- [x] Identity function
- [x] Multi param
- [x] Closure captures env
- [x] Closure over closure (make-adder)
- [x] Implicit begin in body
- [x] No params
- [x] Wrong arity → error
- [x] Bad param list → error

**E1.10 — Function application:**
- [x] Builtin call
- [x] Closure call
- [x] Nested calls
- [x] Higher-order function
- [x] Non-procedure in operator → error

**REFACTOR** then **COMMIT** after each story.

## Phase 5 — Builtins
*Stories: E1.11, E1.12, E1.13, E1.14, E1.15*

### TDD Steps
For each story, row-by-row through Test Matrix:

**E1.11 — Arithmetic:**
- [x] `(+)` → 0
- [x] `(+ 5)` → 5
- [x] `(+ 1 2 3 4)` → 10
- [x] `(+ 1.5 2.5)` → 4.0
- [x] `(- 5)` → -5
- [x] `(- 10 3)` → 7
- [x] `(- 10 3 2)` → 5
- [x] `(*)` → 1
- [x] `(* 3 4)` → 12
- [x] `(/ 10 2)` → 5
- [x] `(/ 7 2)` → 3.5
- [x] `(/ 1 0)` → error
- [x] `(mod 10 3)` → 1
- [x] `(mod 10 0)` → error
- [x] `(+ 1 "a")` → error

**E1.12 — Comparison & equality:**
- [x] `<`, `>`, `<=`, `>=` — true/false cases
- [x] `=` — numeric equality
- [x] `equal?` — atoms, strings, lists, nested, different
- [x] Non-number arg → error

**E1.13 — List operations:**
- [x] `cons` onto list / dotted
- [x] `car` / `cdr`
- [x] `list` empty / many
- [x] `null?` / `pair?` — various types
- [x] `car` of empty → error
- [x] `cdr` of atom → error

**E1.14 — and/or/not:**
- [x] `and` — all true, short-circuit, empty, one false
- [x] `or` — first true, all false, short-circuit, empty
- [x] `not` — true, false, truthy

**E1.15 — Type predicates & I/O:**
- [x] Each predicate: positive and negative case
- [x] `display` string / number
- [x] `newline`

**REFACTOR** then **COMMIT** after each story.

## Phase 6 — REPL & Entry Point
*Stories: E1.16*

### TDD Steps
- [x] Simple expression → prints result
- [x] Computation → prints result
- [x] Define then use across inputs
- [x] Multi-line input (balanced parens)
- [x] Error recovery (error then normal expr)
- [x] Unbound variable error
- [x] `(exit)` terminates
- [x] `(quit)` terminates
- [x] EOF terminates
- [x] Wire `strawman.rkt`: no args → REPL, file arg → execute
- **REFACTOR** then **COMMIT**

## Phase 7 — Namespaces & Recursion
*Stories: E2.1 through E2.5*

### TDD Steps

**E2.1 — let:**
- [x] Simple / two bindings / parallel semantics
- [x] Shadowing / outer unchanged
- [x] Body implicit begin / nested let / empty bindings
- [x] Malformed → error

**E2.2 — let*:**
- [x] Sequential deps / three deps / shadow across / empty

**E2.3 — letrec:**
- [x] Self-recursive (factorial)
- [x] Mutual recursion (even?/odd?)
- [x] Non-lambda value

**E2.4 — Define shorthand:**
- [x] `(define (f x) body)` — simple, multi-param, with body, recursive, no params

**E2.5 — Integration:**
- [x] Factorial 0 / factorial 10
- [x] Fibonacci 10
- [x] Map with lambda
- [x] Filter with lambda
- [x] Closure scope (lexical vs dynamic)
- [x] Accumulator (mutable closure)

**REFACTOR** then **COMMIT** after each story.

---

*Phases 8–15 below are deferred until the MVP (Phases 1–7) is solid.
Same TDD cycle applies: RED → RED → GREEN → GREEN → FIX → REFACTOR → REPEAT.*

---

## Phase 8 — Continuations
*Stories: E3.1 through E3.5*

### TDD Steps
- [x] **E3.1** — Run all existing tests against CPS evaluator → must pass unchanged
- [x] **E3.2** — `call/cc`: normal return, early exit, in expression, saved continuation, non-procedure error
- [x] **E3.3** — `catch`/`throw`: no throw, simple throw, nested, wrong tag, throw in function
- [x] **E3.4** — `block`/`return-from`: no return, early return, nested, inner, unknown block
- [x] **E3.5** — `unwind-protect`: normal, with throw, cleanup order

## Phase 9 — Side Effects & Mutation
*Stories: E4.1 through E4.4*

### TDD Steps
- [x] **E4.1** — Shared mutation between closures, lambda captures box
- [x] **E4.2** — `set-car!`, `set-cdr!`, non-pair error
- [x] **E4.3** — `eq?` vs `equal?`: same symbol, same number, different lists, same binding, structural equality
- [x] **E4.4** — Vectors: make/ref, set/ref, length, type pred, out of bounds

## Phase 10 — Denotational Semantics
*Stories: E5.1, E5.2*

### TDD Steps
- [x] **E5.1** — Write `docs/semantics.md`
- [x] **E5.2** — Derive one test per valuation clause, run full suite

## Phase 11 — Fast Interpretation
*Stories: E6.1 through E6.3*

### TDD Steps
- [x] **E6.1** — Run all existing tests through pretreat + fast-eval → identical results
- [ ] **E6.2** — Lexical addressing: local var, free var one level, multiple params
- [ ] **E6.3** — Benchmark harness: factorial, fibonacci, ackermann, map

## Phase 12 — Bytecode Compilation
*Stories: E7.1 through E7.4*

### TDD Steps
- [ ] **E7.1** — Document instruction set in `docs/bytecode.md`
- [ ] **E7.2** — Compile each core form, test bytecode output
- [ ] **E7.3** — VM execute: all existing tests via compile + VM → identical results
- [ ] **E7.4** — REPL with `--compiled` flag, all REPL tests pass in both modes

## Phase 13 — Eval & Reflection
*Stories: E8.1 through E8.2*

### TDD Steps
- [ ] **E8.1** — `eval`: simple, constructed, with define, nested, non-list
- [ ] **E8.2** — First-class envs: capture, isolation, type predicate

## Phase 14 — Macros
*Stories: E9.1 through E9.4*

### TDD Steps
- [ ] **E9.1** — `define-macro`: simple macro, swap macro, expansion
- [ ] **E9.2** — `macroexpand`: expand once, non-macro unchanged
- [ ] **E9.3** — Quasiquote: simple unquote, splicing, nested, no unquote
- [ ] **E9.4** — Standard macros: `cond` (first/second/else/none), `when`, `unless`

## Phase 15 — Object System
*Stories: E10.1 through E10.4*

### TDD Steps
- [ ] **E10.1** — `define-class`: simple, subclass, duplicate field error, unknown parent error
- [ ] **E10.2** — Instantiation: construct, access, mutate, predicate, wrong arity, wrong type
- [ ] **E10.3** — Generics: single method, inherited, no method, override
- [ ] **E10.4** — Inheritance: inherited field, own field, `is-a?` direct/parent/unrelated

---

## File Structure

```
strawman/
  strawman.rkt           # Entry point (Phase 6)
  src/
    lexer.rkt            # Phase 1
    parser.rkt           # Phase 2
    env.rkt              # Phase 3
    eval.rkt             # Phase 4
    builtins.rkt         # Phase 5
    repl.rkt             # Phase 6
  tests/
    test-lexer.rkt       # Phase 1
    test-parser.rkt      # Phase 2
    test-env.rkt         # Phase 3
    test-eval.rkt        # Phase 4
    test-builtins.rkt    # Phase 5
    test-repl.rkt        # Phase 6
    test-integration.rkt # Phase 7
  examples/
    hello.straw
    factorial.straw
    fibonacci.straw
  docs/
    semantics.md         # Phase 10
    bytecode.md          # Phase 12
  bench/
    run.rkt              # Phase 11
  spec.md
  plan.md
  CLAUDE.md
  README.md
```

## Definition of Done (per phase)

1. Every Test Matrix row has a corresponding failing-then-passing test
2. Full suite passes (run `test_dir_command` from config.json)
3. No regressions in previous phases
4. Code refactored (no duplication, clear naming)
5. Committed with a descriptive message
