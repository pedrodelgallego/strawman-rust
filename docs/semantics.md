# Denotational Semantics of Strawman Lisp

This document defines the formal denotational semantics for the core forms
of Strawman Lisp, following the style of *Lisp in Small Pieces* (Queinnec).
The semantics are written in standard mathematical notation using an ASCII
approximation.

---

## 1. Semantic Domains

```
Val  = Num + Str + Bool + Sym + List(Val) + Pair(Val, Val)
     + Vec(Val*) + Closure(Var*, Expr*, Env) + Builtin(Val* -> Val)
     + Cont(Val -> Val) + Void

Env  = Var -> (Val + unbound)

Cont = Val -> Val       -- continuation (result handler)

Err  = String           -- error messages
```

**Val** is the domain of all Strawman values. It is the disjoint union of:

| Domain component              | Description                           |
|-------------------------------|---------------------------------------|
| `Num`                         | IEEE 754 double-precision floats      |
| `Str`                         | Character strings                     |
| `Bool`                        | `#t` or `#f`                          |
| `Sym`                         | Interned symbols                      |
| `List(Val)`                   | Proper lists (possibly empty)         |
| `Pair(Val, Val)`              | Dotted pairs (improper lists)         |
| `Vec(Val*)`                   | Mutable vectors                       |
| `Closure(Var*, Expr*, Env)`   | User-defined functions                |
| `Builtin(Val* -> Val)`        | Host-language primitives              |
| `Cont(Val -> Val)`            | Reified first-class continuations     |
| `Void`                        | Unit value (side-effect results)      |

**Env** maps variable names to values. Environments are chained: each
environment has an optional parent. Lookup traverses the chain.

**Cont** is the continuation domain. Continuations receive a value and
produce the final result.

---

## 2. Truthiness

Strawman uses a minimal falsity rule:

```
falsy?(v) = (v = Bool(#f))
truthy?(v) = not(falsy?(v))
```

Only `#f` is falsy. All other values --- including `0`, `""`, `'()`, and
`Void` --- are truthy.

---

## 3. Environment Operations

```
lookup(x, env) =
  if x in dom(env.bindings) then env.bindings(x)
  else if env.parent != nil then lookup(x, env.parent)
  else error("unbound variable: " ++ x)

set(x, v, env) =
  env.bindings(x) := v

update(x, v, env) =
  if x in dom(env.bindings) then env.bindings(x) := v
  else if env.parent != nil then update(x, v, env.parent)
  else error("cannot set! unbound variable: " ++ x)

extend(env, [x1..xn], [v1..vn]) =
  if n != m then error("arity mismatch: expected n, got m")
  else let env' = new Env with parent = env
       in  for i = 1..n: set(xi, vi, env')
           env'
```

---

## 4. Valuation Function E

The main valuation function is:

```
E : Expr * Env * Cont -> Val
```

It takes an expression, an environment, and a continuation, and produces
a value (or signals an error).

### 4.1 Self-Evaluating Forms (Literals)

```
E[n, env, k]       = k(Num(n))             -- number literal
E[s, env, k]       = k(Str(s))             -- string literal
E[b, env, k]       = k(Bool(b))            -- boolean literal (#t or #f)
```

Numbers, strings, and booleans evaluate to themselves.

### 4.2 Symbol (Variable Reference)

```
E[x, env, k]       = k(lookup(x, env))
```

A symbol evaluates to its binding in the current environment chain.
Signals `"unbound variable: x"` if not found.

### 4.3 Quote

```
E[(quote e), env, k] = k(datum(e))
```

where `datum` converts syntax to data:

```
datum(n)           = Num(n)
datum(s)           = Str(s)
datum(b)           = Bool(b)
datum(x)           = Sym(x)           -- symbol, not looked up
datum((e1 .. en))  = List(datum(e1), .., datum(en))
```

### 4.4 Conditional (if)

```
E[(if e1 e2 e3), env, k] =
  let v = E[e1, env, id] in
    if truthy?(v) then E[e2, env, k]
    else E[e3, env, k]

E[(if e1 e2), env, k] =
  let v = E[e1, env, id] in
    if truthy?(v) then E[e2, env, k]
    else k(Void)
```

The condition is evaluated with the identity continuation. Only `#f` selects
the else branch. Missing else produces `Void`.

### 4.5 Sequence (begin)

```
E[(begin), env, k]          = k(Void)
E[(begin e1 .. en), env, k] =
  let _ = E[e1, env, id] in
  ..
  let _ = E[en-1, env, id] in
  E[en, env, k]
```

`begin` evaluates forms left-to-right. The value of the last form is
passed to the continuation. An empty `begin` yields `Void`.

Note: intermediate forms are evaluated with the identity continuation `id`;
only the final form receives the outer continuation `k`.

### 4.6 Definition (define)

#### Simple definition

```
E[(define x e), env, k] =
  let v = E[e, env, id] in
  set(x, v, env);
  k(Void)
```

#### Function shorthand

```
E[(define (f x1..xn) body1..bodym), env, k] =
  let closure = Closure([x1..xn], [body1..bodym], env) in
  set(f, closure, env);
  k(Void)
```

The function shorthand `(define (f x1 .. xn) body)` is equivalent to
`(define f (lambda (x1 .. xn) body))`.

### 4.7 Assignment (set!)

```
E[(set! x e), env, k] =
  let v = E[e, env, id] in
  update(x, v, env);
  k(Void)
```

`set!` mutates an existing binding. Signals `"cannot set! unbound variable: x"`
if `x` is not bound in any enclosing scope.

### 4.8 Lambda

```
E[(lambda (x1..xn) body1..bodym), env, k] =
  k(Closure([x1..xn], [body1..bodym], env))
```

Lambda captures the current environment, creating a closure. The closure
records the parameter names, the body expressions, and the defining
environment.

### 4.9 Function Application

```
E[(e0 e1 .. en), env, k] =
  let f   = E[e0, env, id] in
  let v1  = E[e1, env, id] in
  ..
  let vn  = E[en, env, id] in
  apply(f, [v1..vn], k)
```

The operator and operands are evaluated left-to-right with the identity
continuation. The result of application is passed to `k`.

```
apply(Builtin(f), args, k)                    = k(f(args))
apply(Closure(params, body, cenv), args, k)   =
  let env' = extend(cenv, params, args) in
  let result = E-seq[body, env', id] in
  k(result)
apply(Cont(kc), [v], k)                       = escape(kc(v))
apply(_, _, _)                                 = error("not a procedure")
```

where `E-seq` evaluates a sequence and returns the last value:

```
E-seq([e1..en], env, k) =
  let _ = E[e1, env, id] in .. let _ = E[en-1, env, id] in E[en, env, k]
```

Applying a reified continuation `Cont(kc)` invokes `kc` on the argument,
abandoning the current computation (escape semantics).

### 4.10 Logical Connectives (and, or)

```
E[(and), env, k]             = k(Bool(#t))
E[(and e1 .. en), env, k]    =
  for i = 1 to n-1:
    let vi = E[ei, env, id] in
    if falsy?(vi) then return k(Bool(#f))
  E[en, env, k]
```

`and` short-circuits on the first falsy value, returning `#f`. If all
values are truthy, the last value is passed to `k`.

```
E[(or), env, k]              = k(Bool(#f))
E[(or e1 .. en), env, k]     =
  for i = 1 to n-1:
    let vi = E[ei, env, id] in
    if truthy?(vi) then return k(vi)
  E[en, env, k]
```

`or` short-circuits on the first truthy value.

### 4.11 Let Forms

#### let

```
E[(let ((x1 e1) .. (xn en)) body1..bodym), env, k] =
  let v1 = E[e1, env, id] in
  ..
  let vn = E[en, env, id] in
  let env' = new Env with parent = env in
  set(x1, v1, env'); .. set(xn, vn, env');
  E-seq[body, env', k]
```

All init expressions are evaluated in the **outer** environment before
any bindings take effect. This means `let` bindings cannot refer to
each other.

#### let*

```
E[(let* ((x1 e1) .. (xn en)) body1..bodym), env, k] =
  let env1 = new Env with parent = env in
  let v1 = E[e1, env, id] in set(x1, v1, env1);
  let env2 = new Env with parent = env1 in
  let v2 = E[e2, env1, id] in set(x2, v2, env2);
  ..
  let envn = new Env with parent = env(n-1) in
  let vn = E[en, env(n-1), id] in set(xn, vn, envn);
  E-seq[body, envn, k]
```

Each binding is visible to subsequent init expressions. Each binding
creates a new scope frame.

#### letrec

```
E[(letrec ((x1 e1) .. (xn en)) body1..bodym), env, k] =
  let env' = new Env with parent = env in
  set(x1, Void, env'); .. set(xn, Void, env');
  let v1 = E[e1, env', id] in .. let vn = E[en, env', id] in
  set(x1, v1, env'); .. set(xn, vn, env');
  E-seq[body, env', k]
```

All variables are first bound to `Void` in a shared environment. Then all
init expressions are evaluated in that environment (allowing mutual
recursion). Finally, the bindings are updated with the computed values.

### 4.12 Mutation (set-car!, set-cdr!)

```
E[(set-car! x e), env, k] =
  let target = E[x, env, id] in
  let v = E[e, env, id] in
  match target with
  | Pair(_, d)     -> update(x, Pair(v, d), env); k(Void)
  | List(a :: rest) -> update(x, List(v :: rest), env); k(Void)
  | _              -> error("expected mutable pair")

E[(set-cdr! x e), env, k] =
  let target = E[x, env, id] in
  let v = E[e, env, id] in
  match target with
  | Pair(a, _)     -> update(x, Pair(a, v), env); k(Void)
  | List(a :: rest) -> update(x, Pair(a, v), env); k(Void)
  | _              -> error("expected mutable pair")
```

These special forms mutate pair structure in place. They require the first
argument to be a symbol bound to a pair or non-empty list.

### 4.13 First-Class Continuations (call/cc)

```
E[(call/cc e), env, k] =
  let f = E[e, env, id] in
  match f with
  | Closure([x], body, cenv) ->
      let cont_val = Cont(k) in
      let env' = new Env with parent = cenv in
      set(x, cont_val, env');
      let result = E-seq[body, env', id] in
      k(result)
  | _ -> error("expected procedure")
```

`call/cc` reifies the current continuation `k` as a first-class value.
When the reified continuation is invoked (via `apply(Cont(kc), [v], _)`),
the computation escapes to the point where `call/cc` was called, returning
`v` as the result.

### 4.14 Catch and Throw

```
E[(catch tag-expr body), env, k] =
  let tag = E[tag-expr, env, id] in
  match tag with
  | Sym(t) ->
      match E[body, env, id] with
      | normal(v)        -> k(v)
      | thrown(t', v') ->
          if t = t' then k(v')
          else propagate thrown(t', v')
  | _ -> error("catch: tag must be a symbol")

E[(throw tag-expr val-expr), env, k] =
  let tag = E[tag-expr, env, id] in
  match tag with
  | Sym(t) ->
      let v = E[val-expr, env, id] in
      signal thrown(t, v)
  | _ -> error("throw: tag must be a symbol")
```

`catch` establishes a handler for throws with a matching tag. `throw`
signals a non-local exit. If no enclosing `catch` matches, the error
`"no matching catch for tag: t"` is raised.

### 4.15 Block and Return-From

```
E[(block name body1..bodyn), env, k] =
  for each bodyi:
    match E[bodyi, env, id] with
    | normal(v) -> continue (v becomes result if last)
    | returned(name', v') ->
        if name = name' then return k(v')
        else propagate returned(name', v')
  k(result-of-last-body)

E[(return-from name expr), env, k] =
  let v = E[expr, env, id] in
  signal returned(name, v)
```

`block` establishes a named exit point. `return-from` performs a non-local
exit to the nearest enclosing `block` with the matching name. If no match
is found, `"unknown block: name"` is raised.

### 4.16 Unwind-Protect

```
E[(unwind-protect protected cleanup1..cleanupn), env, k] =
  let result = E[protected, env, id] in   -- may succeed or fail
  E[cleanup1, env, id]; .. E[cleanupn, env, id];  -- always executed
  match result with
  | normal(v) -> k(v)
  | error(e)  -> propagate error(e)
```

The cleanup forms are **always** executed, whether the protected form
completes normally or signals an error.

---

## 5. Builtins

Builtins are host-language functions wrapped as `Builtin(name, f)` values.
They live in the default environment. Their denotations:

### 5.1 Arithmetic

```
D[+](v1..vn)      = sum of v1..vn (all must be Num)
D[-](v)            = -v            (unary negation)
D[-](v1, v2..vn)   = v1 - v2 - .. - vn
D[*](v1..vn)       = product of v1..vn
D[/](v1, v2..vn)   = v1 / v2 / .. / vn  (error if any vi=0 for i>1)
D[mod](v1, v2)     = v1 mod v2           (error if v2=0)
```

### 5.2 Comparison

```
D[<](a, b)  = Bool(a < b)      (Num * Num -> Bool)
D[>](a, b)  = Bool(a > b)
D[<=](a, b) = Bool(a <= b)
D[>=](a, b) = Bool(a >= b)
D[=](a, b)  = Bool(a = b)      (numeric equality)
```

### 5.3 Equality

```
D[eq?](a, b)    = identity comparison
                   (for lists: pointer equality; for atoms: value equality)
D[equal?](a, b) = structural deep equality
```

### 5.4 List Operations

```
D[cons](h, List(elems))  = List(h :: elems)
D[cons](h, t)            = Pair(h, t)
D[car](List(a :: _))     = a
D[car](Pair(a, _))       = a
D[car](_)                = error("car: expected pair")
D[cdr](List(_ :: rest))  = List(rest)
D[cdr](Pair(_, d))       = d
D[cdr](_)                = error("cdr: expected pair")
D[list](v1..vn)          = List(v1..vn)
D[null?](List([]))        = Bool(#t)
D[null?](_)              = Bool(#f)
D[pair?](List(a :: _))   = Bool(#t)
D[pair?](Pair(_, _))     = Bool(#t)
D[pair?](_)              = Bool(#f)
```

### 5.5 Type Predicates

```
D[number?](v)    = Bool(v in Num)
D[string?](v)    = Bool(v in Str)
D[symbol?](v)    = Bool(v in Sym)
D[boolean?](v)   = Bool(v in Bool)
D[procedure?](v) = Bool(v in Closure + Builtin)
D[vector?](v)    = Bool(v in Vec)
```

### 5.6 Logic

```
D[not](Bool(#f)) = Bool(#t)
D[not](_)        = Bool(#f)
```

### 5.7 Vectors

```
D[make-vector](n)        = Vec(repeat(Num(0), n))
D[make-vector](n, fill)  = Vec(repeat(fill, n))
D[vector-ref](vec, i)    = vec[i]              (error if out of range)
D[vector-set!](vec, i, v) = vec[i] := v; Void  (error if out of range)
D[vector-length](vec)    = Num(length(vec))
```

Vectors have identity semantics: `vector-set!` mutates in place, and all
references to the same vector observe the mutation.

### 5.8 I/O

```
D[display](v)  = print v to stdout; Void
D[newline]()   = print newline to stdout; Void
```

---

## 6. Error Semantics

Errors are signalled as strings and propagate up the call stack.
The REPL catches all errors and prints `Error: <message>`.

| Condition                 | Error message                              |
|---------------------------|--------------------------------------------|
| Unbound variable          | `"unbound variable: <name>"`               |
| Set! unbound              | `"cannot set! unbound variable: <name>"`   |
| Not a procedure           | `"not a procedure: <value>"`               |
| Arity mismatch            | `"arity mismatch: expected <n>, got <m>"`  |
| Division by zero          | `"division by zero"`                       |
| Car on non-pair           | `"car: expected pair"`                     |
| Cdr on non-pair           | `"cdr: expected pair"`                     |
| Type error                | `"expected number"`, `"expected pair"`, etc.|
| Uncaught throw            | `"no matching catch for tag: <tag>"`       |
| Unknown block             | `"unknown block: <name>"`                  |
| Mutate non-pair           | `"expected mutable pair"`                  |
| Vector index out of range | `"index out of range"`                     |
| Vector type error         | `"expected vector"`                        |

---

## 7. Evaluation Entry Point

```
straw_eval(expr, env) = E[expr, env, id]
```

where `id = lambda v. v` is the identity continuation.

The top-level evaluator catches escape signals:
- **Continuation escape**: returns the escaped value
- **Uncaught throw**: returns `"no matching catch for tag: t"`
- **Uncaught return-from**: returns `"unknown block: name"`
- **Other errors**: propagated as-is

---

## 8. Notation Key

| Symbol     | Meaning                                       |
|------------|-----------------------------------------------|
| `E[e,r,k]` | Evaluate expression `e` in env `r`, cont `k` |
| `k(v)`     | Pass value `v` to continuation `k`           |
| `id`       | Identity continuation: `lambda v. v`          |
| `dom(f)`   | Domain of function/map `f`                    |
| `:=`       | Mutation (assignment to mutable cell)         |
| `++`       | String concatenation                          |
| `a :: b`   | List cons (prepend `a` to list `b`)           |
| `[x1..xn]` | Sequence of `n` elements                     |
