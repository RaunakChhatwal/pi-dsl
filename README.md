# pi-dsl

Dependently typed DSL embedded in Python. Type checking happens in Haskell via FFI.

## Dependencies

- GHC 9.8+
- Cabal
- Python 3.12+

## Building

```
cabal build pi-dsl-shared-lib
cabal run bindgen > pi_dsl/bindings.py
```

The shared library lands somewhere in `dist-newstyle/`. The Python code finds it automatically.

## Usage

```python
from pi_dsl.env import Env
from pi_dsl.sugar import datatype, decl, lam, DataTypeMeta, Self
from pi_dsl.term import Ctor, Pi, Rec, Set, Term, Var

env = Env()

n = Var("n")
@datatype(env)
class Nat(metaclass=DataTypeMeta):
    zero: Ctor[Self]
    succ: Ctor[(n, Self) >> Self]

@decl(env)
def add(n: Var[Nat], m: Var[Nat]) -> Term[Nat]:
    return Rec(Nat)(lam(lambda _: Nat), m, lam(lambda _, acc: Nat.succ(acc)), n)
```

`@datatype` registers an inductive type. `@decl` registers a typed definition. Both call into Haskell and throw `PiDslError` if type checking fails.

## Standard Library

`pi_dsl.std` has: `Bool`, `Nat`, `Eq`, `Maybe`, `Either`, `List`, `Sigma`, `Void`

Plus functions: `add`, `mul`, `sym`, `trans`, `cong`, `exfalso`, `map`, `length`, etc.

Import with:
```python
from pi_dsl.std.env import env
from pi_dsl.std.nat import Nat, add
from pi_dsl.std.eq import Eq, cong, trans
```

## Term Syntax

| Python | Meaning |
|--------|---------|
| `Sort(0)` or `Set` | Type universe |
| `Var("x")` | Local variable |
| `Global("f")` | Reference to declaration |
| `Lam(Var("x"), body)` | Lambda |
| `App(f, x)` or `f(x)` | Application |
| `Pi((x, A), B)` | Dependent function type |
| `A >> B` | Non-dependent function type |
| `(x, A) >> B` | Dependent function type (sugar) |
| `Ann(term, type)` | Type annotation |
| `Rec(T)` | Recursor/eliminator for datatype T |

`lam(lambda x, y: body)` inspects parameter names and builds nested lambdas.

## Proofs

See `test/add_comm.py` for a proof that addition commutes:

```python
@decl(env)
def add_comm(n: Var[Nat], m: Var[Nat]) -> Term[Eq(Nat, n + m, m + n)]:
    motive = lam(lambda m: Eq(Nat, n + m, m + n))
    base_case = add_zero(n)
    inductive_case = lam(lambda m, h:
        trans(Nat, n + Nat.succ(m), Nat.succ(n + m), Nat.succ(m + n),
            add_succ(n, m),
            cong(Nat, Nat, Nat.succ, n + m, m + n, h)))
    return Rec(Nat)(motive, base_case, inductive_case, m)
```

## Debugging

When type checking fails, `PiDslError` includes trace trees from the Haskell side.

```python
try:
    env.declare(...)
except PiDslError as error:
    print(error.message)
    print(error.traces[-1].stack_trace())
```

Or run the TUI:
```
python analyze-traces.py test.module_that_fails
```

## Architecture

```
Python (sugar.py, term.py, env.py)
    ↓ ctypes
Haskell (TypeCheck.hs, Inductive.hs, Equal.hs)
```

Python builds AST → converts to Haskell repr via `bindings.py` → calls `type_check`/`infer_type`/`check_type` → gets back result or error.

`bindings.py` is generated. Don't edit it.

## Tests

```
python -m test.datatypes
python -m test.add_comm
python -m test.motive_clash
```

They succeed silently or throw.
