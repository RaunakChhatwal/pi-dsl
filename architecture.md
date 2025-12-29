# pi-dsl Architecture Documentation

## Overview

pi-dsl is a dependently typed DSL embedded in Python with a Haskell type-checking core. Goal: accessible dependent types in Python with rigorous checking in Haskell.

## High-Level Architecture

The system is organized into three main layers:

1. **Python User Interface Layer**: Provides a Pythonic DSL syntax (`sugar.py`), type checker interface (`env.py`), and standard library (`std.py`)
2. **FFI Bridge Layer**: Auto-generated type bindings (`bindings.py`) and low-level C integration (`base.py`) using `ctypes`
3. **Haskell Type System Core**: Implements the dependently-typed language with modules for syntax, type checking, inductive types, equality, and environment management

## Component Details

### 1. Haskell Type System Core

The Haskell implementation is a fork of the educational pi-forall language by Stephanie Weirich.

#### Key Modules:

- **`Syntax.hs`**: AST (`Sort`, `Var`, `Lam`, `App`, `Pi`, `Ann`, `DataType`, `Ctor`, `Rec`); also defines `Match`/`Pattern` helpers (not a `Term` form)
- **`TypeCheck.hs`**: Bidirectional checking (`inferType`, `checkType`, `tcEntries`)
- **`Environment.hs`**: `TcMonad`, `Env` (datatypes/decls/locals), tracing, and pretty-printed `Err` (no source spans)
- **`Inductive.hs`**: Inductive checking + recursor synthesis/reduction (positivity checking)
- **`Equal.hs`**: Equality + WHNF reduction
- **`PrettyPrint.hs`**: Pretty-printing



### 2. Foreign Function Interface (FFI) Bridge

#### `Export.hs` - Haskell Export Module:
- Exposes FFI entry points: `bind`, `unbind`, `ppr_term`, `type_check`, `trace_type_check`, `infer_type`, `check_type`
- Runtime init/exit lives in `export.c` (`pi_dsl_init`/`pi_dsl_exit`)
- Uses `FFI.hs` helpers to generate `Storable` instances and C-callable wrapper exports

#### `FFI.hs` - FFI Helpers:
- Template Haskell utilities for `Storable` instances (tagged unions/newtypes) and `foreign export ccall` wrapper generation (`exportFunction`)

#### `Bindings.hs` / `bindgen` - Template Haskell Code Generation:
- `Bindings.hs` generates Python `ctypes` bindings for selected Haskell types and exported functions
- The `bindgen` executable (`bindgen/Main.hs`) emits the generated `pi_dsl/bindings.py`

### 3. Python DSL Layer

#### `base.py` / `bindings.py`:
- `ctypes` bridge + generated tagged-union wrappers and function signatures for the Haskell exports

#### `term.py`:
- Python term constructors (`Ann`, `App`, `Ctor`, `DataType`, `Lam`, `Pi`, `Rec`, `Sort`, `Var`) with `binding()` → Haskell

#### `env.py` (+ `tracing.py`):
- Incremental environment (`Env`, `Decl`) that calls the kernel and raises `KernelError` with trace trees

#### `sugar.py` / `std.py`:
- Decorators (`@datatype`, `@decl`), `Self` stub replacement, and a small standard library (`Bool`, `Nat`, `Eq`, `add`, `sym`, `trans`, `cong`)

### 4. Build and Development Infrastructure

- **`pi-dsl.cabal`**: Haskell library + `bindgen` codegen executable + shared foreign library (`pi-dsl-shared-lib`)
- **`flake.nix`**, **`pyproject.toml`**: dev environment and Python packaging
- **`bindgen/Main.hs`**: generates `pi_dsl/bindings.py`

## Data Flow and Interactions

### Type Checking Workflow:

1. Build Python terms (`term.py`/`sugar.py`) and environment entries (`env.py`)
2. Convert to Haskell via `binding()` and call the exported kernel
3. Return either a type/result or an error plus traces (`tracing.py`)
