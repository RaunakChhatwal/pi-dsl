# pi-dsl Architecture Documentation

## Overview

pi-dsl is a dependently typed domain-specific language (DSL) embedded in Python with a Haskell backend. This hybrid architecture combines Haskell's strong type system and formal verification capabilities with Python's accessibility and ecosystem.

**Core Value Proposition**: Making dependent types accessible to Python developers while maintaining rigorous correctness guarantees through a Haskell-implemented type system.

## High-Level Architecture

The system is organized into three main layers:

1. **Python User Interface Layer**: Provides a Pythonic DSL syntax (`sugar.py`), type checker interface (`env.py`), and standard library (`std.py`)
2. **FFI Bridge Layer**: Auto-generated type bindings (`bindings.py`) and low-level C integration (`base.py`) using `ctypes`
3. **Haskell Type System Core**: Implements the dependently-typed language with modules for syntax, type checking, inductive types, equality, and environment management

## Component Details

### 1. Haskell Type System Core

The Haskell implementation is based on the pi-forall language, a simple dependently typed language originally developed for OPLSS 2022.

#### Key Modules:

- **`Syntax.hs`**: Defines the abstract syntax tree (AST) for the language:
  - Core constructs: `TyType`, `Var`, `Lam`, `App`, `Pi`, `Ann`, `TrustMe`
  - Inductive types: `DataType`, `Ctor`, `Rec`
  - Pattern matching: `Match`, `Pattern`
  - Uses `unbound-generics` library for binding management

- **`TypeCheck.hs`**: Implements bidirectional type checking:
  - `inferType`: Synthesizes types for terms
  - `checkType`: Verifies terms against expected types
  - `tcEntries`: Type checks declarations and data types
  - Bidirectional algorithm with type-directed checking

- **`Environment.hs`**: Manages typing contexts and error handling:
  - `TcMonad`: Reader monad transformer stack with environment, freshness, and error handling
  - `Env`: Three-part environment (datatypes, declarations, locals)
  - Tracing support for debugging and error reporting
  - Comprehensive error reporting with source positions

- **`Inductive.hs`**: Handles inductive types and recursion:
  - `checkDataTypeDecl`: Validates data type declarations
  - `synthesizeRecursorType`: Generates type signatures for recursors
  - `reduceRecursor`: Implements pattern matching reduction
  - Strict positivity checking for inductive definitions

- **`Equal.hs`**: Implements equality checking and weak head normal form reduction

- **`PrettyPrint.hs`**: Provides pretty-printing for terms and error messages

#### Design Patterns in Haskell Core:

- **Monadic Type Checking**: `TcMonad` combines reader, state (fresh names), and error monads
- **Bidirectional Type Checking**: Separates type inference from type checking
- **Locally Nameless Representation**: Uses `unbound-generics` for safe binding operations
- **Streaming Traces**: Uses `streaming` package for efficient trace collection

### 2. Foreign Function Interface (FFI) Bridge

#### `Export.hs` - Haskell Export Module:
- Provides C-compatible function exports using `ForeignFunctionInterface`
- Auto-generates `Storable` instances for Haskell types
- Exports key functions: `bind`, `unbind`, `ppr_term`, `type_check`, `infer_type`, `check_type`
- Manages Haskell runtime initialization (`pi_forall_init`)

#### `Bindings.hs` - Template Haskell Code Generation:
- **Auto-generates Python bindings** from Haskell type definitions
- Analyzes Haskell type definitions using Template Haskell
- Generates Python class definitions with proper type parameters
- Creates tagged union representations for Haskell algebraic data types
- Handles recursive type dependencies through topological sorting

### 3. Python DSL Layer

#### `base.py` - Low-Level C Integration:
- Uses Python's `ctypes` library to call Haskell functions
- Implements `TaggedUnion` base class for representing Haskell ADTs
- Provides `List`, `Tuple`, `String` wrappers for Haskell types
- Manages memory allocation and type conversions

#### `bindings.py` - Auto-generated Type Bindings:
- Contains Python classes mirroring Haskell types: `Maybe`, `Either`, `Trace`, `Map`, `Name`, `Bind`, `Term`, `Env`, `Entry`
- Each class provides:
  - Kind discriminators for tagged unions
  - Factory methods (`init_*`) for constructing values
  - Accessor methods (`get_*`) for extracting values
  - Type-safe function signatures for FFI calls

#### `term.py` - Abstract Syntax Tree Representation:
- Python classes representing language terms: `Ann`, `App`, `Ctor`, `DataType`, `Lam`, `Pi`, `Rec`, `Var`
- `Term` base class with `binding()` method to convert to Haskell representation
- Type aliases: `Type = Term`, `Param = Type | tuple[Var, Type]`
- Support for holes and partial applications

#### `env.py` - Environment and Kernel Interface:
- `Env` class manages the typing environment
- `Decl` class represents declarations with type signatures and definitions
- Provides high-level operations: `type_check()`, `infer_type()`, `check_type()`
- Converts between Python term representations and Haskell bindings
- Comprehensive error handling with trace collection

#### `sugar.py` - Syntactic Sugar and Decorators:
- `@datatype` decorator for defining inductive types
- `@decl` decorator for defining functions with type signatures
- `Self` singleton for representing recursive types in constructors
- Automatic stub removal and term normalization
- Pythonic interface for defining dependently typed programs

#### `std.py` - Standard Library:
- Predefined types: `Bool`, `Nat`, `Eq` (equality type)
- Standard operations: `add`, `sym`, `trans`, `cong`
- Demonstrates the DSL's capabilities for theorem proving

### 4. Build and Development Infrastructure

- **`pi-forall.cabal`**: Haskell package configuration with:
  - Library (type system core)
  - Executable (code generation tool)
  - Foreign library (shared library for Python)
- **`flake.nix`**: Nix development environment with Haskell and Python tools
- **`pyproject.toml`**: Python package configuration
- **Template Haskell**: Used in `app/Main.hs` to generate binding code

## Data Flow and Interactions

### Type Checking Workflow:

1. **User defines terms** using Python DSL syntax (decorators and classes)
2. **Python constructs AST** using `term.py` classes
3. **Conversion to Haskell** via `binding()` methods
4. **FFI call** to Haskell type checker through `env.py` interface
5. **Haskell performs type checking** using bidirectional algorithm
6. **Results returned** to Python, with error messages or type information
7. **Traces collected** for debugging and error reporting

### Binding Generation Process:

1. **Template Haskell analysis** of Haskell type definitions
2. **Dependency analysis** to determine declaration order
3. **Python code generation** with proper type parameters
4. **`Storable` instance generation** for FFI compatibility
5. **Export function generation** for Haskell-Python interoperability
