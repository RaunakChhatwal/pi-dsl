from __future__ import annotations

from dataclasses import dataclass, field
import functools
from typing import overload
from .base import init_tuple, Int, List, String, Tuple
from . import bindings
from .bindings import bind, BinderInfo, Entry, ppr_term, TermName

# Wrapper class for annotating terms with type hints used by the DSL
@dataclass
class Hint:
    cls: type
    hint: Type

# Base class for all term types in the DSL
class Term():
    # Convert this term to a Haskell binding representation
    def binding(self) -> bindings.Term:
        raise NotImplementedError

    # Pretty-print this term using Haskell's pretty printer
    def __str__(self) -> str:
        return str(ppr_term(self.binding()))

    # Apply this term to arguments, creating nested App nodes
    def __call__(self, *args: Term) -> Term:
        return functools.reduce(App, args, self)

    # Create a Hint wrapper for class-based type annotations
    def __class_getitem__(cls, hint: Type) -> Hint:
        return Hint(cls, hint)

    # Right-shift creates a non-dependent Pi type (arrow type)
    def __rshift__(self, other: Term) -> Term:
        return Pi(self, other)

    # Right-associative shift for creating Pi types with named parameters
    def __rrshift__(self, other: Param) -> Term:
        return Pi(other, self)

    # Syntactic sugar for addition using the global "add" function
    def __add__(self, other: Term) -> Term:
        return Global("add")(self, other)

# Type alias: types are just terms in a dependently-typed language
type Type = Term

type IntoLevel = int | Level

@dataclass
class Level:
    inner: int | str | LevelMVar | Offset | Max = field(init=False, repr=False, compare=False)

    def __init__(self, inner: int | str | LevelMVar | Offset | Max):
        self.inner = inner

    @staticmethod
    def fr0m(level: IntoLevel) -> Level:
        match level:
            case Level():
                return level
            case _:
                return Level(level)

    def binding(self) -> bindings.Level:
        match self.inner:
            case int(n):
                binding = bindings.Level(bindings.Level.KIND_ZERO)
                for _ in range(n):
                    binding = bindings.Level.init_succ(binding)
                return binding
            case str(param):
                return bindings.Level.init_param(String(param))
            case _:
                return self.inner.binding()

    @staticmethod
    def from_binding(binding: bindings.Level) -> Level:
        match binding.kind:
            case bindings.Level.KIND_ZERO:
                return Level(0)

            case bindings.Level.KIND_PARAM:
                return Level(str(binding.get_param()))

            case bindings.Level.KIND_L_M_VAR:
                return LevelMVar(int(binding.get_l_m_var()))

            case bindings.Level.KIND_SUCC:
                base = binding.get_succ()
                offset = 1
                while base.kind == bindings.Level.KIND_SUCC:
                    base = base.get_succ()
                    offset += 1

                return Offset(Level.from_binding(base), offset)

            case bindings.Level.KIND_MAX:
                def flatten_max_args(binding: bindings.Level) -> list[Level]:
                    match binding.kind:
                        case bindings.Level.KIND_MAX:
                            arg1, arg2 = binding.get_max()
                            return flatten_max_args(arg1) + flatten_max_args(arg2)
                        case _:
                            return [Level.from_binding(binding)]

                return Max(*flatten_max_args(binding))

@dataclass
class LevelMVar(Level):
    id: int

    def __post_init__(self):
        self.inner = self

    def binding(self) -> bindings.Level:
        return bindings.Level.init_l_m_var(Int(self.id))

@dataclass
class Offset(Level):
    base: Level
    offset: int

    def __post_init__(self):
        self.inner = self

    def binding(self) -> bindings.Level:
        binding = self.base.binding()
        for _ in range(self.offset):
            binding = bindings.Level.init_succ(binding)

        return binding

@dataclass
class Max(Level):
    levels: list[Level]

    def __init__(self, *levels: IntoLevel):
        assert len(levels) >= 2
        self.levels = [Level.fr0m(level) for level in levels]
        self.inner = self

    def binding(self) -> bindings.Level:
        acc = self.levels[0].binding()
        for level in self.levels[1:]:
            acc = bindings.Level.init_max(acc, level.binding())
        return acc

# Annotated term: a term with an explicit type annotation
@dataclass
class Ann(Term):
    term: Term
    hint: Type

    # Convert annotation to Haskell binding
    def binding(self) -> bindings.Term:
        return bindings.Term.init_ann(self.term.binding(), self.hint.binding())

# Application: function application of func to arg
@dataclass
class App(Term):
    func: Term
    arg: Term

    # Convert application to Haskell binding
    def binding(self) -> bindings.Term:
        return bindings.Term.init_app(self.func.binding(), self.arg.binding())

# Local variable with a name and unique identifier
@dataclass
class Var(Term):
    name: str
    id: int = 0

    # Convert variable to Haskell binding as a local variable
    def binding(self) -> bindings.Term:
        return bindings.Term.init_l_var(self.name_binding())

    # Create a Haskell Name binding from this variable
    def name_binding(self) -> TermName:
        return bindings.Name[bindings.Term].init_fn(String(self.name), Int(self.id))

    # Construct a Var from a Haskell Name binding
    @staticmethod
    def from_binding(name: TermName) -> Var:
        return Var(str(name.get_fn()[0]), int(name.get_fn()[1]))

@dataclass
class MVar(Term):
    id: int

    # Convert variable to Haskell binding as a local variable
    def binding(self) -> bindings.Term:
        return bindings.Term.init_m_var(Int(self.id))

# Hole variable representing an unnamed/inferred position
hole = Var("_")

@dataclass
class IParam:
    var: Var
    param_type: Type

    @overload
    def __init__(self, param_name_or_type: Type) -> None: ...

    @overload
    def __init__(self, param_name_or_type: Var, param_type: Type) -> None: ...

    def __init__(self, param_name_or_type: Var | Type, param_type: Type | None = None) -> None:
        if param_type is None:
            self.var = hole
            self.param_type = param_name_or_type
        else:
            assert isinstance(param_name_or_type, Var)
            self.var = param_name_or_type
            self.param_type = param_type

# Param is either a type (for non-dependent arrows) or a (name, type) pair
type Param = Type | tuple[Var, Type] | IParam

# Extract the (Var, Type, implicit) triple from a Param, using hole for unnamed params
def raw_param(param: Param) -> tuple[Var, Type, bool]:
    match param:
        case Term() as param_type:
            return (hole, param_type, False)
        case IParam(var, param_type):
            return (var, param_type, True)
        case (var, param_type):
            return (var, param_type, False)

# Marks a parameter binding as implicit (for implicit-arg insertion)
class IVar(Var):
    pass
    # def __class_getitem__(cls, hint: Type) -> Hint:
    #     return Hint(Var, hint, implicit=True)

@dataclass
class Const(Term):
    level_args: list[IntoLevel] = field(default_factory=lambda: [], kw_only=True)

    def const_binding(self) -> bindings.Const:
        raise NotImplementedError

    def binding(self) -> bindings.Term:
        level_args = [Level.fr0m(level).binding() for level in self.level_args]
        return bindings.Term.init_const(self.const_binding(), List[bindings.Level](*level_args))

# Constructor: a named constructor for an inductive datatype
@dataclass
class Ctor(Const):
    name: str
    datatype: DataType
    signature: Type

    # Convert constructor to Haskell binding
    def const_binding(self) -> bindings.Const:
        return bindings.Const.init_ctor(String(self.datatype.name), String(self.name))

# Inductive datatype with a name, signature, and list of constructors
@dataclass
class DataType(Const):
    name: str
    univ_params: list[str]
    signature: Type
    ctors: list[Ctor]

    # Convert datatype reference to Haskell binding
    def const_binding(self) -> bindings.Const:
        return bindings.Const.init_data_type(String(self.name))

    # Convert datatype definition to a Haskell Entry binding
    def entry_binding(self) -> Entry:
        name = String(self.name)
        univ_params = \
            bindings.List[bindings.UnivParamName](*[String(param) for param in self.univ_params])
        ctors = List[Tuple[String, bindings.Type]](
            *[init_tuple(String(ctor.name), ctor.signature.binding()) for ctor in self.ctors])
        return bindings.Entry.init_data(name, univ_params, self.signature.binding(), ctors)

# Global variable: a reference to a top-level declaration by name
@dataclass
class Global(Const):
    name: str

    # Convert global reference to Haskell binding
    def const_binding(self) -> bindings.Const:
        return bindings.Const.init_g_var(String(self.name))

# Lambda abstraction with one or more bound variables
@dataclass
class Lam(Term):
    param_names: Var | list[Var]
    body: Term

    # Convert lambda to Haskell binding, nesting multiple binders
    def binding(self) -> bindings.Term:
        match self.param_names:
            case list() as param_names:
                pass
            case param_name:
                param_names = [param_name]

        binding = self.body.binding()
        for param_name in reversed(param_names):
            if isinstance(param_name, IVar):
                binder_info = BinderInfo(BinderInfo.KIND_IMPLICIT)
            else:
                binder_info = BinderInfo(BinderInfo.KIND_EXPLICIT)
            binding = bindings.Term.init_lam(binder_info, bind(param_name.name_binding(), binding))
        return binding

# Pi type (dependent function type) with one or more parameters
@dataclass
class Pi(Term):
    params: Param | list[Param]
    return_type: Type

    # Convert Pi type to Haskell binding, nesting multiple binders
    def binding(self) -> bindings.Term:
        match self.params:
            case list() as params:
                pass
            case param:
                params = [param]

        binding = self.return_type.binding()
        for (var, param_type, implicit) in map(raw_param, reversed(params)):
            binder_info = \
                BinderInfo(BinderInfo.KIND_IMPLICIT if implicit else BinderInfo.KIND_EXPLICIT)
            binding = bindings.Term.init_pi(
                binder_info, param_type.binding(), bind(var.name_binding(), binding))
        return binding

# Recursor: the elimination principle for an inductive datatype
@dataclass
class Rec(Const):
    datatype: DataType

    # Convert recursor to Haskell binding
    def const_binding(self) -> bindings.Const:
        return bindings.Const.init_rec(String(self.datatype.name))

# Sort: universe level (Set = Sort(0), Set1 = Sort(1), etc.)
@dataclass
class Sort(Term):
    level: Level

    def __init__(self, level: IntoLevel):
        self.level = Level.fr0m(level)

    # Convert sort to Haskell binding with nested Succ levels
    def binding(self) -> bindings.Term:
        return bindings.Term.init_sort(self.level.binding())

# Set is the base universe (Type₀)
Set = Sort(0)
