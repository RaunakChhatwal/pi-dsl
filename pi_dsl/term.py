from __future__ import annotations

from dataclasses import dataclass
import functools
from .base import init_tuple, Int, List, String, Tuple
from . import bindings
from .bindings import bind, Entry, ppr_term, TermName

@dataclass
class Hint:
    cls: type
    hint: Type

class Term():
    def binding(self) -> bindings.Term:
        raise NotImplemented

    def __str__(self) -> str:
        return str(ppr_term(self.binding()))

    def __call__(self, *args: Term) -> Term:
        return functools.reduce(App, args, self)

    def __class_getitem__(cls, hint: Type) -> Hint:
        return Hint(cls, hint)

    def __rshift__(self, other: Term) -> Term:
        return Pi(self, other)

    def __rrshift__(self, other: Param) -> Term:
        return Pi(other, self)

    def __add__(self, other: Term) -> Term:
        return Global("add")(self, other)

type Type = Term

@dataclass
class Ann(Term):
    term: Term
    hint: Type

    def binding(self) -> bindings.Term:
        return bindings.Term.init_ann(self.term.binding(), self.hint.binding())

@dataclass
class App(Term):
    func: Term
    arg: Term

    def binding(self) -> bindings.Term:
        return bindings.Term.init_app(self.func.binding(), self.arg.binding())

@dataclass
class Var(Term):
    name: str

    def binding(self) -> bindings.Term:
        return bindings.Term.init_var(bindings.Var.init_local(self.name_binding()))

    def name_binding(self) -> TermName:
        return bindings.Name[bindings.Term].init_fn(String(self.name), Int(0))

# type Name = str | Var

hole = Var("_")

type Param = Type | tuple[Var, Type]
# type Param = tuple[Var, Type]

def raw_param(param: Param) -> tuple[Var, Type]:
    match param:
        case Term() as param_type:
            return (hole, param_type)
        case (var, param_type):
            return (var, param_type)

@dataclass
class Ctor(Term):
    name: str
    datatype: DataType
    signature: Type

    def binding(self) -> bindings.Term:
        return bindings.Term.init_ctor(String(self.datatype.name), String(self.name))

    # def signature_binding(self) -> bindings.Type:
    #     return Pi(self.params, self.return_type).binding()

@dataclass
class DataType(Term):
    name: str
    signature: Type
    ctors: list[Ctor]

    def binding(self) -> bindings.Term:
        return bindings.Term.init_data_type(String(self.name))

    def entry_binding(self) -> Entry:
        ctor_defs = List[Tuple[String, bindings.Type]](
            *[init_tuple(String(ctor.name), ctor.signature.binding()) for ctor in self.ctors])
        return bindings.Entry.init_data(
            String(self.name), self.signature.binding(), ctor_defs)

@dataclass
class Global(Term):     # TODO: inherit from Var?
    name: str

    def binding(self) -> bindings.Term:
        return bindings.Term.init_var(bindings.Var.init_global(String(self.name)))

@dataclass
class Lam(Term):
    param_names: Var | list[Var]
    body: Term

    def binding(self) -> bindings.Term:
        match self.param_names:
            case list() as param_names:
                pass
            case param_name:
                param_names = [param_name]

        binding = self.body.binding()
        for param_name in reversed(param_names):
            binding = bindings.Term.init_lam(bind(param_name.name_binding(), binding))
        return binding

@dataclass
class Pi(Term):
    params: Param | list[Param]
    return_type: Type

    def binding(self) -> bindings.Term:
        match self.params:
            case list() as params:
                pass
            case param:
                params = [param]

        binding = self.return_type.binding()
        for (var, param_type) in map(raw_param, reversed(params)):
            binding = bindings.Term.init_pi(param_type.binding(), bind(var.name_binding(), binding))
        return binding

@dataclass
class Rec(Term):
    datatype: DataType

    def binding(self) -> bindings.Term:
        return bindings.Term.init_rec(String(self.datatype.name))

@dataclass
class Sort(Term):
    level: int

    def binding(self) -> bindings.Term:
        level_binding = bindings.Level(bindings.Level.KIND_ZERO)
        assert self.level >= 0
        for _ in range(self.level):
            level_binding = bindings.Level.init_succ(level_binding)
        return bindings.Term.init_sort(level_binding)

Set = Sort(0)
