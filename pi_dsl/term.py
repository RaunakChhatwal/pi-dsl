from __future__ import annotations
from abc import ABC, abstractmethod
from dataclasses import dataclass
import functools
from typing import cast, Self
from .base import init_tuple, Int, List, String, Tuple
from . import bindings
from .bindings import bind, Entry, Name, ppr_term, TermName

class Term(ABC):
    @abstractmethod
    def binding(self) -> bindings.Term:
        pass

    def __str__(self) -> str:
        return str(ppr_term(self.binding()))

    def union(self) -> TermUnion:
        return cast(TermUnion, self)

    @classmethod
    def fr0m(cls, term: Term) -> Self:
        assert isinstance(term, cls)
        return term

    def __call__(self, *args: Term) -> Term:
        return functools.reduce(App, args, self)

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
        return bindings.Term.init_var(self.name_binding())

    def name_binding(self) -> TermName:
        return Name[bindings.Term].init_fn(String(self.name), Int(0))

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
    params: list[Param]
    returnType: Type

    def binding(self) -> bindings.Term:
        return bindings.Term.init_ctor(String(self.datatype.name), String(self.name))

    def signature_binding(self) -> bindings.Type:
        return Pi(self.params, self.returnType).binding()

@dataclass
class DataType(Term):
    name: str
    params: list[Param]
    ctors: list[Ctor]

    def binding(self) -> bindings.Term:
        return bindings.Term.init_data_type(String(self.name))

    def entry_binding(self) -> Entry:
        ctor_defs = List[Tuple[String, bindings.Type]](
            *[init_tuple(String(ctor.name), ctor.signature_binding()) for ctor in self.ctors])
        return bindings.Entry.init_data(
            String(self.name), Pi(self.params, Universe).binding(), ctor_defs)

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

class UniverseSingleton(Term):
    def binding(self) -> bindings.Term:
        return bindings.Term(bindings.Term.KIND_TY_TYPE)

Universe = UniverseSingleton()

type TermUnion = Ann | App | Ctor | DataType | Lam | Pi | UniverseSingleton | Var
