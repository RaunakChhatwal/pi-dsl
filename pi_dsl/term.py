from __future__ import annotations
from abc import ABC, abstractmethod
from dataclasses import dataclass
import functools
from typing import cast, Self
from .base import init_tuple, Int, List, String
from . import bindings
from .bindings import bind, CtorDef, Entry, Name, TName

class Term(ABC):
    @abstractmethod
    def binding(self) -> bindings.Term:
        pass

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

    def name_binding(self) -> TName:
        return Name[bindings.Term].init_fn(String(self.name), Int(0))

hole = Var("_")

type Param = Type | tuple[Var, Type]

def raw_param(param: Param) -> tuple[Var, Type]:
    match param:
        case Term() as type_:
            return (hole, type_)
        case (var, type_):
            return (var, type_)

def param_binding(param: Param) -> bindings.Param:
    var, type_ = raw_param(param)
    return init_tuple(var.name_binding(), type_.binding())

@dataclass
class Ctor(Term):
    name: str
    typeName: str
    params: list[Param]
    returnType: Type

    def binding(self) -> bindings.Term:
        return bindings.Term.init_data_con(String(self.name), String(self.typeName))

    def ctor_def(self) -> CtorDef:
        return CtorDef.init_ctor_def(
            String(self.name),
            List[bindings.Param](*map(param_binding, self.params)),
            self.returnType.binding()
        )

@dataclass
class DataType(Term):
    name: str
    params: list[Param]
    ctors: list[Ctor]

    def binding(self) -> bindings.Term:
        return bindings.Term.init_ty_con(String(self.name))

    def entry_binding(self) -> Entry:
        ctor_defs = List[CtorDef](*[ctor.ctor_def() for ctor in self.ctors])
        return bindings.Entry.init_data(
            String(self.name), List[bindings.Param](*map(param_binding, self.params)), ctor_defs)

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

        lam_binding = self.body.binding()
        for param_name in reversed(param_names):
            lam_binding = bindings.Term.init_lam(bind(param_name.name_binding(), lam_binding))
        return lam_binding

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

        pi_binding = self.return_type.binding()
        for (var, type_) in map(raw_param, reversed(params)):
            pi_binding = bindings.Term.init_ty_pi(
                type_.binding(), bind(var.name_binding(), pi_binding))
        return pi_binding

class UniverseSingleton(Term):
    def binding(self) -> bindings.Term:
        return bindings.Term.ty_type

Universe = UniverseSingleton()

type TermUnion = Ann | App | Ctor | DataType | Lam | Pi | Var | UniverseSingleton