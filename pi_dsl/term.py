from __future__ import annotations
from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Any, cast, Self
from .base import init_tuple, Int, List, String, Tuple
from . import bindings
from .bindings import Bind, CtorDef, Entry, Epsilon, Name, Pattern, TName, Telescope, TypeDecl

# def string_to_name(string: str) -> Name[Any]:
#     return Name[Any].init_fn(String(string), Int(0))

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

type Type = Term

@dataclass
class Var(Term):
    name: str

    def binding(self) -> bindings.Term:
        return bindings.Term.init_var(self.name_binding())

    def name_binding(self) -> Name[Any]:
        return Name[Any].init_fn(String(self.name), Int(0))

hole = Var("_")

# Irrelevance marker
class Irr[T]:
    def __init__(self, inner: T):
        self.inner = inner

    def get(self) -> T:
        return self.inner

# Wraps types that are either runtime relevant or irrelevant
type Rel[T] = T | Irr[T]

type Arg = Rel[Term | tuple[Var, Term]]

def args_to_telescope(args: list[Arg]) -> Telescope:
    telescope_entries: list[Entry] = []
    for arg in args:
        match arg:
            case Irr(inner=raw_arg):
                relevance = Epsilon.irr
            case raw_arg:
                relevance = Epsilon.rel

        match raw_arg:
            case Term() as type_:
                var = hole
            case (var, type_):
                pass

        telescope_entries.append(Entry.init_decl(TypeDecl.init_type_decl(
            var.name_binding(), relevance, type_.binding())))

    return List[Entry](*telescope_entries)

@dataclass
class App(Term):
    func: Term
    arg: Rel[Term]

    def binding(self) -> bindings.Term:
        match self.arg:
            case Irr(inner=raw_arg):
                relevance = Epsilon.irr
            case raw_arg:
                relevance = Epsilon.rel

        arg_binding = bindings.Arg.init_arg(relevance, raw_arg.binding())
        return bindings.Term.init_app(self.func.binding(), arg_binding)

@dataclass
class Ctor(Term):
    name: str
    params: list[Arg]

    def binding(self) -> bindings.Term:
        return bindings.Term.init_data_con(String(self.name), List[bindings.Arg]())

    def ctor_def(self) -> CtorDef:
        return CtorDef.init_ctor_def(String(self.name), args_to_telescope(self.params))

type Pat = Rel[Var | Ctor | tuple[Ctor, list[Pat]]]

def pat_binding(pat: Pat) -> Pattern:
    match pat:
        case Irr():
            raise Exception("Top level pattern mustn't be implicit")
        case Var():
            return Pattern.init_pat_var(pat.name_binding())
        case (ctor, arg_pats):
            pass
        case ctor:
            arg_pats = []

    arg_pat_bindings: list[Tuple[Pattern, Epsilon]] = []
    for arg_pat in arg_pats:
        match arg_pat:
            case Irr(inner=raw_pat):
                arg_pat_bindings.append(init_tuple(pat_binding(raw_pat), Epsilon.irr))
            case _:
                arg_pat_bindings.append(init_tuple(pat_binding(arg_pat), Epsilon.rel))
    return Pattern.init_pat_con(String(ctor.name), List[Tuple[Pattern, Epsilon]](*arg_pat_bindings))

@dataclass
class Case(Term):
    case_expr: Term
    match_stmts: list[tuple[Pat, Term]]

    def binding(self) -> bindings.Term:
        match_binds: list[Bind[Pattern, bindings.Term]] = []
        for (pat, body) in self.match_stmts:
           match_binds.append(Bind[Pattern, bindings.Term].init_b(pat_binding(pat), body.binding()))
        return bindings.Term.init_case(self.case_expr.binding(),
            List[Bind[Pattern, bindings.Term]](*match_binds))

@dataclass
class DataType(Term):
    name: str
    type_params: list[Arg]
    ctors: list[Ctor]

    def binding(self) -> bindings.Term:
        return bindings.Term.init_ty_con(String(self.name), List[bindings.Arg]())

    def entry_binding(self) -> Entry:
        telescope = args_to_telescope(self.type_params)
        ctor_defs = List[CtorDef](*[ctor.ctor_def() for ctor in self.ctors])
        return bindings.Entry.init_data(String(self.name), telescope, ctor_defs)

@dataclass
class Lam(Term):
    param: Rel[Var]
    body: Term

    def binding(self) -> bindings.Term:
        match self.param:
            case Var():
                param_name = self.param.name_binding()
                epsilon = Epsilon.rel
            case Irr(inner=param):
                param_name = param.name_binding()
                epsilon = Epsilon.irr

        bind = Bind[TName, bindings.Term].init_b(param_name, self.body.binding())
        return bindings.Term.init_lam(epsilon, bind)

@dataclass
class Pi(Term):
    input_type: Rel[Type | tuple[Var, Type]]
    body: Type

    def binding(self) -> bindings.Term:
        match self.input_type:
            case Irr(inner=raw_input):
                relevance = Epsilon.irr
            case raw_input:
                relevance = Epsilon.rel

        match raw_input:
            case Term() as type_:
                var = hole
            case (var, type_):
                pass

        bind = Bind[TName, bindings.Type].init_b(var.name_binding(), self.body.binding())
        return bindings.Term.init_ty_pi(relevance, type_.binding(), bind)

type TermUnion = App | Case | Ctor | DataType | Lam | Pi | Var