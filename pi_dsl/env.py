from __future__ import annotations
from dataclasses import dataclass
from .base import List, String
from . import bindings, tracing
from .bindings import BinderInfo, Either, Maybe, unbind
from .term import *

# Represents a top-level declaration with a name, type signature, and definition body
@dataclass
class Decl:
    var: Global
    signature: Type
    body: Term

    # Converts this declaration to a Haskell Entry binding for type checking
    def entry_binding(self) -> bindings.Entry:
        return bindings.Entry.init_decl(
            String(self.var.name), self.signature.binding(), self.body.binding())

# Converts a Haskell Term binding back into a Python Term AST node
def binding_to_term(binding: bindings.Term, env: Env) -> Term:
    match binding.kind:
        case bindings.Term.KIND_SORT:
            level = 0
            level_binding = binding.get_sort()
            while level_binding.kind == bindings.Level.KIND_SUCC:
                level += 1
                level_binding = level_binding.get_succ()
            return Sort(level)

        case bindings.Term.KIND_L_VAR:
            return Var.from_binding(binding.get_l_var())

        case bindings.Term.KIND_M_VAR:
            raise NotImplementedError

        case bindings.Term.KIND_CONST:
            const_binding = binding.get_const()
            match const_binding.kind:
                case bindings.Const.KIND_G_VAR:
                    return Global(str(const_binding.get_g_var()))
                case bindings.Const.KIND_DATA_TYPE:
                    return env.datatypes[str(const_binding.get_data_type())]
                case bindings.Const.KIND_CTOR:
                    type_name, ctor_name = const_binding.get_ctor()
                    data_type = env.datatypes[str(type_name)]
                    return [ctor for ctor in data_type.ctors if ctor.name == str(ctor_name)][0]
                case bindings.Const.KIND_REC:
                    return Rec(env.datatypes[str(const_binding.get_rec())])

        case bindings.Term.KIND_LAM:
            binder_info, binder = binding.get_lam()
            param_name, body = unbind(binder).get()
            var = Var.from_binding(param_name)
            if binder_info.kind == BinderInfo.KIND_IMPLICIT:
                var = IVar(var.name, var.id)
            return Lam(var, binding_to_term(body, env))

        case bindings.Term.KIND_APP:
            func, arg = binding.get_app()
            return App(binding_to_term(func, env), binding_to_term(arg, env))

        case bindings.Term.KIND_PI:
            binder_info, param_type_binding, binder = binding.get_pi()
            param_name, return_type = unbind(binder).get()
            implicit = binder_info.kind == bindings.BinderInfo.KIND_IMPLICIT
            var = Var.from_binding(param_name)
            param_type = binding_to_term(param_type_binding, env)
            param = IParam(var, param_type) if implicit else (var, param_type)
            return Pi(param, binding_to_term(return_type, env))

        case bindings.Term.KIND_ANN:
            term, hint = binding.get_ann()
            return Ann(binding_to_term(term, env), binding_to_term(hint, env))

# Exception raised when type checking fails, includes error message and trace trees for debugging
class PiDslError(Exception):
    def __init__(self, message: str, traces: list[tracing.TraceTree]):
        self.message = message
        self.traces = traces
        super().__init__(message)

# Type checking environment that tracks datatypes and declarations, interfaces with Haskell core
class Env:
    datatypes: dict[str, DataType]
    decls: dict[str, Decl]
    entries: list[str]

    # Initializes environment with optional datatype and declaration entries
    def __init__(self, *entries: DataType | Decl):
        self.datatypes = {}
        self.decls = {}
        self.entries = []

        for entry in entries:
            match entry:
                case DataType():
                    self.add_datatype(entry)
                case Decl():
                    self.declare(entry.var, entry.signature, entry.body)

    def binding(self) -> bindings.Env:
        entries = [self.get_entry(entry).entry_binding() for entry in self.entries]
        return bindings.entries_to_env(List[bindings.Entry](*entries))

    # Adds a datatype to the environment and type checks incrementally
    def add_datatype(self, datatype: DataType):
        assert datatype.name not in self.entries
        either, traces = bindings.check_entry(self.binding(), datatype.entry_binding()).get()
        if either.kind == Either.KIND_LEFT:
            raise PiDslError(str(either.get_left()), tracing.from_bindings(traces.get()))

        name, signature_binding, ctor_bindings = either.get_right().get_data()
        datatype = DataType(str(name), binding_to_term(signature_binding, self), [])

        self.entries.append(datatype.name)
        self.datatypes[datatype.name] = datatype

        for ctor_binding in ctor_bindings.get():
            ctor_name, ctor_type = ctor_binding.get()
            datatype.ctors.append(Ctor(str(ctor_name), datatype, binding_to_term(ctor_type, self)))

    # Adds a declaration with signature and body to the environment, type checks incrementally
    def declare(self, var: Global, hint: Type, defn: Term):
        assert var.name not in self.entries
        either, traces = \
            bindings.check_entry(self.binding(), Decl(var, hint, defn).entry_binding()).get()
        if either.kind == Either.KIND_LEFT:
            raise PiDslError(str(either.get_left()), tracing.from_bindings(traces.get()))

        name, type_binding, body_binding = either.get_right().get_decl()
        decl = Decl(
            Global(str(name)),
            binding_to_term(type_binding, self),
            binding_to_term(body_binding, self))

        self.entries.append(var.name)
        self.decls[var.name] = decl

    # Retrieves a datatype or declaration entry by name
    def get_entry(self, name: str) -> DataType | Decl:
        if name in self.datatypes:
            return self.datatypes[name]
        elif name in self.decls:
            return self.decls[name]
        else:
            raise ValueError(f"No entry with name {name}")

    # Infers the type of a term in the current environment
    def infer_type(self, term: Term) -> Term:
        either, traces = bindings.infer_type(self.binding(), term.binding()).get()
        match either.kind:
            case Either.KIND_LEFT:
                raise PiDslError(str(either.get_left()), tracing.from_bindings(traces.get()))
            case Either.KIND_RIGHT:
                return binding_to_term(either.get_right(), self)

    # Checks that a term has the expected type in the current environment
    def check_type(self, term: Term, type_: Type):
        error, traces = bindings.check_type(self.binding(), term.binding(), type_.binding()).get()
        if error.kind == Maybe.KIND_JUST:
            raise PiDslError(str(error.get_just()), tracing.from_bindings(traces.get()))

    def elaborate(self, term: Term) -> Term:
        either, traces = bindings.elaborate(self.binding(), term.binding()).get()
        match either.kind:
            case Either.KIND_LEFT:
                raise PiDslError(str(either.get_left()), tracing.from_bindings(traces.get()))
            case Either.KIND_RIGHT:
                return binding_to_term(either.get_right(), self)

    def delaborate(self, term: Term) -> Term:
        either, traces = bindings.delaborate(self.binding(), term.binding()).get()
        match either.kind:
            case Either.KIND_LEFT:
                raise PiDslError(str(either.get_left()), tracing.from_bindings(traces.get()))
            case Either.KIND_RIGHT:
                return binding_to_term(either.get_right(), self)

    def elaborate_against(self, term: Term, type_: Type) -> Term:
        either, traces = \
            bindings.elaborate_against(self.binding(), term.binding(), type_.binding()).get()
        match either.kind:
            case Either.KIND_LEFT:
                raise PiDslError(str(either.get_left()), tracing.from_bindings(traces.get()))
            case Either.KIND_RIGHT:
                return binding_to_term(either.get_right(), self)

    def delaborate_against(self, term: Term, type_: Type) -> Term:
        either, traces = \
            bindings.delaborate_against(self.binding(), term.binding(), type_.binding()).get()
        match either.kind:
            case Either.KIND_LEFT:
                raise PiDslError(str(either.get_left()), tracing.from_bindings(traces.get()))
            case Either.KIND_RIGHT:
                return binding_to_term(either.get_right(), self)

    def unify(self, term1: Term, term2: Term):
        error, traces = bindings.unify(self.binding(), term1.binding(), term2.binding()).get()
        if error.kind == Maybe.KIND_JUST:
            raise PiDslError(str(error.get_just()), tracing.from_bindings(traces.get()))

    def whnf(self, term: Term) -> Term:
        either, traces = bindings.whnf(self.binding(), term.binding()).get()
        match either.kind:
            case Either.KIND_LEFT:
                raise PiDslError(str(either.get_left()), tracing.from_bindings(traces.get()))
            case Either.KIND_RIGHT:
                return binding_to_term(either.get_right(), self)

    def instantiate_mvars(self, term: Term) -> Term:
        either, traces = bindings.instantiate_mvars(self.binding(), term.binding()).get()
        match either.kind:
            case Either.KIND_LEFT:
                raise PiDslError(str(either.get_left()), tracing.from_bindings(traces.get()))
            case Either.KIND_RIGHT:
                return binding_to_term(either.get_right(), self)
