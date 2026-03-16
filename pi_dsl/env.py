from __future__ import annotations
from dataclasses import dataclass
from .base import String
from . import bindings, tracing
from .bindings import BinderInfo, Either, Maybe, unbind
from .term import *

# Represents a top-level declaration with a name, type signature, and definition body
@dataclass
class Decl:
    var: Global
    univ_params: list[str]
    signature: Type
    body: Term

    # Converts this declaration to a Haskell Entry binding for type checking
    def entry_binding(self) -> bindings.Entry:
        univ_params = \
            bindings.List[bindings.UnivParamName](*[String(param) for param in self.univ_params])
        return bindings.Entry.init_decl(
            String(self.var.name), univ_params, self.signature.binding(), self.body.binding())

def instantiate_datatype(datatype: DataType, level_args: list[IntoLevel]) -> DataType:
    univ_params, signature, ctors = datatype.univ_params, datatype.signature, datatype.ctors
    datatype = DataType(datatype.name, univ_params, signature, [], level_args=level_args)
    datatype.ctors = \
        [Ctor(ctor.name, datatype, ctor.signature, level_args=level_args) for ctor in ctors]
    return datatype

def const_binding_to_term(binding: bindings.Const, levels: list[bindings.Level], env: Env) -> Const:
    level_args: list[IntoLevel] = [Level.from_binding(level) for level in levels]
    match binding.kind:
        case bindings.Const.KIND_G_VAR:
            return Global(str(binding.get_g_var()), level_args=level_args)
        case bindings.Const.KIND_DATA_TYPE:
            return instantiate_datatype(env.datatypes[str(binding.get_data_type())], level_args)
        case bindings.Const.KIND_CTOR:
            type_name, ctor_name = binding.get_ctor()
            datatype = instantiate_datatype(env.datatypes[str(type_name)], level_args)
            return [ctor for ctor in datatype.ctors if ctor.name == str(ctor_name)][0]
        case bindings.Const.KIND_REC:
            datatype = instantiate_datatype(
                env.datatypes[str(binding.get_rec())], level_args[1:] if level_args else [])
            return Rec(datatype, level_args=level_args)

# Converts a Haskell Term binding back into a Python Term AST node
def binding_to_term(binding: bindings.Term, env: Env) -> Term:
    match binding.kind:
        case bindings.Term.KIND_SORT:
            return Sort(Level.from_binding(binding.get_sort()))

        case bindings.Term.KIND_L_VAR:
            return Var.from_binding(binding.get_l_var())

        case bindings.Term.KIND_M_VAR:
            return MVar(int(binding.get_m_var()))

        case bindings.Term.KIND_CONST:
            const_binding, levels_binding = binding.get_const()
            return const_binding_to_term(const_binding, levels_binding.get(), env)

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
    binding: bindings.Env

    # Initializes environment with optional datatype and declaration entries
    def __init__(self, *entries: DataType | Decl):
        self.datatypes = {}
        self.decls = {}
        self.entries = []
        self.binding = bindings.Env.init_env(
            bindings.Map(bindings.Map.KIND_TIP),
            bindings.Map(bindings.Map.KIND_TIP),
            bindings.LocalContext.init_local_context(
                bindings.Map(bindings.Map.KIND_TIP),
                bindings.List[bindings.Tuple[bindings.TermName, bindings.Type]]()),
            bindings.Maybe(bindings.Maybe.KIND_NOTHING))

        for entry in entries:
            match entry:
                case DataType():
                    self.add_datatype(entry)
                case Decl():
                    self.declare(entry.var, entry.univ_params, entry.signature, entry.body)

    # Adds a datatype to the environment and type checks incrementally
    def add_datatype(self, datatype: DataType):
        assert datatype.name not in self.entries
        either, traces = bindings.add_entry(self.binding, datatype.entry_binding()).get()
        if either.kind == Either.KIND_LEFT:
            raise PiDslError(str(either.get_left()), tracing.from_bindings(traces.get()))

        self.entries.append(datatype.name)
        self.datatypes[datatype.name] = datatype
        self.binding = either.get_right()

    # Adds a declaration with signature and body to the environment, type checks incrementally
    def declare(self, var: Global, univ_params: list[str], hint: Type, defn: Term):
        assert var.name not in self.entries
        decl = Decl(var, univ_params, hint, defn)
        either, traces = \
            bindings.add_entry(self.binding, decl.entry_binding()).get()
        if either.kind == Either.KIND_LEFT:
            raise PiDslError(str(either.get_left()), tracing.from_bindings(traces.get()))

        self.entries.append(var.name)
        self.decls[var.name] = decl
        self.binding = either.get_right()

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
        either, traces = bindings.infer_type(self.binding, term.binding()).get()
        match either.kind:
            case Either.KIND_LEFT:
                raise PiDslError(str(either.get_left()), tracing.from_bindings(traces.get()))
            case Either.KIND_RIGHT:
                return binding_to_term(either.get_right(), self)

    # Checks that a term has the expected type in the current environment
    def check_type(self, term: Term, type_: Type):
        error, traces = bindings.check_type(self.binding, term.binding(), type_.binding()).get()
        if error.kind == Maybe.KIND_JUST:
            raise PiDslError(str(error.get_just()), tracing.from_bindings(traces.get()))

    def elaborate(self, term: Term) -> Term:
        either, traces = bindings.elaborate(self.binding, term.binding()).get()
        match either.kind:
            case Either.KIND_LEFT:
                raise PiDslError(str(either.get_left()), tracing.from_bindings(traces.get()))
            case Either.KIND_RIGHT:
                return binding_to_term(either.get_right(), self)

    def delaborate(self, term: Term) -> Term:
        either, traces = bindings.delaborate(self.binding, term.binding()).get()
        match either.kind:
            case Either.KIND_LEFT:
                raise PiDslError(str(either.get_left()), tracing.from_bindings(traces.get()))
            case Either.KIND_RIGHT:
                return binding_to_term(either.get_right(), self)

    def elaborate_against(self, term: Term, type_: Type) -> Term:
        either, traces = \
            bindings.elaborate_against(self.binding, term.binding(), type_.binding()).get()
        match either.kind:
            case Either.KIND_LEFT:
                raise PiDslError(str(either.get_left()), tracing.from_bindings(traces.get()))
            case Either.KIND_RIGHT:
                return binding_to_term(either.get_right(), self)

    def delaborate_against(self, term: Term, type_: Type) -> Term:
        either, traces = \
            bindings.delaborate_against(self.binding, term.binding(), type_.binding()).get()
        match either.kind:
            case Either.KIND_LEFT:
                raise PiDslError(str(either.get_left()), tracing.from_bindings(traces.get()))
            case Either.KIND_RIGHT:
                return binding_to_term(either.get_right(), self)

    def unify(self, term1: Term, term2: Term):
        error, traces = bindings.unify(self.binding, term1.binding(), term2.binding()).get()
        if error.kind == Maybe.KIND_JUST:
            raise PiDslError(str(error.get_just()), tracing.from_bindings(traces.get()))

    def whnf(self, term: Term) -> Term:
        either, traces = bindings.whnf(self.binding, term.binding()).get()
        match either.kind:
            case Either.KIND_LEFT:
                raise PiDslError(str(either.get_left()), tracing.from_bindings(traces.get()))
            case Either.KIND_RIGHT:
                return binding_to_term(either.get_right(), self)

    def instantiate_mvars(self, term: Term) -> Term:
        either, traces = bindings.instantiate_mvars(self.binding, term.binding()).get()
        match either.kind:
            case Either.KIND_LEFT:
                raise PiDslError(str(either.get_left()), tracing.from_bindings(traces.get()))
            case Either.KIND_RIGHT:
                return binding_to_term(either.get_right(), self)
