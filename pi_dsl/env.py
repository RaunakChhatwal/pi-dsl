from __future__ import annotations
from dataclasses import dataclass
from .base import List, String
from . import bindings, tracing
from .bindings import BinderInfo, Either, Entry, Maybe, unbind
from .term import Ann, App, DataType, Global, IParam, IVar, Lam, Pi, Rec, Sort, Term, Type, Var

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

        case bindings.Term.KIND_VAR:
            var_binding = binding.get_var()
            match var_binding.kind:
                case bindings.Var.KIND_LOCAL:
                    return Var.from_binding(var_binding.get_local())
                case bindings.Var.KIND_GLOBAL:
                    return Global(str(var_binding.get_global()))
                case bindings.Var.KIND_META:
                    raise NotImplementedError

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
            binder_info, param_type_binding, bind = binding.get_pi()
            param_name, return_type = unbind(bind).get()
            implicit = binder_info.kind == bindings.BinderInfo.KIND_IMPLICIT
            var = Var.from_binding(param_name)
            param_type = binding_to_term(param_type_binding, env)
            param = IParam(var, param_type) if implicit else (var, param_type)
            return Pi(param, binding_to_term(return_type, env))

        case bindings.Term.KIND_ANN:
            term, hint = binding.get_ann()
            return Ann(binding_to_term(term, env), binding_to_term(hint, env))

        case bindings.Term.KIND_DATA_TYPE:
            return env.datatypes[str(binding.get_data_type())]

        case bindings.Term.KIND_CTOR:
            type_name, ctor_name = binding.get_ctor()
            data_type = env.datatypes[str(type_name)]
            return [ctor for ctor in data_type.ctors if ctor.name == str(ctor_name)][0]

        case bindings.Term.KIND_REC:
            return Rec(env.datatypes[str(binding.get_rec())])

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

    # Adds a datatype to the environment and type checks incrementally
    def add_datatype(self, datatype: DataType):
        assert datatype.name not in self.entries
        self.entries.append(datatype.name)
        self.datatypes[datatype.name] = datatype
        try:
            self.type_check()
        except Exception as error:
            self.entries.pop()
            self.datatypes.pop(datatype.name)
            raise error

    # Adds a declaration with signature and body to the environment, type checks incrementally
    def declare(self, var: Global, hint: Type, defn: Term):
        assert var.name not in self.entries
        self.entries.append(var.name)
        self.decls[var.name] = Decl(var, hint, defn)
        try:
            self.type_check()
        except Exception as error:
            self.entries.pop()
            self.decls.pop(var.name)
            raise error

    # Retrieves a datatype or declaration entry by name
    def get_entry(self, name: str) -> DataType | Decl:
        if name in self.datatypes:
            return self.datatypes[name]
        elif name in self.decls:
            return self.decls[name]
        else:
            raise ValueError(f"No entry with name {name}")

    # Converts all entries to Haskell bindings for FFI calls
    def entry_bindings(self) -> list[Entry]:
        return [self.get_entry(entry).entry_binding() for entry in self.entries]

    # Type checks all entries in the environment via the Haskell core
    def type_check(self):
        error, traces = bindings.type_check(List[bindings.Entry](*self.entry_bindings())).get()
        if error.kind == Maybe.KIND_JUST:
            raise PiDslError(str(error.get_just()), tracing.from_bindings(traces.get()))

    # Infers the type of a term in the current environment
    def infer_type(self, term: Term) -> Term:
        either, traces = \
            bindings.infer_type(List[bindings.Entry](*self.entry_bindings()), term.binding()).get()
        match either.kind:
            case Either.KIND_LEFT:
                raise PiDslError(str(either.get_left()), tracing.from_bindings(traces.get()))
            case Either.KIND_RIGHT:
                return binding_to_term(either.get_right(), self)

    # Checks that a term has the expected type in the current environment
    def check_type(self, term: Term, type_: Type):
        error, traces = bindings.check_type(
            List[bindings.Entry](*self.entry_bindings()), term.binding(), type_.binding()).get()
        if error.kind == Maybe.KIND_JUST:
            raise PiDslError(str(error.get_just()), tracing.from_bindings(traces.get()))
