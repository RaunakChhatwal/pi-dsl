import inspect
# from itertools import chain
# from copy import copy
from typing import Any, Callable
from . import bindings
from .env import Env
from .term import *

# Singleton stub representing the self-referential type in datatype definitions
class SelfSingleton(Term):
    def binding(self) -> bindings.Term:
        raise Exception("Invalid occurrence of `Self` stub")

# Global instance of SelfSingleton used in constructor type annotations
Self = SelfSingleton()

# Metaclass enabling datatype classes to behave as both types and term constructors
class DataTypeMeta(type, DataType):
    # Enables calling the datatype as a term application
    def __call__(cls, *args: Term) -> Term:
        return Term.__call__(cls, *args)

    # Sets the datatype name from the class name
    def __new__(mcls, name: str, *args: Any):
        cls = super().__new__(mcls, name, *args)
        cls.name = name
        return cls

I = IParam

# Recursively replaces Self stub with the actual datatype in a parameter
def remove_stub_from_param(param: Param, self: DataType) -> Param:
    match param:
        case I(var, term):
            return I(var, remove_stub(term, self))
        case Term():
            return remove_stub(param, self)
        case (var, term):
            return (var, remove_stub(term, self))

# Recursively replaces Self stub with the actual datatype in a term
def remove_stub(term: Term, self: DataType) -> Term:
    match term:
        case Ann(body, hint):
            return Ann(remove_stub(body, self), remove_stub(hint, self))
        case App(func, arg):
            return App(remove_stub(func, self), remove_stub(arg, self))
        case Lam(vars, body):
            return Lam(vars, remove_stub(body, self))
        case Pi(params, return_type):
            return_type = remove_stub(return_type, self)
            match params:
                case list():
                    params = [remove_stub_from_param(param, self) for param in params]
                    return Pi(params, return_type)
                case param:
                    return Pi(remove_stub_from_param(param, self), return_type)
        case Const() | Sort() | Local():
            return term
        case _:
            assert isinstance(term, SelfSingleton)
            return self

def infer_univ_params_from_level(level: Level) -> set[str]:
    match level.inner:
        case int() | LevelMVar():
            return set()
        case str(name):
            return {name}
        case Offset(base, _):
            return infer_univ_params_from_level(base)
        case Max(levels):
            univ_params: set[str] = set()
            for level in levels:
                univ_params |= infer_univ_params_from_level(level)
            return univ_params

def infer_univ_params(term: Term) -> set[str]:
    match term:
        case Ann(term, hint):
            return infer_univ_params(term) | infer_univ_params(hint)
        case App(func, arg):
            return infer_univ_params(func) | infer_univ_params(arg)
        case Lam(_, body):
            return infer_univ_params(body)
        case Pi(params, return_type):
            match params:
                case list() as params:
                    pass
                case param:
                    params = [param]

            univ_params: set[str] = set()
            for _, param_type, _ in map(raw_param, params):
                univ_params |= infer_univ_params(param_type)

            return univ_params | infer_univ_params(return_type)
        case Sort(level):
            return infer_univ_params_from_level(level)
        case Const(level_args=level_args):
            univ_params: set[str] = set()
            for level in level_args:
                univ_params |= infer_univ_params_from_level(Level.fr0m(level))
            return univ_params
        case _:
            return set()

# Decorator for defining inductive datatypes with automatic constructor generation
def datatype(env: Env, signature: Type=Set):
    def decorator[T: DataTypeMeta](cls: T) -> T:
        cls.signature = signature
        cls.ctors = []
        cls.level_args = []

        for class_var, hint in inspect.get_annotations(cls, eval_str=True).items():
            if not isinstance(hint, Hint):
                continue

            assert hint.cls is Ctor
            ctor = Ctor(class_var, cls, remove_stub(hint.hint, cls))
            setattr(cls, class_var, ctor)
            cls.ctors.append(ctor)

        univ_params = infer_univ_params(cls.signature)
        for ctor in cls.ctors:
            univ_params |= infer_univ_params(ctor.signature)
        cls.univ_params = sorted(univ_params)

        env.add_datatype(cls)
        return cls

    return decorator

# Creates a lambda term from a Python function by inspecting its parameter names
def lam(func: Callable[..., Term]) -> Term:
    param_vars: list[Local] = []
    for name, param in inspect.signature(func).parameters.items():
        if isinstance(param.annotation, Hint) and param.annotation.cls is IVar:
            param_vars.append(IVar(name))
        else:
            param_vars.append(Var(name))
    return Lam(param_vars, func(*param_vars))

# Decorator for declaring typed terms by extracting signature from annotations
def decl(env: Env):
    def decorator(func: Callable[..., Term]) -> Global:
        func_signature = inspect.signature(func)
        params: list[Param] = []
        for name, param in func_signature.parameters.items():
            if param.annotation.cls is Var:
                params.append((Var(name), param.annotation.hint))
            elif param.annotation.cls is IVar:
                params.append(I(IVar(name), param.annotation.hint))
            else:
                raise Exception(f"Parameter {name} must be annotated by Var or IVar")
        type_ = Pi(params, func_signature.return_annotation.hint)

        var = Global(func.__name__)
        body = lam(func)
        univ_params = sorted(infer_univ_params(type_) | infer_univ_params(body))
        env.declare(var, univ_params, type_, body)
        return var

    return decorator

# TODO: support member functions for data types and dot notation for datatype variables
