import inspect
from typing import Any, Callable
from . import bindings
from .env import Env
from .term import *

class SelfSingleton(Term):
    def binding(self) -> bindings.Term:
        raise Exception("Invalid occurrence of `Self` stub")

Self = SelfSingleton()

class DataTypeMeta(type, DataType):
    def __call__(cls, *args: Term) -> Term:
        return Term.__call__(cls, *args)

    def __new__(mcls, name: str, *args: Any):
        cls = super().__new__(mcls, name, *args)
        cls.name = name
        return cls

def remove_stub_from_param(param: Param, self: DataType) -> Param:
    match param:
        case Term():
            return remove_stub(param, self)
        case (var, term):
            return (var, remove_stub(term, self))

def remove_stub(term: Term, self: DataType) -> Term:
    match term:
        case Ann(body, hint):
            return Ann(remove_stub(body, self), remove_stub(hint, self))
        case App(func, arg):
            return App(remove_stub(func, self), remove_stub(arg, self))
        case Ctor(name, DataType(type_name, type_signature, ctors), signature):
            datatype = DataType(type_name, remove_stub(type_signature, self), ctors)
            return Ctor(name, datatype, remove_stub(signature, self))
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
        case DataType() | Global() | Sort() | Var():
            return term
        case _:
            assert isinstance(term, SelfSingleton)
            return self

def datatype(env: Env, signature: Type=Set):
    def decorator[T: DataTypeMeta](cls: T) -> T:
        cls.signature = signature
        cls.ctors = []

        for class_var, hint in cls.__dict__.get("__annotations__", {}).items():
            if not isinstance(hint, Hint):
                continue

            assert hint.cls is Ctor
            ctor = Ctor(class_var, cls, remove_stub(hint.hint, cls))
            setattr(cls, class_var, ctor)
            cls.ctors.append(ctor)

        env.add_datatype(cls)
        return cls

    return decorator

def lam(func: Callable[..., Term]) -> Term:
    params = inspect.signature(func).parameters
    param_vars: list[Var] = [Var(name) for name in params.keys()]
    return Lam(param_vars, func(*param_vars))

def decl(env: Env):
    def decorator(func: Callable[..., Term]) -> Term:
        signature_ = inspect.signature(func)
        params: list[Param] = \
            [(Var(name), param.annotation.hint) for name, param in signature_.parameters.items()]
        signature = Pi(params, signature_.return_annotation.hint)

        var = Global(func.__name__)
        env.declare(var, signature, lam(func))
        return var

    return decorator

# TODO: support member functions for data types and dot notation for datatype variables
