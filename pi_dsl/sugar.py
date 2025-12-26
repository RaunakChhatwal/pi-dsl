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
            return App(remove_stub(body, self), remove_stub(hint, self))
        case App(func, arg):
            return App(remove_stub(func, self), remove_stub(arg, self))
        case Ctor(name, DataType(type_name, type_params, ctors), params, return_type):
            type_params = [remove_stub_from_param(param, self) for param in type_params]
            datatype = DataType(type_name, type_params, ctors)
            params = [remove_stub_from_param(param, self) for param in params]
            return Ctor(name, datatype, params, remove_stub(return_type, self))
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
        case DataType() | UniverseSingleton() | Var():
            return term
        case _:
            assert isinstance(term, SelfSingleton)
            return self

def datatype(env: Env, type_params: list[Param] = []):
    def decorator[T: DataTypeMeta](cls: T) -> T:
        cls.params = type_params
        cls.ctors = []

        for class_var, hint in cls.__dict__.get("__annotations__", {}).items():
            if hint is not Ctor:
                continue

            ctor = getattr(cls, class_var)
            ctor.name = class_var
            ctor.datatype = cls
            ctor.params = [remove_stub_from_param(param, cls) for param in ctor.params]
            ctor.return_type = remove_stub(ctor.return_type, cls)
            cls.ctors.append(ctor)

        env.add_datatype(cls)
        return cls

    return decorator

def ctor(params: list[Param], return_type: Type) -> Ctor:
    ctor = Ctor.__new__(Ctor)
    ctor.params = params
    ctor.return_type = return_type
    return ctor


def lam(func: Callable[..., Term]) -> Term:
    sig_params = inspect.signature(func).parameters
    param_vars: list[Var] = [Var(name) for name in sig_params.keys()]
    return Lam(param_vars, func(*param_vars))

def decl(env: Env, signature: Type):
    def decorator(func: Callable[..., Term]) -> Term:
        var = Var(func.__name__)
        env.declare(var, signature, lam(func))
        return var

    return decorator
