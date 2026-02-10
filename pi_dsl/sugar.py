import inspect
from typing import Any, Callable
from . import bindings
from .env import Env
from .term import Ann, App, Ctor, DataType, Global, Hint, IParam, IVar, Lam, Param, Pi, Rec, Set, \
    Sort, Term, Type, Var

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
        case DataType() | Global() | Sort() | Rec() | Var():
            return term
        case _:
            assert isinstance(term, SelfSingleton)
            return self

# Decorator for defining inductive datatypes with automatic constructor generation
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

# Creates a lambda term from a Python function by inspecting its parameter names
def lam(func: Callable[..., Term]) -> Term:
    # params = inspect.signature(func).parameters
    param_vars: list[Var] = [] # [Var(name) for name in params.keys()]
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
            if param.annotation.cls is IVar:
                params.append(I(Var(name), param.annotation.hint))
            else:
                params.append((Var(name), param.annotation.hint))
        signature = Pi(params, func_signature.return_annotation.hint)

        var = Global(func.__name__)
        env.declare(var, signature, lam(func))
        return var

    return decorator

# TODO: support member functions for data types and dot notation for datatype variables
