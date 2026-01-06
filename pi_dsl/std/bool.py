from .env import env
from ..term import Ctor, Rec, Term, Set, Var
from ..sugar import datatype, decl, DataTypeMeta, lam, Self

@datatype(env)
class Bool(metaclass=DataTypeMeta):
    false: Ctor[Self]
    true: Ctor[Self]

T = Var("T")
@decl(env)
def if_(T: Var[Set], t: Var[T], f: Var[T], cond: Var[Bool]) -> Term[T]:
    motive = lam(lambda _: T)
    return Rec(Bool)(motive, f, t, cond)

@decl(env)
def not_(cond: Var[Bool]) -> Term[Bool]:
    return if_(Bool, Bool.false, Bool.true, cond)

@decl(env)
def and_(p: Var[Bool], q: Var[Bool]) -> Term[Bool]:
    return if_(Bool, q, Bool.false, p)

@decl(env)
def or_(p: Var[Bool], q: Var[Bool]) -> Term[Bool]:
    return if_(Bool, Bool.true, q, p)
