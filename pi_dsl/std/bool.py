from .env import env
from ..term import Ctor, Rec, Term, Set, Var
from ..sugar import datatype, decl, DataTypeMeta, lam, Self

# Boolean datatype with two constructors: false and true
@datatype(env)
class Bool(metaclass=DataTypeMeta):
    false: Ctor[Self]
    true: Ctor[Self]

# Eliminates a Bool by returning t if true, f if false
T = Var("T")
@decl(env)
def if_(T: Var[Set], t: Var[T], f: Var[T], cond: Var[Bool]) -> Term[T]:
    motive = lam(lambda _: T)
    return Rec(Bool)(motive, f, t, cond)

# Boolean negation: returns false if true, true if false
@decl(env)
def not_(cond: Var[Bool]) -> Term[Bool]:
    return if_(Bool, Bool.false, Bool.true, cond)

# Boolean conjunction: returns true only if both arguments are true
@decl(env)
def and_(p: Var[Bool], q: Var[Bool]) -> Term[Bool]:
    return if_(Bool, q, Bool.false, p)

# Boolean disjunction: returns true if either argument is true
@decl(env)
def or_(p: Var[Bool], q: Var[Bool]) -> Term[Bool]:
    return if_(Bool, Bool.true, q, p)
