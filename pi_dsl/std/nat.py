from .bool import Bool
from .env import env
from ..term import Ctor, Rec, Term, Var
from ..sugar import datatype, decl, DataTypeMeta, lam, Self

n = Var("n")
@datatype(env)
class Nat(metaclass=DataTypeMeta):
    zero: Ctor[Self]
    succ: Ctor[(n, Self) >> Self]

@decl(env)
def add(n: Var[Nat], m: Var[Nat]) -> Term[Nat]:
    return Rec(Nat)(lam(lambda _: Nat), m, lam(lambda _, acc: Nat.succ(acc)), n)

@decl(env)
def mul(n: Var[Nat], m: Var[Nat]) -> Term[Nat]:
    return Rec(Nat)(lam(lambda _: Nat), Nat.zero, lam(lambda _, acc: add(acc, m)), n)

@decl(env)
def is_zero(n: Var[Nat]) -> Term[Bool]:
    return Rec(Nat)(lam(lambda _: Bool), Bool.true, lam(lambda _, __: Bool.false), n)
