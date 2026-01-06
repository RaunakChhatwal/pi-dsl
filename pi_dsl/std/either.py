from .env import env
from ..term import Ctor, Pi, Rec, Term, Set, Var
from ..sugar import datatype, decl, DataTypeMeta, lam, Self

A, B = Var("A"), Var("B")
@datatype(env, signature=Pi([(A, Set), (B, Set)], Set))
class Either(metaclass=DataTypeMeta):
    left: Ctor[Pi([(A, Set), (B, Set), A], Self(A, B))]
    right: Ctor[Pi([(A, Set), (B, Set), B], Self(A, B))]

T = Var("T")
@decl(env)
def either(
    A: Var[Set], B: Var[Set], T: Var[Set], f: Var[A >> T], g: Var[B >> T], e: Var[Either(A, B)]
) -> Term[T]:
    motive = lam(lambda A, B, _: Pi([A >> T, B >> T], T))
    left_case = lam(lambda A, B, a, f, g: f(a))
    right_case = lam(lambda A, B, b, f, g: g(b))
    return Rec(Either)(motive, left_case, right_case, A, B, e, f, g)
