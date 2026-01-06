from .env import env
from ..term import Ctor, Pi, Rec, Term, Set, Var
from ..sugar import datatype, decl, DataTypeMeta, lam, Self

T, U = Var("T"), Var("U")
@datatype(env, signature=Pi([(T, Set)], Set))
class Maybe(metaclass=DataTypeMeta):
    nothing: Ctor[(T, Set) >> Self(T)]
    just: Ctor[(T, Set) >> (T >> Self(T))]

@decl(env)
def maybe(T: Var[Set], U: Var[Set], default: Var[U], f: Var[T >> U], m: Var[Maybe(T)]) -> Term[U]:
    motive = lam(lambda T, _: (T >> U) >> U)
    nothing_case = lam(lambda T, _: default)
    just_case = lam(lambda T, x, f: f(x))
    return Rec(Maybe)(motive, nothing_case, just_case, T, m, f)
