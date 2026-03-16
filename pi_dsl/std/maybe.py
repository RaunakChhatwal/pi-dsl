from .env import env
from ..term import Ctor, Level, Pi, Rec, Term, Sort, Var
from ..sugar import datatype, decl, DataTypeMeta, lam, Self

# Optional value type: Nothing or Just wrapping a value of type T
T, U = Var("T"), Var("U")
u = Level("u")
v = Level("v")
@datatype(env, signature=Pi([(T, Sort(u))], Sort(u)))
class Maybe(metaclass=DataTypeMeta):
    # Constructor for absent value
    nothing: Ctor[(T, Sort(u)) >> Self(T)]
    # Constructor wrapping a value of type T
    just: Ctor[(T, Sort(u)) >> (T >> Self(T))]

# Eliminator for Maybe: applies default on Nothing, f on Just
@decl(env)
def maybe(T: Var[Sort(u)], U: Var[Sort(v)], default: Var[U], f: Var[T >> U], m: Var[Maybe(T)]) -> Term[U]:
    motive = lam(lambda T, _: (T >> U) >> U)
    nothing_case = lam(lambda T, _: default)
    just_case = lam(lambda T, x, f: f(x))
    return Rec(Maybe)(motive, nothing_case, just_case, T, m, f)
