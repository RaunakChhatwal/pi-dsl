from .env import env
from ..term import Ctor, IVar, Level, Max, Pi, Rec, Term, Sort, Var
from ..sugar import datatype, decl, DataTypeMeta, lam, Self

# Sum type: Either A B is either a Left A or a Right B
A, B = Var("A"), Var("B")
u = Level("u")
v = Level("v")
w = Level("w")
@datatype(env, signature=Pi([(A, Sort(u)), (B, Sort(v))], Sort(Max(u, v))))
class Either(metaclass=DataTypeMeta):
    # Left constructor: injects a value of type A into Either A B
    left: Ctor[Pi([(A, Sort(u)), (B, Sort(v)), A], Self(A, B))]
    # Right constructor: injects a value of type B into Either A B
    right: Ctor[Pi([(A, Sort(u)), (B, Sort(v)), B], Self(A, B))]

# Eliminator for Either: applies f to Left values and g to Right values
T = Var("T")
@decl(env)
def either(
    A: IVar[Sort(u)], B: IVar[Sort(v)], T: IVar[Sort(w)], f: Var[A >> T], g: Var[B >> T], e: Var[Either(A, B)]
) -> Term[T]:
    motive = lam(lambda _: Pi([A >> T, B >> T], T))
    left_case = lam(lambda a, f, g: f(a))
    right_case = lam(lambda b, f, g: g(b))
    return Rec(Either)(motive, left_case, right_case, e, f, g)
