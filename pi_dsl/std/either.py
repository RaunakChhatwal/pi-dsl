from .env import env
from ..term import Ctor, Pi, Rec, Term, Set, Var
from ..sugar import datatype, decl, DataTypeMeta, lam, Self

# Sum type: Either A B is either a Left A or a Right B
A, B = Var("A"), Var("B")
@datatype(env, signature=Pi([(A, Set), (B, Set)], Set))
class Either(metaclass=DataTypeMeta):
    # Left constructor: injects a value of type A into Either A B
    left: Ctor[Pi([(A, Set), (B, Set), A], Self(A, B))]
    # Right constructor: injects a value of type B into Either A B
    right: Ctor[Pi([(A, Set), (B, Set), B], Self(A, B))]

# Eliminator for Either: applies f to Left values and g to Right values
T = Var("T")
@decl(env)
def either(
    A: Var[Set], B: Var[Set], T: Var[Set], f: Var[A >> T], g: Var[B >> T], e: Var[Either(A, B)]
) -> Term[T]:
    motive = lam(lambda A, B, _: Pi([A >> T, B >> T], T))
    left_case = lam(lambda A, B, a, f, g: f(a))
    right_case = lam(lambda A, B, b, f, g: g(b))
    return Rec(Either)(motive, left_case, right_case, A, B, e, f, g)
