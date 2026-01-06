from .env import env
from ..term import Ctor, Pi, Rec, Term, Set, Var
from ..sugar import datatype, decl, DataTypeMeta, lam, Self

a, b, A, B = Var("a"), Var("b"), Var("A"), Var("B")
@datatype(env, signature=Pi([(A, Set), (B, A >> Set)], Set))
class Sigma(metaclass=DataTypeMeta):
    pair: Ctor[Pi([(A, Set), (B, A >> Set), (a, A), (b, B(a))], Self(A, B))]

@decl(env)
def fst(A: Var[Set], B: Var[A >> Set], pair: Var[Sigma(A, B)]) -> Term[A]:
    motive = lam(lambda A, B, _: A)
    get_fst = lam(lambda A, B, a, b: a)
    return Rec(Sigma)(motive, get_fst, A, B, pair)

pair = Var("pair")
@decl(env)
def snd(A: Var[Set], B: Var[A >> Set], pair: Var[Sigma(A, B)]) -> Term[B(fst(A, B, pair))]:
    motive = lam(lambda A, B, pair: B(fst(A, B, pair)))
    get_snd = lam(lambda A, B, a, b: b)
    return Rec(Sigma)(motive, get_snd, A, B, pair)
