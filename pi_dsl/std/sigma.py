from .env import env
from ..term import Ctor, Level, Max, Pi, Rec, Term, Sort, Var
from ..sugar import datatype, decl, DataTypeMeta, lam, Self

# Sigma type: dependent pair where second component's type depends on the first
a, b, A, B = Var("a"), Var("b"), Var("A"), Var("B")
u = Level("u")
v = Level("v")
@datatype(env, signature=Pi([(A, Sort(u)), (B, A >> Sort(v))], Sort(Max(u, v))))
class Sigma(metaclass=DataTypeMeta):
    pair: Ctor[Pi([(A, Sort(u)), (B, A >> Sort(v)), (a, A), (b, B(a))], Self(A, B))]

# First projection: extracts the first component of a dependent pair
@decl(env)
def fst(A: Var[Sort(u)], B: Var[A >> Sort(v)], pair: Var[Sigma(A, B)]) -> Term[A]:
    motive = lam(lambda A, B, _: A)
    get_fst = lam(lambda A, B, a, b: a)
    return Rec(Sigma)(motive, get_fst, A, B, pair)

# Variable binding for pair in snd's dependent type
pair = Var("pair")
# Second projection: extracts the second component with its dependent type
@decl(env)
def snd(A: Var[Sort(u)], B: Var[A >> Sort(v)], pair: Var[Sigma(A, B)]) -> Term[B(fst(A, B, pair))]:
    motive = lam(lambda A, B, pair: B(fst(A, B, pair)))
    get_snd = lam(lambda A, B, a, b: b)
    return Rec(Sigma)(motive, get_snd, A, B, pair)
