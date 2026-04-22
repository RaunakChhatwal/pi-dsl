from .env import env
from ..term import Ctor, IVar, Level, Max, Pi, Rec, Term, Sort, Var
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
def fst(A: IVar[Sort(u)], B: IVar[A >> Sort(v)], pair: Var[Sigma(A, B)]) -> Term[A]:
    motive = lam(lambda _: A)
    get_fst = lam(lambda a, b: a)
    return Rec(Sigma)(motive, get_fst, pair)

# Variable binding for pair in snd's dependent type
pair = Var("pair")
# Second projection: extracts the second component with its dependent type
@decl(env)
def snd(A: IVar[Sort(u)], B: IVar[A >> Sort(v)], pair: Var[Sigma(A, B)]) -> Term[B(fst(pair))]:
    motive = lam(lambda pair: B(fst(pair)))
    get_snd = lam(lambda a, b: b)
    return Rec(Sigma)(motive, get_snd, pair)
