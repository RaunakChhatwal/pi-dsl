from .env import env
from .nat import Nat
from ..term import Ctor, IVar, Level, Pi, Rec, Term, Sort, Var
from ..sugar import datatype, decl, DataTypeMeta, lam, Self

# Polymorphic list datatype parameterized by element type T
x, xs, T, U = Var("x"), Var("xs"), Var("T"), Var("U")
u = Level("u")
v = Level("v")
@datatype(env, signature=Pi([(T, Sort(u))], Sort(u)))
class List(metaclass=DataTypeMeta):
    # Empty list constructor
    nil: Ctor[(T, Sort(u)) >> Self(T)]
    # Cons constructor: prepend element x to list xs
    cons: Ctor[Pi([(T, Sort(u)), (x, T), (xs, Self(T))], Self(T))]

# Computes the length of a list as a Nat
@decl(env)
def length(T: IVar[Sort(u)], xs: Var[List(T)]) -> Term[Nat]:
    motive = lam(lambda _: Nat)
    nil_case = Nat.zero
    cons_case = lam(lambda x, xs, acc: Nat.succ(acc))
    return Rec(List)(motive, nil_case, cons_case, xs)

# Applies function f to each element of a list, producing a new list
@decl(env)
def map(T: IVar[Sort(u)], U: IVar[Sort(v)], f: Var[T >> U], xs: Var[List(T)]) -> Term[List(U)]:
    motive = lam(lambda _: (T >> U) >> List(U))
    nil_case = lam(lambda _: List.nil(U))
    cons_case = lam(lambda x, xs, acc, f: List.cons(U, f(x), acc(f)))
    return Rec(List)(motive, nil_case, cons_case, xs, f)
