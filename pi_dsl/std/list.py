from .env import env
from .nat import Nat
from ..term import Ctor, Pi, Rec, Term, Set, Var
from ..sugar import datatype, decl, DataTypeMeta, lam, Self

# Polymorphic list datatype parameterized by element type T
x, xs, T, U = Var("x"), Var("xs"), Var("T"), Var("U")
@datatype(env, signature=Pi([(T, Set)], Set))
class List(metaclass=DataTypeMeta):
    # Empty list constructor
    nil: Ctor[(T, Set) >> Self(T)]
    # Cons constructor: prepend element x to list xs
    cons: Ctor[Pi([(T, Set), (x, T), (xs, Self(T))], Self(T))]

# Computes the length of a list as a Nat
@decl(env)
def length(T: Var[Set], xs: Var[List(T)]) -> Term[Nat]:
    motive = lam(lambda T, _: Nat)
    nil_case = lam(lambda T: Nat.zero)
    cons_case = lam(lambda T, x, xs, acc: Nat.succ(acc))
    return Rec(List)(motive, nil_case, cons_case, T, xs)

# Applies function f to each element of a list, producing a new list
@decl(env)
def map(T: Var[Set], U: Var[Set], f: Var[T >> U], xs: Var[List(T)]) -> Term[List(U)]:
    motive = lam(lambda T, _: (T >> U) >> List(U))
    nil_case = lam(lambda T, _: List.nil(U))
    cons_case = lam(lambda T, x, xs, acc, f: List.cons(U, f(x), acc(f)))
    return Rec(List)(motive, nil_case, cons_case, T, xs, f)
