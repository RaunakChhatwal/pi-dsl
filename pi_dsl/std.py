from .term import Ctor, Pi, Rec, Term, Set, Var
from .env import Env
from .sugar import datatype, decl, DataTypeMeta, lam, Self

env = Env()

@datatype(env)
class Bool(metaclass=DataTypeMeta):
    false: Ctor[Self]
    true: Ctor[Self]

n = Var("n")
@datatype(env)
class Nat(metaclass=DataTypeMeta):
    zero: Ctor[Self]
    succ: Ctor[(n, Self) >> Self]

@decl(env)
def add(n: Var[Nat], m: Var[Nat]) -> Term[Nat]:
    return Rec(Nat)(lam(lambda _: Nat), m, lam(lambda _: Nat.succ), n)

T, a, b = Var("T"), Var("a"), Var("b")
@datatype(env, (T, Set) >> ((a, T) >> ((b, T) >> Set)))
class Eq(metaclass=DataTypeMeta):
    refl: Ctor[(T, Set) >> ((a, T) >> Self(T, a, a))]

@decl(env)
def sym(T: Var[Set], a: Var[T], b: Var[T], h: Var[Eq(T, a, b)]) -> Term[Eq(T, b, a)]:
    motive = lam(lambda T, a, b, _: Eq(T, b, a))
    return Rec(Eq)(motive, Eq.refl, T, a, b, h)

c = Var("c")
@decl(env)
def trans(T: Var[Set], a: Var[T], b: Var[T], c: Var[T], h1: Var[Eq(T, a, b)], h2: Var[Eq(T, b, c)]
) -> Term[Eq(T, a, c)]:
    motive = lam(lambda T, a, b, _: Pi([(c, T), Eq(T, b, c)], Eq(T, a, c)))
    return Rec(Eq)(motive, lam(lambda T, a, c, h: h))(T, a, b, h1, c, h2)

U, f = Var("U"), Var("f")
@decl(env)
def cong(T: Var[Set], U: Var[Set], f: Var[T >> U], a: Var[T], b: Var[T], h: Var[Eq(T, a, b)]
) -> Term[Eq(U, f(a), f(b))]:
    motive = lam(lambda T, a, b, _: Eq(U, f(a), f(b)))
    return Rec(Eq)(motive, lam(lambda T, a: Eq.refl(U, f(a))))(T, a, b, h)
