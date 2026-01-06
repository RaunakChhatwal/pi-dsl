from .env import env
from ..sugar import datatype, decl, DataTypeMeta, lam, Self
from ..term import Ctor, Pi, Rec, Term, Set, Var

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
    motive = lam(lambda T, a, b, _: Pi([(f, T >> U)], Eq(U, f(a), f(b))))
    return Rec(Eq)(motive, lam(lambda T, a, f: Eq.refl(U, f(a))))(T, a, b, h, f)

@datatype(env)
class Void(metaclass=DataTypeMeta):
    pass

@decl(env)
def exfalso(T: Var[Set], void: Var[Void]) -> Term[T]:
    motive = lam(lambda _: T)
    return Rec(Void)(motive, void)
