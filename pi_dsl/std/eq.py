from .env import env
from ..sugar import datatype, decl, DataTypeMeta, lam, Self
from ..term import Ctor, Pi, Rec, Term, Set, Var

# Propositional equality type indexed by a type and two values
T, a, b = Var("T"), Var("a"), Var("b")
@datatype(env, (T, Set) >> ((a, T) >> ((b, T) >> Set)))
class Eq(metaclass=DataTypeMeta):
    # Reflexivity constructor: any value is equal to itself
    refl: Ctor[(T, Set) >> ((a, T) >> Self(T, a, a))]

# Symmetry of equality: if a = b then b = a
@decl(env)
def sym(T: Var[Set], a: Var[T], b: Var[T], h: Var[Eq(T, a, b)]) -> Term[Eq(T, b, a)]:
    motive = lam(lambda T, a, b, _: Eq(T, b, a))
    return Rec(Eq)(motive, Eq.refl, T, a, b, h)

# Transitivity of equality: if a = b and b = c then a = c
c = Var("c")
@decl(env)
def trans(T: Var[Set], a: Var[T], b: Var[T], c: Var[T], h1: Var[Eq(T, a, b)], h2: Var[Eq(T, b, c)]
) -> Term[Eq(T, a, c)]:
    motive = lam(lambda T, a, b, _: Pi([(c, T), Eq(T, b, c)], Eq(T, a, c)))
    return Rec(Eq)(motive, lam(lambda T, a, c, h: h))(T, a, b, h1, c, h2)

# Congruence: if a = b then f(a) = f(b) for any function f
U, f = Var("U"), Var("f")
@decl(env)
def cong(T: Var[Set], U: Var[Set], f: Var[T >> U], a: Var[T], b: Var[T], h: Var[Eq(T, a, b)]
) -> Term[Eq(U, f(a), f(b))]:
    motive = lam(lambda T, a, b, _: Pi([(f, T >> U)], Eq(U, f(a), f(b))))
    return Rec(Eq)(motive, lam(lambda T, a, f: Eq.refl(U, f(a))))(T, a, b, h, f)

# Empty type with no constructors (logical falsity)
@datatype(env)
class Void(metaclass=DataTypeMeta):
    pass

# Ex falso quodlibet: from a proof of Void we can derive anything
@decl(env)
def exfalso(T: Var[Set], void: Var[Void]) -> Term[T]:
    motive = lam(lambda _: T)
    return Rec(Void)(motive, void)
