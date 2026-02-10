from .env import env
from ..sugar import datatype, decl, DataTypeMeta, I, lam, Self
from ..term import Ctor, IVar, Pi, Rec, Term, Set, Var

# Propositional equality type indexed by a type and two values
T, a, b = Var("T"), Var("a"), Var("b")
@datatype(env, I(T, Set) >> ((a, T) >> ((b, T) >> Set)))
class Eq(metaclass=DataTypeMeta):
    # Reflexivity constructor: any value is equal to itself
    refl: Ctor[I(T, Set) >> ((a, T) >> Self(a, a))]

# Symmetry of equality: if a = b then b = a
@decl(env)
def sym(T: IVar[Set], a: IVar[T], b: IVar[T], h: Var[Eq(a, b)]) -> Term[Eq(b, a)]:
    motive = lam(lambda a, b, _: Eq(b, a))
    return Rec(Eq)(motive, Eq.refl, a, b, h)

# Transitivity of equality: if a = b and b = c then a = c
c = Var("c")
@decl(env)
def trans(T: IVar[Set], a: IVar[T], b: IVar[T], c: IVar[T], h1: Var[Eq(a, b)], h2: Var[Eq(b, c)]
) -> Term[Eq(a, c)]:
    motive = lam(lambda a, b, _: Pi([(c, T), Eq(b, c)], Eq(a, c)))
    return Rec(Eq)(motive, lam(lambda a, c, h: h))(a, b, h1, c, h2)

# Congruence: if a = b then f(a) = f(b) for any function f
U, f = Var("U"), Var("f")
@decl(env)
def cong(T: IVar[Set], U: IVar[Set], f: Var[T >> U], a: IVar[T], b: IVar[T], h: Var[Eq(a, b)]
) -> Term[Eq(f(a), f(b))]:
    motive = lam(lambda a, b, _: Pi([(f, T >> U)], Eq(f(a), f(b))))
    return Rec(Eq)(motive, lam(lambda a, f: Eq.refl(f(a))))(a, b, h, f)

# Empty type with no constructors (logical falsity)
@datatype(env)
class Void(metaclass=DataTypeMeta):
    pass

# Ex falso quodlibet: from a proof of Void we can derive anything
@decl(env)
def exfalso(T: Var[Set], void: Var[Void]) -> Term[T]:
    motive = lam(lambda _: T)
    return Rec(Void)(motive, void)
