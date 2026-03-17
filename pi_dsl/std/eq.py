from .env import env
from ..sugar import datatype, decl, DataTypeMeta, I, lam, Self
from ..term import Ctor, IVar, Lam, Level, Pi, Rec, Set, Sort, Term, Var

# Propositional equality type indexed by a type and two values
T, a, b = Var("T"), Var("a"), Var("b")
u = Level("u")
@datatype(env, I(T, Sort(u)) >> ((a, T) >> ((b, T) >> Set)))
class Eq(metaclass=DataTypeMeta):
    # Reflexivity constructor: any value is equal to itself
    refl: Ctor[I(T, Sort(u)) >> ((a, T) >> Self(a, a))]

def eq(self: Term, other: Term) -> Term:
    self.assert_sibling(other)
    return Eq(self, other)

Term.__eq__ = eq

# Symmetry of equality: if a = b then b = a
@decl(env)
def sym(T: IVar[Sort(u)], a: IVar[T], b: IVar[T], h: Var[a == b]) -> Term[b == a]:
    motive = lam(lambda a, b, _: b == a)
    return Rec(Eq)(motive, Eq.refl, a, b, h)

# Transitivity of equality: if a = b and b = c then a = c
c = Var("c")
@decl(env)
def trans(T: IVar[Sort(u)], a: IVar[T], b: IVar[T], c: IVar[T], h1: Var[a == b], h2: Var[b == c]
) -> Term[a == c]:
    motive = Lam(T, lam(lambda a, b, _: Pi([(c, T), b == c], a == c)))
    return Rec(Eq)(motive, lam(lambda a, c, h: h))(a, b, h1, c, h2)

# Congruence: if a = b then f(a) = f(b) for any function f
U, f = Var("U"), Var("f")
@decl(env)
def cong(
    T: IVar[Sort(u)], U: IVar[Sort("v")], f: Var[T >> U], a: IVar[T], b: IVar[T], h: Var[a == b]
) -> Term[f(a) == f(b)]:
    motive = Lam(T, lam(lambda a, b, _: Pi([(f, T >> U)], f(a) == f(b))))
    return Rec(Eq)(motive, lam(lambda a, f: Eq.refl(f(a))))(a, b, h, f)

# Empty type with no constructors (logical falsity)
@datatype(env)
class Void(metaclass=DataTypeMeta):
    pass

# Ex falso quodlibet: from a proof of Void we can derive anything
@decl(env)
def exfalso(T: Var[Sort(u)], void: Var[Void]) -> Term[T]:
    motive = lam(lambda _: T)
    return Rec(Void)(motive, void)
