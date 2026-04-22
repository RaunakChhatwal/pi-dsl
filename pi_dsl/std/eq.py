from .env import env
from ..sugar import datatype, decl, DataTypeMeta, I, lam, Self
from ..term import Ctor, IVar, Level, Pi, Rec, Set, Sort, Term, Var

# Propositional equality type indexed by a type and two values
T, a, b = Var("T"), Var("a"), Var("b")
u, v = Level("u"), Level("v")
@datatype(env, I(T, Sort(u)) >> ((a, T) >> ((b, T) >> Set)))
class Eq(metaclass=DataTypeMeta):
    # Reflexivity constructor: any value is equal to itself
    refl: Ctor[I(T, Sort(u)) >> ((a, T) >> Self(a, a))]

def eq(self: Term, other: Term) -> Term:
    self.assert_sibling(other)
    return Eq(self, other)

Term.__eq__ = eq

# Substitution/transport: if a = b then any property holding at a holds at b
P = Var("P")
@decl(env)
def subst(T: IVar[Sort(u)], a: IVar[T], b: IVar[T], h: Var[a == b], P: Var[T >> Sort(v)], h2: Var[P(a)]) -> Term[P(b)]:
    motive = lam(lambda b, _: Pi([(P, T >> Sort(v)), (h2, P(a))], P(b)))
    return Rec(Eq)(motive, lam(lambda _, h2: h2), h, P, h2)

# Symmetry of equality: if a = b then b = a
@decl(env)
def sym(T: IVar[Sort(u)], a: IVar[T], b: IVar[T], h: Var[a == b]) -> Term[b == a]:
    return subst(h, lam(lambda b: b == a), Eq.refl(a))

# Transitivity of equality: if a = b and b = c then a = c
c = Var("c")
@decl(env)
def trans(T: IVar[Sort(u)], a: IVar[T], b: IVar[T], c: IVar[T], h1: Var[a == b], h2: Var[b == c]) -> Term[a == c]:
    return subst(h2, lam(lambda c: a == c), h1)

# Congruence: if a = b then f(a) = f(b) for any function f
U, f = Var("U"), Var("f")
@decl(env)
def cong(
    T: IVar[Sort(u)], U: IVar[Sort(v)], f: Var[T >> U], a: IVar[T], b: IVar[T], h: Var[a == b]
) -> Term[f(a) == f(b)]:
    return subst(h, lam(lambda b: f(a) == f(b)), Eq.refl(f(a)))

# Empty type with no constructors (logical falsity)
@datatype(env)
class Void(metaclass=DataTypeMeta):
    pass

# Ex falso quodlibet: from a proof of Void we can derive anything
@decl(env)
def exfalso(T: IVar[Sort(u)], void: Var[Void]) -> Term[T]:
    motive = lam(lambda _: T)
    return Rec(Void)(motive, void)
