from .term import Ctor, hole, Lam, Param, Pi, Rec, Term, Universe, Var
from .env import Env
from .sugar import ctor, datatype, decl, DataTypeMeta, Self

env = Env()

@datatype(env)
class Bool(metaclass=DataTypeMeta):
    false: Ctor = ctor([], Self)
    true: Ctor = ctor([], Self)

@datatype(env)
class Nat(metaclass=DataTypeMeta):
    zero: Ctor = ctor([], Self)
    succ: Ctor = ctor([(Var("n"), Self)], Self)

@decl(env, Pi([Nat, Nat], Nat))
def add(n: Var, m: Var) -> Term:
    return Rec(Nat)(Lam(hole, Nat), m, Lam(hole, Nat.succ), n)

T, a, b = Var("T"), Var("a"), Var("b")
@datatype(env, type_params=[(T, Universe), (a, T), (b, T)])
class Eq(metaclass=DataTypeMeta):
    refl: Ctor = ctor([(T, Universe), (a, T)], Self(T, a, a))

@decl(env, Pi([(T, Universe), (a, T), (b, T), Eq(T, a, b)], Eq(T, b, a)))
def sym() -> Term:
    motive = Lam([T, a, b, hole], Eq(T, b, a))
    return Rec(Eq)(motive, Eq.refl)

c, h = Var("c"), Var("h")
@decl(env, Pi([(T, Universe), (a, T), (b, T), (c, T), Eq(T, a, b), Eq(T, b, c)], Eq(T, a, c)))
def trans(T: Var, a: Var, b: Var, c: Var, h1: Var, h2: Var) -> Term:
    motive = Lam([T, a, b, hole], Pi([(c, T), Eq(T, b, c)], Eq(T, a, c)))
    return Rec(Eq)(motive, Lam([T, a, c, h], h))(T, a, b, h1, c, h2)

U, f = Var("U"), Var("f")
params: list[Param] = [(T, Universe), (U, Universe), (f, Pi(T, U)), (a, T), (b, T), Eq(T, a, b)]
@decl(env, Pi(params, Eq(U, f(a), f(b))))
def cong(T: Var, U: Var, f: Var, a: Var, b: Var, h: Var) -> Term:
    motive = Lam([T, a, b, hole], Eq(U, f(a), f(b)))
    return Rec(Eq)(motive, Lam([T, a], Eq.refl(U, f(a))))(T, a, b, h)
