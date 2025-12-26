from .term import Ctor, hole, Lam, Pi, Rec, Universe, Var
from .env import Env
from .sugar import ctor, datatype, DataTypeMeta, Self

env = Env()

@datatype(env)
class Bool(metaclass=DataTypeMeta):
    false: Ctor = ctor([], Self)
    true: Ctor = ctor([], Self)

n = Var("n")
@datatype(env)
class Nat(metaclass=DataTypeMeta):
    zero: Ctor = ctor([], Self)
    succ: Ctor = ctor([(n, Self)], Self)

m = Var("m")
add = Var("add")
defn = Lam([n, m], Rec(Nat)(Lam(hole, Nat), m, Lam(hole, Nat.succ), n))
env.declare(add, Pi([Nat, Nat], Nat), defn)

a, b, T = Var("a"), Var("b"), Var("T")
@datatype(env, type_params=[(T, Universe), (a, T), (b, T)])
class Eq(metaclass=DataTypeMeta):
    refl: Ctor = ctor([(T, Universe), (a, T)], Self(T, a, a))

sym = Var("sym")
motive = Lam([T, a, b, hole], Eq(T, b, a))
defn = Rec(Eq)(motive, Eq.refl)
env.declare(sym, Pi([(T, Universe), (a, T), (b, T), Eq(T, a, b)], Eq(T, b, a)), defn)

c, h = Var("c"), Var("h")
trans = Var("trans")
signature = Pi([(T, Universe), (a, T), (b, T), (c, T), Eq(T, a, b), Eq(T, b, c)], Eq(T, a, c))
motive = Lam([T, a, b, hole], Pi([(c, T), Eq(T, b, c)], Eq(T, a, c)))
defn = Lam([T, a, b, c, h],
    Rec(Eq)(motive, Lam([T, a, c, h], h))(T, a, b, h, c))
env.declare(trans, signature, defn)

U, f = Var("U"), Var("f")
cong = Var("cong")
signature = Pi(
    [(T, Universe), (U, Universe), (f, Pi(T, U)), (a, T), (b, T), Eq(T, a, b)],
    Eq(U, f(a), f(b)))
motive = Lam([T, a, b, h], Eq(U, f(a), f(b)))
body = Rec(Eq)(motive, Lam([T, a], Eq.refl(U, f(a))), T, a, b, h)
defn = Lam([T, U, f, a, b, h], body)
env.declare(cong, signature, defn)