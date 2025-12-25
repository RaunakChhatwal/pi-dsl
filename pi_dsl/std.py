from .term import Ctor, DataType, hole, Lam, Pi, Rec, Universe, Var
from .env import Env

env = Env()

Bool = DataType("Bool", [], [])
false = Ctor("false", Bool, [], Bool)
true = Ctor("true", Bool, [], Bool)
Bool.ctors = [false, true]
env.add_datatype(Bool)

n = Var("n")
Nat = DataType("Nat", [], [])
zero = Ctor("zero", Nat, [], Nat)
succ = Ctor("succ", Nat, [(n, Nat)], Nat)
Nat.ctors = [zero, succ]
env.add_datatype(Nat)

prev, m = Var("prev"), Var("m")
add = Var("add")
defn = Lam([n, m], Rec(Nat)(Lam(hole, Nat), m, Lam([hole, prev], succ(prev)), n))
env.declare(add, Pi([Nat, Nat], Nat), defn)

a, b, T = Var("a"), Var("b"), Var("T")
Eq = DataType ("Eq", [(T, Universe), (a, T), (b, T)], [])
refl = Ctor("refl", Eq, [(T, Universe), (a, T)], Eq(T, a, a))
Eq.ctors = [refl]
env.add_datatype(Eq)

sym = Var("sym")
motive = Lam([T, a, b, hole], Eq(T, b, a))
defn = Rec(Eq)(motive, refl)
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
body = Rec(Eq)(motive, Lam([T, a], refl(U, f(a))), T, a, b, h)
defn = Lam([T, U, f, a, b, h], body)
env.declare(cong, signature, defn)