from pi_dsl.term import hole, Lam, Pi, Var, Universe
from pi_dsl.env import Decl, Entry, type_check

env: list[Entry] = []

T = Var("T")
Nat = Var("Nat")
defn = Pi([(T, Universe), T, Pi(T, T)], T)
env.append(Decl(Nat, Universe, defn))

x, zf, sf = Var("x"), Var("zf"), Var("sf")
zero = Var("zero")
defn = Lam([x, zf, sf], zf)
env.append(Decl(zero, Nat, defn))

n = Var("n")
succ = Var("succ")
defn = Lam([n, x, zf, sf], sf(n(x, zf, sf)))
env.append(Decl(succ, Pi(Nat, Nat), defn))

one, two, three = Var("one"), Var("two"), Var("three")
env.append(Decl(one, Nat, succ(zero)))
env.append(Decl(two, Nat, succ(succ(zero))))
env.append(Decl(three, Nat, succ(succ(succ(zero)))))

y = Var("y")
add = Var("add")
defn = Lam([x, y], x(Nat, y, succ))
env.append(Decl(add, Pi([Nat, Nat], Nat), defn))

Prop = Var("Prop")
Eq = Var("Eq")
signature = Pi([(T, Universe), (x, T), (y, T)], Universe)
defn = Lam([T, x, y], Pi([(Prop, Pi(T, Universe)), Prop(x)], Prop(y)))
env.append(Decl(Eq, signature, defn))

prop = Var("prop")
refl = Var("refl")
signature = Pi([(T, Universe), (x, T)], Eq(T, x, x))
defn = Lam([hole, hole, hole, prop], prop)
env.append(Decl(refl, signature, defn))

type_check(env)
