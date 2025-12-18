from pi_dsl.term import *
from pi_dsl.env import Def, Entry, type_check, TypeDecl

env: list[Entry] = []

T = Var("T")
Nat = Var("Nat")
defn = Pi([(T, Universe), T, Pi(T, T)], T)
env.extend([TypeDecl(Nat, Universe), Def(Nat, defn)])

x, zf, sf = Var("x"), Var("zf"), Var("sf")
zero = Var("zero")
defn = Lam([x, zf, sf], zf)
env.extend([TypeDecl(zero, Nat), Def(zero, defn)])

n = Var("n")
succ = Var("succ")
defn = Lam([n, x, zf, sf], sf(n(x, zf, sf)))
env.extend([TypeDecl(succ, Pi(Nat, Nat)), Def(succ, defn)])

one, two, three = Var("one"), Var("two"), Var("three")
env.extend([TypeDecl(one, Nat), Def(one, succ(zero))])
env.extend([TypeDecl(two, Nat), Def(two, succ(succ(zero)))])
env.extend([TypeDecl(three, Nat), Def(three, succ(succ(succ(zero))))])

y = Var("y")
add = Var("add")
defn = Lam([x, y], x(Nat, y, succ))
env.extend([TypeDecl(add, Pi([Nat, Nat], Nat)), Def(add, defn)])

Prop = Var("Prop")
Eq = Var("Eq")
sgture = Pi([(T, Universe), (x, T), (y, T)], Universe)
defn = Lam([T, x, y], Pi([(Prop, Pi(T, Universe)), Prop(x)], Prop(y)))
env.extend([TypeDecl(Eq, sgture), Def(Eq, defn)])

prop = Var("prop")
refl = Var("refl")
sgture = Pi([(T, Universe), (x, T)], Eq(T, x, x))
defn = Lam([hole, hole, hole, prop], prop)
env.extend([TypeDecl(refl, sgture), Def(refl, defn)])

type_check(env)