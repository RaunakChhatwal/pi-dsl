from pi_dsl.std.env import env
from pi_dsl.std.eq import cong, Eq, trans
from pi_dsl.std.nat import Nat
from pi_dsl.sugar import decl, lam
from pi_dsl.term import Rec, Term, Var

# Proof that n + 0 = n by induction on n
n = Var("n")
@decl(env)
def add_zero(n: Var[Nat]) -> Term[Eq(n + Nat.zero, n)]:
    motive = lam(lambda n: Eq(n + Nat.zero, n))
    base_case = Eq.refl(Nat.zero)
    inductive_case = lam(lambda pred, h: cong(Nat.succ, h))
    return Rec(Nat)(motive, base_case, inductive_case, n)

# Proof that n + succ(m) = succ(n + m) by induction on n
m = Var("m")
@decl(env)
def add_succ(n: Var[Nat], m: Var[Nat]) -> Term[Eq(n + Nat.succ(m), Nat.succ(n + m))]:
    motive = lam(lambda n: Eq(n + Nat.succ(m), Nat.succ(n + m)))
    base_case = Eq.refl(Nat.succ(m))
    inductive_case = lam(lambda pred, h: cong(Nat.succ, h))
    return Rec(Nat)(motive, base_case, inductive_case, n)

# Proof of commutativity: n + m = m + n by induction on m
@decl(env)
def add_comm(n: Var[Nat], m: Var[Nat]) -> Term[Eq(n + m, m + n)]:
    motive = lam(lambda m: Eq(n + m, m + n))
    inductive_case = lam(lambda m, h: trans(add_succ(n, m), cong(Nat.succ, h)))
    return Rec(Nat)(motive, add_zero(n), inductive_case, m)
