from pi_dsl.std.env import env
from pi_dsl.std.eq import cong, Eq, trans
from pi_dsl.std.nat import Nat
from pi_dsl.sugar import decl, lam
from pi_dsl.term import Rec, Term, Var

# n + 0 = n
n = Var("n")
@decl(env)
def add_zero(n: Var[Nat]) -> Term[Eq(Nat, n + Nat.zero, n)]:
    motive = lam(lambda n: Eq(Nat, n + Nat.zero, n))
    base_case = Eq.refl(Nat, Nat.zero)
    inductive_case = lam(lambda pred, h: cong(Nat, Nat, Nat.succ, pred + Nat.zero, pred, h))
    return Rec(Nat)(motive, base_case, inductive_case, n)

# n + m.succ = succ(n + m)
m = Var("m")
@decl(env)
def add_succ(n: Var[Nat], m: Var[Nat]) -> Term[Eq(Nat, n + Nat.succ(m), Nat.succ(n + m))]:
    motive = lam(lambda n: Eq(Nat, n + Nat.succ(m), Nat.succ(n + m)))
    base_case = Eq.refl(Nat, Nat.succ(m))
    inductive_case = lam(lambda pred, h:
        cong(Nat, Nat, Nat.succ, pred + Nat.succ(m), Nat.succ(pred + m), h))
    return Rec(Nat)(motive, base_case, inductive_case, n)

# n + m = m + n
@decl(env)
def add_comm(n: Var[Nat], m: Var[Nat]) -> Term[Eq(Nat, n + m, m + n)]:
    motive = lam(lambda m: Eq(Nat, n + m, m + n))
    base_case = add_zero(n)
    inductive_case = lam(lambda m, h:
        trans(Nat, n + Nat.succ(m), Nat.succ(n + m), Nat.succ(m + n), # redundant args
            add_succ(n, m), # n + m.succ = succ (n + m)
            cong(Nat, Nat, Nat.succ, n + m, m + n, h))) # succ (n + m) = succ (m + n)
    return Rec(Nat)(motive, base_case, inductive_case, m)
