from pi_dsl.term import Lam, Pi, Rec, Term, Var
from pi_dsl.std import add, cong, env, Eq, Nat, trans
from pi_dsl.sugar import decl

# n + 0 = n
h, n, pred = Var("h"), Var("n"), Var("pred")
@decl(env, Pi((n, Nat), Eq(Nat, add(n, Nat.zero), n)))
def add_zero(n: Var) -> Term:
    motive = Lam(n, Eq(Nat, add(n, Nat.zero), n))
    base_case = Eq.refl(Nat, Nat.zero)
    inductive_case = Lam([pred, h], cong(Nat, Nat, Nat.succ, add(pred, Nat.zero), pred, h))
    return Rec(Nat)(motive, base_case, inductive_case, n)

# n + m.succ = succ(n + m)
m = Var("m")
signature = Pi([(n, Nat), (m, Nat)], Eq(Nat, add(n, Nat.succ(m)), Nat.succ(add(n, m))))
@decl(env, signature)
def add_succ(n: Var, m: Var) -> Term:
    motive = Lam(n, Eq(Nat, add(n, Nat.succ(m)), Nat.succ(add(n, m))))
    base_case = Eq.refl(Nat, Nat.succ(m))
    inductive_case = Lam([pred, h], cong(Nat, Nat, Nat.succ,
        add(pred, Nat.succ(m)),
        Nat.succ(add(pred, m)), h))
    return Rec(Nat)(motive, base_case, inductive_case, n)

# n + m = m + n
signature = Pi([(n, Nat), (m, Nat)], Eq(Nat, add(n, m), add(m, n)))
@decl(env, signature)
def add_comm(n: Var, m: Var) -> Term:
    motive = Lam(m, Eq(Nat, add(n, m), add(m, n)))
    base_case = add_zero(n)
    inductive_case = Lam([m, h],
        trans(Nat, add(n, Nat.succ(m)), Nat.succ(add(n, m)), Nat.succ(add(m, n)), # redundant args
            add_succ(n, m), # n + m.succ = succ (n + m)
            cong(Nat, Nat, Nat.succ, add(n, m), add(m, n), h))) # succ (n + m) = succ (m + n)
    return Rec(Nat)(motive, base_case, inductive_case, m)