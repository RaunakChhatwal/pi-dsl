from pi_dsl.term import Lam, Pi, Rec, Var
from pi_dsl.std import add, cong, env, Eq, Nat, trans

# n + 0 = n
h, n, pred = Var("h"), Var("n"), Var("pred")
add_zero = Var("add_zero")
motive = Lam(n, Eq(Nat, add(n, Nat.zero), n))
base_case = Eq.refl(Nat, Nat.zero)
inductive_case = Lam([pred, h], cong(Nat, Nat, Nat.succ, add(pred, Nat.zero), pred, h))
defn = Rec(Nat)(motive, base_case, inductive_case)
env.declare(add_zero, Pi((n, Nat), Eq(Nat, add(n, Nat.zero), n)), defn)

# n + m.succ = succ(n + m)
m = Var("m")
add_succ = Var("add_succ")
signature = Pi([(n, Nat), (m, Nat)], Eq(Nat, add(n, Nat.succ(m)), Nat.succ(add(n, m))))
motive = Lam(n, Eq(Nat, add(n, Nat.succ(m)), Nat.succ(add(n, m))))
base_case = Eq.refl(Nat, Nat.succ(m))
inductive_case = Lam([pred, h], cong(Nat, Nat, Nat.succ,
    add(pred, Nat.succ(m)),
    Nat.succ(add(pred, m)), h))
defn = Lam([n, m], Rec(Nat)(motive, base_case, inductive_case, n))
env.declare(add_succ, signature, defn)

# n + m = m + n
add_comm = Var("add_comm")
motive = Lam(m, Eq(Nat, add(n, m), add(m, n)))
base_case = add_zero(n)
inductive_case = Lam([m, h], trans(
    Nat,
    add(n, Nat.succ(m)),
    Nat.succ(add(n, m)),
    Nat.succ(add(m, n)),
    add_succ(n, m), # n + m.succ = succ (n + m)
    cong(Nat, Nat, Nat.succ, add(n, m), add(m, n), h))) # succ (n + m) = succ (m + n)
defn = Lam(n, Rec(Nat)(motive, base_case, inductive_case))
env.declare(add_comm, Pi([(n, Nat), (m, Nat)], Eq(Nat, add(n, m), add(m, n))), defn)
