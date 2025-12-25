from pi_dsl.term import Lam, Pi, Rec, Var
from pi_dsl.std import add, cong, env, Eq, Nat, refl, succ, trans, zero

# n + 0 = n
h, n, pred = Var("h"), Var("n"), Var("pred")
add_zero = Var("add_zero")
motive = Lam(n, Eq(Nat, add(n, zero), n))
base_case = refl(Nat, zero)
inductive_case = Lam([pred, h], cong(Nat, Nat, succ, add(pred, zero), pred, h))
defn = Rec(Nat)(motive, base_case, inductive_case)
env.declare(add_zero, Pi((n, Nat), Eq(Nat, add(n, zero), n)), defn)

# n + m.succ = succ(n + m)
m = Var("m")
add_succ = Var("add_succ")
motive = Lam(n, Eq(Nat, add(n, succ(m)), succ(add(n, m))))
base_case = refl(Nat, succ(m))
inductive_case = Lam([pred, h], cong(Nat, Nat, succ, add(pred, succ(m)), succ(add(pred, m)), h))
defn = Lam([n, m], Rec(Nat)(motive, base_case, inductive_case, n))
env.declare(add_succ, Pi([(n, Nat), (m, Nat)], Eq(Nat, add(n, succ(m)), succ(add(n, m)))), defn)

# n + m = m + n
add_comm = Var("add_comm")
motive = Lam(m, Eq(Nat, add(n, m), add(m, n)))
base_case = add_zero(n)
inductive_case = Lam([m, h], trans(Nat, add(n, succ(m)), succ(add(n, m)), succ(add(m, n)),
    add_succ(n, m), # n + m.succ = succ (n + m)
    cong(Nat, Nat, succ, add(n, m), add(m, n), h))) # succ (n + m) = succ (m + n)
defn = Lam(n, Rec(Nat)(motive, base_case, inductive_case))
env.declare(add_comm, Pi([(n, Nat), (m, Nat)], Eq(Nat, add(n, m), add(m, n))), defn)
