from pi_dsl.term import *
from pi_dsl.env import Def, Entry, type_check, TypeDecl

env: list[Entry] = []

# false = Ctor("false", [])
# true = Ctor("true", [])
Bool = DataType("Bool", [], [])
false = Ctor("false", [], Bool)
true = Ctor("true", [], Bool)
Bool.ctors = [false, true]
env.append(Bool)

Nat = DataType("Nat", [], [])
zero = Ctor("zero", [], Nat)
succ = Ctor("succ", [Nat], Nat)
Nat.ctors = [zero, succ]
env.append(Nat)

# is_zero = Var("is_zero")
# is_zero_decl = TypeDecl(is_zero, Pi(Nat, Bool))
# x = Var("x")
# is_zero_body = Lam(x, Case(x, [
#     (zero, false),
#     ((succ, [hole]), true)]))
# is_zero_def = Def(is_zero, is_zero_body)
# env.extend([is_zero_decl, is_zero_def])

# pred = Var("pred")
# pred_decl = TypeDecl(pred, Pi(Nat, Nat))
# n = Var("n")
# m = Var("m")
# pred_body = Lam(n, Case(n, [
#     (zero, zero),
#     ((succ, [m]), m)]))
# pred_def = Def(pred, pred_body)
# env.extend([pred_decl, pred_def])

# nat_eq = Var("nat_eq")
# nat_eq_decl = TypeDecl(nat_eq, Pi(Nat, Pi(Nat, Bool)))
# y = Var("y")
# nat_eq_body = Lam(x, Lam(y, Case(x, [
#     (zero, Case(y, [
#         (zero, true),
#         ((succ, [n]), false)])),
#     ((succ, [n]), Case(y, [
#         (zero, false),
#         ((succ, [m]), App(App(nat_eq, n), m))]))])))
# nat_eq_def = Def(nat_eq, nat_eq_body)
# env.extend([nat_eq_decl, nat_eq_def])

# plus = Var("plus")
# plus_decl = TypeDecl(plus, Pi(Nat, Pi(Nat, Nat)))
# plus_body = Lam(n, Lam(m, Case(n, [
#     (zero, m),
#     ((succ, [pred]), App(succ, App(App(plus, pred), m)))])))
# plus_def = Def(plus, plus_body)
# env.extend([plus_decl, plus_def])

# type_check(env)
type_check(env)