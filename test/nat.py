from pi_dsl.term import *
from pi_dsl.env import Def, type_check, TypeDecl

false = Ctor("false", [])
true = Ctor("true", [])
Bool = DataType("Bool", [], [false, true])

Nat = DataType("Nat", [], [])
zero = Ctor("zero", [])
succ = Ctor("succ", [Nat])
Nat.ctors = [zero, succ]

is_zero = Var("is_zero")
is_zero_decl = TypeDecl(is_zero, Pi(Nat, Bool))
x = Var("x")
is_zero_body = Lam(x, Case(x, [
    (zero, false),
    ((succ, [hole]), true)]))
is_zero_def = Def(is_zero, is_zero_body)

type_check([Bool, Nat, is_zero_decl, is_zero_def])