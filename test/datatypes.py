from pi_dsl.term import Ctor, DataType, Pi, Universe, Var
from pi_dsl.env import Entry, type_check

env: list[Entry] = []

Bool = DataType("Bool", [], [])
false = Ctor("false", Bool, [], Bool)
true = Ctor("true", Bool, [], Bool)
Bool.ctors = [false, true]
env.append(Bool)

Nat = DataType("Nat", [], [])
zero = Ctor("zero", Nat, [], Nat)
succ = Ctor("succ", Nat, [Nat], Nat)
Nat.ctors = [zero, succ]
env.append(Nat)

a, b, T = Var("a"), Var("b"), Var("T")
Eq = DataType ("Eq", [(T, Universe), (a, T), (b, T)], [])
refl = Ctor("refl", Eq, [(T, Universe), (a, T)], Eq(T, a, a))
Eq.ctors = [refl]
env.append(Eq)

type_check(env)

try:
    Bad = DataType("Bad", [], [])
    bad = Ctor("bad", Bad, [Pi(Bad, Bad)], Bad)
    Bad.ctors = [bad]
    type_check([Bad])
    raise Exception("Bad data type type checked without errors")
except TypeError:
    pass

try:
    Bad = DataType("Bad", [], [])
    bad = Ctor("bad", Bad, [Bad], Bool)
    Bad.ctors = [bad]
    type_check([Bool, Bad])
    raise Exception("Bad data type type checked without errors")
except TypeError:
    pass