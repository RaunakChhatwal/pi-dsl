from pi_dsl.term import Ctor, DataType, Pi, Rec, Universe, Var
from pi_dsl.env import Env, KernelError

env = Env()

Bool = DataType("Bool", [], [])
false = Ctor("false", Bool, [], Bool)
true = Ctor("true", Bool, [], Bool)
Bool.ctors = [false, true]
env.add_datatype(Bool)
env.check_type(env.infer_type(Rec(Bool)), Universe)

n = Var("n")
Nat = DataType("Nat", [], [])
zero = Ctor("zero", Nat, [], Nat)
succ = Ctor("succ", Nat, [(n, Nat)], Nat)
Nat.ctors = [zero, succ]
env.add_datatype(Nat)
env.check_type(env.infer_type(Rec(Nat)), Universe)

a, b, T = Var("a"), Var("b"), Var("T")
Eq = DataType ("Eq", [(T, Universe), (a, T), (b, T)], [])
refl = Ctor("refl", Eq, [(T, Universe), (a, T)], Eq(T, a, a))
Eq.ctors = [refl]
env.add_datatype(Eq)
env.check_type(env.infer_type(Rec(Eq)), Universe)

# Finite types indexed by Nat
m = Var("m")
Fin = DataType("Fin", [(n, Nat)], [])
zero_fin = Ctor("zero", Fin, [(n, Nat)], Fin(succ(n)))
succ_fin = Ctor("succ", Fin, [(n, Nat), (m, Fin(n))], Fin(succ(n)))
Fin.ctors = [zero_fin, succ_fin]
env.add_datatype(Fin)
env.check_type(env.infer_type(Rec(Fin)), Universe)

# N-ary trees indexed by branching factor
NTree = DataType("NTree", [(n, Nat)], [])
node = Ctor("node", NTree, [(n, Nat), (Var("subtrees"), Pi((m, Fin(n)), NTree(n)))], NTree(n))
NTree.ctors = [node]
env.add_datatype(NTree)
env.check_type(env.infer_type(Rec(NTree)), Universe)

try:
    Bad = DataType("Bad", [], [])
    bad = Ctor("bad", Bad, [Pi(Bad, Bad)], Bad)
    Bad.ctors = [bad]
    Env(Bad)
    raise Exception("Bad data type type checked without errors")
except KernelError:
    pass

try:
    Bad = DataType("Bad", [], [])
    bad = Ctor("bad", Bad, [Bad], Bool)
    Bad.ctors = [bad]
    Env(Bool, Bad)
    raise Exception("Bad data type type checked without errors")
except KernelError:
    pass