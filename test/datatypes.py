from pi_dsl.env import Env, PiDslError
from pi_dsl.std.bool import Bool
from pi_dsl.std.env import env
from pi_dsl.std.eq import Eq
from pi_dsl.std.nat import Nat
from pi_dsl.sugar import datatype, DataTypeMeta, Self
from pi_dsl.term import Ctor, Pi, Rec, Set, Sort, Var

env.check_type(env.infer_type(Rec(Bool)), Sort(1))
env.check_type(env.infer_type(Rec(Nat)), Sort(1))
env.check_type(env.infer_type(Rec(Eq)), Sort(1))

# Finite types indexed by Nat
n, m = Var("n"), Var("m")
@datatype(env, Nat >> Set)
class Fin(metaclass=DataTypeMeta):
    zero: Ctor[(n, Nat) >> Self(Nat.succ(n))]
    succ: Ctor[(n, Nat) >> ((m, Self(n)) >> Self(Nat.succ(n)))]

env.check_type(env.infer_type(Rec(Fin)), Sort(1))

# N-ary trees indexed by branching factor
subtrees = Var("subtrees")
@datatype(env, Nat >> Set)
class NTree(metaclass=DataTypeMeta):
    leaf: Ctor[(n, Nat) >> Self(n)]
    node: Ctor[Pi([(n, Nat), (subtrees, (m, Fin(n)) >> Self(n))], Self(n))]

env.check_type(env.infer_type(Rec(NTree)), Sort(1))

try:
    @datatype(Env())
    class Bad(metaclass=DataTypeMeta):
        bad: Ctor[Pi(Self, Self) >> Self]
    raise Exception("Bad data type type checked without errors")
except PiDslError:
    pass

try:
    @datatype(Env(Bool))
    class Bad2(metaclass=DataTypeMeta):
        bad: Ctor[Self >> Bool]
    raise Exception("Bad data type type checked without errors")
except PiDslError:
    pass
