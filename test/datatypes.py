from pi_dsl.env import Env, PiDslError
from pi_dsl.std.bool import Bool
from pi_dsl.std.env import env
from pi_dsl.std.eq import Eq
from pi_dsl.std.nat import Nat
from pi_dsl.sugar import datatype, DataTypeMeta, Self
from pi_dsl.term import Ctor, Pi, Rec, Set, Sort, Var

# Verify that Bool, Nat, and Eq recursors have type Sort(1)
env.check_type(env.infer_type(Rec(Bool)), Sort(1))
env.check_type(env.infer_type(Rec(Nat)), Sort(1))
env.check_type(env.infer_type(Rec(Eq)), Sort(1))

# Finite types indexed by Nat: Fin(n) has exactly n inhabitants
n, m = Var("n"), Var("m")
@datatype(env, Nat >> Set)
class Fin(metaclass=DataTypeMeta):
    # Base case: zero is in Fin(succ(n)) for any n
    zero: Ctor[(n, Nat) >> Self(Nat.succ(n))]
    # Inductive case: if m is in Fin(n), then succ(m) is in Fin(succ(n))
    succ: Ctor[(n, Nat) >> ((m, Self(n)) >> Self(Nat.succ(n)))]

# Verify Fin recursor type
env.check_type(env.infer_type(Rec(Fin)), Sort(1))

# N-ary trees indexed by branching factor n
subtrees = Var("subtrees")
@datatype(env, Nat >> Set)
class NTree(metaclass=DataTypeMeta):
    # Leaf constructor: a leaf node with branching factor n
    leaf: Ctor[(n, Nat) >> Self(n)]
    # Node constructor: contains n subtrees selected by Fin(n) indices
    node: Ctor[Pi([(n, Nat), (subtrees, (m, Fin(n)) >> Self(n))], Self(n))]

# Verify NTree recursor type
env.check_type(env.infer_type(Rec(NTree)), Sort(1))

# Test: negative occurrence of Self should fail positivity check
try:
    @datatype(Env())
    class Bad(metaclass=DataTypeMeta):
        bad: Ctor[Pi(Self, Self) >> Self]
    raise Exception("Bad data type type checked without errors")
except PiDslError:
    pass

# Test: non-strictly positive occurrence should fail
try:
    @datatype(Env(Bool))
    class Bad2(metaclass=DataTypeMeta):
        bad: Ctor[Self >> Bool]
    raise Exception("Bad data type type checked without errors")
except PiDslError:
    pass
