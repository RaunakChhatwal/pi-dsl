from pi_dsl.term import Ctor, Pi, Rec, Universe, Var
from pi_dsl.env import Env, KernelError
from pi_dsl.sugar import ctor, datatype, DataTypeMeta, Self

env = Env()

@datatype(env)
class Bool(metaclass=DataTypeMeta):
    false: Ctor = ctor([], Self)
    true: Ctor = ctor([], Self)

env.check_type(env.infer_type(Rec(Bool)), Universe)

n = Var("n")
@datatype(env)
class Nat(metaclass=DataTypeMeta):
    zero: Ctor = ctor([], Self)
    succ: Ctor = ctor([(n, Self)], Self)

env.check_type(env.infer_type(Rec(Nat)), Universe)

a, b, T = Var("a"), Var("b"), Var("T")
@datatype(env, type_params=[(T, Universe), (a, T), (b, T)])
class Eq(metaclass=DataTypeMeta):
    refl: Ctor = ctor([(T, Universe), (a, T)], Self(T, a, a))

env.check_type(env.infer_type(Rec(Eq)), Universe)

# Finite types indexed by Nat
m = Var("m")
@datatype(env, type_params=[(n, Nat)])
class Fin(metaclass=DataTypeMeta):
    zero: Ctor = ctor([(n, Nat)], Self(Nat.succ(n)))
    succ: Ctor = ctor([(n, Nat), (m, Self(n))], Self(Nat.succ(n)))

env.check_type(env.infer_type(Rec(Fin)), Universe)

# N-ary trees indexed by branching factor
@datatype(env, type_params=[(n, Nat)])
class NTree(metaclass=DataTypeMeta):
    leaf: Ctor = ctor([(n, Nat)], Self(n))
    node: Ctor = ctor([(n, Nat), (Var("subtrees"), Pi((m, Fin(n)), Self(n)))], Self(n))

env.check_type(env.infer_type(Rec(NTree)), Universe)

try:
    @datatype(Env())
    class Bad(metaclass=DataTypeMeta):
        bad: Ctor = ctor([Pi(Self, Self)], Self)
    raise Exception("Bad data type type checked without errors")
except KernelError:
    pass

try:
    @datatype(Env(Bool))
    class Bad2(metaclass=DataTypeMeta):
        bad: Ctor = ctor([Self], Bool)
    raise Exception("Bad data type type checked without errors")
except KernelError:
    pass
