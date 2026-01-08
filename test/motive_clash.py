from pi_dsl.std.bool import Bool, env
from pi_dsl.sugar import DataTypeMeta, datatype, decl, Self
from pi_dsl.term import Ctor, Rec, Set, Sort, Term

# Declaration that returns Bool as a type (used to test motive clash detection)
@decl(env)
def motive() -> Term[Set]:
    return Bool

# Test datatype whose constructor takes a motive as argument
@datatype(env)
class T(metaclass=DataTypeMeta):
    mk: Ctor[motive >> Self]

# Verify the recursor type infers correctly and is in Sort(1)
print(env.infer_type(Rec(T)))
env.check_type(env.infer_type(Rec(T)), Sort(1))