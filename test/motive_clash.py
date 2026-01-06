from pi_dsl.std.bool import Bool, env
from pi_dsl.sugar import DataTypeMeta, datatype, decl, Self
from pi_dsl.term import Ctor, Rec, Set, Sort, Term

@decl(env)
def motive() -> Term[Set]:
    return Bool

@datatype(env)
class T(metaclass=DataTypeMeta):
    mk: Ctor[motive >> Self]

print(env.infer_type(Rec(T)))
env.check_type(env.infer_type(Rec(T)), Sort(1))
