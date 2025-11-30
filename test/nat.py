from pi_dsl import *

def var(name: str) -> Name[Any]:
    return Name[Any].init_fn(String(name), Int(0))

hole = var("_")

zero_ctor_def = CtorDef.init_ctor_def(String("zero"), List[Entry]())
Nat = Term.init_ty_con(String("Nat"), List[Arg]())
nat_param_type = Entry.init_decl(TypeDecl.init_type_decl(hole, Epsilon.rel, Nat))
succ_ctor_def = CtorDef.init_ctor_def(String("succ"), init_list(nat_param_type))
nat_entry = Entry.init_data(String("Nat"), List[Entry](), init_list(zero_ctor_def, succ_ctor_def))

Bool = Term.init_ty_con(String("Bool"), List[Arg]())
is_zero_type = Term.init_ty_pi(Epsilon.rel, Nat, Bind[TName, Type].init_b(hole, Bool))
is_zero = var("is_zero")
is_zero_decl = Entry.init_decl(TypeDecl.init_type_decl(is_zero, Epsilon.rel, is_zero_type))
is_zero_def = Entry.init_def(is_zero, Term.trust_me)

# Type check is_zero
env_entries = prelude_data_decls + [nat_entry, is_zero_decl, is_zero_def]
env = Env.init_env(init_list(*env_entries), Int(len(env_entries)), List[TypeDecl]())
assert check_type(env, Term.trust_me, is_zero_type).kind == Maybe.KIND_NOTHING