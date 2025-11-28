from pi_dsl import *

# Test ppr on boolean literals
true = Term.init_lit_bool(Bool(True))
false = Term.init_lit_bool(Bool(False))
assert str(ppr_term(true)) == "True"
assert str(ppr_term(false)) == "False"

# Pretty print if-then-else
if_term = Term.init_if(Tuple[Term, Term, Term](true, false, true))
assert str(ppr_term(if_term)) == "if True then False else True"

# Create an empty environment
empty_entries = List[Entry]([])
counter = Int(0)
empty_type_decls = List[TypeDecl]([])
env_tuple = Tuple[List[Entry], Int, List[TypeDecl]](empty_entries, counter, empty_type_decls)
env = Env.init_env(env_tuple)

# Test type inference on a boolean literal
result = infer_type(env, true)
assert result.get_right().kind == Term.KIND_TY_BOOL

# Test type inference on undeclared variable
var_name = TName.init_fn(Tuple[String, Int](String("x"), Int(69)))
result = infer_type(env, Term.init_var(var_name))
assert "not found" in str(result.get_left())

# Check type of true is Bool
assert check_type(env, true, Term.ty_bool).kind == Maybe.KIND_NOTHING
assert "Expected Unit but found Bool" in str(check_type(env, true, Term.ty_unit).get_just())
