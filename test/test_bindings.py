from pi_dsl import *

# Test ppr on boolean literals
true = Term.init_lit_bool(Bool(True))
false = Term.init_lit_bool(Bool(False))
assert str(ppr_term(true)) == "True"
assert str(ppr_term(false)) == "False"

# Pretty print if-then-else
if_term = Term.init_if(true, false, true)
assert str(ppr_term(if_term)) == "if True then False else True"

# Test type inference on a boolean literal
result = infer_type(empty_env, true)
assert result.get_right().kind == Term.KIND_TY_BOOL

# Test type inference on undeclared variable
var_name = TName.init_fn(String("x"), Int(69))
result = infer_type(empty_env, Term.init_var(var_name))
assert "not found" in str(result.get_left())

# Check type of true is Bool
assert check_type(empty_env, true, Term.ty_bool).kind == Maybe.KIND_NOTHING
assert "Expected Unit but found Bool" in str(check_type(empty_env, true, Term.ty_unit).get_just())
