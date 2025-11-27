import ctypes
from ctypes import POINTER
from pi_dsl import *

# Test ppr_term function
print("Testing ppr_term function...")
lib.ppr_term.argtypes = [POINTER(Term)]
lib.ppr_term.restype = POINTER(String)

# Test ppr on boolean literals
print("Creating boolean literals with init method...")
true = Term.init_lit_bool(Bool(True))
false = Term.init_lit_bool(Bool(False))
result = lib.ppr_term(ctypes.byref(true)).contents
print(f"ppr_term(True) = {result}")
result = lib.ppr_term(ctypes.byref(false)).contents
print(f"ppr_term(False) = {result}")

# Pretty print if-then-else
print("Testing if-then-else with init methods...")
if_term = Term.init_if(Tuple[Term, Term, Term](true, false, true))
result = lib.ppr_term(ctypes.byref(if_term)).contents
print(f"ppr_term(if True then False else True) = {result}")

# Create an empty environment
empty_entries = List[Entry]([])
counter = Int(0)
empty_type_decls = List[TypeDecl]([])
env_tuple = Tuple[List[Entry], Int, List[TypeDecl]](empty_entries, counter, empty_type_decls)
env = Env.init_env(env_tuple)

# Test type inference on a boolean literal
print("Testing infer_type on `True`...")
result = infer_type(env, true)
print(f"infer_type(True) = Either object with kind={result.kind}")

# Test the new getter methods
match result.kind:
    case Either.KIND_LEFT:
        print(f"Error: {result.get_left()}")
    case Either.KIND_RIGHT:
        inferred_type = result.get_right()
        type_result = lib.ppr_term(ctypes.byref(inferred_type)).contents
        print(f"Inferred type: {type_result}")

# Test another getter method on Term
print("Testing getter method on Term...")
assert true.get_lit_bool()
assert not false.get_lit_bool()

# Test Bind getter method
print("Testing Bind getter method...")
name = TName.init_fn(Tuple[String, Int](String("bool"), Int(42)))
bind_instance = Bind[TName, Term].init_b(Tuple[TName, Term](name, true))
bind_instance.get_b()   # TODO: assert equality with bind_value
