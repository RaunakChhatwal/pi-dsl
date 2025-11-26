import ctypes
from ctypes import c_bool, POINTER
from pi_dsl import *

# Initialize Haskell runtime
print("Initializing Haskell runtime...")
lib.pi_forall_init.argtypes = []
lib.pi_forall_init.restype = None
lib.pi_forall_init()

# Test ppr_term function
print("Testing ppr_term function...")
lib.ppr_term.argtypes = [POINTER(Term)]
lib.ppr_term.restype = POINTER(String)

# Create a boolean literal term (True) using init method
print("Creating boolean literal (True) with init method...")
term_true = Term.init_lit_bool(c_bool(True))
result_ptr = lib.ppr_term(ctypes.byref(term_true))
print(f"ppr_term(True) = {result_ptr.contents}")

# Test with False as well
print("Creating boolean literal (False) with init method...")
term_false = Term.init_lit_bool(c_bool(False))
result_ptr_false = lib.ppr_term(ctypes.byref(term_false))
print(f"ppr_term(False) = {result_ptr_false.contents}")

# Create a complex term: if True then False else True
print("Testing complex term (if-then-else) with init methods...")
condition = Term.init_lit_bool(c_bool(True))
then_branch = Term.init_lit_bool(c_bool(False))
else_branch = Term.init_lit_bool(c_bool(True))
if_tuple = Tuple[Term, Term, Term](condition, then_branch, else_branch)
if_term = Term.init_if(if_tuple)
result_ptr = lib.ppr_term(ctypes.byref(if_term))
print(f"ppr_term(if True then False else True) = {result_ptr.contents}")

# Create an empty environment
empty_entries = List[Entry]([])
counter = Int(0)
empty_type_decls = List[TypeDecl]([])
env_tuple = Tuple[List[Entry], Int, List[TypeDecl]](empty_entries, counter, empty_type_decls)
env = Env.init_env(env_tuple)

# Test type inference on a boolean literal
print("Testing infer_type on boolean literal (True)...")
result = infer_type(env, Term.init_lit_bool(Bool(True)))
print(f"infer_type(True) = Either object with kind={result.kind}")
match result.kind:
    case Either.KIND_LEFT:
        print("Type inference failed!")
        # Extract the error string from the union
        error_ptr = ctypes.cast(result.union, POINTER(String))
        print(f"Error: {error_ptr.contents}")
    case Either.KIND_RIGHT:
        print("Type inference succeeded!")
        # Extract the Type from the union and pretty print it
        type_ptr = ctypes.cast(result.union, POINTER(Term))
        type_result = lib.ppr_term(type_ptr)
        print(f"Inferred type: {type_result.contents}")

# Shutdown Haskell runtime
print("Shutting down Haskell runtime...")
lib.pi_forall_exit.argtypes = []
lib.pi_forall_exit.restype = None
lib.pi_forall_exit()

print("Done!")
