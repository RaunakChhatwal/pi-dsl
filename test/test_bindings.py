import ctypes
from ctypes import c_bool, c_int32, POINTER
import subprocess as sp
from pi_dsl import String, Term, TermUnion, Tuple
# from base import String, Tuple
# from bindings import Term, TermUnion

# Load the shared library
command = "fd pi-forall-lib.so dist-newstyle"
path = sp.run(command.split(), capture_output=True, check=True, text=True).stdout.strip()
lib = ctypes.CDLL(path)

# Initialize Haskell runtime
print("Initializing Haskell runtime...")
lib.pi_forall_init.argtypes = []
lib.pi_forall_init.restype = None
lib.pi_forall_init()

# Test ppr_term function
print("Testing ppr_term function...")
lib.ppr_term.argtypes = [POINTER(Term)]
lib.ppr_term.restype = POINTER(String)

# Create a boolean literal term (True)
# For lit_bool, we need kind = 7 (based on position in TermUnion) and lit_bool = True
term_union = TermUnion()
term_union.lit_bool = c_bool(True)

term = Term()
term.kind = c_int32(13)  # LitBool is the 14th field (0-indexed = 13)
term.term_union = ctypes.cast(ctypes.pointer(term_union), ctypes.c_void_p)

result_ptr = lib.ppr_term(ctypes.byref(term))
result_string = result_ptr.contents
print(f"ppr_term(True) = {result_string}")

# Test with False as well
term_union_false = TermUnion()
term_union_false.lit_bool = c_bool(False)

term_false = Term()
term_false.kind = c_int32(13)  # LitBool
term_false.term_union = ctypes.cast(ctypes.pointer(term_union_false), ctypes.c_void_p)

result_ptr_false = lib.ppr_term(ctypes.byref(term_false))
result_string_false = result_ptr_false.contents
print(f"ppr_term(False) = {result_string_false}")

# Create a complex term: if True then False else True
print("Testing complex term (if-then-else)...")
# from base import Tuple

# Create the condition (True)
cond_union = TermUnion()
cond_union.lit_bool = c_bool(True)
condition = Term()
condition.kind = c_int32(13)  # LitBool
condition.term_union = ctypes.cast(ctypes.pointer(cond_union), ctypes.c_void_p)

# Create the then branch (False)  
then_union = TermUnion()
then_union.lit_bool = c_bool(False)
then_branch = Term()
then_branch.kind = c_int32(13)  # LitBool
then_branch.term_union = ctypes.cast(ctypes.pointer(then_union), ctypes.c_void_p)

# Create the else branch (True)
else_union = TermUnion()
else_union.lit_bool = c_bool(True)
else_branch = Term()
else_branch.kind = c_int32(13)  # LitBool  
else_branch.term_union = ctypes.cast(ctypes.pointer(else_union), ctypes.c_void_p)

# Create the if-then-else term
IfTuple = Tuple[Term, Term, Term]
if_tuple = IfTuple(condition, then_branch, else_branch)

if_union = TermUnion()
setattr(if_union, 'if', if_tuple)  # Use setattr since 'if' is a Python keyword

if_term = Term()
if_term.kind = c_int32(14)  # If constructor
if_term.term_union = ctypes.cast(ctypes.pointer(if_union), ctypes.c_void_p)

result_ptr = lib.ppr_term(ctypes.byref(if_term))
result_string = result_ptr.contents
print(f"ppr_term(if True then False else True) = {result_string}")

# Shutdown Haskell runtime
print("Shutting down Haskell runtime...")
lib.pi_forall_exit.argtypes = []
lib.pi_forall_exit.restype = None
lib.pi_forall_exit()

print("Done!")