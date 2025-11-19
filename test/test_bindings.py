import ctypes
from ctypes import c_bool, POINTER
import subprocess as sp
from pi_dsl import String, Term, Tuple

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

# Shutdown Haskell runtime
print("Shutting down Haskell runtime...")
lib.pi_forall_exit.argtypes = []
lib.pi_forall_exit.restype = None
lib.pi_forall_exit()

print("Done!")
