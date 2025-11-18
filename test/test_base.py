import ctypes
from ctypes import POINTER
import subprocess as sp
from pi_dsl import Bool, Int, List, String, Tuple

# Load the shared library
command = "fd pi-forall-lib.so dist-newstyle"
path = sp.run(command.split(), capture_output=True, check=True, text=True).stdout.strip()
lib = ctypes.CDLL(path)

# Test the library
print("Initializing Haskell runtime...")
lib.pi_forall_init.argtypes = []
lib.pi_forall_init.restype = None
lib.pi_forall_init()

print("Testing add_length function...")
lib.add_length.argtypes = [POINTER(Tuple[Int, String])]
lib.add_length.restype = Int
input_tuple = Tuple[Int, String](42, String("hello"))
result = lib.add_length(ctypes.byref(input_tuple))
print(f"add_length(42, 'hello') = {result}")

print("Testing sum_true function...")
BoolIntList = List[Tuple[Bool, Int]]
lib.sum_true.argtypes = [POINTER(BoolIntList)]
lib.sum_true.restype = Int
test_list = BoolIntList([Tuple[Bool, Int](*pair) for pair in [(True, 10), (False, 5), (True, 3)]])
result = lib.sum_true(ctypes.byref(test_list))
print(f"sum_true([(True, 10), (False, 5), (True, 3)]) = {result}")

print("Testing concat_int function...")
lib.concat_int.argtypes = [POINTER(Tuple[String, Int])]
lib.concat_int.restype = POINTER(String)
input_tuple = Tuple[String, Int](String("hello"), 123)
result_ptr = lib.concat_int(ctypes.byref(input_tuple))
print(f"concat_int('hello', 123) = {result_ptr.contents}")

print("Shutting down Haskell runtime...")
lib.pi_forall_exit.argtypes = []
lib.pi_forall_exit.restype = None
lib.pi_forall_exit()

print("Done!")
