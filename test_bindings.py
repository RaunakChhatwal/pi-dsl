import ctypes
from ctypes import sizeof
import subprocess as sp
from bindings import *

def test_structure_sizes():
    """Test that structures have non-zero sizes"""
    print("=== Structure Size Tests ===")
    sizes = {
        'Term': sizeof(Term),
        'TermUnion': sizeof(TermUnion), 
        'Pattern': sizeof(Pattern),
        'PatternUnion': sizeof(PatternUnion),
        'Epsilon': sizeof(Epsilon),
        'Arg': sizeof(Arg),
        'Name[Term]': sizeof(Name[Term]),
        'Bind[Pattern, Term]': sizeof(Bind[Pattern, Term]),
        'SourcePos': sizeof(SourcePos),
    }
    
    for name, size in sizes.items():
        print(f"{name:20}: {size:2d} bytes")
        if size == 0:
            print(f"  ❌ ERROR: {name} has zero size!")
            return False
        else:
            print(f"  ✓ OK")
    
    return True

def test_structure_instantiation():
    """Test that structures can be instantiated"""
    print("\n=== Structure Instantiation Tests ===")
    
    try:
        # Basic structures
        epsilon = Epsilon()
        epsilon.kind = 42
        print(f"✓ Epsilon: kind={epsilon.kind}")
        
        # Recursive structures  
        term = Term()
        term.kind = 1
        print(f"✓ Term: kind={term.kind}")
        
        pattern = Pattern() 
        pattern.kind = 2
        print(f"✓ Pattern: kind={pattern.kind}")
        
        # Template structures
        name_term = Name[Term]()
        name_term.kind = 3
        print(f"✓ Name[Term]: kind={name_term.kind}")
        
        bind_pt = Bind[Pattern, Term]()
        bind_pt.kind = 4
        print(f"✓ Bind[Pattern, Term]: kind={bind_pt.kind}")
        
        return True
        
    except Exception as e:
        print(f"❌ ERROR: {e}")
        return False

def test_pointer_fields():
    """Test that pointer fields work correctly"""
    print("\n=== Pointer Field Tests ===")
    
    try:
        # Create a Term with union pointer
        term = Term()
        term.kind = 1
        
        # Create TermUnion on heap and point to it
        term_union = TermUnion()
        term.union = ctypes.cast(ctypes.pointer(term_union), ctypes.c_void_p)
        print(f"✓ Term.union points to TermUnion at {term.union}")
        
        # Test Pattern pointer field
        pattern = Pattern()
        pattern.kind = 2
        
        pattern_union = PatternUnion()
        pattern.union = ctypes.cast(ctypes.pointer(pattern_union), ctypes.c_void_p)
        print(f"✓ Pattern.union points to PatternUnion at {pattern.union}")
        
        return True
        
    except Exception as e:
        print(f"❌ ERROR: {e}")
        return False

def test_nested_structures():
    """Test nested structure access"""
    print("\n=== Nested Structure Tests ===")
    
    try:
        # Create Arg structure
        arg = Arg()
        arg.kind = 5
        print(f"✓ Arg: kind={arg.kind}")
        
        # Arg contains POINTER(Tuple[Epsilon, Term])
        epsilon = Epsilon()
        epsilon.kind = 10
        
        term = Term() 
        term.kind = 11
        
        # Create tuple of Epsilon and Term
        EpsilonTermTuple = Tuple[Epsilon, Term]
        tuple_val = EpsilonTermTuple()
        tuple_val.field0 = epsilon
        tuple_val.field1 = term
        
        # Point Arg to the tuple
        arg.Arg = ctypes.cast(ctypes.pointer(tuple_val), ctypes.c_void_p)
        print(f"✓ Arg.Arg points to Tuple[Epsilon, Term]")
        # Cast back to access contents
        tuple_ptr = ctypes.cast(arg.Arg, ctypes.POINTER(EpsilonTermTuple))
        print(f"  - field0.kind = {tuple_ptr.contents.field0.kind}")
        print(f"  - field1.kind = {tuple_ptr.contents.field1.kind}")
        
        return True
        
    except Exception as e:
        print(f"❌ ERROR: {e}")
        return False

def test_library_integration():
    """Test integration with Haskell library if available"""
    print("\n=== Library Integration Tests ===")
    
    try:
        # Try to find and load the shared library
        command = "fd pi-forall-shared.so dist-newstyle"
        result = sp.run(command.split(), capture_output=True, text=True)
        
        if result.returncode != 0:
            print("⚠️  Shared library not found, skipping integration tests")
            return True
            
        path = result.stdout.strip()
        lib = ctypes.CDLL(path)
        
        # Set up basic runtime functions
        lib.pi_forall_init.argtypes = []
        lib.pi_forall_init.restype = None
        
        lib.pi_forall_exit.argtypes = []
        lib.pi_forall_exit.restype = None
        
        print("✓ Shared library loaded successfully")
        print("✓ Initializing Haskell runtime...")
        lib.pi_forall_init()
        
        print("✓ Shutting down Haskell runtime...")
        lib.pi_forall_exit()
        
        return True
        
    except Exception as e:
        print(f"❌ ERROR: {e}")
        return False

def main():
    """Run all tests"""
    print("Testing Pi-Forall Python Bindings")
    print("=" * 50)
    
    tests = [
        test_structure_sizes,
        test_structure_instantiation,  
        test_pointer_fields,
        test_nested_structures,
        test_library_integration
    ]
    
    passed = 0
    total = len(tests)
    
    for test in tests:
        if test():
            passed += 1
        else:
            print("❌ Test failed!")
    
    print(f"\n{'=' * 50}")
    print(f"Results: {passed}/{total} tests passed")
    
    if passed == total:
        print("🎉 All tests passed!")
        return 0
    else:
        print("💥 Some tests failed!")
        return 1

if __name__ == "__main__":
    exit(main())