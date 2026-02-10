from __future__ import annotations
import ctypes
from ctypes import c_int32, c_size_t, c_void_p, POINTER, Structure
from functools import cache
from pathlib import Path
from typing import Any, cast, Optional, TypeVar
import weakref

here = Path(__file__).resolve().parent
lib_name = "libpi-dsl-shared-lib.so"
bundled = here / "_native" / lib_name
if bundled.exists():
    lib = ctypes.CDLL(bundled)
else:
    artifacts_root = here.parent / "dist-newstyle"
    error = Exception(f"Couldn't find {lib_name}")
    if not artifacts_root.exists():
        raise error

    if match := next(artifacts_root.rglob(lib_name), None):
        lib = ctypes.CDLL(match)
    else:
        raise error

# Initialize Haskell runtime
lib.pi_dsl_init.argtypes = []
lib.pi_dsl_init.restype = None
lib.pi_dsl_init()

# Base class for ctypes structures that track parent-child ownership
class Managed:
    # Reference to parent object (keeps parent alive)
    parent: Optional[Any]
    # List of child objects (prevents premature garbage collection)
    children: list[Managed]

    # Initialize ownership tracking fields
    def __init__(self):
        self.parent = None
        self.children = []

    # Set parent reference for lifetime management
    def set_parent(self, parent: Any):
        self.parent = parent

    # Register a child object to prevent its garbage collection
    def append_child(self, child: Any):
        assert isinstance(child, Managed)
        self.children.append(child)

# Tagged union structure for representing Haskell sum types
class TaggedUnion(Structure, Managed):
    # Memory layout: tag discriminant and pointer to payload
    _fields_ = [
        ("kind", c_int32),
        ("union", c_void_p)
    ]

    # Create a tagged union with given tag and optional payload
    def __init__(self, kind: int, value: Optional[Any] = None):
        Managed.__init__(self)

        self.kind = ctypes.c_int32(kind)
        if value is None:
            self.union = c_void_p(None)
        else:
            self.append_child(value)
            self.union = ctypes.cast(ctypes.pointer(value), c_void_p)

    # Create parameterized tagged union types (e.g. TaggedUnion[Int])
    @classmethod
    @cache
    def __class_getitem__(cls, type_args: type | tuple[type, ...]) -> type:
        if not isinstance(type_args, tuple):
            assert isinstance(type_args, type)
            type_args = (type_args,)
        return type(cls.__name__, (cls,), { "type_ctor": cls, "type_args": type_args })

    # Resolve type variables to concrete types using type_args
    def concretize_type_hint(self, hint: Any) -> type:
        if hint is String:
            return hint     # HACK: otherwise would return subclass List[Char] for String

        if not hasattr(self, "type_args"):
            assert isinstance(hint, type)
            return hint

        if isinstance(hint, TypeVar):
            return self.type_args[self.type_ctor.__parameters__.index(hint)]

        if not hasattr(hint, "type_args"):
            return hint

        concrete_args = tuple(map(self.concretize_type_hint, hint.type_args))
        return hint.type_ctor.__class_getitem__(concrete_args)

    # Extract payload as concrete type T
    def get_field[T](self, field_type_hint: TypeVar | type[T]) -> T:
        field_type = self.concretize_type_hint(field_type_hint)
        field = ctypes.cast(self.union, cast(Any, POINTER(field_type))).contents
        field.set_parent(self)
        return field

# Generic list structure for representing Haskell lists
class List[T](Structure, Managed):
    # Memory layout: length and pointer to array data
    _fields_ = [
        ("length", c_size_t),
        ("data", c_void_p)
    ]

    # Create a list from items
    def __init__(self, *items: T):
        self.length = len(items)
        array_type = self.type_args[0] * len(items)
        self._array = array_type(*items)
        self.data = ctypes.cast(self._array, c_void_p)

        Managed.__init__(self)
        for item in items:
            self.append_child(item)

    # Create parameterized list types (e.g. List[Int])
    @classmethod
    @cache
    def __class_getitem__(cls, type_args: type[T]) -> type:
        return type("List", (cls,), { "type_ctor": cls, "type_args": (type_args,) })

    # Extract list contents as Python list
    def get(self) -> list[T]:
        array_type = self.type_args[0] * self.length
        items = list(array_type.from_address(self.data))
        for item in items:
            item.set_parent(self)
        return items

# Helper to create a typed list from items (infers element type)
def init_list[T](*items: T) -> List[T]:
    assert len(items) > 0, "Cannot infer type from empty list"
    return List.__class_getitem__(type(items[0]))(*items)

# Generic tuple structure for representing Haskell tuples
class Tuple[*Ts](Structure, Managed):
    # Create a tuple from items
    def __init__(self, *items: *Ts):
        Managed.__init__(self)
        for i, item in enumerate(items):
            self.append_child(item)
            setattr(self, f"field{i}", item)

    # Create parameterized tuple types (e.g. Tuple[Int, String])
    @classmethod
    @cache
    def __class_getitem__(cls, type_args: type | tuple[type | TypeVar, ...]) -> type:
        match type_args:
            case tuple():
                pass
            case _:
                type_args = (type_args,)
        memory_fields = [(f"field{i}", type_args[i]) for i in range(len(type_args))]
        type_fields: dict[str, Any] = { "type_ctor": cls, "type_args": type_args }
        if all([isinstance(type_arg, type) for type_arg in type_args]):
            type_fields["_fields_"] = memory_fields
        return type("Tuple", (cls,), type_fields)

    # Extract tuple contents as Python tuple
    def get(self) -> tuple[*Ts]:
        items = tuple(getattr(self, f"field{i}") for i in range(len(self._fields_)))
        for item in items:
            item.set_parent(self)
        return items

# Helper to create a typed tuple from items (infers element types)
def init_tuple[*Ts](*items: *Ts) -> Tuple[*Ts]:
    return Tuple.__class_getitem__(tuple(map(type, items)))(*items)

# Unicode character structure (32-bit code point)
class Char(Structure, Managed):
    # Memory layout: single 32-bit integer for code point
    _fields_ = [("code_point", ctypes.c_int32)]

    # Create a Char from integer code point
    def __init__(self, code_point: int):
        self.code_point = ctypes.c_int32(code_point)

    # Convert to Python int
    def __int__(self) -> int:
        return int(self.code_point)

# String as a list of Chars (Haskell-compatible representation)
class String(List[Char]):
    # Create a String from Python str
    def __init__(self, string: str):
        super().__init__(*[Char(ord(char)) for char in string])

    # Convert to Python str
    def __str__(self) -> str:
        return "".join([chr(int(code_point)) for code_point in super().get()])

    # String representation for debugging
    def __repr__(self) -> str:
        return self.__str__()

# 64-bit signed integer structure
class Int(Structure, Managed):
    # Memory layout: single 64-bit integer
    _fields_ = [("n", ctypes.c_int64)]

    # Create an Int from Python int
    def __init__(self, n: int):
        self.n = ctypes.c_int64(n)

    # Convert to Python int
    def __int__(self) -> int:
        return int(self.n)

# Register an exported Haskell function's type signature with ctypes
def set_export_signature(name: str, param_types: list[type], return_type: type):
    export_func = getattr(lib, name)
    export_func.argtypes = [POINTER(param_type) for param_type in param_types]
    export_func.restype = POINTER(return_type)

    free_func = getattr(lib, f"free_{name}_result")
    free_func.argtypes = [POINTER(return_type)]
    free_func.restype = None

# Call an exported Haskell function and manage result lifetime
def call_export(name: str, args: list[Any]) -> Any:
    export_func = getattr(lib, name)
    result_ptr = export_func(*[ctypes.byref(arg) for arg in args])
    result = result_ptr.contents

    result.set_parent(result_ptr)    # tie result_ptr's lifetime to result
    free_func = getattr(lib, f"free_{name}_result")
    weakref.finalize(result, free_func, result_ptr)

    return result
