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

class Managed:
    parent: Optional[Any]
    children: list[Managed]

    def __init__(self):
        self.parent = None
        self.children = []

    def set_parent(self, parent: Any):
        self.parent = parent

    def append_child(self, child: Any):
        assert isinstance(child, Managed)
        self.children.append(child)

class TaggedUnion(Structure, Managed):
    _fields_ = [
        ("kind", c_int32),
        ("union", c_void_p)
    ]

    def __init__(self, kind: int, value: Optional[Any] = None):
        Managed.__init__(self)

        self.kind = ctypes.c_int32(kind)
        if value is None:
            self.union = c_void_p(None)
        else:
            self.append_child(value)
            self.union = ctypes.cast(ctypes.pointer(value), c_void_p)

    @classmethod
    @cache
    def __class_getitem__(cls, type_args: type | tuple[type, ...]) -> type:
        if not isinstance(type_args, tuple):
            assert isinstance(type_args, type)
            type_args = (type_args,)
        return type(cls.__name__, (cls,), { "type_ctor": cls, "type_args": type_args })

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

    def get_field[T](self, field_type_hint: TypeVar | type[T]) -> T:
        field_type = self.concretize_type_hint(field_type_hint)
        field = ctypes.cast(self.union, cast(Any, POINTER(field_type))).contents
        field.set_parent(self)
        return field

class List[T](Structure, Managed):
    _fields_ = [
        ("length", c_size_t),
        ("data", c_void_p)
    ]

    def __init__(self, *items: T):
        self.length = len(items)
        array_type = self.type_args[0] * len(items)
        self.data = ctypes.cast(array_type(*items), c_void_p)

        Managed.__init__(self)
        for item in items:
            self.append_child(item)

    @classmethod
    @cache
    def __class_getitem__(cls, type_args: type[T]) -> type:
        return type("List", (cls,), { "type_ctor": cls, "type_args": (type_args,) })

    def get(self) -> list[T]:
        array_type = self.type_args[0] * self.length
        items = list(array_type.from_address(self.data))
        for item in items:
            item.set_parent(self)
        return items

def init_list[T](*items: T) -> List[T]:
    assert len(items) > 0, "Cannot infer type from empty list"
    return List.__class_getitem__(type(items[0]))(*items)

class Tuple[*Ts](Structure, Managed):
    def __init__(self, *items: *Ts):
        Managed.__init__(self)
        for i, item in enumerate(items):
            self.append_child(item)
            setattr(self, f"field{i}", item)

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

    def get(self) -> tuple[*Ts]:
        items = tuple(getattr(self, f"field{i}") for i in range(len(self._fields_)))
        for item in items:
            item.set_parent(self)
        return items

def init_tuple[*Ts](*items: *Ts) -> Tuple[*Ts]:
    return Tuple.__class_getitem__(tuple(map(type, items)))(*items)

class Char(Structure, Managed):
    _fields_ = [("code_point", ctypes.c_int32)]

    def __init__(self, code_point: int):
        self.code_point = ctypes.c_int32(code_point)

    def __int__(self) -> int:
        return int(self.code_point)

class String(List[Char]):
    def __init__(self, string: str):
        super().__init__(*[Char(ord(char)) for char in string])

    def __str__(self) -> str:
        return "".join([chr(int(code_point)) for code_point in super().get()])

    def __repr__(self) -> str:
        return self.__str__()

class Int(Structure, Managed):
    _fields_ = [("n", ctypes.c_int64)]

    def __init__(self, n: int):
        self.n = ctypes.c_int64(n)

    def __int__(self) -> int:
        return int(self.n)

def set_export_signature(name: str, param_types: list[type], return_type: type):
    export_func = getattr(lib, name)
    export_func.argtypes = [POINTER(param_type) for param_type in param_types]
    export_func.restype = POINTER(return_type)

    free_func = getattr(lib, f"free_{name}_result")
    free_func.argtypes = [POINTER(return_type)]
    free_func.restype = None

def call_export(name: str, args: list[Any]) -> Any:
    export_func = getattr(lib, name)
    result_ptr = export_func(*[ctypes.byref(arg) for arg in args])
    result = result_ptr.contents

    result.set_parent(result_ptr)    # tie result_ptr's lifetime to result
    free_func = getattr(lib, f"free_{name}_result")
    weakref.finalize(result, free_func, result_ptr)

    return result
