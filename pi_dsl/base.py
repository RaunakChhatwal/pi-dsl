from __future__ import annotations
import ctypes
from ctypes import c_size_t, c_void_p, POINTER, Structure
from functools import cache
import subprocess as sp
from typing import Any, cast, TypeVar

# Load the shared library
command = "fd pi-forall-lib.so dist-newstyle"
path = sp.run(command.split(), capture_output=True, check=True, text=True).stdout.strip()
lib = ctypes.CDLL(path)

class TaggedUnion(Structure):
    def __init__(self, kind: int, value: Any):
        self.kind = ctypes.c_int32(kind)
        self.union = ctypes.cast(ctypes.pointer(value), c_void_p)

    @classmethod
    @cache
    def __class_getitem__(cls, type_args: type | tuple[type, ...]) -> type:
        if not isinstance(type_args, tuple):
            assert isinstance(type_args, type)
            type_args = (type_args,)
        return type(cls.__name__, (cls,), { "type_ctor": cls, "type_args": type_args })

    def concretize_type_hint(self, hint: Any) -> Any:
        if not hasattr(self, "type_args"):
            assert isinstance(hint, type)
            return hint

        if isinstance(hint, TypeVar):
            return self.type_args[self.type_ctor.__parameters__.index(hint)]

        if not hasattr(hint, "type_args"):
            return hint

        concrete_args = tuple(map(self.concretize_type_hint, hint.type_args))
        return hint.type_ctor.__class_getitem__(concrete_args)

    def get_field(self, field_type: Any) -> Any:
        concrete_field_type = self.concretize_type_hint(field_type)
        ptr_type = cast(Any, POINTER(concrete_field_type))
        return ctypes.cast(self.union, ptr_type).contents

class List[T](TaggedUnion):
    _fields_ = [
        ("length", c_size_t),
        ("data", c_void_p)
    ]

    def __init__(self, items: list[T]):
        self.length = len(items)
        array_type = self.type_args[0] * len(items)
        self.data = ctypes.cast(array_type(*items), c_void_p)

    def get(self) -> list[T]:
        array_type = self.type_args[0] * self.length
        return list(array_type.from_address(self.data))

class Tuple[*Ts](Structure):
    def __init__(self, *items: *Ts):
        for i, item in enumerate(items):
            setattr(self, f"field{i}", item)

    @classmethod
    @cache
    def __class_getitem__(cls, type_args: tuple[type, ...]) -> type:
        fields = [(f"field{i}", type_args[i]) for i in range(len(type_args))]
        return type("Tuple", (cls,), {
            "type_ctor": cls,
            "type_args": type_args,
            "_fields_": fields
        })

    def get(self) -> tuple[*Ts]:
        return tuple(getattr(self, f"field{i}") for i in range(len(self._fields_)))

Char = ctypes.c_int32

class String(List[Char]):
    def __init__(self, string: str):
        return super().__init__([Char(ord(char)) for char in string])

    def __repr__(self) -> str:
        return "".join([chr(int(code_point)) for code_point in super().get()])

Bool = ctypes.c_bool
Int = ctypes.c_int64

def set_export_signature(name: str, param_types: list[type], return_type: type):
    export = getattr(lib, name)
    export.argtypes = [POINTER(param_type) for param_type in param_types]
    export.restype = POINTER(return_type)

def call_export(name: str, args: list[Any]) -> Any:
    export = getattr(lib, name)
    return export(*[ctypes.byref(arg) for arg in args]).contents