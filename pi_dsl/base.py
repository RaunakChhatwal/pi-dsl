import ctypes
from ctypes import c_size_t, c_void_p, POINTER, Structure
from functools import cache
import subprocess as sp
from typing import Any

# Load the shared library
command = "fd pi-forall-lib.so dist-newstyle"
path = sp.run(command.split(), capture_output=True, check=True, text=True).stdout.strip()
lib = ctypes.CDLL(path)

class List[T](Structure):
    _fields_ = [
        ("length", c_size_t),
        ("data", c_void_p)
    ]

    def __init__(self, items: list[T]):
        self.length = len(items)
        array_type = self.item_type * len(items)
        self.data = ctypes.cast(array_type(*items), c_void_p)

    @classmethod
    @cache
    def __class_getitem__(cls, item_type: type[T]) -> type:
        return type("List", (cls,), { "item_type": item_type })

    def get(self) -> list[T]:
        array_type = self.item_type * self.length
        return list(array_type.from_address(self.data))

class Tuple[*Ts](Structure):
    def __init__(self, *items: *Ts):
        for i, item in enumerate(items):
            setattr(self, f"field{i}", item)

    @classmethod
    @cache
    def __class_getitem__(cls, item: tuple[type, ...]) -> type:
        fields = [(f"field{i}", item[i]) for i in range(len(item))]
        return type("Tuple", (cls,), { "_fields_": fields })

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

def init_tagged_union[T: Structure](cls: type[T], kind: int, value: Any) -> T:
    instance = cls()
    instance.kind = ctypes.c_int32(kind)
    instance.union = ctypes.cast(ctypes.pointer(value), c_void_p)
    return instance

def set_export_signature(name: str, param_types: list[type], return_type: type):
    export = getattr(lib, name)
    export.argtypes = [POINTER(param_type) for param_type in param_types]
    export.restype = POINTER(return_type)

def call_export(name: str, args: list[Any]) -> Any:
    export = getattr(lib, name)
    return export(*[ctypes.byref(arg) for arg in args]).contents