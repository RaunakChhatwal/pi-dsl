import ctypes
from ctypes import c_size_t, c_void_p, Structure
from functools import cache
from typing import Any

class List[T](Structure):
    def __init__(self, items: list[T]):
        self.length = len(items)
        array_type = self.item_type * len(items)
        self.data = ctypes.cast(array_type(*items), c_void_p)

    @classmethod
    @cache
    def __class_getitem__(cls, item_type: type[T]) -> type:
        fields: list[tuple[str, type]] = [
            ("length", c_size_t),
            ("data", c_void_p)
        ]

        return type("List", (cls,), { "item_type": item_type, "_fields_": fields })

    def get(self) -> list[T]:
        array_type = self.item_type * self.length
        return list(array_type.from_address(self.data))

class Tuple(Structure):
    _fields_ = []

    def __init__(self, *items: Any):
        for i, item in enumerate(items):
            setattr(self, f"field{i}", item)

    @classmethod
    @cache
    def __class_getitem__(cls, item: tuple[type, ...]) -> type:
        fields = [(f"field{i}", item[i]) for i in range(len(item))]
        return type("Tuple", (cls,), { "_fields_": fields })

    def get(self) -> tuple[Any, ...]:
        return tuple(getattr(self, f"field{i}") for i in range(len(self._fields_)))

Char = ctypes.c_int32

class String(List[Char]):
    def __init__(self, string: str):
        return super().__init__([Char(ord(char)) for char in string])

    def __repr__(self) -> str:
        return "".join([chr(int(code_point)) for code_point in super().get()])

Bool = ctypes.c_bool
Int = ctypes.c_int64