from dataclasses import dataclass
from .base import *
from . import bindings
from .bindings import Maybe
from .term import DataType, Term, Type, Var

@dataclass
class TypeDecl:
    name: Var
    signature: Type

    def entry_binding(self) -> bindings.Entry:
        return bindings.Entry.init_decl(
            bindings.TypeDecl.init_type_decl(self.name.name_binding(), self.signature.binding()))

@dataclass
class Def:
    name: Var
    body: Term

    def entry_binding(self) -> bindings.Entry:
        return bindings.Entry.init_def(self.name.name_binding(), self.body.binding())

type Entry = TypeDecl | Def | DataType

def type_check(entries: list[Entry]):
    entry_bindings = [entry.entry_binding() for entry in entries]
    error = bindings.type_check(List[bindings.Entry](*entry_bindings))
    if error.kind == Maybe.KIND_JUST:
        raise Exception(error.get_just())
