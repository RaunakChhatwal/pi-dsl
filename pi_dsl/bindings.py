from __future__ import annotations
from typing import Literal
from .base import *

class Maybe[T1](TaggedUnion):
    kind: Literal[0, 1]
    
    KIND_NOTHING = 0
    KIND_JUST = 1
    
    @classmethod
    def init_just(cls, value: T1):
        return cls(cls.KIND_JUST, value)
    
    def get_just(self) -> T1:
        assert self.kind == self.KIND_JUST
        return self.get_field(T1)

class Either[T1, T2](TaggedUnion):
    kind: Literal[0, 1]
    
    KIND_LEFT = 0
    KIND_RIGHT = 1
    
    @classmethod
    def init_left(cls, value: T1):
        return cls(cls.KIND_LEFT, value)
    
    def get_left(self) -> T1:
        assert self.kind == self.KIND_LEFT
        return self.get_field(T1)
    
    @classmethod
    def init_right(cls, value: T2):
        return cls(cls.KIND_RIGHT, value)
    
    def get_right(self) -> T2:
        assert self.kind == self.KIND_RIGHT
        return self.get_field(T2)

class Trace(TaggedUnion):
    kind: Literal[0, 1, 2]
    
    KIND_INVOC = 0
    KIND_EVENT = 1
    KIND_RESULT = 2
    
    @classmethod
    def init_invoc(cls, *values: *tuple[String, List[String]]):
        return cls(cls.KIND_INVOC, init_tuple(*values))
    
    def get_invoc(self) -> tuple[String, List[String]]:
        assert self.kind == self.KIND_INVOC
        return self.get_field(Tuple[String, List[String]]).get()
    
    @classmethod
    def init_event(cls, value: String):
        return cls(cls.KIND_EVENT, value)
    
    def get_event(self) -> String:
        assert self.kind == self.KIND_EVENT
        return self.get_field(String)
    
    @classmethod
    def init_result(cls, value: String):
        return cls(cls.KIND_RESULT, value)
    
    def get_result(self) -> String:
        assert self.kind == self.KIND_RESULT
        return self.get_field(String)

class Map[T1, T2](TaggedUnion):
    kind: Literal[0, 1]
    
    KIND_BIN = 0
    KIND_TIP = 1
    
    @classmethod
    def init_bin(cls, *values: *tuple[Size, T1, T2, Map[T1, T2], Map[T1, T2]]):
        return cls(cls.KIND_BIN, init_tuple(*values))
    
    def get_bin(self) -> tuple[Size, T1, T2, Map[T1, T2], Map[T1, T2]]:
        assert self.kind == self.KIND_BIN
        return self.get_field(Tuple[Size, T1, T2, Map[T1, T2], Map[T1, T2]]).get()

class Level(TaggedUnion):
    kind: Literal[0, 1]
    
    KIND_ZERO = 0
    KIND_SUCC = 1
    
    @classmethod
    def init_succ(cls, value: Level):
        return cls(cls.KIND_SUCC, value)
    
    def get_succ(self) -> Level:
        assert self.kind == self.KIND_SUCC
        return self.get_field(Level)

class Name[T1](TaggedUnion):
    kind: Literal[0, 1]
    
    KIND_FN = 0
    KIND_BN = 1
    
    @classmethod
    def init_fn(cls, *values: *tuple[String, Int]):
        return cls(cls.KIND_FN, init_tuple(*values))
    
    def get_fn(self) -> tuple[String, Int]:
        assert self.kind == self.KIND_FN
        return self.get_field(Tuple[String, Int]).get()
    
    @classmethod
    def init_bn(cls, *values: *tuple[Int, Int]):
        return cls(cls.KIND_BN, init_tuple(*values))
    
    def get_bn(self) -> tuple[Int, Int]:
        assert self.kind == self.KIND_BN
        return self.get_field(Tuple[Int, Int]).get()

class Var(TaggedUnion):
    kind: Literal[0, 1]
    
    KIND_LOCAL = 0
    KIND_GLOBAL = 1
    
    @classmethod
    def init_local(cls, value: TermName):
        return cls(cls.KIND_LOCAL, value)
    
    def get_local(self) -> TermName:
        assert self.kind == self.KIND_LOCAL
        return self.get_field(TermName)
    
    @classmethod
    def init_global(cls, value: String):
        return cls(cls.KIND_GLOBAL, value)
    
    def get_global(self) -> String:
        assert self.kind == self.KIND_GLOBAL
        return self.get_field(String)

class Bind[T1, T2](TaggedUnion):
    kind: Literal[0]
    
    KIND_B = 0
    
    @classmethod
    def init_b(cls, *values: *tuple[T1, T2]):
        return cls(cls.KIND_B, init_tuple(*values))
    
    def get_b(self) -> tuple[T1, T2]:
        assert self.kind == self.KIND_B
        return self.get_field(Tuple[T1, T2]).get()

class Term(TaggedUnion):
    kind: Literal[0, 1, 2, 3, 4, 5, 6, 7, 8]
    
    KIND_SORT = 0
    KIND_VAR = 1
    KIND_LAM = 2
    KIND_APP = 3
    KIND_PI = 4
    KIND_ANN = 5
    KIND_DATA_TYPE = 6
    KIND_CTOR = 7
    KIND_REC = 8
    
    @classmethod
    def init_sort(cls, value: Level):
        return cls(cls.KIND_SORT, value)
    
    def get_sort(self) -> Level:
        assert self.kind == self.KIND_SORT
        return self.get_field(Level)
    
    @classmethod
    def init_var(cls, value: Var):
        return cls(cls.KIND_VAR, value)
    
    def get_var(self) -> Var:
        assert self.kind == self.KIND_VAR
        return self.get_field(Var)
    
    @classmethod
    def init_lam(cls, value: Bind[TermName, Term]):
        return cls(cls.KIND_LAM, value)
    
    def get_lam(self) -> Bind[TermName, Term]:
        assert self.kind == self.KIND_LAM
        return self.get_field(Bind[TermName, Term])
    
    @classmethod
    def init_app(cls, *values: *tuple[Term, Term]):
        return cls(cls.KIND_APP, init_tuple(*values))
    
    def get_app(self) -> tuple[Term, Term]:
        assert self.kind == self.KIND_APP
        return self.get_field(Tuple[Term, Term]).get()
    
    @classmethod
    def init_pi(cls, *values: *tuple[Type, Bind[TermName, Type]]):
        return cls(cls.KIND_PI, init_tuple(*values))
    
    def get_pi(self) -> tuple[Type, Bind[TermName, Type]]:
        assert self.kind == self.KIND_PI
        return self.get_field(Tuple[Type, Bind[TermName, Type]]).get()
    
    @classmethod
    def init_ann(cls, *values: *tuple[Term, Type]):
        return cls(cls.KIND_ANN, init_tuple(*values))
    
    def get_ann(self) -> tuple[Term, Type]:
        assert self.kind == self.KIND_ANN
        return self.get_field(Tuple[Term, Type]).get()
    
    @classmethod
    def init_data_type(cls, value: DataTypeName):
        return cls(cls.KIND_DATA_TYPE, value)
    
    def get_data_type(self) -> DataTypeName:
        assert self.kind == self.KIND_DATA_TYPE
        return self.get_field(DataTypeName)
    
    @classmethod
    def init_ctor(cls, *values: *tuple[DataTypeName, CtorName]):
        return cls(cls.KIND_CTOR, init_tuple(*values))
    
    def get_ctor(self) -> tuple[DataTypeName, CtorName]:
        assert self.kind == self.KIND_CTOR
        return self.get_field(Tuple[DataTypeName, CtorName]).get()
    
    @classmethod
    def init_rec(cls, value: DataTypeName):
        return cls(cls.KIND_REC, value)
    
    def get_rec(self) -> DataTypeName:
        assert self.kind == self.KIND_REC
        return self.get_field(DataTypeName)

class Env(TaggedUnion):
    kind: Literal[0]
    
    KIND_ENV = 0
    
    @classmethod
    def init_env(cls, *values: *tuple[Map[DataTypeName, Tuple[Type, List[Tuple[CtorName, Type]]]], Map[String, Tuple[Type, Term]], Map[TermName, Type]]):
        return cls(cls.KIND_ENV, init_tuple(*values))
    
    def get_env(self) -> tuple[Map[DataTypeName, Tuple[Type, List[Tuple[CtorName, Type]]]], Map[String, Tuple[Type, Term]], Map[TermName, Type]]:
        assert self.kind == self.KIND_ENV
        return self.get_field(Tuple[Map[DataTypeName, Tuple[Type, List[Tuple[CtorName, Type]]]], Map[String, Tuple[Type, Term]], Map[TermName, Type]]).get()

class Entry(TaggedUnion):
    kind: Literal[0, 1]
    
    KIND_DECL = 0
    KIND_DATA = 1
    
    @classmethod
    def init_decl(cls, *values: *tuple[String, Type, Term]):
        return cls(cls.KIND_DECL, init_tuple(*values))
    
    def get_decl(self) -> tuple[String, Type, Term]:
        assert self.kind == self.KIND_DECL
        return self.get_field(Tuple[String, Type, Term]).get()
    
    @classmethod
    def init_data(cls, *values: *tuple[DataTypeName, Type, List[Tuple[CtorName, Type]]]):
        return cls(cls.KIND_DATA, init_tuple(*values))
    
    def get_data(self) -> tuple[DataTypeName, Type, List[Tuple[CtorName, Type]]]:
        assert self.kind == self.KIND_DATA
        return self.get_field(Tuple[DataTypeName, Type, List[Tuple[CtorName, Type]]]).get()

Size = Int

DataTypeName = String

TermName = Name[Term]

CtorName = String

Type = Term

set_export_signature("bind", [TermName, Term], Bind[TermName, Term])
def bind(var: TermName, body: Term) -> Bind[TermName, Term]:
    return call_export("bind", [var, body])

set_export_signature("unbind", [Bind[TermName, Term]], Tuple[TermName, Term])
def unbind(binding: Bind[TermName, Term]) -> Tuple[TermName, Term]:
    return call_export("unbind", [binding])

set_export_signature("ppr_term", [Term], String)
def ppr_term(term: Term) -> String:
    return call_export("ppr_term", [term])

set_export_signature("type_check", [List[Entry]], Maybe[String])
def type_check(entries: List[Entry]) -> Maybe[String]:
    return call_export("type_check", [entries])

set_export_signature("trace_type_check", [List[Entry]], Tuple[Maybe[String], List[Trace]])
def trace_type_check(entries: List[Entry]) -> Tuple[Maybe[String], List[Trace]]:
    return call_export("trace_type_check", [entries])

set_export_signature("infer_type", [List[Entry], Term], Either[Type, String])
def infer_type(entries: List[Entry], term: Term) -> Either[Type, String]:
    return call_export("infer_type", [entries, term])

set_export_signature("check_type", [List[Entry], Term, Type], Maybe[String])
def check_type(entries: List[Entry], term: Term, type: Type) -> Maybe[String]:
    return call_export("check_type", [entries, term, type])
