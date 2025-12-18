from __future__ import annotations
from typing import ClassVar, Literal, Self
from .base import *

class Maybe[T1](TaggedUnion):
    kind: Literal[0, 1]
    
    KIND_NOTHING = 0
    KIND_JUST = 1
    
    nothing: ClassVar[Self]
    
    @classmethod
    def init_just(cls, value: T1):
        return cls(cls.KIND_JUST, value)
    
    def get_just(self) -> T1:
        assert self.kind == self.KIND_JUST
        return self.get_field(T1)

Maybe.nothing = Maybe(Maybe.KIND_NOTHING)

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
    
    KIND_TY_TYPE = 0
    KIND_VAR = 1
    KIND_LAM = 2
    KIND_APP = 3
    KIND_TY_PI = 4
    KIND_ANN = 5
    KIND_TRUST_ME = 6
    KIND_TY_CON = 7
    KIND_DATA_CON = 8
    
    ty_type: ClassVar[Self]
    trust_me: ClassVar[Self]
    
    @classmethod
    def init_var(cls, value: TName):
        return cls(cls.KIND_VAR, value)
    
    def get_var(self) -> TName:
        assert self.kind == self.KIND_VAR
        return self.get_field(TName)
    
    @classmethod
    def init_lam(cls, value: Bind[TName, Term]):
        return cls(cls.KIND_LAM, value)
    
    def get_lam(self) -> Bind[TName, Term]:
        assert self.kind == self.KIND_LAM
        return self.get_field(Bind[TName, Term])
    
    @classmethod
    def init_app(cls, *values: *tuple[Term, Term]):
        return cls(cls.KIND_APP, init_tuple(*values))
    
    def get_app(self) -> tuple[Term, Term]:
        assert self.kind == self.KIND_APP
        return self.get_field(Tuple[Term, Term]).get()
    
    @classmethod
    def init_ty_pi(cls, *values: *tuple[Type, Bind[TName, Type]]):
        return cls(cls.KIND_TY_PI, init_tuple(*values))
    
    def get_ty_pi(self) -> tuple[Type, Bind[TName, Type]]:
        assert self.kind == self.KIND_TY_PI
        return self.get_field(Tuple[Type, Bind[TName, Type]]).get()
    
    @classmethod
    def init_ann(cls, *values: *tuple[Term, Type]):
        return cls(cls.KIND_ANN, init_tuple(*values))
    
    def get_ann(self) -> tuple[Term, Type]:
        assert self.kind == self.KIND_ANN
        return self.get_field(Tuple[Term, Type]).get()
    
    @classmethod
    def init_ty_con(cls, value: TyConName):
        return cls(cls.KIND_TY_CON, value)
    
    def get_ty_con(self) -> TyConName:
        assert self.kind == self.KIND_TY_CON
        return self.get_field(TyConName)
    
    @classmethod
    def init_data_con(cls, value: DataConName):
        return cls(cls.KIND_DATA_CON, value)
    
    def get_data_con(self) -> DataConName:
        assert self.kind == self.KIND_DATA_CON
        return self.get_field(DataConName)

Term.ty_type = Term(Term.KIND_TY_TYPE)
Term.trust_me = Term(Term.KIND_TRUST_ME)

class TypeDecl(TaggedUnion):
    kind: Literal[0]
    
    KIND_TYPE_DECL = 0
    
    @classmethod
    def init_type_decl(cls, *values: *tuple[TName, Type]):
        return cls(cls.KIND_TYPE_DECL, init_tuple(*values))
    
    def get_type_decl(self) -> tuple[TName, Type]:
        assert self.kind == self.KIND_TYPE_DECL
        return self.get_field(Tuple[TName, Type]).get()

class CtorDef(TaggedUnion):
    kind: Literal[0]
    
    KIND_CTOR_DEF = 0
    
    @classmethod
    def init_ctor_def(cls, *values: *tuple[DataConName, Telescope, Type]):
        return cls(cls.KIND_CTOR_DEF, init_tuple(*values))
    
    def get_ctor_def(self) -> tuple[DataConName, Telescope, Type]:
        assert self.kind == self.KIND_CTOR_DEF
        return self.get_field(Tuple[DataConName, Telescope, Type]).get()

class Entry(TaggedUnion):
    kind: Literal[0, 1, 2]
    
    KIND_DECL = 0
    KIND_DEF = 1
    KIND_DATA = 2
    
    @classmethod
    def init_decl(cls, value: TypeDecl):
        return cls(cls.KIND_DECL, value)
    
    def get_decl(self) -> TypeDecl:
        assert self.kind == self.KIND_DECL
        return self.get_field(TypeDecl)
    
    @classmethod
    def init_def(cls, *values: *tuple[TName, Term]):
        return cls(cls.KIND_DEF, init_tuple(*values))
    
    def get_def(self) -> tuple[TName, Term]:
        assert self.kind == self.KIND_DEF
        return self.get_field(Tuple[TName, Term]).get()
    
    @classmethod
    def init_data(cls, *values: *tuple[TyConName, Telescope, List[CtorDef]]):
        return cls(cls.KIND_DATA, init_tuple(*values))
    
    def get_data(self) -> tuple[TyConName, Telescope, List[CtorDef]]:
        assert self.kind == self.KIND_DATA
        return self.get_field(Tuple[TyConName, Telescope, List[CtorDef]]).get()

class Env(TaggedUnion):
    kind: Literal[0]
    
    KIND_ENV = 0
    
    @classmethod
    def init_env(cls, *values: *tuple[List[Entry], Int, List[TypeDecl]]):
        return cls(cls.KIND_ENV, init_tuple(*values))
    
    def get_env(self) -> tuple[List[Entry], Int, List[TypeDecl]]:
        assert self.kind == self.KIND_ENV
        return self.get_field(Tuple[List[Entry], Int, List[TypeDecl]]).get()

Type = Term

TyConName = String

DataConName = String

TName = Name[Term]

Telescope = List[TypeDecl]

set_export_signature("bind", [TName, Term], Bind[TName, Term])
def bind(var: TName, body: Term) -> Bind[TName, Term]:
    return call_export("bind", [var, body])

set_export_signature("ppr_term", [Term], String)
def ppr_term(term: Term) -> String:
    return call_export("ppr_term", [term])

set_export_signature("type_check", [List[Entry]], Maybe[String])
def type_check(entries: List[Entry]) -> Maybe[String]:
    return call_export("type_check", [entries])

set_export_signature("trace_type_check", [List[Entry]], Tuple[Maybe[String], List[Trace]])
def trace_type_check(entries: List[Entry]) -> Tuple[Maybe[String], List[Trace]]:
    return call_export("trace_type_check", [entries])
