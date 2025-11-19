from __future__ import annotations
from ctypes import c_int32, c_void_p, Structure
from functools import cache
from .base import Bool, init_tagged_union, Int, List, String, Tuple

class Name[T1](Structure):
    KIND_FN = 0
    KIND_BN = 1
    
    _fields_ = [
        ("kind", c_int32),
        ("union", c_void_p)
    ]
    
    @classmethod
    @cache
    def __class_getitem__(cls, type_args: tuple[type[T1]]) -> type:
        return type("Name", (cls,), { "type_args": type_args })
    
    @classmethod
    def init_fn(cls, value: Tuple[String, Int]):
        return init_tagged_union(cls, cls.KIND_FN, value)
    
    @classmethod
    def init_bn(cls, value: Tuple[Int, Int]):
        return init_tagged_union(cls, cls.KIND_BN, value)

class Epsilon(Structure):
    KIND_REL = 0
    KIND_IRR = 1
    
    _fields_ = [
        ("kind", c_int32),
        ("union", c_void_p)
    ]

class Bind[T1, T2](Structure):
    KIND_B = 0
    
    _fields_ = [
        ("kind", c_int32),
        ("union", c_void_p)
    ]
    
    @classmethod
    @cache
    def __class_getitem__(cls, type_args: tuple[type[T1], type[T2]]) -> type:
        return type("Bind", (cls,), { "type_args": type_args })
    
    @classmethod
    def init_b(cls, value: Tuple[T1, T2]):
        return init_tagged_union(cls, cls.KIND_B, value)

class Arg(Structure):
    KIND_ARG = 0
    
    _fields_ = [
        ("kind", c_int32),
        ("union", c_void_p)
    ]
    
    @classmethod
    def init_arg(cls, value: Tuple[Epsilon, Term]):
        return init_tagged_union(cls, cls.KIND_ARG, value)

class SourcePos(Structure):
    KIND_SOURCE_POS = 0
    
    _fields_ = [
        ("kind", c_int32),
        ("union", c_void_p)
    ]
    
    @classmethod
    def init_source_pos(cls, value: Tuple[SourceName, Line, Column]):
        return init_tagged_union(cls, cls.KIND_SOURCE_POS, value)

class Pattern(Structure):
    KIND_PAT_CON = 0
    KIND_PAT_VAR = 1
    
    _fields_ = [
        ("kind", c_int32),
        ("union", c_void_p)
    ]
    
    @classmethod
    def init_pat_con(cls, value: Tuple[DataConName, List[Tuple[Pattern, Epsilon]]]):
        return init_tagged_union(cls, cls.KIND_PAT_CON, value)
    
    @classmethod
    def init_pat_var(cls, value: TName):
        return init_tagged_union(cls, cls.KIND_PAT_VAR, value)

class Term(Structure):
    KIND_TY_TYPE = 0
    KIND_VAR = 1
    KIND_LAM = 2
    KIND_APP = 3
    KIND_TY_PI = 4
    KIND_ANN = 5
    KIND_POS = 6
    KIND_TRUST_ME = 7
    KIND_PRINT_ME = 8
    KIND_LET = 9
    KIND_TY_UNIT = 10
    KIND_LIT_UNIT = 11
    KIND_TY_BOOL = 12
    KIND_LIT_BOOL = 13
    KIND_IF = 14
    KIND_TY_SIGMA = 15
    KIND_PROD = 16
    KIND_LET_PAIR = 17
    KIND_TY_EQ = 18
    KIND_REFL = 19
    KIND_SUBST = 20
    KIND_CONTRA = 21
    KIND_TY_CON = 22
    KIND_DATA_CON = 23
    KIND_CASE = 24
    
    _fields_ = [
        ("kind", c_int32),
        ("union", c_void_p)
    ]
    
    @classmethod
    def init_var(cls, value: TName):
        return init_tagged_union(cls, cls.KIND_VAR, value)
    
    @classmethod
    def init_lam(cls, value: Tuple[Epsilon, Bind[TName, Term]]):
        return init_tagged_union(cls, cls.KIND_LAM, value)
    
    @classmethod
    def init_app(cls, value: Tuple[Term, Arg]):
        return init_tagged_union(cls, cls.KIND_APP, value)
    
    @classmethod
    def init_ty_pi(cls, value: Tuple[Epsilon, Type, Bind[TName, Type]]):
        return init_tagged_union(cls, cls.KIND_TY_PI, value)
    
    @classmethod
    def init_ann(cls, value: Tuple[Term, Type]):
        return init_tagged_union(cls, cls.KIND_ANN, value)
    
    @classmethod
    def init_pos(cls, value: Tuple[SourcePos, Term]):
        return init_tagged_union(cls, cls.KIND_POS, value)
    
    @classmethod
    def init_let(cls, value: Tuple[Term, Bind[TName, Term]]):
        return init_tagged_union(cls, cls.KIND_LET, value)
    
    @classmethod
    def init_lit_bool(cls, value: Bool):
        return init_tagged_union(cls, cls.KIND_LIT_BOOL, value)
    
    @classmethod
    def init_if(cls, value: Tuple[Term, Term, Term]):
        return init_tagged_union(cls, cls.KIND_IF, value)
    
    @classmethod
    def init_ty_sigma(cls, value: Tuple[Term, Bind[TName, Term]]):
        return init_tagged_union(cls, cls.KIND_TY_SIGMA, value)
    
    @classmethod
    def init_prod(cls, value: Tuple[Term, Term]):
        return init_tagged_union(cls, cls.KIND_PROD, value)
    
    @classmethod
    def init_let_pair(cls, value: Tuple[Term, Bind[Tuple[TName, TName], Term]]):
        return init_tagged_union(cls, cls.KIND_LET_PAIR, value)
    
    @classmethod
    def init_ty_eq(cls, value: Tuple[Term, Term]):
        return init_tagged_union(cls, cls.KIND_TY_EQ, value)
    
    @classmethod
    def init_subst(cls, value: Tuple[Term, Term]):
        return init_tagged_union(cls, cls.KIND_SUBST, value)
    
    @classmethod
    def init_contra(cls, value: Term):
        return init_tagged_union(cls, cls.KIND_CONTRA, value)
    
    @classmethod
    def init_ty_con(cls, value: Tuple[TyConName, List[Arg]]):
        return init_tagged_union(cls, cls.KIND_TY_CON, value)
    
    @classmethod
    def init_data_con(cls, value: Tuple[DataConName, List[Arg]]):
        return init_tagged_union(cls, cls.KIND_DATA_CON, value)
    
    @classmethod
    def init_case(cls, value: Tuple[Term, List[Match]]):
        return init_tagged_union(cls, cls.KIND_CASE, value)

TName = Name[Term]

Type = Term

SourceName = String

Line = Int

Column = Int

TyConName = String

DataConName = String

Match = Bind[Pattern, Term]
