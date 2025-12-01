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

class Epsilon(TaggedUnion):
    kind: Literal[0, 1]
    
    KIND_REL = 0
    KIND_IRR = 1
    
    rel: ClassVar[Self]
    irr: ClassVar[Self]

Epsilon.rel = Epsilon(Epsilon.KIND_REL)
Epsilon.irr = Epsilon(Epsilon.KIND_IRR)

class Bind[T1, T2](TaggedUnion):
    kind: Literal[0]
    
    KIND_B = 0
    
    @classmethod
    def init_b(cls, *values: *tuple[T1, T2]):
        return cls(cls.KIND_B, init_tuple(*values))
    
    def get_b(self) -> tuple[T1, T2]:
        assert self.kind == self.KIND_B
        return self.get_field(Tuple[T1, T2]).get()

class Arg(TaggedUnion):
    kind: Literal[0]
    
    KIND_ARG = 0
    
    @classmethod
    def init_arg(cls, *values: *tuple[Epsilon, Term]):
        return cls(cls.KIND_ARG, init_tuple(*values))
    
    def get_arg(self) -> tuple[Epsilon, Term]:
        assert self.kind == self.KIND_ARG
        return self.get_field(Tuple[Epsilon, Term]).get()

class Pattern(TaggedUnion):
    kind: Literal[0, 1]
    
    KIND_PAT_CON = 0
    KIND_PAT_VAR = 1
    
    @classmethod
    def init_pat_con(cls, *values: *tuple[DataConName, List[Tuple[Pattern, Epsilon]]]):
        return cls(cls.KIND_PAT_CON, init_tuple(*values))
    
    def get_pat_con(self) -> tuple[DataConName, List[Tuple[Pattern, Epsilon]]]:
        assert self.kind == self.KIND_PAT_CON
        return self.get_field(Tuple[DataConName, List[Tuple[Pattern, Epsilon]]]).get()
    
    @classmethod
    def init_pat_var(cls, value: TName):
        return cls(cls.KIND_PAT_VAR, value)
    
    def get_pat_var(self) -> TName:
        assert self.kind == self.KIND_PAT_VAR
        return self.get_field(TName)

class Term(TaggedUnion):
    kind: Literal[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22]
    
    KIND_TY_TYPE = 0
    KIND_VAR = 1
    KIND_LAM = 2
    KIND_APP = 3
    KIND_TY_PI = 4
    KIND_ANN = 5
    KIND_TRUST_ME = 6
    KIND_LET = 7
    KIND_TY_UNIT = 8
    KIND_LIT_UNIT = 9
    KIND_TY_BOOL = 10
    KIND_LIT_BOOL = 11
    KIND_IF = 12
    KIND_TY_SIGMA = 13
    KIND_PROD = 14
    KIND_LET_PAIR = 15
    KIND_TY_EQ = 16
    KIND_REFL = 17
    KIND_SUBST = 18
    KIND_CONTRA = 19
    KIND_TY_CON = 20
    KIND_DATA_CON = 21
    KIND_CASE = 22
    
    ty_type: ClassVar[Self]
    trust_me: ClassVar[Self]
    ty_unit: ClassVar[Self]
    lit_unit: ClassVar[Self]
    ty_bool: ClassVar[Self]
    refl: ClassVar[Self]
    
    @classmethod
    def init_var(cls, value: TName):
        return cls(cls.KIND_VAR, value)
    
    def get_var(self) -> TName:
        assert self.kind == self.KIND_VAR
        return self.get_field(TName)
    
    @classmethod
    def init_lam(cls, *values: *tuple[Epsilon, Bind[TName, Term]]):
        return cls(cls.KIND_LAM, init_tuple(*values))
    
    def get_lam(self) -> tuple[Epsilon, Bind[TName, Term]]:
        assert self.kind == self.KIND_LAM
        return self.get_field(Tuple[Epsilon, Bind[TName, Term]]).get()
    
    @classmethod
    def init_app(cls, *values: *tuple[Term, Arg]):
        return cls(cls.KIND_APP, init_tuple(*values))
    
    def get_app(self) -> tuple[Term, Arg]:
        assert self.kind == self.KIND_APP
        return self.get_field(Tuple[Term, Arg]).get()
    
    @classmethod
    def init_ty_pi(cls, *values: *tuple[Epsilon, Type, Bind[TName, Type]]):
        return cls(cls.KIND_TY_PI, init_tuple(*values))
    
    def get_ty_pi(self) -> tuple[Epsilon, Type, Bind[TName, Type]]:
        assert self.kind == self.KIND_TY_PI
        return self.get_field(Tuple[Epsilon, Type, Bind[TName, Type]]).get()
    
    @classmethod
    def init_ann(cls, *values: *tuple[Term, Type]):
        return cls(cls.KIND_ANN, init_tuple(*values))
    
    def get_ann(self) -> tuple[Term, Type]:
        assert self.kind == self.KIND_ANN
        return self.get_field(Tuple[Term, Type]).get()
    
    @classmethod
    def init_let(cls, *values: *tuple[Term, Bind[TName, Term]]):
        return cls(cls.KIND_LET, init_tuple(*values))
    
    def get_let(self) -> tuple[Term, Bind[TName, Term]]:
        assert self.kind == self.KIND_LET
        return self.get_field(Tuple[Term, Bind[TName, Term]]).get()
    
    @classmethod
    def init_lit_bool(cls, value: Bool):
        return cls(cls.KIND_LIT_BOOL, value)
    
    def get_lit_bool(self) -> Bool:
        assert self.kind == self.KIND_LIT_BOOL
        return self.get_field(Bool)
    
    @classmethod
    def init_if(cls, *values: *tuple[Term, Term, Term]):
        return cls(cls.KIND_IF, init_tuple(*values))
    
    def get_if(self) -> tuple[Term, Term, Term]:
        assert self.kind == self.KIND_IF
        return self.get_field(Tuple[Term, Term, Term]).get()
    
    @classmethod
    def init_ty_sigma(cls, *values: *tuple[Term, Bind[TName, Term]]):
        return cls(cls.KIND_TY_SIGMA, init_tuple(*values))
    
    def get_ty_sigma(self) -> tuple[Term, Bind[TName, Term]]:
        assert self.kind == self.KIND_TY_SIGMA
        return self.get_field(Tuple[Term, Bind[TName, Term]]).get()
    
    @classmethod
    def init_prod(cls, *values: *tuple[Term, Term]):
        return cls(cls.KIND_PROD, init_tuple(*values))
    
    def get_prod(self) -> tuple[Term, Term]:
        assert self.kind == self.KIND_PROD
        return self.get_field(Tuple[Term, Term]).get()
    
    @classmethod
    def init_let_pair(cls, *values: *tuple[Term, Bind[Tuple[TName, TName], Term]]):
        return cls(cls.KIND_LET_PAIR, init_tuple(*values))
    
    def get_let_pair(self) -> tuple[Term, Bind[Tuple[TName, TName], Term]]:
        assert self.kind == self.KIND_LET_PAIR
        return self.get_field(Tuple[Term, Bind[Tuple[TName, TName], Term]]).get()
    
    @classmethod
    def init_ty_eq(cls, *values: *tuple[Term, Term]):
        return cls(cls.KIND_TY_EQ, init_tuple(*values))
    
    def get_ty_eq(self) -> tuple[Term, Term]:
        assert self.kind == self.KIND_TY_EQ
        return self.get_field(Tuple[Term, Term]).get()
    
    @classmethod
    def init_subst(cls, *values: *tuple[Term, Term]):
        return cls(cls.KIND_SUBST, init_tuple(*values))
    
    def get_subst(self) -> tuple[Term, Term]:
        assert self.kind == self.KIND_SUBST
        return self.get_field(Tuple[Term, Term]).get()
    
    @classmethod
    def init_contra(cls, value: Term):
        return cls(cls.KIND_CONTRA, value)
    
    def get_contra(self) -> Term:
        assert self.kind == self.KIND_CONTRA
        return self.get_field(Term)
    
    @classmethod
    def init_ty_con(cls, *values: *tuple[TyConName, List[Arg]]):
        return cls(cls.KIND_TY_CON, init_tuple(*values))
    
    def get_ty_con(self) -> tuple[TyConName, List[Arg]]:
        assert self.kind == self.KIND_TY_CON
        return self.get_field(Tuple[TyConName, List[Arg]]).get()
    
    @classmethod
    def init_data_con(cls, *values: *tuple[DataConName, List[Arg]]):
        return cls(cls.KIND_DATA_CON, init_tuple(*values))
    
    def get_data_con(self) -> tuple[DataConName, List[Arg]]:
        assert self.kind == self.KIND_DATA_CON
        return self.get_field(Tuple[DataConName, List[Arg]]).get()
    
    @classmethod
    def init_case(cls, *values: *tuple[Term, List[Match]]):
        return cls(cls.KIND_CASE, init_tuple(*values))
    
    def get_case(self) -> tuple[Term, List[Match]]:
        assert self.kind == self.KIND_CASE
        return self.get_field(Tuple[Term, List[Match]]).get()

Term.ty_type = Term(Term.KIND_TY_TYPE)
Term.trust_me = Term(Term.KIND_TRUST_ME)
Term.ty_unit = Term(Term.KIND_TY_UNIT)
Term.lit_unit = Term(Term.KIND_LIT_UNIT)
Term.ty_bool = Term(Term.KIND_TY_BOOL)
Term.refl = Term(Term.KIND_REFL)

class TypeDecl(TaggedUnion):
    kind: Literal[0]
    
    KIND_TYPE_DECL = 0
    
    @classmethod
    def init_type_decl(cls, *values: *tuple[TName, Epsilon, Type]):
        return cls(cls.KIND_TYPE_DECL, init_tuple(*values))
    
    def get_type_decl(self) -> tuple[TName, Epsilon, Type]:
        assert self.kind == self.KIND_TYPE_DECL
        return self.get_field(Tuple[TName, Epsilon, Type]).get()

class CtorDef(TaggedUnion):
    kind: Literal[0]
    
    KIND_CTOR_DEF = 0
    
    @classmethod
    def init_ctor_def(cls, *values: *tuple[DataConName, Telescope]):
        return cls(cls.KIND_CTOR_DEF, init_tuple(*values))
    
    def get_ctor_def(self) -> tuple[DataConName, Telescope]:
        assert self.kind == self.KIND_CTOR_DEF
        return self.get_field(Tuple[DataConName, Telescope]).get()

class Entry(TaggedUnion):
    kind: Literal[0, 1, 2, 3]
    
    KIND_DECL = 0
    KIND_DEF = 1
    KIND_DEMOTE = 2
    KIND_DATA = 3
    
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
    def init_demote(cls, value: Epsilon):
        return cls(cls.KIND_DEMOTE, value)
    
    def get_demote(self) -> Epsilon:
        assert self.kind == self.KIND_DEMOTE
        return self.get_field(Epsilon)
    
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

Match = Bind[Pattern, Term]

TName = Name[Term]

Telescope = List[Entry]

set_export_signature("ppr_term", [Term], String)
def ppr_term(term: Term) -> String:
    return call_export("ppr_term", [term])

set_export_signature("type_check", [List[Entry]], Maybe[String])
def type_check(entries: List[Entry]) -> Maybe[String]:
    return call_export("type_check", [entries])
