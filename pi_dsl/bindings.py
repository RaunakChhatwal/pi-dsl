from __future__ import annotations
from typing import Literal
from .base import Bool, call_export, Int, List, set_export_signature, String, TaggedUnion, Tuple

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
    def init_fn(cls, value: Tuple[String, Int]):
        return cls(cls.KIND_FN, value)
    
    def get_fn(self) -> Tuple[String, Int]:
        assert self.kind == self.KIND_FN
        return self.get_field(Tuple[String, Int])
    
    @classmethod
    def init_bn(cls, value: Tuple[Int, Int]):
        return cls(cls.KIND_BN, value)
    
    def get_bn(self) -> Tuple[Int, Int]:
        assert self.kind == self.KIND_BN
        return self.get_field(Tuple[Int, Int])

class Epsilon(TaggedUnion):
    kind: Literal[0, 1]
    
    KIND_REL = 0
    KIND_IRR = 1

class Bind[T1, T2](TaggedUnion):
    kind: Literal[0]
    
    KIND_B = 0
    
    @classmethod
    def init_b(cls, value: Tuple[T1, T2]):
        return cls(cls.KIND_B, value)
    
    def get_b(self) -> Tuple[T1, T2]:
        assert self.kind == self.KIND_B
        return self.get_field(Tuple[T1, T2])

class Arg(TaggedUnion):
    kind: Literal[0]
    
    KIND_ARG = 0
    
    @classmethod
    def init_arg(cls, value: Tuple[Epsilon, Term]):
        return cls(cls.KIND_ARG, value)
    
    def get_arg(self) -> Tuple[Epsilon, Term]:
        assert self.kind == self.KIND_ARG
        return self.get_field(Tuple[Epsilon, Term])

class Pattern(TaggedUnion):
    kind: Literal[0, 1]
    
    KIND_PAT_CON = 0
    KIND_PAT_VAR = 1
    
    @classmethod
    def init_pat_con(cls, value: Tuple[DataConName, List[Tuple[Pattern, Epsilon]]]):
        return cls(cls.KIND_PAT_CON, value)
    
    def get_pat_con(self) -> Tuple[DataConName, List[Tuple[Pattern, Epsilon]]]:
        assert self.kind == self.KIND_PAT_CON
        return self.get_field(Tuple[DataConName, List[Tuple[Pattern, Epsilon]]])
    
    @classmethod
    def init_pat_var(cls, value: TName):
        return cls(cls.KIND_PAT_VAR, value)
    
    def get_pat_var(self) -> TName:
        assert self.kind == self.KIND_PAT_VAR
        return self.get_field(TName)

class Term(TaggedUnion):
    kind: Literal[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23]
    
    KIND_TY_TYPE = 0
    KIND_VAR = 1
    KIND_LAM = 2
    KIND_APP = 3
    KIND_TY_PI = 4
    KIND_ANN = 5
    KIND_TRUST_ME = 6
    KIND_PRINT_ME = 7
    KIND_LET = 8
    KIND_TY_UNIT = 9
    KIND_LIT_UNIT = 10
    KIND_TY_BOOL = 11
    KIND_LIT_BOOL = 12
    KIND_IF = 13
    KIND_TY_SIGMA = 14
    KIND_PROD = 15
    KIND_LET_PAIR = 16
    KIND_TY_EQ = 17
    KIND_REFL = 18
    KIND_SUBST = 19
    KIND_CONTRA = 20
    KIND_TY_CON = 21
    KIND_DATA_CON = 22
    KIND_CASE = 23
    
    @classmethod
    def init_var(cls, value: TName):
        return cls(cls.KIND_VAR, value)
    
    def get_var(self) -> TName:
        assert self.kind == self.KIND_VAR
        return self.get_field(TName)
    
    @classmethod
    def init_lam(cls, value: Tuple[Epsilon, Bind[TName, Term]]):
        return cls(cls.KIND_LAM, value)
    
    def get_lam(self) -> Tuple[Epsilon, Bind[TName, Term]]:
        assert self.kind == self.KIND_LAM
        return self.get_field(Tuple[Epsilon, Bind[TName, Term]])
    
    @classmethod
    def init_app(cls, value: Tuple[Term, Arg]):
        return cls(cls.KIND_APP, value)
    
    def get_app(self) -> Tuple[Term, Arg]:
        assert self.kind == self.KIND_APP
        return self.get_field(Tuple[Term, Arg])
    
    @classmethod
    def init_ty_pi(cls, value: Tuple[Epsilon, Type, Bind[TName, Type]]):
        return cls(cls.KIND_TY_PI, value)
    
    def get_ty_pi(self) -> Tuple[Epsilon, Type, Bind[TName, Type]]:
        assert self.kind == self.KIND_TY_PI
        return self.get_field(Tuple[Epsilon, Type, Bind[TName, Type]])
    
    @classmethod
    def init_ann(cls, value: Tuple[Term, Type]):
        return cls(cls.KIND_ANN, value)
    
    def get_ann(self) -> Tuple[Term, Type]:
        assert self.kind == self.KIND_ANN
        return self.get_field(Tuple[Term, Type])
    
    @classmethod
    def init_let(cls, value: Tuple[Term, Bind[TName, Term]]):
        return cls(cls.KIND_LET, value)
    
    def get_let(self) -> Tuple[Term, Bind[TName, Term]]:
        assert self.kind == self.KIND_LET
        return self.get_field(Tuple[Term, Bind[TName, Term]])
    
    @classmethod
    def init_lit_bool(cls, value: Bool):
        return cls(cls.KIND_LIT_BOOL, value)
    
    def get_lit_bool(self) -> Bool:
        assert self.kind == self.KIND_LIT_BOOL
        return self.get_field(Bool)
    
    @classmethod
    def init_if(cls, value: Tuple[Term, Term, Term]):
        return cls(cls.KIND_IF, value)
    
    def get_if(self) -> Tuple[Term, Term, Term]:
        assert self.kind == self.KIND_IF
        return self.get_field(Tuple[Term, Term, Term])
    
    @classmethod
    def init_ty_sigma(cls, value: Tuple[Term, Bind[TName, Term]]):
        return cls(cls.KIND_TY_SIGMA, value)
    
    def get_ty_sigma(self) -> Tuple[Term, Bind[TName, Term]]:
        assert self.kind == self.KIND_TY_SIGMA
        return self.get_field(Tuple[Term, Bind[TName, Term]])
    
    @classmethod
    def init_prod(cls, value: Tuple[Term, Term]):
        return cls(cls.KIND_PROD, value)
    
    def get_prod(self) -> Tuple[Term, Term]:
        assert self.kind == self.KIND_PROD
        return self.get_field(Tuple[Term, Term])
    
    @classmethod
    def init_let_pair(cls, value: Tuple[Term, Bind[Tuple[TName, TName], Term]]):
        return cls(cls.KIND_LET_PAIR, value)
    
    def get_let_pair(self) -> Tuple[Term, Bind[Tuple[TName, TName], Term]]:
        assert self.kind == self.KIND_LET_PAIR
        return self.get_field(Tuple[Term, Bind[Tuple[TName, TName], Term]])
    
    @classmethod
    def init_ty_eq(cls, value: Tuple[Term, Term]):
        return cls(cls.KIND_TY_EQ, value)
    
    def get_ty_eq(self) -> Tuple[Term, Term]:
        assert self.kind == self.KIND_TY_EQ
        return self.get_field(Tuple[Term, Term])
    
    @classmethod
    def init_subst(cls, value: Tuple[Term, Term]):
        return cls(cls.KIND_SUBST, value)
    
    def get_subst(self) -> Tuple[Term, Term]:
        assert self.kind == self.KIND_SUBST
        return self.get_field(Tuple[Term, Term])
    
    @classmethod
    def init_contra(cls, value: Term):
        return cls(cls.KIND_CONTRA, value)
    
    def get_contra(self) -> Term:
        assert self.kind == self.KIND_CONTRA
        return self.get_field(Term)
    
    @classmethod
    def init_ty_con(cls, value: Tuple[TyConName, List[Arg]]):
        return cls(cls.KIND_TY_CON, value)
    
    def get_ty_con(self) -> Tuple[TyConName, List[Arg]]:
        assert self.kind == self.KIND_TY_CON
        return self.get_field(Tuple[TyConName, List[Arg]])
    
    @classmethod
    def init_data_con(cls, value: Tuple[DataConName, List[Arg]]):
        return cls(cls.KIND_DATA_CON, value)
    
    def get_data_con(self) -> Tuple[DataConName, List[Arg]]:
        assert self.kind == self.KIND_DATA_CON
        return self.get_field(Tuple[DataConName, List[Arg]])
    
    @classmethod
    def init_case(cls, value: Tuple[Term, List[Match]]):
        return cls(cls.KIND_CASE, value)
    
    def get_case(self) -> Tuple[Term, List[Match]]:
        assert self.kind == self.KIND_CASE
        return self.get_field(Tuple[Term, List[Match]])

class TypeDecl(TaggedUnion):
    kind: Literal[0]
    
    KIND_TYPE_DECL = 0
    
    @classmethod
    def init_type_decl(cls, value: Tuple[TName, Epsilon, Type]):
        return cls(cls.KIND_TYPE_DECL, value)
    
    def get_type_decl(self) -> Tuple[TName, Epsilon, Type]:
        assert self.kind == self.KIND_TYPE_DECL
        return self.get_field(Tuple[TName, Epsilon, Type])

class ConstructorDef(TaggedUnion):
    kind: Literal[0]
    
    KIND_CONSTRUCTOR_DEF = 0
    
    @classmethod
    def init_constructor_def(cls, value: Tuple[DataConName, Telescope]):
        return cls(cls.KIND_CONSTRUCTOR_DEF, value)
    
    def get_constructor_def(self) -> Tuple[DataConName, Telescope]:
        assert self.kind == self.KIND_CONSTRUCTOR_DEF
        return self.get_field(Tuple[DataConName, Telescope])

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
    def init_def(cls, value: Tuple[TName, Term]):
        return cls(cls.KIND_DEF, value)
    
    def get_def(self) -> Tuple[TName, Term]:
        assert self.kind == self.KIND_DEF
        return self.get_field(Tuple[TName, Term])
    
    @classmethod
    def init_demote(cls, value: Epsilon):
        return cls(cls.KIND_DEMOTE, value)
    
    def get_demote(self) -> Epsilon:
        assert self.kind == self.KIND_DEMOTE
        return self.get_field(Epsilon)
    
    @classmethod
    def init_data(cls, value: Tuple[TyConName, Telescope, List[ConstructorDef]]):
        return cls(cls.KIND_DATA, value)
    
    def get_data(self) -> Tuple[TyConName, Telescope, List[ConstructorDef]]:
        assert self.kind == self.KIND_DATA
        return self.get_field(Tuple[TyConName, Telescope, List[ConstructorDef]])

class Env(TaggedUnion):
    kind: Literal[0]
    
    KIND_ENV = 0
    
    @classmethod
    def init_env(cls, value: Tuple[List[Entry], Int, List[TypeDecl]]):
        return cls(cls.KIND_ENV, value)
    
    def get_env(self) -> Tuple[List[Entry], Int, List[TypeDecl]]:
        assert self.kind == self.KIND_ENV
        return self.get_field(Tuple[List[Entry], Int, List[TypeDecl]])

Type = Term

TyConName = String

DataConName = String

Match = Bind[Pattern, Term]

TName = Name[Term]

Telescope = List[Entry]

set_export_signature("infer_type", [Env, Term], Either[String, Type])
def infer_type(env: Env, term: Term) -> Either[String, Type]:
    return call_export("infer_type", [env, term])
