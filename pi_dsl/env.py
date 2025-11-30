from .base import *
from .bindings import *

sigma_prod = CtorDef.init_ctor_def(String("Prod"), List[Entry]())
sigma_entry = Entry.init_data(String("Sigma"), List[Entry](), init_list(sigma_prod))

unit_ctor = CtorDef.init_ctor_def(String("()"), List[Entry]())
unit_entry = Entry.init_data(String("Unit"), List[Entry](), init_list(unit_ctor))

false_ctor = CtorDef.init_ctor_def(String("False"), List[Entry]())
true_ctor = CtorDef.init_ctor_def(String("True"), List[Entry]())
bool_entry = \
    Entry.init_data(String("Bool"), List[Entry](), init_list(false_ctor, true_ctor))

prelude_data_decls = [sigma_entry, unit_entry, bool_entry]
empty_env = \
    Env.init_env(init_list(*prelude_data_decls), Int(len(prelude_data_decls)), List[TypeDecl]())