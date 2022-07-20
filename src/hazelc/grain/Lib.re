open Expr;
open Module;

module type M = {
  let name: var;
  let path: import_path;
};

module type META = {
  include M;

  let file_mod: FileModule.t;
  let file_mod_import: FileModuleImport.t;

  let import: import;

  let ident: var => var;
  let var: var => expr;

  let ap: (var, list(expr)) => expr;
  let ap1: (var, expr) => expr;
  let ap2: (var, expr, expr) => expr;
  let ap3: (var, expr, expr, expr) => expr;
  let ap4: (var, expr, expr, expr, expr) => expr;

  let ctor: (var, list(expr)) => expr;
  let ctor1: (var, expr) => expr;
  let ctor2: (var, expr, expr) => expr;
  let ctor3: (var, expr, expr, expr) => expr;
  let ctor4: (var, expr, expr, expr, expr) => expr;

  let pat_ctor: (var, list(pat)) => pat;
  let pat_ctor1: (var, pat) => pat;
  let pat_ctor2: (var, pat, pat) => pat;
  let pat_ctor3: (var, pat, pat, pat) => pat;
  let pat_ctor4: (var, pat, pat, pat, pat) => pat;
};

module Make = (M: M) => {
  let name = M.name;
  let path = M.path;

  let file_mod = FileModule.mk_lib(path);
  let file_mod_import = FileModuleImport.mk(file_mod, name);

  let import = FileModuleImport.import(file_mod_import);

  open FileModuleImport;
  let ident = ident(file_mod_import);
  let var = var(file_mod_import);

  let ap = ap(file_mod_import);
  let ap1 = ap1(file_mod_import);
  let ap2 = ap2(file_mod_import);
  let ap3 = ap3(file_mod_import);
  let ap4 = ap4(file_mod_import);

  let ctor = ctor(file_mod_import);
  let ctor1 = ctor1(file_mod_import);
  let ctor2 = ctor2(file_mod_import);
  let ctor3 = ctor3(file_mod_import);
  let ctor4 = ctor4(file_mod_import);

  let pat_ctor = pat_ctor(file_mod_import);
  let pat_ctor1 = pat_ctor1(file_mod_import);
  let pat_ctor2 = pat_ctor2(file_mod_import);
  let pat_ctor3 = pat_ctor3(file_mod_import);
  let pat_ctor4 = pat_ctor4(file_mod_import);
};
