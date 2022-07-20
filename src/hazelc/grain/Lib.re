open Expr;
open Module;

module type M = {
  let name: ident;
  let path: import_path;
};

module type META = {
  include M;

  let file_mod: FileModule.t;
  let file_mod_import: FileModuleImport.t;

  let import: import;

  let ident: ident => ident;
  let var: ident => expr;
  let pvar: ident => pat;

  let ap: (ident, args) => expr;
  let ap1: (ident, expr) => expr;
  let ap2: (ident, expr, expr) => expr;
  let ap3: (ident, expr, expr, expr) => expr;
  let ap4: (ident, expr, expr, expr, expr) => expr;

  let ctor: (ident, args) => expr;
  let ctor0: ident => expr;
  let ctor1: (ident, expr) => expr;
  let ctor2: (ident, expr, expr) => expr;
  let ctor3: (ident, expr, expr, expr) => expr;
  let ctor4: (ident, expr, expr, expr, expr) => expr;

  let pctor: (ident, params) => pat;
  let pctor0: ident => pat;
  let pctor1: (ident, pat) => pat;
  let pctor2: (ident, pat, pat) => pat;
  let pctor3: (ident, pat, pat, pat) => pat;
  let pctor4: (ident, pat, pat, pat, pat) => pat;
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
  let pvar = pvar(file_mod_import);

  let ap = ap(file_mod_import);
  let ap1 = ap1(file_mod_import);
  let ap2 = ap2(file_mod_import);
  let ap3 = ap3(file_mod_import);
  let ap4 = ap4(file_mod_import);

  let ctor = ctor(file_mod_import);
  let ctor0 = ctor0(file_mod_import);
  let ctor1 = ctor1(file_mod_import);
  let ctor2 = ctor2(file_mod_import);
  let ctor3 = ctor3(file_mod_import);
  let ctor4 = ctor4(file_mod_import);

  let pctor = pctor(file_mod_import);
  let pctor0 = pctor0(file_mod_import);
  let pctor1 = pctor1(file_mod_import);
  let pctor2 = pctor2(file_mod_import);
  let pctor3 = pctor3(file_mod_import);
  let pctor4 = pctor4(file_mod_import);
};
