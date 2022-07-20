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

module Make: (M: M) => META;
