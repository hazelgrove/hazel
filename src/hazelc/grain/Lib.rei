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
  let pat_var: ident => pat;

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

  let pat_ctor: (ident, params) => pat;
  let pat_ctor0: ident => pat;
  let pat_ctor1: (ident, pat) => pat;
  let pat_ctor2: (ident, pat, pat) => pat;
  let pat_ctor3: (ident, pat, pat, pat) => pat;
  let pat_ctor4: (ident, pat, pat, pat, pat) => pat;
};

module Make: (M: M) => META;
