open Expr;
open Module;

[@deriving sexp]
type t = {
  name: var,
  path: import_path,
};

/**
  [mk name path] is the import for the module given by [path] via the
  identifier [name].
 */
let mk: (FileModule.t, var) => t;

/**
  [name fmdi] is the imported name for the module.
 */
let name: t => var;

/**
  [path fmdi] is the path for the module.
 */
let path: t => import_path;

/**
  [import fmdi] is the import statement for the module.
 */
let import: t => import;

/**
  [ident fmdi] is an identifier referencing a module member.
 */
let ident: (t, var) => var;

/**
  [var fmdi x] is [EVar(ident(fmdi, x))].
 */
let var: (t, var) => expr;

let ap: (t, var, list(expr)) => expr;
let ap1: (t, var, expr) => expr;
let ap2: (t, var, expr, expr) => expr;
let ap3: (t, var, expr, expr, expr) => expr;
let ap4: (t, var, expr, expr, expr, expr) => expr;

let ctor: (t, var, list(expr)) => expr;
let ctor1: (t, var, expr) => expr;
let ctor2: (t, var, expr, expr) => expr;
let ctor3: (t, var, expr, expr, expr) => expr;
let ctor4: (t, var, expr, expr, expr, expr) => expr;

let pat_ctor: (t, var, list(pat)) => pat;
let pat_ctor1: (t, var, pat) => pat;
let pat_ctor2: (t, var, pat, pat) => pat;
let pat_ctor3: (t, var, pat, pat, pat) => pat;
let pat_ctor4: (t, var, pat, pat, pat, pat) => pat;
