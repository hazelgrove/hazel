open Expr;
open Module;

[@deriving sexp]
type t = {
  name: ident,
  path: import_path,
};

/**
  [mk name path] is the import for the module given by [path] via the
  identifier [name].
 */
let mk: (FileModule.t, ident) => t;

/**
  [name fmdi] is the imported name for the module.
 */
let name: t => ident;

/**
  [path fmdi] is the path for the module.
 */
let path: t => import_path;

/**
  [import fmdi] is the import statement for the module.
 */
let import: t => import;

/**
  [ident fmdi] is a ident referencing a module member.
 */
let ident: (t, ident) => ident;

/**
  [var fmdi x] is [Expr.mk_var(var(fmdi, x))].
 */
let var: (t, ident) => expr;

/**
  [pat_var fmdi x] is [Expr.mk_pat_var(var(fmdi, x))].
 */
let pat_var: (t, ident) => pat;

let ap: (t, ident, list(expr)) => expr;
let ap1: (t, ident, expr) => expr;
let ap2: (t, ident, expr, expr) => expr;
let ap3: (t, ident, expr, expr, expr) => expr;
let ap4: (t, ident, expr, expr, expr, expr) => expr;

let ctor: (t, ident, list(expr)) => expr;
let ctor0: (t, ident) => expr;
let ctor1: (t, ident, expr) => expr;
let ctor2: (t, ident, expr, expr) => expr;
let ctor3: (t, ident, expr, expr, expr) => expr;
let ctor4: (t, ident, expr, expr, expr, expr) => expr;

let pat_ctor: (t, ident, list(pat)) => pat;
let pat_ctor0: (t, ident) => pat;
let pat_ctor1: (t, ident, pat) => pat;
let pat_ctor2: (t, ident, pat, pat) => pat;
let pat_ctor3: (t, ident, pat, pat, pat) => pat;
let pat_ctor4: (t, ident, pat, pat, pat, pat) => pat;
