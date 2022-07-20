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
  [var fmdi x] is [Expr.var(var(fmdi, x))].
 */
let var: (t, ident) => expr;

/**
  [pvar fmdi x] is [Expr.pvar(var(fmdi, x))].
 */
let pvar: (t, ident) => pat;

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

let pctor: (t, ident, list(pat)) => pat;
let pctor0: (t, ident) => pat;
let pctor1: (t, ident, pat) => pat;
let pctor2: (t, ident, pat, pat) => pat;
let pctor3: (t, ident, pat, pat, pat) => pat;
let pctor4: (t, ident, pat, pat, pat, pat) => pat;
