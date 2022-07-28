/**
  The type of an import.
 */
[@deriving sexp]
type t;

/**
  [mk name path] is an import using the symbol [name] and the path [path].
 */
let mk: (Ident.t, ImportPath.t) => t;

let name: t => Ident.t;
let path: t => ImportPath.t;

module H: {
  /**
    [ident imp] is a ident referencing a module member.
   */
  let ident: (t, Ident.t) => Ident.t;

  /**
    [var imp x] is [Expr.var(ident(imp, x))].
   */
  let var: (t, Ident.t) => Expr.t;

  /**
    [pvar imp x] is [Expr.pvar(ident(fmodli, x))].
   */
  let pvar: (t, Ident.t) => Pat.t;

  let ap: (t, Ident.t, list(Expr.t)) => Expr.t;
  let ap1: (t, Ident.t, Expr.t) => Expr.t;
  let ap2: (t, Ident.t, Expr.t, Expr.t) => Expr.t;
  let ap3: (t, Ident.t, Expr.t, Expr.t, Expr.t) => Expr.t;
  let ap4: (t, Ident.t, Expr.t, Expr.t, Expr.t, Expr.t) => Expr.t;
  let ap5: (t, Ident.t, Expr.t, Expr.t, Expr.t, Expr.t, Expr.t) => Expr.t;

  let ctor: (t, Ident.t, list(Expr.t)) => Expr.t;
  let ctor0: (t, Ident.t) => Expr.t;
  let ctor1: (t, Ident.t, Expr.t) => Expr.t;
  let ctor2: (t, Ident.t, Expr.t, Expr.t) => Expr.t;
  let ctor3: (t, Ident.t, Expr.t, Expr.t, Expr.t) => Expr.t;
  let ctor4: (t, Ident.t, Expr.t, Expr.t, Expr.t, Expr.t) => Expr.t;
  let ctor5: (t, Ident.t, Expr.t, Expr.t, Expr.t, Expr.t, Expr.t) => Expr.t;

  let pctor: (t, Ident.t, list(Pat.t)) => Pat.t;
  let pctor0: (t, Ident.t) => Pat.t;
  let pctor1: (t, Ident.t, Pat.t) => Pat.t;
  let pctor2: (t, Ident.t, Pat.t, Pat.t) => Pat.t;
  let pctor3: (t, Ident.t, Pat.t, Pat.t, Pat.t) => Pat.t;
  let pctor4: (t, Ident.t, Pat.t, Pat.t, Pat.t, Pat.t) => Pat.t;
  let pctor5: (t, Ident.t, Pat.t, Pat.t, Pat.t, Pat.t, Pat.t) => Pat.t;
};
