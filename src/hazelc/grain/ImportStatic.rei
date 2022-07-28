module type I = {
  let v: [> | `Import(Import.t) | `Parts(FileModule.t, Ident.t, Path.t)];
};

module type S = {
  let imp: Import.t;

  let ident: Ident.t => Ident.t;
  let var: Ident.t => Expr.t;
  let pvar: Ident.t => Pat.t;

  let ap: (Ident.t, list(Expr.t)) => Expr.t;
  let ap1: (Ident.t, Expr.t) => Expr.t;
  let ap2: (Ident.t, Expr.t, Expr.t) => Expr.t;
  let ap3: (Ident.t, Expr.t, Expr.t, Expr.t) => Expr.t;
  let ap4: (Ident.t, Expr.t, Expr.t, Expr.t, Expr.t) => Expr.t;
  let ap5: (Ident.t, Expr.t, Expr.t, Expr.t, Expr.t, Expr.t) => Expr.t;

  let ctor: (Ident.t, list(Expr.t)) => Expr.t;
  let ctor0: Ident.t => Expr.t;
  let ctor1: (Ident.t, Expr.t) => Expr.t;
  let ctor2: (Ident.t, Expr.t, Expr.t) => Expr.t;
  let ctor3: (Ident.t, Expr.t, Expr.t, Expr.t) => Expr.t;
  let ctor4: (Ident.t, Expr.t, Expr.t, Expr.t, Expr.t) => Expr.t;
  let ctor5: (Ident.t, Expr.t, Expr.t, Expr.t, Expr.t, Expr.t) => Expr.t;

  let pctor: (Ident.t, list(Pat.t)) => Pat.t;
  let pctor0: Ident.t => Pat.t;
  let pctor1: (Ident.t, Pat.t) => Pat.t;
  let pctor2: (Ident.t, Pat.t, Pat.t) => Pat.t;
  let pctor3: (Ident.t, Pat.t, Pat.t, Pat.t) => Pat.t;
  let pctor4: (Ident.t, Pat.t, Pat.t, Pat.t, Pat.t) => Pat.t;
  let pctor5: (Ident.t, Pat.t, Pat.t, Pat.t, Pat.t, Pat.t) => Pat.t;
};

module Of: (I) => S;
