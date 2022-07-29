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

module Of = (I: I) => {
  open Import;

  let imp =
    switch (I.v) {
    | `Import(imp) => imp
    | `Parts(fmodl, name, from) => FileModule.import(fmodl, ~name, ~from)
    };

  let ident = H.ident(imp);
  let var = H.var(imp);
  let pvar = H.pvar(imp);

  let ap = H.ap(imp);
  let ap1 = H.ap1(imp);
  let ap2 = H.ap2(imp);
  let ap3 = H.ap3(imp);
  let ap4 = H.ap4(imp);
  let ap5 = H.ap5(imp);

  let ctor = H.ctor(imp);
  let ctor0 = H.ctor0(imp);
  let ctor1 = H.ctor1(imp);
  let ctor2 = H.ctor2(imp);
  let ctor3 = H.ctor3(imp);
  let ctor4 = H.ctor4(imp);
  let ctor5 = H.ctor5(imp);

  let pctor = H.pctor(imp);
  let pctor0 = H.pctor0(imp);
  let pctor1 = H.pctor1(imp);
  let pctor2 = H.pctor2(imp);
  let pctor3 = H.pctor3(imp);
  let pctor4 = H.pctor4(imp);
  let pctor5 = H.pctor5(imp);
};
