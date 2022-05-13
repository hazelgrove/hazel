module type InnerMeta = {
  let name: Var.t;
  let path: GrainIR.import_path;
};

module Inner = (X: InnerMeta) => {
  let ident = (x: Var.t): Var.t => X.name ++ "." ++ x;

  let import = (X.name, X.path);

  let mk_ap = (x: Var.t): (list(GrainIR.expr) => GrainIR.expr) => {
    args => EAp(EVar(ident(x)), args);
  };

  let mk_ctor = (x: Var.t): (list(GrainIR.expr) => GrainIR.expr) => {
    args => ECtor(ident(x), args);
  };

  let mk_ctor_pat = (x: Var.t): (list(GrainIR.pat) => GrainIR.pat) => {
    pats => PCtor(ident(x), pats);
  };
};

module Map = {
  include Inner({
    let name = "Map";
    let path = GrainIR.ImportStd("map");
  });

  let from_list = xs => mk_ap("fromList", [xs]);
};
