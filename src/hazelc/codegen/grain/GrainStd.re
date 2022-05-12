module type InnerMeta = {let name: Var.t;};

module Inner = (X: InnerMeta) => {
  let ident = (x: Var.t): Var.t => X.name ++ "." ++ x;

  let mk_ap = (x: Var.t): (list(GrainIR.expr) => GrainIR.expr) => {
    args => EAp(EVar(ident(x)), args);
  };

  let mk_ctor = (x: Var.t): (list(GrainIR.expr) => GrainIR.expr) => {
    args => ECtor(ident(x), args);
  };
};

module Map = {
  open Inner({
         let name = "Map";
       });

  let from_list = xs => mk_ap("fromList", [xs]);
};

module Hazel = {
  module Ast = {
    open Inner({
           let name = "Ast";
         });

    let empty_hole = (u, i, sigma) => mk_ctor("EmptyHole", [u, i, sigma]);
  };
};
