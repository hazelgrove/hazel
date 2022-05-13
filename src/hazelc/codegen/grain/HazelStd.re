module Inner = GrainStd.Inner;

let mk_path = path => GrainIR.ImportRel("hazel/" ++ path);

module Rt = {
  let mk_path = path => mk_path("rt/" ++ path);

  module Ast = {
    include Inner({
      let name = "Ast";
      let path = mk_path("ast");
    });

    module HTyp = {
      let name = ident("HTyp");

      let hole = mk_var("Hole");
      let int = mk_var("Int");
      let float = mk_var("Float");
      let bool = mk_var("Bool");
      let arrow = (t1, t2) => mk_ctor("Arrow", [t1, t2]);
      let sum = (t1, t2) => mk_ctor("Sum", [t1, t2]);
      let prod = ts => mk_ctor("Prod", ts);
      let list = t => mk_ctor("List", [t]);
    };

    module HoleReason = {
      let name = ident("HoleReason");
      let type_inconsistent = ident("TypeInconsistent");
      let wrong_length = ident("WrongLength");
    };

    let empty_hole = (u, i, sigma) => mk_ctor("EmptyHole", [u, i, sigma]);

    let non_empty_hole = (reason, u, i, sigma, e) =>
      mk_ctor("NonEmptyHole", [reason, u, i, sigma, e]);

    let cast = (e, t1, t2) => mk_ctor("Cast", [e, t1, t2]);
  };

  module AstSexp = {
    include Inner({
      let name = "AstSexp";
      let path = mk_path("ast_sexp");
    });

    let sexp_of_ast = ast => mk_ap(ident("sexpOfAst"), [ast]);
  };

  module Sum = {
    include Inner({
      let name = "Sum";
      let path = mk_path("sum");
    });

    let inj_l = e => mk_ctor("L", [e]);
    let inj_r = e => mk_ctor("R", [e]);

    let inj_l_pat = p => mk_ctor_pat("L", [p]);
    let inj_r_pat = p => mk_ctor_pat("R", [p]);
  };
};
