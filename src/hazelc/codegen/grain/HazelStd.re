module Inner = GrainStd.Inner;

let mk_path = path => GrainIR.ImportRel("hazel/" ++ path);

module Rt = {
  let mk_path = path => mk_path("rt/" ++ path);

  module Ast = {
    include Inner({
      let name = "Ast";
      let path = mk_path("ast");
    });

    module HoleReason = {
      let name = ident("HoleReason");
      let type_inconsistent = ident("TypeInconsistent");
      let wrong_length = ident("WrongLength");
    };

    let empty_hole = (u, i, sigma) => mk_ctor("EmptyHole", [u, i, sigma]);

    let non_empty_hole = (reason, u, i, sigma, e) =>
      mk_ctor("NonEmptyHole", [reason, u, i, sigma, e]);
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
