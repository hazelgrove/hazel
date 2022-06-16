module Inner = GrainStd.Inner;

let mk_path = path => GrainIR.ImportStd(Filename.concat("hazel", path));

module Rt = {
  let mk_path = path => mk_path(Filename.concat("rt", path));

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
      let arrow = (t1, t2) => mk_nary_ctor("Arrow", [t1, t2]);
      let sum = (t1, t2) => mk_nary_ctor("Sum", [t1, t2]);
      let prod = ts => mk_nary_ctor("Prod", ts);
      let list = t => mk_nary_ctor("List", [t]);
    };

    module HoleReason = {
      let name = ident("HoleReason");
      let type_inconsistent = ident("TypeInconsistent");
      let wrong_length = ident("WrongLength");
    };

    let empty_hole = (u, i, sigma) =>
      mk_nary_ctor("EmptyHole", [u, i, sigma]);

    let non_empty_hole = (reason, u, i, sigma, e) =>
      mk_nary_ctor("NonEmptyHole", [reason, u, i, sigma, e]);

    let cast = (e, t1, t2) => mk_nary_ctor("Cast", [e, t1, t2]);
  };

  module AstSexp = {
    include Inner({
      let name = "AstSexp";
      let path = mk_path("ast_sexp");
    });

    let sexp_of_ast = mk_unary_ap("sexpOfAst");
  };

  module AstOps = {
    include Inner({
      let name = "AstOps";
      let path = mk_path("ast_ops");
    });

    let indet_and = mk_binary_ap("and");
    let indet_or = mk_binary_ap("or");

    let indet_plus = mk_binary_ap("plus");
    let indet_minus = mk_binary_ap("minus");
    let indet_times = mk_binary_ap("times");
    let indet_divide = mk_binary_ap("divide");

    let indet_less_than = mk_binary_ap("lessThan");
    let indet_greater_than = mk_binary_ap("greaterThan");
    let indet_equals = mk_binary_ap("equals");

    let indet_fplus = mk_binary_ap("fplus");
    let indet_fminus = mk_binary_ap("fminus");
    let indet_ftimes = mk_binary_ap("ftimes");
    let indet_fdivide = mk_binary_ap("fdivide");

    let indet_fless_than = mk_binary_ap("flessThan");
    let indet_fgreater_than = mk_binary_ap("fgreaterThan");
    let indet_fequals = mk_binary_ap("fequals");
  };

  module AstPrint = {
    include Inner({
      let name = "AstPrint";
      let path = mk_path("ast_print");
    });

    let print = mk_unary_ap("print");
  };

  module AstMk = {
    include Inner({
      let name = "AstMk";
      let path = mk_path("ast_mk");
    });

    let mk_empty_hole = mk_unary_ap("mkEmptyHole");

    let mk_bool_lit = mk_unary_ap("mkBoolLit");
  };

  module Sum = {
    include Inner({
      let name = "Sum";
      let path = mk_path("sum");
    });

    let inj_l = e => mk_nary_ctor("L", [e]);
    let inj_r = e => mk_nary_ctor("R", [e]);

    let inj_l_pat = p => mk_nary_ctor_pat("L", [p]);
    let inj_r_pat = p => mk_nary_ctor_pat("R", [p]);
  };
};
