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

    let sexp_of_ast = ast => mk_ap("sexpOfAst", [ast]);
  };

  module AstOps = {
    include Inner({
      let name = "AstOps";
      let path = mk_path("ast_ops");
    });

    let indet_and = (ast1, ast2) => mk_ap("and", [ast1, ast2]);
    let indet_or = (ast1, ast2) => mk_ap("or", [ast1, ast2]);

    let indet_plus = (ast1, ast2) => mk_ap("plus", [ast1, ast2]);
    let indet_minus = (ast1, ast2) => mk_ap("minus", [ast1, ast2]);
    let indet_times = (ast1, ast2) => mk_ap("times", [ast1, ast2]);
    let indet_divide = (ast1, ast2) => mk_ap("divide", [ast1, ast2]);

    let indet_less_than = (ast1, ast2) => mk_ap("lessThan", [ast1, ast2]);
    let indet_greater_than = (ast1, ast2) =>
      mk_ap("greaterThan", [ast1, ast2]);
    let indet_equals = (ast1, ast2) => mk_ap("equals", [ast1, ast2]);

    let indet_fplus = (ast1, ast2) => mk_ap("fplus", [ast1, ast2]);
    let indet_fminus = (ast1, ast2) => mk_ap("fminus", [ast1, ast2]);
    let indet_ftimes = (ast1, ast2) => mk_ap("ftimes", [ast1, ast2]);
    let indet_fdivide = (ast1, ast2) => mk_ap("fdivide", [ast1, ast2]);

    let indet_fless_than = (ast1, ast2) => mk_ap("flessThan", [ast1, ast2]);
    let indet_fgreater_than = (ast1, ast2) =>
      mk_ap("fgreaterThan", [ast1, ast2]);
    let indet_fequals = (ast1, ast2) => mk_ap("fequals", [ast1, ast2]);
  };

  module AstPrint = {
    include Inner({
      let name = "AstPrint";
      let path = mk_path("ast_print");
    });

    let print = v => mk_ap("print", [v]);
  };

  module AstMk = {
    include Inner({
      let name = "AstMk";
      let path = mk_path("ast_mk");
    });

    let mk_empty_hole = v => mk_ap("mkEmptyHole", [v]);

    let mk_bool_lit = v => mk_ap("mkBoolLit", [v]);
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
