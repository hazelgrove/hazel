open Grain;

module type M = {
  let name: var;
  let path: var;
};

module Make = (M: M) => {
  include Lib.Make({
    let name = M.name;
    let path = ImportStd(M.path);
  });
};

let mk_path = path => Filename.concat("hazel", path);

module Rt = {
  let mk_path = path => mk_path(Filename.concat("rt", path));

  module Ast = {
    include Make({
      let name = "Ast";
      let path = mk_path("ast");
    });

    module HTyp = {
      let name = ident("HTyp");

      let hole = mk_var("Hole");
      let int = mk_var("Int");
      let float = mk_var("Float");
      let bool = mk_var("Bool");
      let arrow = (t1, t2) => ctor2("Arrow", t1, t2);
      let sum = (t1, t2) => ctor2("Sum", t1, t2);
      let prod = ts => ctor("Prod", ts);
      let list = t => ctor1("List", t);
    };

    module HoleReason = {
      let name = ident("HoleReason");
      let type_inconsistent = ident("TypeInconsistent");
      let wrong_length = ident("WrongLength");
    };

    let empty_hole = (u, i, sigma) => ctor3("EmptyHole", u, i, sigma);

    let non_empty_hole = (reason, u, i, sigma, e) =>
      ctor("NonEmptyHole", [reason, u, i, sigma, e]);

    let cast = (e, t1, t2) => ctor3("Cast", e, t1, t2);
  };

  module AstSexp = {
    include Make({
      let name = "AstSexp";
      let path = mk_path("ast_sexp");
    });

    let sexp_of_ast = ap1("sexpOfAst");
  };

  module AstOps = {
    include Make({
      let name = "AstOps";
      let path = mk_path("ast_ops");
    });

    let indet_and = ap2("and");
    let indet_or = ap2("or");

    let indet_plus = ap2("plus");
    let indet_minus = ap2("minus");
    let indet_times = ap2("times");
    let indet_divide = ap2("divide");

    let indet_less_than = ap2("lessThan");
    let indet_greater_than = ap2("greaterThan");
    let indet_equals = ap2("equals");

    let indet_fplus = ap2("fplus");
    let indet_fminus = ap2("fminus");
    let indet_ftimes = ap2("ftimes");
    let indet_fdivide = ap2("fdivide");

    let indet_fless_than = ap2("flessThan");
    let indet_fgreater_than = ap2("fgreaterThan");
    let indet_fequals = ap2("fequals");
  };

  module AstPrint = {
    include Make({
      let name = "AstPrint";
      let path = mk_path("ast_print");
    });

    let print = ap1("print");
  };

  module AstMk = {
    include Make({
      let name = "AstMk";
      let path = mk_path("ast_mk");
    });

    let mk_empty_hole = ap1("mkEmptyHole");

    let mk_bool_lit = ap1("mkBoolLit");
  };

  module Sum = {
    include Make({
      let name = "Sum";
      let path = mk_path("sum");
    });

    let inj_l = e => ctor1("L", e);
    let inj_r = e => ctor1("R", e);

    let inj_l_pat = p => pat_ctor1("L", p);
    let inj_r_pat = p => pat_ctor1("R", p);
  };
};
