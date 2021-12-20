open Sexplib;

let string_of_exp = e => {
  Sexp.to_string(UHExp.sexp_of_t(e));
};

let string_of_zexp = e => {
  Sexp.to_string(ZExp.sexp_of_t(e));
};

let exp_of_string = (s: string): UHExp.t =>
  UHExp.t_of_sexp(Sexp.of_string(s));

let fixed_exp_of_string = (s: string): UHExp.t => {
  let e = UHExp.t_of_sexp(Sexp.of_string(s));
  let (e, _, _) = Statics_Exp.fix_and_renumber_holes(Contexts.empty, e);
  e;
};
