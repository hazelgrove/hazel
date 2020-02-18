open Sexplib;

let exp_of_string = (s: string): UHExp.t =>
  UHExp.t_of_sexp(Sexp.of_string(s));

let fixed_exp_of_string = (s: string): UHExp.t => {
  let e = UHExp.t_of_sexp(Sexp.of_string(s));
  let (e, _, _) = Statics.Exp.fix_and_renumber_holes(Contexts.empty, e);
  e;
};
