include Set.Make(Int);

let sexp_of_t = (set: t): Sexplib.Sexp.t =>
  Sexplib.Std.(set |> elements |> sexp_of_list(sexp_of_int));

let t_of_sexp = (sexp: Sexplib.Sexp.t): t =>
  Sexplib.Std.(sexp |> list_of_sexp(int_of_sexp) |> of_list);
