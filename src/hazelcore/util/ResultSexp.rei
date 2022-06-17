let sexp_of_result:
  ('a => Sexplib.Sexp.t, 'b => Sexplib.Sexp.t, result('a, 'b)) =>
  Sexplib.Sexp.t;

let result_of_sexp:
  (Sexplib.Sexp.t => 'a, Sexplib.Sexp.t => 'b, Sexplib.Sexp.t) =>
  result('a, 'b);
