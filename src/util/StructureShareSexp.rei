[@deriving sexp]
type structure_shared = (Sexplib0.Sexp.t, Id.Map.t(Sexplib0.Sexp.t));
let structure_share_sexp_of_t:
  ('a => Id.t, 'a => Sexplib0.Sexp.t, 'a) => Sexplib0.Sexp.t;
let structure_share_t_of_sexp: (Sexplib0.Sexp.t => 'a, Sexplib0.Sexp.t) => 'a;
let structure_share_in:
  ('a => Sexplib0.Sexp.t, Sexplib0.Sexp.t => 'a) =>
  ('a => Sexplib0.Sexp.t, Sexplib0.Sexp.t => 'a);
