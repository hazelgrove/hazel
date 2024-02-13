// To be used on the data structure where the structure sharing takes place
let structure_share_here:
  ('a => Id.t, 'a => Sexplib.Sexp.t, Sexplib.Sexp.t => 'a) =>
  ('a => Sexplib.Sexp.t, Sexplib.Sexp.t => 'a);

// To be used only on the root of the data structure currently being serialized
let structure_share_in:
  ('a => Sexplib.Sexp.t, Sexplib.Sexp.t => 'a) =>
  ('a => Sexplib.Sexp.t, Sexplib.Sexp.t => 'a);
