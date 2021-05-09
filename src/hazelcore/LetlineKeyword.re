[@deriving sexp]
type t =
  | Let
  | And;

/*  sexp_of_t : t -> Sexplib.Sexp.t
    t_of_sexp : Sexplib.Sexp.t -> t  */

// print_endline(Sexplib.Sexp.to_string(sexp_of_t(x)))
