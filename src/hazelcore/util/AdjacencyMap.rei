module Make:
  (M: Sexpable.Map, S: Sexpable.Set) =>
   {
    include (module type of {
      include M;
    });

    type t = M.t(S.t);

    type binding = (M.key, S.t);

    let sexp_of_t: t => Sexplib.Sexp.t;

    let t_of_sexp: Sexplib.Sexp.t => t;

    let add: (M.key, S.elt, t) => t;

    let remove: (M.key, S.elt, t) => t;

    let find_sources: (S.elt, t) => list(M.key);

    let remove_source: (M.key, t) => t;

    let remove_target: (S.elt, t) => t;
  };
