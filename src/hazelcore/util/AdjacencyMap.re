module Make = (M: Sexpable.Map, S: Sexpable.Set) => {
  include M;

  type t = M.t(S.t);

  type binding = (M.key, S.t);

  let sexp_of_t = (adj: t): Sexplib.Sexp.t =>
    adj |> M.sexp_of_t(S.sexp_of_t);

  let t_of_sexp = (sexp: Sexplib.Sexp.t): t =>
    sexp |> M.t_of_sexp(S.t_of_sexp);

  let add = (x: M.key, y: S.elt, adj: t): t => {
    let insert = y =>
      fun
      | None => Some(S.singleton(y))
      | Some(ys) => Some(ys |> S.add(y));
    adj |> M.update(x, insert(y));
  };

  let find_sources = (y: S.elt, adj: t): list(M.key) =>
    adj
    |> bindings
    |> List.filter_map(((x, ys)) => ys |> S.mem(y) ? Some(x) : None);

  let remove = (x: M.key, y: S.elt, adj: t): t =>
    adj |> M.update(x, Option.map(S.remove(y)));

  let remove_source: (M.key, t) => t = M.remove;

  let remove_target = (y: S.elt, adj: t): t =>
    adj
    |> find_sources(y)
    |> List.fold_left((adj, x) => adj |> remove(x, y), adj);
};
