// TODO:
// * I just kind aput stuff for the prod types, they probably need actual thought

include ProvMap;
type data = (Prov.t, list(Prov.t), PossibleTypeSet.t);
type data_elem = UnionFind.elem(data);
type t = ProvMap.t(data_elem);

let lookup = (p: StringProv.t, m: t): data_elem => {
  let res = ProvMap.find(p, m);
  res;
};
let lookup_prov = (p: Prov.t, m: t): data_elem =>
  lookup(StringProv.of_prov(p), m);
// let lookup_get = (p: Prov.t, m: t): data =>
//   UnionFind.get(lookup_prov(p, m));

let merge_data = ((p, l1, l2): data, (_, l3, l4): data): data => {
  (p, l1 @ l3, PossibleTypeSet.union(l2, l4));
};

let update_data = (p: Prov.t, d: data, m: t): unit => {
  let elem_p = lookup_prov(p, m);
  UnionFind.set(elem_p, merge_data(UnionFind.get(elem_p), d));
};

let add_if_absent = (p: Prov.t, m: t): t =>
  if (!ProvMap.mem(StringProv.of_prov(p), m)) {
    ProvMap.add(
      StringProv.of_prov(p),
      UnionFind.make((p, [], PossibleTypeSet.empty)),
      m,
    );
  } else {
    m;
  };

let rec provs_in_typ = (~include_prov=_ => true, t: Typ.t): list(Prov.t) => {
  switch (t |> Typ.term_of) {
  | Unknown(p) when Prov.is_identified(p) && include_prov(p) => [p]
  | Unknown(_) => []
  | Atom(_) => []
  | Arrow(t1, t2) =>
    provs_in_typ(~include_prov, t1) @ provs_in_typ(~include_prov, t2)
  | Prod(args) =>
    List.map(t => provs_in_typ(~include_prov, t), args) |> List.flatten
  | Label(_) => []
  | TupLabel(label, arg) =>
    provs_in_typ(~include_prov, label) @ provs_in_typ(~include_prov, arg)
  | List(elt) => provs_in_typ(~include_prov, elt)
  | Sum(_) => []
  | Parens(term) => provs_in_typ(~include_prov, term)
  | Rec(_, ty) => provs_in_typ(~include_prov, ty)
  | Poly(_, ty) => provs_in_typ(~include_prov, ty)
  | Var(_) => []
  | ProofOf(_) => []
  | ExplicitNonlabel => []
  | ProdProjection(ty1, ty2)
  | ProdExtension(ty1, ty2) =>
    provs_in_typ(~include_prov, ty1) @ provs_in_typ(~include_prov, ty2)
  };
};

let unsolved_provs_in_typ = (t: Typ.t, sm: SolutionMap.t) => {
  let filter = (p: Prov.t) => !SolutionMap.mem(StringProv.of_prov(p), sm);
  provs_in_typ(t, ~include_prov=filter);
};

let update_prov_map_of_constramnot =
    (c: CanonicalConstramnot.t, prov_map: t, sol_map: SolutionMap.t): t => {
  switch (c) {
  // a provenance is directly constrained to another provenance, in which
  // case once solved, both of them should have identical solutions, so
  // they are merged
  | Con(prov, {term: Unknown(other_prov), _})
      when
        !(
          SolutionMap.mem(StringProv.of_prov(prov), sol_map)
          || SolutionMap.mem(StringProv.of_prov(other_prov), sol_map)
        ) =>
    let prov_map' =
      add_if_absent(prov, prov_map) |> add_if_absent(other_prov);
    let _ =
      UnionFind.merge(
        merge_data,
        lookup_prov(prov, prov_map'),
        lookup_prov(other_prov, prov_map'),
      );
    prov_map';

  // a provenance is constraint to a type (e.g. ?1 ~ ?2 -> ?3), in which case
  // the provenance should dominate all provenances in the type
  | Con(prov, constrained_typ)
      when !SolutionMap.mem(StringProv.of_prov(prov), sol_map) =>
    let prov_map = add_if_absent(prov, prov_map);

    let provs_in_constrained_typ =
      unsolved_provs_in_typ(constrained_typ, sol_map);
    let prov_map =
      List.fold_left(
        (m, q) => add_if_absent(q, m),
        prov_map,
        provs_in_constrained_typ,
      );

    // the provenances in the type are dominated by prov
    List.iter(
      q => {
        update_data(
          q,
          (Internal |> Prov.anonymous, [prov], PossibleTypeSet.empty),
          prov_map,
        )
      },
      provs_in_constrained_typ,
    );

    update_data(
      prov,
      (
        Internal |> Prov.anonymous,
        [],
        PossibleTypeSet.singleton(constrained_typ),
      ),
      prov_map,
    );
    prov_map;
  | _ => prov_map
  };
};

let of_constramnots =
    (cs: list(CanonicalConstramnot.t), sm: SolutionMap.t): t => {
  List.fold_left(
    (m, c) => update_prov_map_of_constramnot(c, m, sm),
    ProvMap.empty,
    cs,
  );
};

/* finds a dominant provenance, or if there is none, then picks one that
    is cyclic

   An example of dominant provenance is a in: ?a ~ ?L(a) -> ?R(a)
    */
let find_dominant_provs = (m: t): (list(Prov.t), bool) => {
  let dom =
    List.filter_map(
      ((_, d)) => {
        let (p, qs, _) = UnionFind.get(d);
        if (List.is_empty(qs)) {
          Some(p);
        } else {
          None;
        };
      },
      ProvMap.bindings(m),
    );

  if (List.is_empty(dom)) {
    switch (ProvMap.bindings(m)) {
    | [] => ([], true)
    | [(_, d), ..._] =>
      let (p, _, _) = UnionFind.get(d);
      ([p], true);
    };
  } else {
    (dom, false);
  };
};

let solve_prov = (prov: Prov.t, prov_tys_map: t): Solution.t => {
  let (_, _, ts) =
    UnionFind.get(find(StringProv.of_prov(prov), prov_tys_map));
  let ts_list = PossibleTypeSet.to_list(ts);
  List.fold_left(
    Solution.refine_solution(prov),
    Solution.Unknown(Hole(EmptyHole) |> Prov.anonymous) |> Solution.temp,
    ts_list,
  );
};

// let string_of_data = ((_, ps, ts): data): string =>
//   "["
//   ++ String.concat(
//        ", ",
//        List.map(p => StringProv.of_prov(p) |> StringProv.show, ps),
//      )
//   ++ "] | ["
//   ++ String.concat(
//        ", ",
//        List.map(
//          t => t |> Typ.term_of |> TermBase.show_typ_term,
//          PossibleTypeSet.to_list(ts),
//        ),
//      )
//   ++ "]";

// let to_string = (m: t): string => {
//   let f: ((StringProv.t, data_elem)) => string =
//     ((p, d)) =>
//       StringProv.show(p) ++ ": " ++ string_of_data(UnionFind.get(d));
//   let l: list((StringProv.t, data_elem)) = ProvMap.bindings(m);
//   "{" ++ String.concat("\n", List.map(f, l)) ++ "}";
// };
