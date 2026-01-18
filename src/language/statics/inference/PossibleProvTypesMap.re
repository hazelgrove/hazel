// TODO:
// * I just kind aput stuff for the prod types, they probably need actual thought

module type TermInfo = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t;

  let provs_in_term: (Prov.t => bool, t) => list(Prov.t);
};

module TypInfo: TermInfo with type t := Typ.t = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Typ.t;
  let rec provs_in_term = (include_prov: Prov.t => bool, typ: t) => {
    let provs_in_term = provs_in_term(include_prov);
    switch (typ |> Typ.term_of) {
    | Unknown(p) when Prov.is_identified(p) && include_prov(p) => [p]
    | Unknown(_) => []
    | Atom(_) => []
    | Arrow(t1, t2) => provs_in_term(t1) @ provs_in_term(t2)
    | Prod(args) => List.map(t => provs_in_term(t), args) |> List.flatten
    | Label(_) => []
    | TupLabel(label, arg) => provs_in_term(label) @ provs_in_term(arg)
    | List(elt) => provs_in_term(elt)
    | Sum(_) => []
    | Parens(term) => provs_in_term(term)
    | Rec(_, ty) => provs_in_term(ty)
    | Poly(_, ty) => provs_in_term(ty)
    | Var(_) => []
    | ProofOf(_) => []
    | ExplicitNonlabel => []
    | ProdProjection(ty1, ty2)
    | ProdExtension(ty1, ty2) => provs_in_term(ty1) @ provs_in_term(ty2)
    };
  };
};

module TPatInfo: TermInfo with type t := TPat.t = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = TPat.t;
  let rec provs_in_term = (include_prov: Prov.t => bool, tpat: t) => {
    switch (tpat |> IdTagged.term_of) {
    | Unknown(p) when Prov.is_identified(p) && include_prov(p) => [p]
    | Unknown(_) => []
    | Var(_) => []
    };
  };
};

// module type PossibleProvTypesMap = {
// : {
//          type data;
//          type data_elem;
//          type t;

//          let of_constramnots:
//            (
//              list(CanonicalConstramnot.equiv(Solution.SolType.t)),
//              SolutionMap.t(Solution.SolType.t)
//            ) =>
//            t;
//          let find_dominant_provs: t => (list(Prov.t), bool);
//          let lookup: (StringProv.t, t) => data_elem;
//          let lookup_prov: (Prov.t, t) => data_elem;

//          let solve_prov: (Prov.t, t) => Solution.t;
//        }
// };

module Make =
       (
         Solution: Solution.SolutionBase,
         SolutionTermInfo: TermInfo with type t := Solution.SolType.t,
       ) => {
  module PossibleSolutions:
    PossibleSolutionSet.Type with type elt_t := Solution.SolType.t =
    PossibleSolutionSet.Make(Solution.SolType);
  module SolutionMap = SolutionMap.Make(Solution);

  include ProvMap;
  type data = (Prov.t, list(Prov.t), PossibleSolutions.t);
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
    (p, l1 @ l3, PossibleSolutions.union(l2, l4));
  };

  let update_data = (p: Prov.t, d: data, m: t): unit => {
    let elem_p = lookup_prov(p, m);
    UnionFind.set(elem_p, merge_data(UnionFind.get(elem_p), d));
  };

  let add_if_absent = (p: Prov.t, m: t): t =>
    if (!ProvMap.mem(StringProv.of_prov(p), m)) {
      ProvMap.add(
        StringProv.of_prov(p),
        UnionFind.make((p, [], PossibleSolutions.empty)),
        m,
      );
    } else {
      m;
    };

  let unsolved_provs_in_term = (t: Solution.SolType.t, sm: SolutionMap.t) => {
    let filter = (p: Prov.t) => !SolutionMap.mem(StringProv.of_prov(p), sm);
    SolutionTermInfo.provs_in_term(filter, t);
  };

  let update_prov_map_of_constramnot =
      (
        c: CanonicalConstramnot.equiv(Solution.SolType.t),
        prov_map: t,
        sol_map: SolutionMap.t,
      )
      : t => {
    switch (c) {
    // a provenance is directly constrained to another provenance, in which
    // case once solved, both of them should have identical solutions, so
    // they are merged
    | EquivCon(prov, other_prov)
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
    | DominateCon(prov, constrained_term)
        when !SolutionMap.mem(StringProv.of_prov(prov), sol_map) =>
      let prov_map = add_if_absent(prov, prov_map);

      let provs_in_constrained_typ =
        unsolved_provs_in_term(constrained_term, sol_map);
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
            (Internal |> Prov.anonymous, [prov], PossibleSolutions.empty),
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
          PossibleSolutions.singleton(constrained_term),
        ),
        prov_map,
      );
      prov_map;
    | _ => prov_map
    };
  };

  let of_constramnots =
      (
        cs: list(CanonicalConstramnot.equiv(Solution.SolType.t)),
        sm: SolutionMap.t,
      )
      : t => {
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
    let ts_list = PossibleSolutions.to_list(ts);
    List.fold_left(Solution.refine_solution, Solution.anon_unknown, ts_list);
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
};
