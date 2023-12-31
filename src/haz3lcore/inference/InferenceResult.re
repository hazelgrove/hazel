type status =
  | Solved(ITyp.t)
  | Unsolved(PotentialTypeSet.t);

type t = (ITyp.t, status);

type type_hole_to_solution = Hashtbl.t(Id.t, status);

type unannotated_patterns = list(Id.t);

type exphole_to_sugg_loc_and_solution =
  Hashtbl.t(Id.t, (unannotated_patterns, status));

type global_inference_info = {
  enabled: bool,
  typehole_suggestions: type_hole_to_solution,
  exphole_suggestions: exphole_to_sugg_loc_and_solution,
};

type suggestion('a) =
  | Solvable('a)
  | NestedInconsistency('a)
  | NoSuggestion(reason_for_silence)
and reason_for_silence =
  | SuggestionsDisabled
  | NonTypeHoleId
  | OnlyHoleSolutions
  | InconsistentSet;

let get_suggestion_text_for_id =
    (id: Id.t, global_inference_info: global_inference_info)
    : suggestion(string) =>
  if (global_inference_info.enabled) {
    switch (Hashtbl.find_opt(global_inference_info.typehole_suggestions, id)) {
    | Some(Solved(Unknown(_))) => NoSuggestion(OnlyHoleSolutions)
    | Some(Solved(ityp)) =>
      let typ_to_string = x => Typ.typ_to_string(x, false);
      Solvable(ityp |> ITyp.ityp_to_typ |> typ_to_string);
    | Some(Unsolved([potential_typ])) =>
      NestedInconsistency(
        PotentialTypeSet.string_of_potential_typ(false, potential_typ),
      )
    | Some(Unsolved(_)) => NoSuggestion(InconsistentSet)
    | None =>
      switch (Hashtbl.find_opt(global_inference_info.exphole_suggestions, id)) {
      | Some((_, Unsolved(tys))) when List.length(tys) > 1 =>
        NoSuggestion(InconsistentSet)
      | _ => NoSuggestion(NonTypeHoleId)
      }
    };
  } else {
    NoSuggestion(SuggestionsDisabled);
  };

let hole_nib: Nib.t = {shape: Convex, sort: Any};
let hole_mold: Mold.t = {out: Any, in_: [], nibs: (hole_nib, hole_nib)};

let empty_solutions = (): type_hole_to_solution => Hashtbl.create(20);

let mk_global_inference_info =
    (annotations_enabled, (typ_hole_sugg, exp_hole_sugg)) => {
  enabled: annotations_enabled,
  typehole_suggestions: typ_hole_sugg,
  exphole_suggestions: exp_hole_sugg,
};

let empty_info = (): global_inference_info => {
  enabled: true,
  typehole_suggestions: Hashtbl.create(20),
  exphole_suggestions: Hashtbl.create(20),
};

let rec get_all_pattern_var_neighbors =
        (potential_typ_set: PotentialTypeSet.t): list(Id.t) => {
  switch (potential_typ_set) {
  | [] => []
  | [hd, ...tl] =>
    switch (hd) {
    | Base(BUnknown(ExpHole(PatternVar, p_id))) => [
        p_id,
        ...get_all_pattern_var_neighbors(tl),
      ]
    | _ => get_all_pattern_var_neighbors(tl)
    }
  };
};

let condense =
    (potential_typ_set: MutablePotentialTypeSet.t, key: ITyp.t): status => {
  let (potential_typ_set, err) =
    MutablePotentialTypeSet.snapshot_class(potential_typ_set, key);
  let sorted_potential_typ_set =
    PotentialTypeSet.sort_potential_typ_set(potential_typ_set);

  let hole_filtered_potential_typ_set =
    PotentialTypeSet.filter_unneeded_holes(
      PotentialTypeSet.is_known,
      sorted_potential_typ_set,
    );

  let redundant_var_filtered_potential_typ_set =
    PotentialTypeSet.filter_vars(hole_filtered_potential_typ_set);

  switch (err) {
  | Some(_) => Unsolved(redundant_var_filtered_potential_typ_set)
  | None =>
    let solved_opt =
      PotentialTypeSet.filtered_potential_typ_set_to_typ(
        redundant_var_filtered_potential_typ_set,
      );
    switch (solved_opt) {
    | Some(typ) => Solved(typ)
    | None => Unsolved(redundant_var_filtered_potential_typ_set)
    };
  };
};

let rec prov_to_priority = (prov: Typ.type_provenance): string => {
  switch (prov) {
  | NoProvenance => ""
  | ExpHole(_, id)
  | TypeHole(id) => Id.to_string(id)
  | Matched(_, prov) => prov_to_priority(prov)
  };
};

let rec convert_leftmost_to_priority = (typ: ITyp.t): string => {
  switch (typ) {
  | Int
  | Unit
  | Float
  | String
  | Bool => ""
  | Var(name) => name
  | Unknown(prov) => prov_to_priority(prov)
  | List(elt_typ) => convert_leftmost_to_priority(elt_typ)
  | Arrow(typ_lhs, typ_rhs)
  | Prod(typ_lhs, typ_rhs)
  | Sum(typ_lhs, typ_rhs) =>
    let lhs = convert_leftmost_to_priority(typ_lhs);
    let rhs = convert_leftmost_to_priority(typ_rhs);
    switch (lhs, rhs) {
    | ("", "") => ""
    | ("", _) => rhs
    | _ => lhs
    };
  };
};

let comp_results = ((ty1, _): t, (ty2, _): t): int => {
  let priority1 = convert_leftmost_to_priority(ty1);
  let priority2 = convert_leftmost_to_priority(ty2);
  Stdlib.compare(priority1, priority2);
};

let build_type_hole_to_solution =
    (unfiltered_inference_results: list(t)): type_hole_to_solution => {
  let id_and_status_if_type_hole = (result: t): option((Id.t, status)) => {
    switch (result) {
    | (Unknown(TypeHole(id)), status) => Some((id, status))
    | _ => None
    };
  };

  let elts =
    List.filter_map(id_and_status_if_type_hole, unfiltered_inference_results);
  let new_map = Hashtbl.create(List.length(elts));

  List.iter(((id, annot)) => Hashtbl.add(new_map, id, annot), elts);

  new_map;
};

let build_exphole_to_sugg_loc_and_solution =
    (inference_pts_graph: PTSGraph.t): exphole_to_sugg_loc_and_solution => {
  let acc_results =
      (
        key: ITyp.t,
        mut_potential_typ_set: MutablePotentialTypeSet.t,
        acc: list((Id.t, (list(Id.t), status))),
      )
      : list((Id.t, (list(Id.t), status))) => {
    switch (key) {
    | Unknown(ExpHole(PatternVar, _)) => acc
    | Unknown(ExpHole(_, id)) =>
      let (potential_typ_set, _) =
        MutablePotentialTypeSet.snapshot_class(mut_potential_typ_set, key);
      switch (get_all_pattern_var_neighbors(potential_typ_set)) {
      | [] => acc
      | _ as suggestion_locations => [
          (
            id,
            (suggestion_locations, condense(mut_potential_typ_set, key)),
          ),
          ...acc,
        ]
      };
    | _ => acc
    };
  };

  Hashtbl.fold(acc_results, inference_pts_graph, [])
  |> List.to_seq
  |> Hashtbl.of_seq;
};

let get_desired_solutions =
    (inference_pts_graph: PTSGraph.t)
    : (type_hole_to_solution, exphole_to_sugg_loc_and_solution) => {
  let acc_results =
      (
        key: ITyp.t,
        mut_potential_typ_set: MutablePotentialTypeSet.t,
        acc: list(t),
      )
      : list(t) => {
    [(key, condense(mut_potential_typ_set, key)), ...acc];
  };

  let unsorted_results = Hashtbl.fold(acc_results, inference_pts_graph, []);

  let unfiltered_inference_results =
    List.fast_sort(comp_results, unsorted_results);

  (
    build_type_hole_to_solution(unfiltered_inference_results),
    build_exphole_to_sugg_loc_and_solution(inference_pts_graph),
  );
};
