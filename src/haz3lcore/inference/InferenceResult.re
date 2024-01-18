type status =
  | Solved(ITyp.t)
  | Unsolved(occurs_failure, PotentialTypeSet.t)
and occurs_failure = bool;

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
  | NotSuggestableHoleId
  | OnlyHoleSolutions
  | OccursFailed
  | InconsistentSet;

type suggestion_source =
  | ExpHole
  | TypHole
  | None;

let id_has_failed_occurs =
    (id: Id.t, global_inference_info: global_inference_info): bool => {
  switch (
    Hashtbl.find_opt(global_inference_info.typehole_suggestions, id),
    Hashtbl.find_opt(global_inference_info.exphole_suggestions, id),
  ) {
  | (Some(Unsolved(true, _)), _) => true
  | (_, Some((_, Unsolved(true, _)))) => true
  | _ => false
  };
};

let get_suggestion_text_for_id =
    (id: Id.t, global_inference_info: global_inference_info)
    : (suggestion(string), suggestion_source) =>
  if (global_inference_info.enabled) {
    let status_to_suggestion = status =>
      switch (status) {
      | Solved(Unknown(_)) => NoSuggestion(OnlyHoleSolutions)
      | Solved(ityp) =>
        let typ_to_string = x => Typ.typ_to_string(x, false);
        Solvable(ityp |> ITyp.ityp_to_typ |> typ_to_string);
      | Unsolved(occurs, [potential_typ]) =>
        occurs
          ? NoSuggestion(OccursFailed)
          : NestedInconsistency(
              PotentialTypeSet.string_of_potential_typ(false, potential_typ),
            )
      | Unsolved(_) => NoSuggestion(InconsistentSet)
      };
    switch (Hashtbl.find_opt(global_inference_info.typehole_suggestions, id)) {
    | Some(status) => (status_to_suggestion(status), TypHole)
    | None =>
      switch (Hashtbl.find_opt(global_inference_info.exphole_suggestions, id)) {
      | Some((_, status)) => (status_to_suggestion(status), ExpHole)
      | None => (NoSuggestion(NotSuggestableHoleId), None)
      }
    };
  } else {
    (NoSuggestion(SuggestionsDisabled), None);
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
        (potential_typ_set: PotentialTypeSet.t, desired_exp_hole_id: Id.t)
        : list(Id.t) => {
  switch (potential_typ_set) {
  | [] => []
  | [hd, ...tl] =>
    switch (hd) {
    | Base(BUnknown(ExpHole(PatternVar(bound_exp), p_id)))
        when bound_exp == desired_exp_hole_id => [
        p_id,
        ...get_all_pattern_var_neighbors(tl, desired_exp_hole_id),
      ]
    | _ => get_all_pattern_var_neighbors(tl, desired_exp_hole_id)
    }
  };
};

let condense =
    (potential_typ_set: MutablePotentialTypeSet.t, key: ITyp.t): status => {
  let (potential_typ_set, err) =
    MutablePotentialTypeSet.snapshot_class(potential_typ_set, key);
  let sorted_potential_typ_set =
    PotentialTypeSet.sort_potential_typ_set(potential_typ_set);

  // When prepping solutions, it often isn't useful to have holes or type variables
  // as suggestions when alternatives exist. This method filters 'not useful' suggestions.
  let filter_redundant_nodes = occurs => {
    // Always filter holes eagerly where alternatives exist when populating suggestions
    // as even if occurs failure happens, suggesting ? will never be useful
    // and ? and A -> ? are consistent with one another
    let filter_redundant_holes =
      PotentialTypeSet.filter_unneeded_nodes(PotentialTypeSet.is_unknown);

    // Filter all type vars when alternatives exist, as their assigned value must be in the pts
    // unless an occurs failure has occurred- in that case, filter all but one variable
    // Why: 
    //  Unlike holes, consistency of a type variable and another nonhole type requires equality to it
    //    * Suppose the occurs check failed and our solution S = {a, {b} -> S, c}.
    //      a and {b} -> S cannot be equal as doing so triggers unbound expansion.
    //      Therefore, both must different possible suggestions.
    //    * Suppose the occurs check was passed and our solution S = {c, {b} -> {a}}.
    //      c and {b} -> {a} can be treated as equivalent without issue 
    //      (this is barring transitively generated inconsistencies which may on their own cause multiple suggestions)
    //      We choose to suggest {b} -> {a} as it is more specific (and therefore arguably more helpful)
    let filter_redundant_vars =
      occurs
        ? PotentialTypeSet.filter_unneeded_nodes_class(
            PotentialTypeSet.is_var,
            false,
          )
        : PotentialTypeSet.filter_unneeded_nodes(PotentialTypeSet.is_var);

    sorted_potential_typ_set |> filter_redundant_holes |> filter_redundant_vars;
  };

  switch (err) {
  | Some(Occurs) => Unsolved(true, filter_redundant_nodes(true))
  | None =>
    let filtered_pts = filter_redundant_nodes(false);
    let solved_opt =
      PotentialTypeSet.filtered_potential_typ_set_to_typ(filtered_pts);
    switch (solved_opt) {
    | Some(typ) => Solved(typ)
    | None => Unsolved(false, filtered_pts)
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
    | Unknown(ExpHole(PatternVar(_), _)) => acc
    | Unknown(ExpHole(_, id)) =>
      let (potential_typ_set, _) =
        MutablePotentialTypeSet.snapshot_class(mut_potential_typ_set, key);
      switch (get_all_pattern_var_neighbors(potential_typ_set, id)) {
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
