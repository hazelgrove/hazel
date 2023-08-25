type status =
  | Solved(ITyp.t)
  | Unsolved(PotentialTypeSet.t);

type t = (ITyp.t, status);

type type_hole_to_solution = Hashtbl.t(Id.t, status);

type global_inference_info = {
  enabled: bool,
  solution_statuses: type_hole_to_solution,
  ctx: Infer.Ctx.t,
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
    let status_opt =
      Hashtbl.find_opt(global_inference_info.solution_statuses, id);
    switch (status_opt) {
    | Some(Solved(Unknown(_))) => NoSuggestion(OnlyHoleSolutions)
    | Some(Solved(ityp)) =>
      Solvable(ityp |> ITyp.ityp_to_typ |> Typ.typ_to_string)
    | Some(Unsolved([potential_typ])) =>
      NestedInconsistency(
        PotentialTypeSet.string_of_potential_typ(false, potential_typ),
      )
    | Some(Unsolved(_)) => NoSuggestion(InconsistentSet)
    | None => NoSuggestion(NonTypeHoleId)
    };
  } else {
    NoSuggestion(SuggestionsDisabled);
  };

let hole_nib: Nib.t = {shape: Convex, sort: Any};
let hole_mold: Mold.t = {out: Any, in_: [], nibs: (hole_nib, hole_nib)};

let empty_solutions = (): type_hole_to_solution => Hashtbl.create(20);

let mk_global_inference_info = (enabled, annotations, ctx) => {
  {enabled, solution_statuses: annotations, ctx};
};

let empty_info = (): global_inference_info =>
  mk_global_inference_info(true, empty_solutions(), Infer.Ctx.create());

let get_desired_solutions =
    (inference_results: list(t)): type_hole_to_solution => {
  let id_and_status_if_ast_node = (result: t): option((Id.t, status)) => {
    switch (result) {
    | (Unknown(AstNode(id)), status) => Some((id, status))
    | _ => None
    };
  };

  let elts = List.filter_map(id_and_status_if_ast_node, inference_results);
  let new_map = Hashtbl.create(List.length(elts));

  List.iter(((id, annot)) => Hashtbl.add(new_map, id, annot), elts);

  new_map;
};

let condense =
    (potential_typ_set: MutablePotentialTypeSet.t, key: ITyp.t): status => {
  let (potential_typ_set, err) =
    MutablePotentialTypeSet.snapshot_class(potential_typ_set, key);
  let sorted_potential_typ_set =
    PotentialTypeSet.sort_potential_typ_set(potential_typ_set);

  let filtered_potential_typ_set =
    PotentialTypeSet.filter_unneeded_holes(
      PotentialTypeSet.is_known,
      sorted_potential_typ_set,
    );

  switch (err) {
  | Some(_) => Unsolved(filtered_potential_typ_set)
  | None =>
    let solved_opt =
      PotentialTypeSet.filtered_potential_typ_set_to_typ(
        filtered_potential_typ_set,
      );
    switch (solved_opt) {
    | Some(typ) => Solved(typ)
    | None => Unsolved(filtered_potential_typ_set)
    };
  };
};

let rec prov_to_priority = (prov: Typ.type_provenance): int => {
  switch (prov) {
  | NoProvenance => (-1)
  | SynSwitch(id)
  | AstNode(id) => id
  | Matched(_, prov) => prov_to_priority(prov)
  };
};

let rec convert_leftmost_to_priority = (typ: ITyp.t): int => {
  switch (typ) {
  | Int
  | Unit
  | Float
  | String
  | Bool => (-1)
  | Unknown(prov) => prov_to_priority(prov)
  | List(elt_typ) => convert_leftmost_to_priority(elt_typ)
  | Arrow(typ_lhs, typ_rhs)
  | Prod(typ_lhs, typ_rhs)
  | Sum(typ_lhs, typ_rhs) =>
    let lhs = convert_leftmost_to_priority(typ_lhs);
    let rhs = convert_leftmost_to_priority(typ_rhs);
    switch (lhs, rhs) {
    | ((-1), (-1)) => (-1)
    | ((-1), _) => rhs
    | _ => lhs
    };
  };
};

let comp_results = ((ty1, _): t, (ty2, _): t): int => {
  let priority1 = convert_leftmost_to_priority(ty1);
  let priority2 = convert_leftmost_to_priority(ty2);
  Stdlib.compare(priority1, priority2);
};
