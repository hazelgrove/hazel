open Virtual_dom.Vdom;
open Haz3lcore;

type cursor_inspector_suggestion =
  | SolvedTypeHole(Typ.t)
  | UnsolvedTypeHole(occurs_failure, list(Typ.t))
  | SolvedExpHole(Id.t, Typ.t)
  | UnsolvedExpHole(occurs_failure, Id.t, list(Typ.t))
  | NoSuggestion
and occurs_failure = bool;

let get_suggestion_ui_for_id =
    (
      ~font_metrics,
      id: Id.t,
      global_inference_info: InferenceResult.global_inference_info,
      colored_ui: bool,
    )
    : (InferenceResult.suggestion(Node.t), InferenceResult.suggestion_source) =>
  if (global_inference_info.enabled) {
    let status_to_suggestion =
        (status: InferenceResult.status): InferenceResult.suggestion(Node.t) =>
      switch (status) {
      | Solved(Unknown(_)) => NoSuggestion(OnlyHoleSolutions)
      | Solved(ityp) =>
        Solvable(
          ityp
          |> ITyp.ityp_to_typ
          |> Type.view(~font_metrics=Some(font_metrics), ~with_cls=false),
        )
      | Unsolved(occurs, [potential_typ]) =>
        let ptyp_node =
          Type.view_of_potential_typ(
            ~font_metrics,
            ~with_cls=colored_ui,
            false,
            potential_typ,
          );
        occurs ? NoSuggestion(OccursFailed) : NestedInconsistency(ptyp_node);
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

let svg_display_settings =
    (~global_inference_info: InferenceResult.global_inference_info, id: Id.t)
    : option(EmptyHoleDec.hole_svg_style) => {
  // Determines if a hexagon (svg) should be used to represent a type hole, and if so, how it should look
  let (suggestion, source) =
    InferenceResult.get_suggestion_text_for_id(id, global_inference_info);
  let failed_occurs =
    InferenceResult.id_has_failed_occurs(id, global_inference_info);
  switch (source) {
  | ExpHole =>
    switch (suggestion) {
    | Solvable(_)
    | NestedInconsistency(_) =>
      failed_occurs ? Some(ErrorHole) : Some(PromptHole)
    | NoSuggestion(OccursFailed)
    | NoSuggestion(InconsistentSet) => Some(ErrorHole)
    | NoSuggestion(_) => Some(StandardHole)
    }
  | None => Some(StandardHole)
  | TypHole =>
    switch (suggestion) {
    | Solvable(_)
    | NestedInconsistency(_) => None
    | NoSuggestion(OccursFailed)
    | NoSuggestion(InconsistentSet) => Some(ErrorHole)
    | NoSuggestion(_) => Some(StandardHole)
    }
  };
};

let get_cursor_inspect_result =
    (~global_inference_info: InferenceResult.global_inference_info, id: Id.t)
    : cursor_inspector_suggestion =>
  if (global_inference_info.enabled) {
    switch (Hashtbl.find_opt(global_inference_info.typehole_suggestions, id)) {
    | None =>
      switch (Hashtbl.find_opt(global_inference_info.exphole_suggestions, id)) {
      | Some(([id, ..._], exp_hole_status)) =>
        switch (exp_hole_status) {
        | Unsolved(occurs, potential_typ_set) =>
          UnsolvedExpHole(
            occurs,
            id,
            potential_typ_set
            |> PotentialTypeSet.potential_typ_set_to_ityp_unroll(id)
            |> List.map(ITyp.ityp_to_typ),
          )
        | Solved(ityp) => SolvedExpHole(id, ITyp.ityp_to_typ(ityp))
        }
      | _ => NoSuggestion
      }
    | Some(status) =>
      switch (status) {
      | Unsolved(occurs, potential_typ_set) =>
        UnsolvedTypeHole(
          occurs,
          potential_typ_set
          |> PotentialTypeSet.potential_typ_set_to_ityp_unroll(id)
          |> List.map(ITyp.ityp_to_typ),
        )
      | Solved(ityp) => SolvedTypeHole(ITyp.ityp_to_typ(ityp))
      }
    };
  } else {
    NoSuggestion;
  };

let acceptSuggestionIfAvailable =
    (global_inference_info, zipper, defaultAction) => {
  open Util;
  let suggestion_of_direction = (dir: Direction.t) => {
    open Util.OptUtil.Syntax;
    let dir_to_tup_projector = dir == Left ? fst : snd;
    let+ p =
      zipper
      |> Zipper.sibs_with_sel
      |> Siblings.neighbors
      |> dir_to_tup_projector;
    InferenceResult.get_suggestion_text_for_id(
      Piece.id(p),
      global_inference_info,
    );
  };
  let (sugg_l_opt, sugg_r_opt) = (
    suggestion_of_direction(Left),
    suggestion_of_direction(Right),
  );
  let suggestion_opt =
    switch (sugg_l_opt) {
    | Some((NoSuggestion(_), _))
    | None => sugg_r_opt
    | _ => sugg_l_opt
    };
  switch (suggestion_opt) {
  | Some((Solvable(typ_filling), TypHole))
  | Some((NestedInconsistency(typ_filling), TypHole)) =>
    // question marks (holes) can't be inserted manually, so filter them out
    let join = List.fold_left((s, acc) => s ++ acc, "");
    let no_hole_marks =
      typ_filling
      |> StringUtil.to_list
      |> List.filter(s => s != "?" && s != "!")
      |> join;
    Some(UpdateAction.Paste(no_hole_marks));
  | _ => defaultAction
  };
};
