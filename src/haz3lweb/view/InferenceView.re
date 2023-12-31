open Virtual_dom.Vdom;
open Haz3lcore;

type cursor_inspector_suggestion =
  | SolvedTypeHole(Typ.t)
  | UnsolvedTypeHole(list(Typ.t))
  | SolvedExpHole(Id.t, Typ.t)
  | UnsolvedExpHole(Id.t, list(Typ.t))
  | NoSuggestion;

let get_suggestion_ui_for_id =
    (
      ~font_metrics,
      id: Id.t,
      global_inference_info: InferenceResult.global_inference_info,
      colored_ui: bool,
    )
    : InferenceResult.suggestion(Node.t) =>
  if (global_inference_info.enabled) {
    let status_opt =
      Hashtbl.find_opt(global_inference_info.typehole_suggestions, id);
    switch (status_opt) {
    | Some(Solved(Unknown(_))) => NoSuggestion(OnlyHoleSolutions)
    | Some(Solved(ityp)) =>
      Solvable(
        ityp
        |> ITyp.ityp_to_typ
        |> Type.view(~font_metrics=Some(font_metrics), ~with_cls=false),
      )
    | Some(Unsolved([potential_typ])) =>
      let ptyp_node =
        Type.view_of_potential_typ(
          ~font_metrics,
          ~with_cls=colored_ui,
          false,
          potential_typ,
        );
      NestedInconsistency(ptyp_node);
    | Some(Unsolved(_)) => NoSuggestion(InconsistentSet)
    | None =>
      switch (Hashtbl.find_opt(global_inference_info.exphole_suggestions, id)) {
      | Some((_, Unsolved(typs))) when List.length(typs) > 1 =>
        NoSuggestion(InconsistentSet)
      | _ => NoSuggestion(NonTypeHoleId)
      }
    };
  } else {
    NoSuggestion(SuggestionsDisabled);
  };

let svg_display_settings =
    (~global_inference_info: InferenceResult.global_inference_info, id: Id.t)
    : (bool, bool) => {
  // Determines if a hexagon (svg) should be used to represent a type hole, and if so, how it should look
  let (show_svg, is_unsolved) =
    switch (
      InferenceResult.get_suggestion_text_for_id(id, global_inference_info)
    ) {
    | Solvable(_) => (false, false)
    | NestedInconsistency(_) => (false, true)
    | NoSuggestion(SuggestionsDisabled)
    | NoSuggestion(OnlyHoleSolutions) => (true, false)
    | NoSuggestion(NonTypeHoleId) => (true, false)
    | NoSuggestion(InconsistentSet) => (true, true)
    };
  (show_svg, is_unsolved);
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
        | Unsolved(potential_typ_set) =>
          UnsolvedExpHole(
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
      | Unsolved(potential_typ_set) =>
        UnsolvedTypeHole(
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
