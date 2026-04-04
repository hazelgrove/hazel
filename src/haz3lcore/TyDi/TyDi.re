/* TyDi: Type-directed assist (completion + scaffold).
 *
 * This is the top-level entry point. It combines text completions
 * (TyDiComplete) and structural scaffold (TyDiScaffold) into a
 * unified suggestion pipeline.
 *
 * External callers should use suggest_assist for buffer content
 * and the re-exported helpers for buffer inspection. */

open Util.OptUtil.Syntax;
open Language;

/* ---- Re-exports from TyDiComplete ---- */

let token_to_left = TyDiComplete.token_to_left;
let mk_unparsed_buffer = TyDiComplete.mk_unparsed_buffer;
let buffer_to_string = TyDiComplete.buffer_to_string;
let get_unparsed_buffer = TyDiComplete.get_unparsed_buffer;
let set_buffer = TyDiComplete.set_buffer;

/* ---- Helpers ---- */

/* Compute the best completion suffix for a given ci and zipper.
 * Returns None if no completion applies (no prefix, exact match, etc.) */
let completion_suffix = (~ci: option(Info.t), z: Zipper.t): option(Token.t) => {
  let* ci = ci;
  let* tok_to_left = TyDiComplete.token_to_left(z);
  let prefix_len = String.length(tok_to_left);
  let* _ = prefix_len >= 1 ? Some() : None;
  let suggestions = TyDiComplete.suggest(ci, z);
  let suggestions =
    suggestions
    |> List.filter(({content, _}: TyDiSuggestion.t) =>
         String.starts_with(~prefix=tok_to_left, content)
       );
  let* _ =
    prefix_len >= TyDiComplete.min_prefix_len || List.length(suggestions) == 1
      ? Some() : None;
  let has_exact_match =
    List.exists(
      ({content, _}: TyDiSuggestion.t) => content == tok_to_left,
      suggestions,
    );
  let* _ = has_exact_match ? None : Some();
  let* top_suggestion = suggestions |> Util.ListUtil.hd_opt;
  TyDiComplete.suffix_of(top_suggestion.content, tok_to_left);
};

/* Check if a token is a valid variable consistent with a type */
let token_is_valid_at = (~ctx: Ctx.t, ~tok: Token.t, ~typ: Typ.t): bool =>
  switch (Ctx.lookup_var(ctx, tok)) {
  | Some({typ: var_typ, _}) =>
    let var_typ = Typ.weak_head_normalize(ctx, var_typ);
    switch (Typ.term_of(var_typ)) {
    | Unknown(_) => false
    | _ => Typ.is_consistent(ctx, var_typ, typ)
    };
  | None => false
  };

/* Compute the per-element ana type when inside a Prod context.
 * Returns (element_ci, element_type) if applicable. */
let element_context =
    (~ci: option(Info.t), z: Zipper.t): option((Info.t, Typ.t)) =>
  switch (ci) {
  | Some(InfoExp(exp_info)) =>
    let ana = Typ.weak_head_normalize(exp_info.ctx, exp_info.ana);
    switch (Typ.term_of(TyDiScaffold.unwrap_parens(ana))) {
    | Prod(tys) when List.length(tys) >= 2 =>
      let scoped_l = TyDiScaffold.inner_left_siblings(z);
      let existing =
        TyDiScaffold.count_commas_in(scoped_l)
        + TyDiScaffold.count_commas_in(snd(z.relatives.siblings));
      switch (List.nth_opt(tys, existing)) {
      | Some(element_ty) =>
        Some((
          InfoExp({
            ...exp_info,
            ana: element_ty,
          }),
          element_ty,
        ))
      | None => None
      };
    | _ => None
    };
  | _ => None
  };

/* ---- Public API ---- */

/* Produce the assist buffer content: the unified entry point for
 * completion + scaffold. Considers both element-type and full-Prod
 * completions when in scaffold context, and returns the best result
 * as a buffer segment ready to set on the zipper.
 *
 * Returns None if no assist applies. */
let suggest_assist =
    (~info_map: Statics.Map.t, z: Zipper.t): option(Segment.t) => {
  /* Precondition: no existing buffer or selection */
  let* _ =
    switch (z.selection.mode) {
    | Buffer(Unparsed | Parsed) => Some()
    | Normal when Selection.is_empty(z.selection) => Some()
    | Normal => None
    };
  let ci = Indicated.ci_of(z, info_map);
  let scaffold = TyDiScaffold.display(~info_map, z);

  switch (scaffold) {
  | Some(scaffold_seg) =>
    /* Scaffold context — consider completions at both types */
    let elem_ctx = element_context(~ci, z);
    let elem_ci = Option.map(fst, elem_ctx);
    let elem_ty = Option.map(snd, elem_ctx);
    let element_suffix = completion_suffix(~ci=elem_ci, z);
    let full_suffix = completion_suffix(~ci, z);

    switch (element_suffix, full_suffix) {
    | (Some(elem_text), Some(full_text)) =>
      /* Both match. Prefer element + scaffold if shorter. */
      if (String.length(elem_text) <= String.length(full_text)) {
        Some(TyDiComplete.mk_unparsed_buffer(elem_text) @ scaffold_seg);
      } else {
        Some(TyDiComplete.mk_unparsed_buffer(full_text));
      }
    | (Some(elem_text), None) =>
      /* Only element match — completion + scaffold */
      Some(TyDiComplete.mk_unparsed_buffer(elem_text) @ scaffold_seg)
    | (None, _) =>
      /* No element completion. If token is a valid variable at
       * the element type (exact match suppression), show scaffold
       * only. Otherwise try full-Prod completion. */
      let element_suppressed =
        switch (TyDiComplete.token_to_left(z), elem_ty) {
        | (Some(tok), Some(ety)) =>
          let ctx = Info.ctx_of(Option.get(ci));
          token_is_valid_at(~ctx, ~tok, ~typ=ety);
        | _ => false
        };
      if (element_suppressed) {
        Some(scaffold_seg);
      } else {
        switch (full_suffix) {
        | Some(full_text) =>
          Some(TyDiComplete.mk_unparsed_buffer(full_text))
        | None => Some(scaffold_seg)
        };
      };
    };
  | None =>
    /* No scaffold — plain completion */
    let* suffix = completion_suffix(~ci, z);
    Some(TyDiComplete.mk_unparsed_buffer(suffix));
  };
};
