open Util;
open OptUtil.Syntax;

let buffer_clear = (z: Zipper.t): Zipper.t =>
  switch (z.selection.mode) {
  | Buffer(Unparsed) => Zipper.clear_unparsed_buffer(z)
  | Buffer(Parsed) => z |> Zipper.destroy_selection |> Zipper.regrout(Left)
  | Normal => z
  };

/* Should we clear the assist buffer before performing this action?
 * Clear on every action except Accept (which consumes the buffer).
 * For parsed (LLM) buffers, also preserve on resize actions to
 * permit incremental acceptance token-by-token or line-by-line. */
let should_clear =
    (~settings: Language.CoreSettings.t, ~a: Action.t, z: Zipper.t): bool =>
  settings.assist
  && settings.statics
  && a != Buffer(Accept)
  && !(
       Selection.non_empty_parsed_buffer(z.selection)
       && (
         switch (a) {
         | Select(Resize(Local(_))) => true
         | _ => false
         }
       )
     );

let set_tydi_buffer = (ci: option(Language.Info.t), z: Zipper.t): Zipper.t =>
  switch (TyDi.set_buffer(~ci, z)) {
  | None => z
  | Some(z) => z
  };

/* Set the assist buffer. When inside a Prod context (scaffold applies),
 * completions are generated for both the full Prod type and the
 * per-element type, producing candidates like:
 *   - "args" (matches full Prod, no scaffold needed)
 *   - "g" + scaffold (arg matches element type, scaffold for rest)
 * The best candidate wins based on: element-type matches with scaffold
 * preferred over full-Prod matches without (shorter, more specific). */
let set_assist_buffer =
    (~info_map: Language.Statics.Map.t, z: Zipper.t): Zipper.t => {
  let ci = Indicated.ci_of(z, info_map);
  let scaffold = TyDiScaffold.display(~info_map, z);

  switch (scaffold) {
  | Some(scaffold_seg) =>
    /* Scaffold applies — try completions at per-element type.
     * Extract the element ana from the Prod type. */
    let has_element_ci = ref(false);
    let element_ci =
      switch (ci) {
      | Some(InfoExp(exp_info)) =>
        let ana =
          Language.Typ.weak_head_normalize(exp_info.ctx, exp_info.ana);
        switch (Language.Typ.term_of(TyDiScaffold.unwrap_parens(ana))) {
        | Prod(tys) when List.length(tys) >= 2 =>
          /* Count existing commas to determine current element index */
          let scoped_l = TyDiScaffold.inner_left_siblings(z);
          let existing =
            TyDiScaffold.count_commas_in(scoped_l)
            + TyDiScaffold.count_commas_in(snd(z.relatives.siblings));
          switch (List.nth_opt(tys, existing)) {
          | Some(element_ty) =>
            has_element_ci := true;
            Some(
              Language.Info.InfoExp({
                ...exp_info,
                ana: element_ty,
              }),
            );
          | None => ci
          };
        | _ => ci
        };
      | _ => ci
      };
    /* Try completion at element type (e.g., ar → arg : String) */
    let z_element = set_tydi_buffer(element_ci, z);
    let element_completion = TyDi.get_unparsed_buffer(z_element);
    /* Try completion at full Prod type (e.g., ar → args : (S,S,S)) */
    let z_full = set_tydi_buffer(ci, z);
    let full_completion = TyDi.get_unparsed_buffer(z_full);
    switch (element_completion, full_completion) {
    | (Some(elem_text), Some(full_text)) =>
      /* Both match. Prefer element + scaffold if element completion
       * is shorter (more specific match). Otherwise use full (no scaffold). */
      if (String.length(elem_text) <= String.length(full_text)) {
        let content = TyDi.mk_unparsed_buffer(elem_text) @ scaffold_seg;
        Zipper.set_buffer(z, ~content, ~mode=Unparsed);
      } else {
        z_full;
      }
    | (Some(elem_text), None) =>
      /* Only element completion — use it with scaffold */
      let content = TyDi.mk_unparsed_buffer(elem_text) @ scaffold_seg;
      Zipper.set_buffer(z, ~content, ~mode=Unparsed);
    | (None, Some(_)) when has_element_ci^ =>
      /* Element ci exists but no element completion. Two cases:
       * - Token is a valid variable at element type (exact match
       *   suppression, e.g., arg : String) → scaffold only
       * - No match at element type → fall through to full-Prod */
      switch (TyDi.token_to_left(z)) {
      | Some(tok)
          when
            TyDiScaffold.completion_would_suppress(
              ~completion_text="",
              ~info_map,
              z,
            )
            == false
            /* Check if token is a valid var at element type */
            && {
              let element_ana =
                switch (element_ci) {
                | Some(Language.Info.InfoExp({ana, _})) => Some(ana)
                | _ => None
                };
              switch (element_ana) {
              | Some(element_ty) =>
                let ctx = Language.Info.ctx_of(Option.get(ci));
                switch (Language.Ctx.lookup_var(ctx, tok)) {
                | Some({typ, _}) =>
                  Language.Typ.is_consistent(ctx, typ, element_ty)
                | None => false
                };
              | None => false
              };
            } =>
        /* Token is valid at element type — scaffold only */
        TyDiScaffold.set(~info_map, z)
      | _ =>
        /* No element match — try full-Prod completion */
        z_full
      }
    | (None, Some(_)) =>
      /* No element ci — use full-Prod match */
      z_full
    | (None, None) =>
      /* No completion — scaffold only */
      TyDiScaffold.set(~info_map, z)
    };
  | None =>
    /* No scaffold context — plain completion */
    let z_with_completion = set_tydi_buffer(ci, z);
    switch (TyDi.get_unparsed_buffer(z_with_completion)) {
    | Some(_) => z_with_completion
    | None => z
    };
  };
};

let set_llm_buffer = (z: Zipper.t, response: string): Zipper.t =>
  switch (
    {
      //TODO: Check for incomplete syntax, report errors
      let+ res = Parser.to_zipper(response);
      Zipper.zip(res);
    }
  ) {
  | None => z
  | Some(content) => Zipper.set_buffer(z, ~content, ~mode=Parsed)
  };

/* For scaffold buffers, extract the text to emit on Tab.
 * Emits one "chunk" at a time for progressive acceptance.
 * The insertable text includes formatting whitespace, so each
 * chunk includes any trailing space after a comma.
 *
 * - If the insertable starts with a label (e.g. "x=, ..."):
 *   emit just the label prefix "x=" so the user can fill in the value
 * - Otherwise: emit up to and including the first comma and its
 *   trailing space (e.g. ", " not just ",")
 *
 * e.g. ", ?"         → insertable ", "    → emit ", "
 *      ", ?, , ?"    → insertable ", , "  → emit ", "
 *      "x=?, , "     → insertable "x=, "  → emit "x="
 *      ", y=?"       → insertable ", y="  → emit ", "  */
let scaffold_emit_text = (content: Segment.t): string => {
  let insertable = TyDiScaffold.insertable(content);
  let len = String.length(insertable);
  /* Skip leading whitespace to find the first structural character */
  let rec skip_space = (i: int): int =>
    if (i >= len) {
      i;
    } else if (insertable.[i] == ' ') {
      skip_space(i + 1);
    } else {
      i;
    };
  let first_nonspace = skip_space(0);
  /* If there's leading whitespace (formatting fixup for a user-typed
   * comma without trailing space), emit just the space so the caret
   * lands right after it — ready for the user to type the next arg.
   * The remaining scaffold regenerates on the next cycle. */
  if (first_nonspace > 0) {
    String.sub(insertable, 0, first_nonspace);
  } else {
    /* No leading space — check for label prefix or comma chunk */
    /* Check if the insertable starts with a label prefix (chars before '=').
     * If so, emit just the label prefix (up to and including '='). */
    let rec find_eq_before_comma = (i: int): option(int) =>
      if (i >= len) {
        None;
      } else if (insertable.[i] == '=') {
        Some(i + 1);
      } else if (insertable.[i] == ',') {
        None; /* Comma comes before any '=' */
      } else {
        find_eq_before_comma(i + 1);
      };
    switch (find_eq_before_comma(0)) {
    | Some(end_pos) => String.sub(insertable, 0, end_pos)
    | None =>
      /* Emit up to and including the first comma plus any
       * trailing space after it */
      let rec find_comma = (i: int): int =>
        if (i >= len) {
          len;
        } else if (insertable.[i] == ',') {
          /* Include trailing space after comma if present */
          let after = i + 1;
          if (after < len && insertable.[after] == ' ') {
            after + 1;
          } else {
            after;
          };
        } else {
          find_comma(i + 1);
        };
      let end_pos = find_comma(0);
      String.sub(insertable, 0, end_pos);
    };
  };
};

let buffer_accept = (z: Zipper.t): option(Zipper.t) =>
  switch (z.selection.mode) {
  | Normal => None
  | Buffer(Parsed) => Some(Zipper.directional_unselect(Right, z))
  | Buffer(Unparsed) when TyDiScaffold.is_scaffold(z) =>
    /* Scaffold buffer: emit one chunk progressively.
     * The scaffold segment includes formatting whitespace (spaces
     * after commas), so insertable/emit_text preserves it — no
     * post-hoc space insertion needed. */
    let to_emit = scaffold_emit_text(z.selection.content);
    let z = Zipper.clear_unparsed_buffer(z);
    Parser.to_zipper(~zipper_init=z, to_emit);
  | Buffer(Unparsed) =>
    switch (TyDi.get_unparsed_buffer(z)) {
    | None => None
    | Some(completion)
        when Token.match(Token.regexp(".*\\)::$"), completion) =>
      /* Slightly hacky. There's currently only one genre of completion
       * that creates more than one hole on intial expansion: when on eg
       * 1 :: a|, we suggest "abs( )::" via lookahead. In such a case we
       * want the caret to end up to the left of the first hole, whereas
       * pasting would leave it to the left of the second. Thus we move
       * left to the previous hole. */
      let z = {
        open OptUtil.Syntax;
        let* z = Parser.to_zipper(~zipper_init=z, completion);
        let* z = Move.to_next_grout(Left, z);
        Move.local(ByToken, Left, z);
      };
      z;
    | Some(completion) => Parser.to_zipper(~zipper_init=z, completion)
    }
  };

let go =
    (~ci: option(Language.Info.t), a: Action.buffer, z: Zipper.t)
    : Result.t(Zipper.t, Action.Failure.t) =>
  switch (a) {
  | Set(TyDi) => Ok(set_tydi_buffer(ci, z))
  | Set(LLM(response)) => Ok(set_llm_buffer(z, response))
  | Accept =>
    buffer_accept(z) |> Result.of_option(~error=Action.Failure.CantAccept)
  | Clear => Ok(buffer_clear(z))
  };
