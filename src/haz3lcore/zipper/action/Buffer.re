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

/* Set the assist buffer using the unified suggest_assist entry point.
 * All completion + scaffold logic (element-type completions, suppression,
 * candidate ranking) lives in TyDi.suggest_assist. This function just
 * sets the result on the zipper, handling grout stripping for scaffold. */
let set_assist_buffer =
    (~info_map: Language.Statics.Map.t, z: Zipper.t): Zipper.t =>
  switch (TyDi.suggest_assist(~info_map, z)) {
  | None => z
  | Some(content) =>
    if (TyDiScaffold.has_structural(content)) {
      /* Scaffold content needs grout stripping at buffer edges */
      TyDiScaffold.set(
        ~info_map,
        ~content,
        z,
      );
    } else {
      Zipper.set_buffer(z, ~content, ~mode=Unparsed);
    }
  };

let set_llm_buffer = (z: Zipper.t, response: string): Zipper.t =>
  switch (
    {
      //TODO: Check for incomplete syntax, report errors
      let+ res = Parser.to_zipper(response, ~root=Exp);
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
    Parser.to_zipper(~root=Exp, ~zipper_init=z, to_emit);
  | Buffer(Unparsed) =>
    switch (TyDi.get_unparsed_buffer(z)) {
    | None => None
    | Some(completion) =>
      /* Drop the unparsed buffer before handing the completion to
       * Parser.to_zipper. Parser.to_zipper threads characters through
       * Insert.go, which treats a non-empty selection as something
       * to wrap when the first char is an opening delimiter. Leaving
       * the buffer in the selection would cause e.g. a `(` completion
       * to wrap the comment-formatted buffer content instead of
       * replacing it. */
      let z = Zipper.clear_unparsed_buffer(z);
      if (Token.match(Token.regexp(".*\\)::$"), completion)) {
        /* Slightly hacky. There's currently only one genre of completion
         * that creates more than one hole on initial expansion: when on eg
         * 1 :: a|, we suggest "abs( )::" via lookahead. In such a case we
         * want the caret to end up to the left of the first hole, whereas
         * pasting would leave it to the left of the second. Thus we move
         * left to the previous hole. */
        let* z = Parser.to_zipper(~root=Exp, ~zipper_init=z, completion);
        let* z = Move.to_next_grout(Left, z);
        Move.local(ByToken, Left, z);
      } else {
        Parser.to_zipper(~root=Exp, ~zipper_init=z, completion);
      };
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
