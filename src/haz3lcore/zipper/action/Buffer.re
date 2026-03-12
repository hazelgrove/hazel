open Util;
open OptUtil.Syntax;

let buffer_clear = (z: Zipper.t): Zipper.t =>
  switch (z.selection.mode) {
  | Buffer(Unparsed) => Zipper.clear_unparsed_buffer(z)
  | Buffer(Parsed) => z |> Zipper.destroy_selection |> Zipper.regrout(Left)
  | Normal => z
  };

let set_tydi_buffer = (ci: option(Language.Info.t), z: Zipper.t): Zipper.t =>
  switch (TyDi.set_buffer(~ci, z)) {
  | None => z
  | Some(z) => z
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
 * Emits one "chunk" at a time for progressive acceptance:
 *
 * - If the insertable starts with a label (e.g. "x=,..."):
 *   emit just the label prefix "x=" so the user can fill in the value
 * - Otherwise: emit up to and including the first comma
 *
 * e.g. display ", ○"     → insertable ","    → emit ","
 *      display ", ○, ○"  → insertable ",,"   → emit ","
 *      display "x=○, "   → insertable "x=,"  → emit "x="
 *      display ", y=○"   → insertable ",y="  → emit ","  */
let scaffold_emit_text = (display: string): string => {
  let insertable = TyDi.strip_scaffold_display(display);
  let len = String.length(insertable);
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
    /* No leading label — emit up to and including the first comma */
    let rec find_comma = (i: int): int =>
      if (i >= len) {
        len;
      } else if (insertable.[i] == ',') {
        i + 1;
      } else {
        find_comma(i + 1);
      };
    let end_pos = find_comma(0);
    String.sub(insertable, 0, end_pos);
  };
};

let buffer_accept = (z: Zipper.t): option(Zipper.t) =>
  switch (z.selection.mode) {
  | Normal => None
  | Buffer(Parsed) => Some(Zipper.directional_unselect(Right, z))
  | Buffer(Unparsed) =>
    switch (TyDi.get_unparsed_buffer(z)) {
    | None => None
    | Some(display) when TyDi.is_scaffold(display) =>
      /* Scaffold buffer: emit one chunk progressively.
       * Add a trailing space after commas for readability
       * (f(1, ?) instead of f(1,?)), but not after label
       * prefixes (f(x=¦ not f(x= ¦)).
       *
       * When caret is Inner (e.g., inside a string literal),
       * the scaffold renders at the token-level gap (after the
       * string), but the character cursor is inside the token.
       * Tab advances the caret to Outer so the scaffold becomes
       * directly actionable; the next Tab press accepts it. */
      if (z.caret != Outer) {
        /* Clear buffer, then advance caret past current token to
         * exit the string/delimiter. Move.local handles the proper
         * traversal from Inner to the next Outer position. */
        let z = Zipper.clear_unparsed_buffer(z);
        Move.local(ByToken, Right, z);
      } else {
        let to_emit = scaffold_emit_text(display);
        let ends_with_comma =
          String.length(to_emit) > 0
          && to_emit.[String.length(to_emit) - 1] == ',';
        let to_emit = ends_with_comma ? to_emit ++ " " : to_emit;
        let z = Zipper.clear_unparsed_buffer(z);
        Parser.to_zipper(~zipper_init=z, to_emit);
      };
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
