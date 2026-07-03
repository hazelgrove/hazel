open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type state = {
  zipper: Zipper.t,
  col_target: option(int),
};

let return = (error: Action.Failure.t, z: option(Zipper.t)) =>
  Result.of_option(~error, z);

let go =
    (
      ~settings: Language.CoreSettings.t,
      ~statics: CachedStatics.t,
      ~syntax: CachedSyntax.t,
      ~root,
      a: Action.t,
      {zipper: z, col_target}: state,
    )
    : Action.Result.t(Zipper.t) => {
  /* Splice switcheroo: when the caret is inside a splice, swap that
   * splice's cached data into main_splice so geometry-dependent actions
   * (point/vertical movement, drag selection, hole goals) resolve in
   * the splice's own coordinate frame. The term_data/projectors/splices
   * fields on [syntax] are global and unaffected by the swap.
   * [syntax_main] keeps the unswapped main frame for the Point cases
   * below, which arrive from the main editor in main coordinates. */
  let syntax_main = syntax;
  let syntax =
    switch (Zipper.splice_context(z)) {
    | Some(id) =>
      switch (CachedSyntax.splice_opt(id, syntax)) {
      | Some(splice) => {
          ...syntax,
          main_splice: splice,
        }
      | None => syntax
      }
    | None => syntax
    };
  let maybe_reassoc = settings.deep_reassociate ? Reassociate.go : Fun.id;
  /* Paste is a rare bulk edit that can leave incomplete delimiter forms
     anywhere in the pasted region, so it gets the thorough (full-relatives)
     reassociation guard rather than the cheap caret-local one. */
  let maybe_reassoc_thorough =
    settings.deep_reassociate ? Reassociate.go_thorough : Fun.id;
  switch (a) {
  | Introduce =>
    Select.current_term(
      syntax.term_data,
      ~defs_exclude_bodies=false,
      ~case_rules=false,
      z,
    )
    |> OptUtil.and_then(
         Introduce.introduce(Indicated.ci_of(z, statics.info_map)),
       )
    |> return(CantIntroduce)
  | Paste(clipboard) =>
    switch (Parser.try_segment_paste(clipboard, z, ~root)) {
    | Some(z) => Ok(maybe_reassoc_thorough(z))
    | None =>
      (
        Parser.can_fast_paste(clipboard, z, ~root)
          ? Parser.fast_paste(clipboard, z, ~root)
          : Parser.to_zipper(~root, ~zipper_init=z, clipboard)
      )
      |> Option.map(maybe_reassoc_thorough)
      |> return(CantPaste)
    }
  | Cut =>
    /* System clipboard handling is done in Page.view handlers */
    Destruct.go(Left, z, ~root) |> return(Cant_destruct)
  | Copy =>
    /* System clipboard handling itself is done in Page.view handlers.
     * This doesn't change state but is included here for logging purposes */
    Ok(z)
  | Reparse =>
    /* This serializes the current editor to text, resets the current
       editor, and then deserializes. It is intended as a (tactical)
       nuclear option for weird backpack states */
    Parser.to_zipper(~root, Printer.of_zipper(~holes="", ~indent="", z))
    |> return(CantReparse)
  | PrettyPrint =>
    /* Remember which tile the caret was on so we can restore the
       caret position after prettifying. Pretty-printing preserves
       tile IDs (it only rearranges whitespace), so the same piece
       can be located in the new segment. Falls back to the default
       caret position (end of document) if there is no indicated
       tile or the ID can't be located. */
    let prev_id = Indicated.index(z);
    let seg = Zipper.unselect_and_zip(z);
    let pretty = PrettySegment.prettify(seg);
    let z = {
      ...Zipper.unzip(pretty),
      refractors: z.refractors,
    };
    let z =
      switch (prev_id) {
      | Some(id) =>
        switch (Move.jump_to_id_indicated(z, id)) {
        | Some(z') => z'
        | None => z
        }
      | None => z
      };
    Some(z) |> return(CantReparse);
  | Buffer(a) =>
    Buffer.go(~ci=Indicated.ci_for_completion(z, statics.info_map), a, z)
  | Project(a) =>
    let refractor_list =
      List.map(fst, z.refractors.manuals)
      @ List.map(fst, Id.Map.to_list(z.refractors.multis.ephemerals));
    ProjectorPerform.go(
      syntax.term_data,
      a,
      z,
      syntax.projector_list,
      refractor_list,
      ~elaborated=statics.elaborated,
      ~root,
    );
  | Move(SplicePoint(splice_id, goal)) =>
    switch (CachedSyntax.splice_opt(splice_id, syntax)) {
    | None => Error(Cant_move)
    | Some(splice) =>
      Move.to_splice_point(
        ~measured=splice.measured,
        ~projectors=syntax.projectors,
        ~splice_id,
        ~goal,
        z,
      )
      |> return(Cant_move)
    }
  | Move(Vertical(d, _)) when Zipper.splice_context(z) != None =>
    /* Vertical movement inside a splice: within the splice's own rows
     * it behaves normally (against the swapped, splice-local measured).
     * At the top/bottom row it moves to the previous/next splice of the
     * same projector — for a vlist, the row above/below — landing on
     * that splice's nearest row at the current column target; with no
     * neighboring splice it exits the projector and continues in the
     * enclosing frame. */
    let measured = CachedSyntax.measured(syntax);
    let cur = Zipper.Caret.point(measured, z);
    let col = Option.value(col_target, ~default=cur.col);
    let leaving =
      switch (d) {
      | Up => cur.row <= 0
      | Down => cur.row >= Measured.last_row(measured)
      };
    if (!leaving) {
      Move.vertical(~measured, ~col_target=col, d, z) |> return(Cant_move);
    } else {
      let exit_d: Util.Direction.t = d == Up ? Left : Right;
      switch (Move.exit_current_splice(exit_d, z)) {
      | None => Error(Cant_move)
      | Some(z) =>
        switch (Zipper.splice_context(z)) {
        | Some(next_id) =>
          switch (CachedSyntax.splice_opt(next_id, syntax)) {
          | None => Ok(z)
          | Some(next) =>
            let row = d == Down ? 0 : Measured.last_row(next.measured);
            Move.to_point(
              ~measured=next.measured,
              ~goal=
                Point.{
                  row,
                  col,
                },
              z,
            )
            |> return(Cant_move);
          }
        | None =>
          /* Exited the projector: continue vertically in the main
           * frame (the caret sits at the projector's edge, whose
           * measurement spans its full placeholder). If there is no
           * line beyond the projector, the exit position itself is
           * the landing spot. */
          Ok(
            Move.vertical(
              ~measured=CachedSyntax.measured(syntax_main),
              ~col_target=col,
              d,
              z,
            )
            |> Option.value(~default=z),
          )
        }
      };
    };
  | Move(Point(goal, _)) when Zipper.splice_context(z) != None =>
    /* Point actions arrive from the main editor in main-frame
     * coordinates (sub-editors emit SplicePoint instead): a click
     * outside the splice exits it, then resolves the point against
     * the main measured. */
    Move.to_point(
      ~measured=CachedSyntax.measured(syntax_main),
      ~goal,
      Move.to_top(z),
    )
    |> return(Cant_move)
  | Move(d) =>
    Move.go(
      ~statics=statics.info_map,
      ~problem_ids=
        Seq.append(
          List.to_seq(statics.error_ids),
          Seq.append(
            List.to_seq(statics.warning_ids),
            Seq.filter_map(
              (g: Grout.t) => g.shape == Convex ? Some(g.id) : None,
              List.to_seq(Segment.holes(CachedSyntax.segment(syntax))),
            ),
          ),
        ),
      ~col_target=Option.value(col_target, ~default=0),
      ~measured=CachedSyntax.measured(syntax),
      d,
      z,
    )
    |> return(Cant_move)
  | Unselect(Some(d)) => Ok(Zipper.directional_unselect(d, z))
  | Unselect(None) => Ok(Zipper.unselect(z))
  | Select(Resize(Local(d, ByToken))) =>
    Select.local(d, z) |> return(Cant_select)
  | Select(Resize(Local(d, ByChar))) =>
    Select.local_by_char(d, z) |> return(Cant_select)
  | Select(Resize(Local(d, BySmart))) =>
    Select.local_smart(d, z) |> return(Cant_select)
  | Select(Resize(Vertical(d, chunkiness))) =>
    Select.vertical(
      ~col_target=Option.value(col_target, ~default=0),
      ~measured=CachedSyntax.measured(syntax),
      ~chunkiness,
      d,
      z,
    )
    |> return(Cant_select)
  | Select(Resize(Start)) => Ok(Select.to_start(z))
  | Select(Resize(End)) => Ok(Select.to_end(z))
  | Select(Resize(Line(d))) =>
    Select.to_linebreak(d, z) |> return(Cant_select)
  | Select(Resize(Point(goal, _))) when Zipper.splice_context(z) != None =>
    /* Shift-click/drag from inside a splice to the main editor:
     * selections across the splice boundary are not supported, so exit
     * the splice and land the caret at the point instead. */
    Move.to_point(
      ~measured=CachedSyntax.measured(syntax_main),
      ~goal,
      Move.to_top(z),
    )
    |> return(Cant_select)
  | Select(Resize(Point(goal, override))) =>
    /* Mouse drag obeys the "Character-level mouse" setting by default
     * (off → smart, on → char). The drag handler may pass an explicit
     * `Some(chunkiness)` to override — e.g. Alt+drag on Mac (Ctrl+drag
     * on PC) selects the opposite chunkiness. */
    let chunkiness: Action.chunkiness =
      switch (override) {
      | Some(c) => c
      | None => settings.selection_chunkiness ? ByChar : BySmart
      };
    Select.to_point(
      ~chunkiness,
      ~measured=CachedSyntax.measured(syntax),
      ~goal,
      z,
    )
    |> return(Cant_select);
  | Select(Resize(SplicePoint(splice_id, goal))) =>
    switch (CachedSyntax.splice_opt(splice_id, syntax)) {
    | None => Error(Cant_select)
    | Some(splice) when Zipper.splice_context(z) == Some(splice_id) =>
      /* Drag within the splice the caret is already in: resolve against
       * the splice's own frame. Char-level; smart rounding would need
       * splice-aware token spans. */
      Select.to_point(~chunkiness=ByChar, ~measured=splice.measured, ~goal, z)
      |> return(Cant_select)
    | Some(splice) =>
      /* Drag/shift-click that starts outside this splice: land the caret
       * at the point instead. Selections across splice boundaries are
       * not yet supported. */
      Move.to_splice_point(
        ~measured=splice.measured,
        ~projectors=syntax.projectors,
        ~splice_id,
        ~goal,
        z,
      )
      |> return(Cant_select)
    }
  | Select(Resize(Goal(_))) => failwith("Select not implemented for goals")
  | Select(All) => Ok(Select.all(z))
  | Select(PointToPoint((p1, p2))) =>
    /* Precise range selection from two exact points — always char-level
     * regardless of the smart-selection setting. Smart rounding would
     * overshoot the intended endpoints. */
    z
    |> Move.to_point(~measured=CachedSyntax.measured(syntax), ~goal=p1)
    |> OptUtil.and_then(z =>
         Select.to_point(
           ~chunkiness=ByChar,
           ~measured=CachedSyntax.measured(syntax),
           ~goal=p2,
           z,
         )
       )
    |> return(Cant_select)
  | Select(Term(Current)) =>
    Select.select_enclosing_term(
      syntax.term_data,
      CachedSyntax.measured(syntax),
      statics.info_map,
      z,
    )
    |> return(Cant_select)
  | Select(Smart(n)) =>
    Select.smart(syntax.term_data, statics.info_map, n, z)
    |> return(Cant_select)
  | Select(Term(Id(id, d))) =>
    switch (
      Select.term(
        ~defs_exclude_bodies=false,
        ~case_rules=false,
        syntax.term_data,
        id,
        z,
      )
    ) {
    | Some(z) => Ok(d == Right ? z : Zipper.toggle_focus(z))
    | None => Error(Cant_select)
    }
  | Select(Tile(Current)) => Select.current_tile(z) |> return(Cant_select)
  | Select(Tile(Id(id, d))) =>
    switch (Select.tile(id, z)) {
    | Some(z) => Ok(d == Right ? z : Zipper.toggle_focus(z))
    | None => Error(Cant_select)
    }
  | Select(ToggleFocus) => Ok(Zipper.toggle_focus(z))
  | Select(SetFocus(d)) => Ok(Zipper.set_focus(z, d))
  | Destruct(d) =>
    Destruct.go(d, z, ~root)
    |> Option.map(maybe_reassoc)
    |> return(Cant_destruct)
  | Insert(char) =>
    z
    |> Insert.go(
         ~deep_reassociate=settings.deep_reassociate,
         char,
         ~ci=Indicated.ci_of(z, statics.info_map),
         ~root,
       )
    |> Option.map(maybe_reassoc)
    |> return(Cant_insert)
  | Put_down =>
    Zipper.put_down(z, ~root)
    |> Option.map(maybe_reassoc)
    |> return(Cant_put_down)
  | Probe(a) => Ok(ProbePerform.go(~statics, ~syntax, a, z))
  | Dump => Ok(Dump.to_zipper(z, ~root))
  | ToggleLineComment =>
    Comment.go(~deep_reassociate=settings.deep_reassociate, z, ~root)
    |> return(Cant_destruct)
  | Structural(a) => CompositionGo.Public.go(~syntax, ~z, ~a)
  };
};
