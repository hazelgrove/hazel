open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type state = {
  zipper: Zipper.t,
  col_target: option(int),
};

let return = (error: Action.Failure.t, z: option(Zipper.t)) =>
  Result.of_option(~error, z);

/* Put_down splices complete shard pieces without lexing, so it can
   create bare glom junctions the lexer could never produce (dropping
   `in` after `end` gives end|in). Insert the space the lexer would
   have forced, on both sides of the drop. */
let space_put_down_boundary = (z: Zipper.t): Zipper.t => {
  let needs = (l: Piece.t, r: Piece.t) =>
    switch (SpaceNormalize.last_token(l), SpaceNormalize.first_token(r)) {
    | (Some(a), Some(b)) => SpaceNormalize.needs_space(a, b)
    | _ => false
    };
  let (pre, suf) = z.relatives.siblings;
  /* The drop may have REASSEMBLED into its tile, making the junction
     interior (def-child `end` vs the tile's own `in` shard) —
     normalize_piece fixes child<->shard junctions; grout/holes have
     no token so hole-adjacent layouts are untouched. Left junction
     only at top level: the right side (dropped shard abutting a
     following keyword) is a transient wrap state whose glued form is
     load-bearing for existing flows. */
  let pre =
    switch (List.rev(pre)) {
    | [last, ...rest] =>
      let last = SpaceNormalize.normalize_piece(last);
      switch (rest) {
      | [prev, ...rest'] when needs(prev, last) =>
        List.rev([last, SpaceNormalize.space(), prev, ...rest'])
      | _ => List.rev([last, ...rest])
      };
    | [] => pre
    };
  {
    ...z,
    relatives: {
      ...z.relatives,
      siblings: (pre, suf),
    },
  };
};

/* Format the whole buffer as a segment via ~f, restoring the caret
   through the indicated tile (formatting preserves tile IDs), falling
   back to its statics ancestors — a vanished id must not dump the
   caret at the document end. */
let format_via_segment =
    (~info_map, ~f: Segment.t => Segment.t, z: Zipper.t): Zipper.t => {
  let anchors =
    switch (Indicated.index(z)) {
    | None => []
    | Some(id) =>
      [id]
      @ (
        switch (Id.Map.find_opt(id, info_map)) {
        | Some(Language.Info.InfoExp({ancestors, _}))
        | Some(InfoPat({ancestors, _})) => ancestors
        | _ => []
        }
      )
    };
  let seg = Zipper.unselect_and_zip(z) |> f;
  let z' = {
    ...Zipper.unzip(seg),
    refractors: z.refractors,
  };
  switch (Move.jump_to_first_indicated(z', anchors)) {
  | Some(z'') => z''
  | None => z'
  };
};

let rec go =
        (
          ~settings: Language.CoreSettings.t,
          ~statics: CachedStatics.t,
          ~syntax: CachedSyntax.t,
          ~root,
          a: Action.t,
          {zipper: z, col_target}: state,
        )
        : Action.Result.t(Zipper.t) => {
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
    Destruct.go(Local(Left, ByChar), z, ~root) |> return(Cant_destruct)
  | Copy =>
    /* System clipboard handling itself is done in Page.view handlers.
     * This doesn't change state but is included here for logging purposes */
    Ok(z)
  | Reparse =>
    /* This serializes the current editor to text, resets the current
       editor, and then deserializes. It is intended as a (tactical)
       nuclear option for weird backpack states */
    Parser.to_zipper(
      ~root,
      ~zipper_init=Zipper.init(),
      Printer.of_zipper(~holes="", ~indent="", z),
    )
    |> return(CantReparse)
  | Format(Preferred) =>
    switch (settings.format_shortcut) {
    | Language.CoreSettings.FormatShortcut.Nothing => Ok(z)
    | Language.CoreSettings.FormatShortcut.Indent =>
      go(
        ~settings,
        ~statics,
        ~syntax,
        ~root,
        Action.Format(Indent),
        {
          zipper: z,
          col_target,
        },
      )
    | Language.CoreSettings.FormatShortcut.Spaces =>
      go(
        ~settings,
        ~statics,
        ~syntax,
        ~root,
        Action.Format(Spacing),
        {
          zipper: z,
          col_target,
        },
      )
    | Language.CoreSettings.FormatShortcut.Breaks =>
      go(
        ~settings,
        ~statics,
        ~syntax,
        ~root,
        Action.Format(Pretty),
        {
          zipper: z,
          col_target,
        },
      )
    }
  | Format(Spacing) =>
    /* Re-indent, then canonicalize within-line spacing. Linebreaks
       and comments untouched; caret restored as in Format(Pretty). */
    let z = AutoFormat.zipper(z);
    Some(
      format_via_segment(
        ~info_map=statics.info_map,
        ~f=SpaceNormalize.go(~canonicalize=true),
        z,
      ),
    )
    |> return(CantReparse);
  | Format(Pretty) =>
    /* SpaceNormalize first: a repair no-op on parsed buffers (they
       can't contain bare glom junctions) but totalizes synthesized
       segments (agent/structural edits). */
    let f = seg => seg |> SpaceNormalize.go |> PrettySegment.prettify;
    Some(format_via_segment(~info_map=statics.info_map, ~f, z))
    |> return(CantReparse);
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
              List.to_seq(Segment.holes(syntax.segment)),
            ),
          ),
        ),
      ~col_target=Option.value(col_target, ~default=0),
      ~measured=syntax.measured,
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
      ~measured=syntax.measured,
      ~chunkiness,
      d,
      z,
    )
    |> return(Cant_select)
  | Select(Resize(Start)) => Ok(Select.to_start(z))
  | Select(Resize(End)) => Ok(Select.to_end(z))
  | Select(Resize(Line(d))) =>
    Select.to_linebreak(d, z) |> return(Cant_select)
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
    Select.to_point(~chunkiness, ~measured=syntax.measured, ~goal, z)
    |> return(Cant_select);
  | Select(Resize(Goal(_))) => failwith("Select not implemented for goals")
  | Select(All) => Ok(Select.all(z))
  | Select(PointToPoint((p1, p2))) =>
    /* Precise range selection from two exact points — always char-level
     * regardless of the smart-selection setting. Smart rounding would
     * overshoot the intended endpoints. */
    z
    |> Move.to_point(~measured=syntax.measured, ~goal=p1)
    |> OptUtil.and_then(z =>
         Select.to_point(
           ~chunkiness=ByChar,
           ~measured=syntax.measured,
           ~goal=p2,
           z,
         )
       )
    |> return(Cant_select)
  | Select(Term(Current)) =>
    Select.select_enclosing_term(
      syntax.term_data,
      syntax.measured,
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
    let before = LocalReformat.snapshot(~enabled=settings.auto_reindent, z);
    z
    |> Insert.go(
         ~deep_reassociate=settings.deep_reassociate,
         char,
         ~ci=Indicated.ci_of(z, statics.info_map),
         ~root,
       )
    |> Option.map(maybe_reassoc)
    |> Option.map(LocalReformat.go(~before))
    |> return(Cant_insert);
  | Refactor(k) =>
    Refactor.go(~info_map=statics.info_map, k, z) |> return(Cant_refactor)
  | Put_down =>
    let before = LocalReformat.snapshot(~enabled=settings.auto_reindent, z);
    Zipper.put_down(z, ~root)
    |> Option.map(space_put_down_boundary)
    |> Option.map(maybe_reassoc)
    |> Option.map(LocalReformat.go(~before))
    |> return(Cant_put_down);
  | Probe(a) => Ok(ProbePerform.go(~statics, ~syntax, a, z))
  | Format(Indent) => Ok(AutoFormat.zipper(z))
  | Dump =>
    /* Experimental: Use CanonicalCompletion instead of Dump */
    let seg =
      z
      |> Zipper.clear_unparsed_buffer
      |> Zipper.unselect_and_zip(~erase_buffer=true);
    let result =
      CanonicalCompletion.complete_segment_deep(~sort=Sort.Exp, seg);
    Ok({
      selection: Selection.mk([]),
      relatives: {
        siblings: ([], result.completed_seg),
        ancestors: [],
      },
      caret: Outer,
      refractors: z.refractors,
    });
  | ToggleLineComment =>
    Comment.go(~deep_reassociate=settings.deep_reassociate, z, ~root)
    |> return(Cant_destruct)
  | Structural(a) => CompositionGo.Public.go(~syntax, ~z, ~a, ~return)
  };
};
