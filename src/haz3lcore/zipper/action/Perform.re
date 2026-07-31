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

/* Caret sits at its line's leading-whitespace boundary: everything
   left of it at its level, up to a linebreak (or buffer start), is
   spaces. True at first-content and inside the indentation run. */
let at_line_leading_whitespace = (z: Zipper.t): bool =>
  z.caret == Outer
  && z.selection.content == []
  && {
    let rec all_white = (ps: list(Piece.t)) =>
      switch (ps) {
      | [] => z.relatives.ancestors == []
      | [Piece.Secondary(s), ...rest] =>
        Secondary.is_space(s) ? all_white(rest) : Secondary.is_linebreak(s)
      | _ => false
      };
    all_white(List.rev(fst(z.relatives.siblings)));
  };

/* Backspace inverts enter: when the caret sits at first-content (or a
   blank line's position) with [linebreak ++ spaces*] immediately left
   of it at its own level, return (space count, linebreak id) — one
   keystroke then removes the whole indentation AND its linebreak.
   The consumer gates on the run being no wider than the line's
   AUTO-INDENT level: spaces the user typed beyond the indent are
   real material, deleted one per press (andrew 2026-07-22). */
let indent_join_run = (z: Zipper.t): option((int, Id.t)) =>
  if (z.caret != Outer || z.selection.content != []) {
    None;
  } else {
    let right_is_space =
      switch (z.relatives.siblings) {
      | (_, [Piece.Secondary(w), ..._]) => Secondary.is_space(w)
      | _ => false
      };
    if (right_is_space) {
      None; /* inside the run: normal char-delete */
    } else {
      let rec scan = (n, ps: list(Piece.t)) =>
        switch (ps) {
        | [Piece.Secondary(s), ...rest] when Secondary.is_space(s) =>
          scan(n + 1, rest)
        | [Piece.Secondary(s), ..._] when Secondary.is_linebreak(s) =>
          Some((n, s.id))
        | _ => None
        };
      scan(0, List.rev(fst(z.relatives.siblings)));
    };
  };

/* Last linebreak (textual order) within a segment, deep */
let rec last_lb_in_seg = (seg: Segment.t): option(Id.t) =>
  List.fold_left(
    (acc, p: Piece.t) =>
      switch (p) {
      | Secondary(w) when Secondary.is_linebreak(w) => Some(w.id)
      | Tile(t) =>
        switch (
          List.fold_left(
            (a, ch) =>
              switch (last_lb_in_seg(ch)) {
              | Some(id) => Some(id)
              | None => a
              },
            None,
            t.children,
          )
        ) {
        | Some(id) => Some(id)
        | None => acc
        }
      | _ => acc
      },
    None,
    seg,
  );

let rec all_lbs_in_seg = (seg: Segment.t): list(Id.t) =>
  List.concat_map(
    (p: Piece.t) =>
      switch (p) {
      | Secondary(w) when Secondary.is_linebreak(w) => [w.id]
      | Tile(t) => List.concat_map(all_lbs_in_seg, t.children)
      | _ => []
      },
    seg,
  );

/* Linebreak governing the caret's line: nearest textually-preceding
   linebreak (current level, then ancestor left segments outward);
   None = the buffer's first line. */
let governing_lb = (z: Zipper.t): option(Id.t) =>
  switch (last_lb_in_seg(fst(z.relatives.siblings))) {
  | Some(id) => Some(id)
  | None =>
    List.fold_left(
      (acc, (_, sibs): Ancestors.generation) =>
        acc != None ? acc : last_lb_in_seg(fst(sibs)),
      None,
      z.relatives.ancestors,
    )
  };

/* Indent/dedent one level (2 spaces) for the caret's line, or every
   line intersecting the selection; dedent clamps at column 0. */
let adjust_indent = (d: Direction.t, z: Zipper.t): Zipper.t => {
  let first_line_lb = governing_lb(z);
  let affected =
    (
      switch (first_line_lb) {
      | Some(id) => [id]
      | None => []
      }
    )
    @ all_lbs_in_seg(z.selection.content)
    |> List.to_seq
    |> Id.Set.of_seq;
  let line0 = first_line_lb == None;
  let indent = d == Direction.Right;
  let adjust_run = (seg: Segment.t): Segment.t =>
    if (indent) {
      [
        Piece.secondary(Secondary.mk_space(Id.mk())),
        Piece.secondary(Secondary.mk_space(Id.mk())),
        ...seg,
      ];
    } else {
      let rec drop = (k, sg: Segment.t) =>
        switch (sg) {
        | [Piece.Secondary(w), ...rest] when k > 0 && Secondary.is_space(w) =>
          drop(k - 1, rest)
        | _ => sg
        };
      drop(2, seg);
    };
  let rec walk = (seg: Segment.t): Segment.t =>
    switch (seg) {
    | [] => []
    | [Piece.Secondary(w) as p, ...rest]
        when Secondary.is_linebreak(w) && Id.Set.mem(w.id, affected) => [
        p,
        ...walk(adjust_run(rest)),
      ]
    | [Piece.Tile(t), ...rest] => [
        Piece.Tile({
          ...t,
          children: List.map(walk, t.children),
        }),
        ...walk(rest),
      ]
    | [p, ...rest] => [p, ...walk(rest)]
    };
  CaretPreserving.transform(z, seg =>
    (line0 ? adjust_run(seg) : seg) |> walk
  );
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
  let maybe_reassoc = Reassociate.go;
  /* Paste is a rare bulk edit that can leave incomplete delimiter forms
     anywhere in the pasted region, so it gets the thorough (full-relatives)
     reassociation guard rather than the cheap caret-local one. */
  let maybe_reassoc_thorough = Reassociate.go_thorough;
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
    /* pasted material can complete enclosing tiles (the completion
       trigger) AND carries its source indentation (the region
       trigger re-indents the pasted lines themselves) */
    let before = LocalReformat.snapshot(~enabled=settings.auto_reindent, z);
    let before_pieces =
      LocalReformat.snapshot_pieces(~enabled=settings.auto_reindent, z);
    let finish = z =>
      z
      |> maybe_reassoc_thorough
      |> LocalReformat.go(~before)
      |> LocalReformat.go_region(~before_pieces);
    switch (Parser.try_segment_paste(clipboard, z, ~root)) {
    | Some(z) => Ok(finish(z))
    | None =>
      (
        Parser.can_fast_paste(clipboard, z, ~root)
          ? Parser.fast_paste(clipboard, z, ~root)
          : Parser.to_zipper(~root, ~zipper_init=z, clipboard)
      )
      |> Option.map(finish)
      |> return(CantPaste)
    };
  | Cut =>
    /* System clipboard handling is done in Page.view handlers.
       Deletion can COMPLETE a tile (removing junk between split
       shards lets reassembly merge them), so the completion trigger
       applies; ordinary un-settling deletions leave it silent. */
    let before = LocalReformat.snapshot(~enabled=settings.auto_reindent, z);
    Destruct.go(Local(Left, ByChar), z, ~root)
    |> Option.map(LocalReformat.go(~before))
    |> return(Cant_destruct);
  | Copy =>
    /* System clipboard handling itself is done in Page.view handlers.
     * This doesn't change state but is included here for logging purposes */
    Ok(z)
  | Reparse =>
    /* This serializes the current editor to text, resets the current
       editor, and then deserializes. It is intended as a (tactical)
       nuclear option for weird missing-shard states */
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
      CaretPreserving.transform(z, SpaceNormalize.go(~canonicalize=true)),
    )
    |> return(CantReparse);
  | Format(Pretty) =>
    /* SpaceNormalize first: a repair no-op on parsed buffers (they
       can't contain bare glom junctions) but totalizes synthesized
       segments (agent/structural edits). */
    let f = seg => seg |> SpaceNormalize.go |> PrettySegment.prettify;
    Some(CaretPreserving.transform(z, f)) |> return(CantReparse);
  | Buffer(a) =>
    /* accepting a TyDi suggestion inserts delimiter text like typing
       it, but via a separate path from the Insert arm */
    let before = LocalReformat.snapshot(~enabled=settings.auto_reindent, z);
    switch (
      Buffer.go(~ci=Indicated.ci_for_completion(z, statics.info_map), a, z)
    ) {
    | Ok(z) => Ok(LocalReformat.go(~before, z))
    | Error(_) as e => e
    };
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
  | AdjustIndent(d, gate) =>
    if (gate == Action.AtBoundary && !at_line_leading_whitespace(z)) {
      /* a held shift during ordinary corrections must not dedent:
         fall through to plain backspace */
      go(
        ~settings,
        ~statics,
        ~syntax,
        ~root,
        Action.Destruct(Local(Left, ByChar)),
        {
          zipper: z,
          col_target,
        },
      );
    } else {
      Ok(adjust_indent(d, z));
    }
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
    |> Option.map(z' =>
         !settings.indentation_ux
           ? z'
           : (
             switch (d) {
             | Local(dir, ByChar) => Move.skip_indent(dir, z')
             | Vertical(_, ByChar)
             | Line(Left) => Move.skip_indent(Direction.Right, z')
             | _ => z'
             }
           )
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
    /* see Cut: fires only on completion-by-deletion */
    let before = LocalReformat.snapshot(~enabled=settings.auto_reindent, z);
    let join =
      switch (d) {
      | Local(Left, ByChar) when settings.indentation_ux =>
        switch (indent_join_run(z)) {
        | Some((n, lb_id)) =>
          /* the one-keystroke join covers AUTO-INDENT width only:
             a run wider than the line's indent level means typed
             spaces beyond it — those delete one per press */
          let level =
            Indentation.level_of(
              ~target_id=lb_id,
              Zipper.unselect_and_zip(z),
            );
          n <= level ? Some(n) : None;
        | None => None
        }
      | _ => None
      };
    switch (join) {
    | Some(_) =>
      /* backspace inverts enter: delete indentation + linebreak as a
         single action (one undo step). Adaptive: destruct's own
         whitespace cleanup can consume more than one piece per call,
         so re-inspect the left neighbor each step instead of
         counting. */
      let left_neighbor = (z: Zipper.t) =>
        switch (fst(z.relatives.siblings) |> List.rev) {
        | [Piece.Secondary(w), ..._] =>
          Secondary.is_space(w)
            ? `Space : Secondary.is_linebreak(w) ? `Linebreak : `Other
        | _ => `Other
        };
      let rec del_run = (~fuel=10000, z) =>
        fuel <= 0
          ? Some(z)
          : (
            switch (left_neighbor(z)) {
            | `Space =>
              Option.bind(Destruct.go(d, z, ~root), del_run(~fuel=fuel - 1))
            | `Linebreak => Destruct.go(d, z, ~root)
            | `Other => Some(z)
            }
          );
      del_run(z)
      |> Option.map(maybe_reassoc)
      |> Option.map(LocalReformat.go(~before))
      |> return(Cant_destruct);
    | None =>
      Destruct.go(d, z, ~root)
      |> Option.map(maybe_reassoc)
      |> Option.map(LocalReformat.go(~before))
      |> return(Cant_destruct)
    };
  | Insert(char) =>
    let before = LocalReformat.snapshot(~enabled=settings.auto_reindent, z);
    z
    |> Insert.go(char, ~ci=Indicated.ci_of(z, statics.info_map), ~root)
    |> Option.map(maybe_reassoc)
    |> Option.map(LocalReformat.go(~before))
    |> return(Cant_insert);
  | Refactor(k) =>
    Refactor.go(~info_map=statics.info_map, ~term=statics.term, k, z)
    |> Option.map(
         LocalReformat.go_refactor(~enabled=settings.auto_reindent),
       )
    |> return(Cant_refactor)
  | RefactorGesture(g) =>
    switch (
      Refactor.gesture(~info_map=statics.info_map, ~term=statics.term, g, z)
    ) {
    | Some(k) =>
      /* negate toggles: invoked from then/else, the caret lands on
         the OPPOSITE delimiter (the arm you moved now lives there),
         so repeating the gesture flips back */
      let toggle =
        switch (k, Indicated.index(z), Indicated.shard_index(z)) {
        | (NegateIf, Some(tile), Some(shard)) when shard == 1 || shard == 2 =>
          Some((tile, 3 - shard))
        | _ => None
        };
      Refactor.go(~info_map=statics.info_map, ~term=statics.term, k, z)
      |> Option.map(z' =>
           switch (toggle) {
           | Some((tile, shard)) =>
             Move.jump_to_shard(z', tile, shard) |> Option.value(~default=z')
           | None => z'
           }
         )
      |> Option.map(
           LocalReformat.go_refactor(~enabled=settings.auto_reindent),
         )
      |> return(Cant_refactor);
    | None => Error(Cant_refactor)
    }
  | ApplyCompletion(All) => Ok(Materialize.all(z, ~root))
  | ApplyCompletion(One(id)) =>
    Materialize.one(z, ~root, id)
    |> Result.of_option(~error=Action.Failure.Cant_put_down)
  | Put_down =>
    let before = LocalReformat.snapshot(~enabled=settings.auto_reindent, z);
    Zipper.put_down(z, ~root)
    |> Option.map(space_put_down_boundary)
    |> Option.map(maybe_reassoc)
    |> Option.map(LocalReformat.go(~before))
    |> return(Cant_put_down);
  | Probe(a) => Ok(ProbePerform.go(~statics, ~syntax, a, z))
  | Format(Indent) => Ok(AutoFormat.zipper(z))
  | ToggleLineComment =>
    /* uncommenting can restore delimiters that complete enclosing
       forms; the comment-out direction leaves the trigger silent */
    let before = LocalReformat.snapshot(~enabled=settings.auto_reindent, z);
    Comment.go(z, ~root)
    |> Option.map(LocalReformat.go(~before))
    |> return(Cant_destruct);
  | Structural(a) =>
    /* agent edits funnel pasted code through introduce with indentation
       stripped; re-indent new lines like user Paste */
    let before = LocalReformat.snapshot(~enabled=settings.auto_reindent, z);
    let before_pieces =
      LocalReformat.snapshot_pieces(~enabled=settings.auto_reindent, z);
    switch (CompositionGo.Public.go(~syntax, ~z, ~a)) {
    | Ok(z) =>
      Ok(
        z
        |> LocalReformat.go(~before)
        |> LocalReformat.go_region(~before_pieces),
      )
    | Error(_) as e => e
    };
  };
};
