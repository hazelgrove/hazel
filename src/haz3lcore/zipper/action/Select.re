open Util_web;
open OptUtil.Syntax;
open Zipper;

let local = (d: Direction.t, z: t): option(t) =>
  if (z.caret == Outer) {
    Zipper.select(d, z);
  } else if (d == Left) {
    z
    |> Zipper.Caret.set(Outer)
    |> Zipper.move(Right)
    |> OptUtil.and_then(Zipper.select(d));
  } else {
    z |> Zipper.Caret.set(Outer) |> Zipper.select(d);
  };

/* Max inner index for a piece (Token.length - 2, or None for non-tokens / single-char) */
let piece_max_idx = (p: Piece.t): option(int) => {
  let* tok = Piece.token_of(p);
  let max_idx = Token.length(tok) - 2;
  max_idx < 0 ? None : Some(max_idx);
};

/* Get the focus-side boundary piece from selection content */
let focus_boundary_piece = (z: Zipper.t): option(Piece.t) =>
  Selection.focus_piece(z.selection);

/* Max inner index of the focus-side boundary piece in the selection */
let focus_max_idx = (z: Zipper.t): int =>
  switch (focus_boundary_piece(z)) {
  | Some(p) => piece_max_idx(p) |> Option.value(~default=0)
  | None => 0
  };

/* Set caret to the edge of a token when entering it from direction d.
 * Right means entering from the left (set Inner(0)),
 * Left means entering from the right (set Inner(max_idx)). */
let enter_token_edge = (d: Direction.t, max_idx: int, z: Zipper.t): Zipper.t =>
  switch (d) {
  | Right => Zipper.Caret.set(Inner(0), z)
  | Left => Zipper.Caret.set(Inner(max_idx), z)
  };

/* If the next sibling in direction d is a multi-shard tile (with
 * children), decompose it in-place so that grow_selection picks up
 * individual shards rather than the entire tile tree. */
let decompose_multi_shard_neighbor = (d: Direction.t, z: Zipper.t): Zipper.t => {
  let focus = z.selection.focus;
  let dir = Selection.is_empty(z.selection) ? d : focus;
  let (l, r) = z.relatives.siblings;
  switch (dir) {
  | Right =>
    switch (r) {
    | [Tile(t), ...rest] when List.length(t.shards) > 1 =>
      let pieces = Tile.disassemble(t);
      {
        ...z,
        relatives: {
          ...z.relatives,
          siblings: (l, pieces @ rest),
        },
      };
    | _ => z
    }
  | Left =>
    switch (ListUtil.split_last_opt(l)) {
    | Some((init, Tile(t))) when List.length(t.shards) > 1 =>
      let pieces = Tile.disassemble(t);
      {
        ...z,
        relatives: {
          ...z.relatives,
          siblings: (init @ pieces, r),
        },
      };
    | _ => z
    }
  };
};

/* Character-level selection: grow the selection by one character in
 * direction d. Uses anchor_caret to track partial tokens at the
 * anchor end of the selection. */
let rec local_by_char = (d: Direction.t, z: Zipper.t): option(Zipper.t) => {
  let is_growing = Selection.is_empty(z.selection) || d == z.selection.focus;

  if (is_growing) {
    grow_by_char(d, z);
  } else {
    shrink_by_char(d, z);
  };
}

and grow_by_char = (d: Direction.t, z: Zipper.t): option(Zipper.t) => {
  let is_empty = Selection.is_empty(z.selection);
  /* Ensure focus direction matches growth direction */
  let z =
    if (is_empty) {
      {
        ...z,
        selection: {
          ...z.selection,
          focus: d,
        },
      };
    } else {
      z;
    };

  switch (z.caret) {
  | Inner(n) when is_empty =>
    /* Starting from inside a token: pop it into selection and set
     * anchor_caret. max_idx must be read post-grow via focus_max_idx;
     * Siblings.neighbor pre-grow returns the whole multi-shard tile
     * (token_of=None, defaults to 0), which would force Outer here
     * and over-select. */
    switch (d) {
    | Right =>
      let+ z =
        Zipper.grow_selection({
          ...z,
          caret: Outer,
        });
      let z = {
        ...z,
        selection: {
          ...z.selection,
          anchor_caret: CaretBase.Inner(n),
        },
      };
      let max_idx = focus_max_idx(z);
      n < max_idx
        ? Zipper.Caret.set(Inner(n + 1), z) : Zipper.Caret.set(Outer, z);
    | Left =>
      let+ z =
        {
          ...z,
          caret: Outer,
          selection: {
            ...z.selection,
            focus: Left,
          },
        }
        |> Zipper.move(Right)
        |> OptUtil.and_then(z => Zipper.grow_selection(z));
      let z = {
        ...z,
        selection: {
          ...z.selection,
          anchor_caret: CaretBase.Inner(n),
        },
      };
      n > 0
        ? Zipper.Caret.set(Inner(n - 1), z) : Zipper.Caret.set(Outer, z);
    }

  | Inner(n) =>
    /* Already have a selection; caret is inside the focus-side
     * boundary piece of selection.content. */
    let max_idx = focus_max_idx(z);
    switch (z.selection.focus) {
    | Right when n < max_idx => Some(Zipper.Caret.set(Inner(n + 1), z))
    | Right => Some(Zipper.Caret.set(Outer, z))
    | Left when n > 0 => Some(Zipper.Caret.set(Inner(n - 1), z))
    | Left => Some(Zipper.Caret.set(Outer, z))
    };

  | Outer =>
    /* `grow_selection_raw` skips reassembly inside selection content
     * (which would re-fuse shards and break Inner position tracking);
     * `decompose_multi_shard_neighbor` ensures we select individual
     * shards rather than whole tile trees. */
    let z = decompose_multi_shard_neighbor(d, z);
    let+ z =
      d == z.selection.focus || Selection.is_empty(z.selection)
        ? Zipper.grow_selection_raw(z) : Zipper.shrink_selection(z);
    let p =
      switch (d) {
      | Right => ListUtil.last(z.selection.content)
      | Left => List.hd(z.selection.content)
      };
    switch (piece_max_idx(p)) {
    | None => z
    | Some(max_idx) => enter_token_edge(d, max_idx, z)
    };
  };
}

and shrink_by_char = (d: Direction.t, z: Zipper.t): option(Zipper.t) => {
  /* Shrinking: d is opposite to focus. Pulling focus toward anchor. */
  switch (z.caret) {
  | Inner(n) =>
    /* `at_crossover` is meaningful only for single-piece content; with
     * multiple pieces the anchor lives on the opposite end and these
     * index comparisons would be coincidental. */
    let at_crossover = {
      let sel = z.selection;
      switch (sel.content) {
      | [_single] =>
        switch (sel.anchor_caret) {
        | CaretBase.Outer =>
          switch (d) {
          | Left => n == 0
          | Right =>
            let max = focus_max_idx(z);
            n == max;
          }
        | CaretBase.Inner(an) =>
          switch (d) {
          | Left => n == an + 1 || n == an
          | Right => n == an - 1 || n == an
          }
        }
      | _ => false
      };
    };

    if (at_crossover) {
      let anchor_caret = z.selection.anchor_caret;
      switch (anchor_caret) {
      | CaretBase.Outer =>
        let anchor_dir = Direction.toggle(z.selection.focus);
        Some(
          Zipper.Caret.set(
            Outer,
            Zipper.directional_unselect(anchor_dir, z),
          ),
        );
      | CaretBase.Inner(an) =>
        let locator =
          Move.shard_locator(Selection.anchor_piece(z.selection));
        let z = Zipper.directional_unselect(Left, z);
        Some(
          Move.canonicalize_inner_unselect(
            ~locator,
            ~target_caret=Inner(an),
            z,
          ),
        );
      };
    } else {
      switch (d) {
      | Left when n > 0 => Some(Zipper.Caret.set(Inner(n - 1), z))
      | Right =>
        let max = focus_max_idx(z);
        n < max
          ? Some(Zipper.Caret.set(Inner(n + 1), z))
          : Zipper.shrink_selection(z)
            |> Option.map(Zipper.Caret.set(Outer));
      | Left =>
        /* n == 0 but not at crossover: more pieces; pop focus-side
         * piece back to siblings and continue at Outer. */
        Zipper.shrink_selection(z) |> Option.map(Zipper.Caret.set(Outer))
      };
    };

  | Outer =>
    /* Focus is at a piece boundary. Look at the focus-side boundary
     * piece in the selection to see if we should enter it. */
    switch (focus_boundary_piece(z)) {
    | None =>
      /* Empty selection — toggle and grow */
      let selection = Selection.toggle_focus(z.selection);
      grow_by_char(
        d,
        {
          ...z,
          selection,
        },
      );
    | Some(p) =>
      switch (piece_max_idx(p)) {
      | None =>
        /* Single-char / non-token piece: shrink by whole piece */
        Zipper.shrink_selection(z)
      | Some(max_idx) =>
        /* If entering from the focus side at Inner(entry_idx) would
         * coincide with anchor_caret's Inner index, skip the
         * intermediate state and collapse directly — also keeps the
         * caret column changing, which honors do_towards_point's
         * no-progress guard. Only meaningful for single-piece content;
         * the Inner-branch at_crossover above has the same guard. */
        let entry_idx = d == Right ? 0 : max_idx;
        let crossover_at_edge =
          switch (z.selection.content, z.selection.anchor_caret) {
          | ([_single], CaretBase.Inner(an)) => an == entry_idx
          | _ => false
          };
        if (crossover_at_edge) {
          let locator =
            Move.shard_locator(Selection.anchor_piece(z.selection));
          let z = Zipper.directional_unselect(Left, z);
          Some(
            Move.canonicalize_inner_unselect(
              ~locator,
              ~target_caret=Inner(entry_idx),
              z,
            ),
          );
        } else {
          Some(enter_token_edge(d, max_idx, z));
        };
      }
    }
  };
};

/* Smart-rounded selection: char-granular while the selection stays
 * within the "starting token" (the token the selection was first
 * anchored in or entered). When the focus goes *past* the starting
 * token's edge — i.e., we extend into a new piece — the display
 * rounds up to the whole starting token (via `smart_rounded`).
 * Reaching the token's outer edge via a char step is *not* itself a
 * round-up; that at-edge state renders as a normal partial char
 * selection. The round-up fires only when the selection crosses into
 * a new piece.
 *
 * The underlying `anchor_caret` is preserved across round-up;
 * shrinking back to single-piece content clears `smart_rounded`, so
 * the original partial-token anchor re-displays automatically.
 *
 * Growing:
 * - empty: bootstrap via local_by_char.
 * - content=[p], Inner(fn): char step (no round-up).
 * - content=[p], Outer, growing: extend past the starting token by
 *   one whole piece. If anchor_caret was Inner, set smart_rounded=true
 *   on this step — this is the "going past" transition.
 * - content has >1 piece, growing: extend by next whole piece.
 *
 * Shrinking:
 * - content=[p], Inner: char shrink.
 * - content=[p], Outer: char shrink — re-enter starting token at
 *   Inner(max_idx), or pop if single-char piece.
 * - content has >1 pieces: pop focus-side whole piece. If content
 *   becomes single-piece as a result, clear smart_rounded so the
 *   original anchor re-displays. */
let local_smart = (d: Direction.t, z: Zipper.t): option(Zipper.t) => {
  /* If we arrived here with an Inner caret in multi-piece state
   * (reachable from a ByChar phase before a chunkiness switch),
   * round the focus to the Outer edge before dispatching. Smart
   * mode operates with caret=Outer in multi-piece state; otherwise
   * the stale Inner(n) gets re-interpreted against later focus
   * pieces and produces caret jumps several chars into them. The
   * partial-token offset is intentionally discarded — re-engaging
   * the modifier resumes ByChar from this Outer position. */
  let z =
    switch (z.selection.content, z.caret) {
    | ([_, _, ..._], Inner(_)) => Zipper.Caret.set(Outer, z)
    | _ => z
    };
  let is_growing = Selection.is_empty(z.selection) || d == z.selection.focus;
  switch (z.selection.content, z.caret, is_growing) {
  | ([], _, _)
  | ([_], Inner(_), _)
  | ([_], Outer, false) =>
    /* Char phase. No round-up here — reaching the starting token's
     * outer edge via a char step renders as a normal partial char
     * selection. */
    local_by_char(d, z)
  | ([_], Outer, true) =>
    /* Single-piece at edge, growing: extend past the starting token.
     * This is the step that moves *past* the edge; enable
     * smart_rounded (only visible if anchor_caret is Inner). */
    let z = decompose_multi_shard_neighbor(d, z);
    let+ z' = Zipper.grow_selection_raw(z);
    switch (z'.selection.anchor_caret) {
    | CaretBase.Inner(_) => {
        ...z',
        selection: {
          ...z'.selection,
          smart_rounded: true,
        },
      }
    | CaretBase.Outer => z'
    };
  | (_, _, true) =>
    /* Multi-piece growing: extend by next whole piece. smart_rounded
     * preserved from prior step. */
    let z = decompose_multi_shard_neighbor(d, z);
    Zipper.grow_selection_raw(z);
  | (_, _, false) =>
    /* Multi-piece shrinking: pop focus-side whole piece. If this pop
     * brings us back to single-piece content, clear smart_rounded so
     * the original (partial-token) anchor re-displays. */
    let+ z' = Zipper.shrink_selection(z);
    List.length(z'.selection.content) == 1
      ? {
        ...z',
        selection: {
          ...z'.selection,
          smart_rounded: false,
        },
      }
      : z';
  };
};

/* Basic term selection uses term data, which is out of date
 * with the parsing logic which makes list listerals. We also
 * treat tuples as including the parens (if any), though this
 * is a free choice. We also handle case rules, whose parent
 * term in tylr is considered to be the combination of the
 * rules and the scrutinee, but we want to consider it to be
 * the whole case expression. */
let current_term_id = (z: t): option(Id.t) => {
  let* {piece: p, relation: rel, _} = Indicated.for_decoration(z);
  switch (p) {
  | Secondary(_) => None
  | Grout(_)
  | Projector(_) => Some(Piece.id(p))
  | Tile(t) =>
    switch (t.label, Zipper.parent(z)) {
    | ([","], Some(Tile({label: ["[", "]"] | ["(", ")"], id, _}))) =>
      Some(id)
    | (["|", "=>"], Some(Tile({label: ["case", "end"], id, _})))
        when rel == Sibling =>
      Some(id)
    | (["|", "=>"], Some(Tile({label: ["|", "=>"], _})))
        when rel == Parent =>
      switch (z.relatives.ancestors) {
      | [_, (gp, _), ..._] => Some(gp.id)
      | _ => None
      }
    | _ => Some(Piece.id(p))
    }
  };
};

let tile = (id: Id.t, z: t): option(t) => {
  let* z = Move.jump_to_side_of_id(Left, z, id);
  switch (z.relatives.siblings) {
  | (_, []) => None
  | (l, [r, ...rs]) =>
    let z = Zipper.update_siblings(_ => (l, rs), z);
    let z = Zipper.replace_selection(Right, [r], z);
    Some(z);
  };
};

let current_tile = (z: t): option(t) => {
  let* id = Indicated.index(z);
  tile(id, z);
};

/* Select the containing case rule, if any */
let containing_rule = (z: t): option(t) => {
  let grow_left_by_piece = (z: t): option(t) => {
    switch (z.relatives.siblings) {
    | (_, []) => None
    | (l, [r, ...rs]) =>
      let z = Zipper.Caret.set(Outer, z);
      let z = Zipper.update_siblings(_ => (l, rs), z);
      Some({
        ...z,
        selection: Selection.push(r, z.selection),
      });
    };
  };
  let shrink_right_by_piece = (z: t): option(t) => {
    switch (z.selection.content) {
    | [] => None
    | [_, ..._] as s =>
      let z = Zipper.Caret.set(Outer, z);
      let (s, p) = ListUtil.split_last(s);
      let z = Zipper.update_siblings(((l, r)) => (l, [p, ...r]), z);
      Some({
        ...z,
        selection: {
          ...z.selection,
          content: s,
        },
      });
    };
  };
  let rule_or_end_of_seg_to_right =
    fun
    | (_, None | Some(Piece.Tile({label: ["|", "=>"], _}))) => true
    | _ => false;
  let grow_right_until_case_or_rule = z =>
    Zipper.do_until_piece(grow_left_by_piece, rule_or_end_of_seg_to_right, z);
  let secondary_to_left =
    fun
    | (Some(Piece.Secondary(_)), _) => true
    | _ => false;
  let shrink_past_secondary = z =>
    !secondary_to_left(Siblings.neighbors(z.relatives.siblings))
      ? Some(z)
      : Zipper.do_until_piece(shrink_right_by_piece, secondary_to_left, z);
  let* z = current_tile(z);
  let* z = grow_right_until_case_or_rule(z);
  let* z = shrink_past_secondary(z);
  Some(z);
};

/* Check if a piece matches a shard, accounting for reassembled tiles.
 * A full tile like (0+1,1) matches shard 0 or shard 1 of the same tile. */
let piece_matches_shard = (piece: Piece.t, shard: Piece.t): bool =>
  piece == shard
  || (
    switch (piece, shard) {
    | (Tile(t1), Tile(t2)) =>
      Id.equal(t1.id, t2.id)
      && (
        switch (t2.shards) {
        | [s] => List.mem(s, t1.shards)
        | _ => false
        }
      )
    | _ => false
    }
  );

/* Select the (inclusive) range between two shards */
let shard_range = (l: Piece.t, r: Piece.t, z: t): option(t) => {
  let pl = neighbors =>
    switch (neighbors) {
    | (_, Some(piece)) => piece_matches_shard(piece, l)
    | _ => false
    };
  let pr = neighbors =>
    switch (neighbors) {
    | (Some(piece), _) => piece_matches_shard(piece, r)
    | _ => false
    };
  let* z =
    pl(Zipper.generalized_neighbors(z))
      ? Some(z) : Zipper.do_until(Move.local(ByToken, Left), pl, z);
  Zipper.do_until(local(Right), pr, z);
};

/* Select the currently indicated term. Optionally, we can consider
 * definitions to not include their bodies, and we can consider case
 * rules as separate pseudo-terms. */
let current_term =
    (
      term_data: TermData.t,
      ~defs_exclude_bodies: bool,
      ~case_rules: bool,
      z: t,
    ) => {
  let* {piece: p, _} = Indicated.for_decoration(z);
  switch (p) {
  | Tile({label: ["let" | "type" | "module", "=", "in"], _})
      when defs_exclude_bodies =>
    current_tile(z)
  | Tile({label: ["|", "=>"], _}) when case_rules => containing_rule(z)
  | _ =>
    let* id = current_term_id(z);
    switch (TermData.extreme_ids(id, term_data)) {
    | Some((lid, rid)) when Id.equal(lid, rid) =>
      /* Term bounded by a single tile (parens, brackets, etc.);
       * shard_range can't handle same-tile extremes after reassembly */
      tile(lid, z)
    | _ =>
      let* (l, r) = TermData.extremes_shards(id, term_data);
      shard_range(l, r, z);
    };
  };
};

/* Select a term by its id using term_data extremes, without
 * needing to navigate to the term first. Used as fallback for
 * terms whose id doesn't correspond to any tile (e.g., Ap from
 * juxtaposition, where MakeTerm assigns a fresh id). */
let term_by_extremes = (id: Id.t, term_data: TermData.t, z: t): option(t) =>
  switch (TermData.extreme_ids(id, term_data)) {
  | Some((lid, rid)) when Id.equal(lid, rid) => tile(lid, z)
  | _ =>
    let* (l, r) = TermData.extremes_shards(id, term_data);
    shard_range(l, r, z);
  };

/* Select a term by id. Navigates to the term and uses current_term
 * (which applies special cases for defs, case rules, comma→parens).
 * Falls back to term_by_extremes for virtual term ids. */
let term =
    (
      ~defs_exclude_bodies: bool,
      ~case_rules: bool,
      term_data: TermData.t,
      id: Id.t,
      z: t,
    )
    : option(t) =>
  switch (Move.jump_to_id_indicated(z, id)) {
  | Some(z) => current_term(term_data, ~defs_exclude_bodies, ~case_rules, z)
  | None => term_by_extremes(id, term_data, z)
  };

/* Select the containing run of secondary if any */
let containing_secondary_run = (z: t): option(t) => {
  let not_comment_or_space = (p: Piece.t) =>
    switch (p) {
    | Secondary(s) => Secondary.is_linebreak(s)
    | _ => true
    };
  let not_comment_or_space_to_right =
    fun
    | (_, Some(piece)) => not_comment_or_space(piece)
    | (_, None) => true;
  let not_comment_or_space_to_left =
    fun
    | (Some(piece), _) => not_comment_or_space(piece)
    | (None, _) => true;
  let grow_right_until_not_comment_or_space =
    Zipper.do_until(local(Right), not_comment_or_space_to_right);
  let move_left_until_not_comment_or_space = z =>
    not_comment_or_space_to_left(Zipper.generalized_neighbors(z))
      ? Some(z)
      : Zipper.do_until(
          Move.local(ByToken, Left),
          not_comment_or_space_to_left,
          z,
        );
  let z =
    switch (move_left_until_not_comment_or_space(z)) {
    | None => z
    | Some(z) => z
    };
  grow_right_until_not_comment_or_space(z);
};

/* Select the indicated token. For the purposes of this function,
 * contiguous spans of secondary are considered a single token,
 * although technically this is not the case */
let indicated_token = (z: t) =>
  switch (Indicated.for_index(z)) {
  | Some({piece: Secondary(_), _}) =>
    /* If there is secondary on both sides, select the
     * largest contiguous run of non-linebreak secondary */
    containing_secondary_run(z)
  | Some({side: Left, _}) when z.caret == Outer =>
    /* If we're on the far right side of a non-secondary piece, we
     * still prefer to select it over secondary to the right */
    let* z = Move.local(ByToken, Left, z);
    local(Right, z);
  | Some(_) => local(Right, z)
  | _ => None
  };

/* See `parent_of_indicated` */
let def_body_indicated =
    (z: t, info_map: Language.Statics.Map.t): option(Id.t) => {
  let* id = Indicated.index(z);
  let* parent_id =
    Language.Statics.Map.ancestors_of(id, info_map) |> ListUtil.hd_opt;
  let* ci_parent = Language.Statics.Map.lookup_exp(parent_id, info_map);
  switch (ci_parent) {
  | {user_term: {term: Let(_, _, body) | TyAlias(_, _, body), _}, _} =>
    let body_id = Language.IdTagged.rep_id(body);
    id == body_id ? Some(body_id) : None;
  | _ => None
  };
};

/* Rules aren't treated as terms in the syntax model, but we
 * do want to treat them as possible parents for selection */
let parent_is_rule = (z: t, info_map): option(Id.t) => {
  let is_case_or_rule = (p: Piece.t) =>
    switch (p) {
    | Tile({label: ["case", "end"], _}) => true
    | Tile({label: ["|", "=>"], _}) => true
    | _ => false
    };
  let move_left_until_case_or_rule =
    Zipper.do_until(Move.local(ByToken, Left), neighbors =>
      switch (neighbors) {
      | (Some(piece), _) => is_case_or_rule(piece)
      | _ => false
      }
    );
  let is_inside_rule = (z: t) => {
    let* z = move_left_until_case_or_rule(z);
    let* {piece: p, _} = Indicated.for_decoration(z);
    switch (p) {
    | Tile({label: ["|", "=>"], id, _}) => Some(id)
    | _ => None
    };
  };
  let parent_cls = (z: t, info_map) => {
    let* id = Indicated.index(z);
    let* parent_id =
      Language.Statics.Map.ancestors_of(id, info_map) |> ListUtil.hd_opt;
    let+ parent_statics = Language.Statics.Map.lookup(parent_id, info_map);
    Language.Statics.Info.cls_of(parent_statics);
  };
  switch (is_inside_rule(z)) {
  | Some(id) when parent_cls(z, info_map) == Some(Exp(Match)) => Some(id)
  | _ => None
  };
};

let parent_term_id = (z: t, info_map) => {
  let* base_id = Indicated.index(z);
  switch (parent_is_rule(z, info_map)) {
  | Some(id) => Some(id)
  | None =>
    Language.Statics.Map.ancestors_of(base_id, info_map) |> ListUtil.hd_opt
  };
};

let is_rule_tile =
  fun
  | Piece.Tile({label: ["|", "=>"], _}) => true
  | _ => false;

/* Check if id has a module item cls (ModLet, ModType, etc.).
 * Module items are elaborated as nested Lets, so they need
 * special handling to avoid escalating between siblings. */
let has_mod_cls = (id, info_map) =>
  switch (Id.Map.find_opt(id, info_map)) {
  | Some(
      Language.Statics.Info.InfoExp({
        cls: Mod(ModLet | ModType | ModuleMod | ModExp),
        _,
      }),
    ) =>
    true
  | _ => false
  };

/* Check if from_id is the body of the definition at parent_id.
 * Returns false for module items (elaborated as nested Lets
 * where each item appears as the "body" of the previous). */
let is_def_body = (from_id, parent_id, info_map) =>
  switch (Language.Statics.Map.lookup(parent_id, info_map)) {
  | Some(
      Language.Statics.Info.InfoExp({
        cls: Mod(ModLet | ModType | ModuleMod),
        _,
      }),
    ) =>
    false
  | Some(
      Language.Statics.Info.InfoExp({
        user_term: {term: Let(_, _, body) | TyAlias(_, _, body), _},
        _,
      }),
    ) =>
    Language.IdTagged.rep_id(body) == from_id
  | _ => false
  };

/* Select a term as a parent. In general we use
 * defs_exclude_bodies=true so definitions are treated as
 * pseudo-terms (header only). But if the indicated piece
 * is the body of a definition, we use false so the parent
 * selection includes that body. Take care in refactoring
 * this, as it's easy to overgeneralize: only the body
 * of a def should exhibit this behavior, not the name
 * or def terms. */
let select_as_parent =
    (parent_id: Id.t, z: t, term_data: TermData.t, info_map) => {
  let defs_exclude_bodies =
    switch (def_body_indicated(z, info_map)) {
    | Some(_) => false
    | None => true
    };
  term(~defs_exclude_bodies, ~case_rules=true, term_data, parent_id, z);
};

let parent_of_indicated = (z: t, term_data, info_map) => {
  let* id = parent_term_id(z, info_map);
  select_as_parent(id, z, term_data, info_map);
};

/* Escalate from a fully-matched selection to its parent.
 * Priority order:
 * 1. Def body → parent def (include body in selection)
 * 2. Module item → enclosing module tile {…}
 * 3. Inside case rule → enclosing rule tile |=>
 * 4. Default → parent term from info_map ancestors */
let escalate_from_term =
    (root_id: Id.t, parent_id: Id.t, z: t, term_data: TermData.t, info_map) =>
  if (is_def_body(root_id, parent_id, info_map)) {
    term(
      ~defs_exclude_bodies=false,
      ~case_rules=true,
      term_data,
      parent_id,
      z,
    );
  } else if (has_mod_cls(root_id, info_map)) {
    let* p = Zipper.parent(z);
    select_as_parent(Piece.id(p), z, term_data, info_map);
  } else {
    /* Find the closest enclosing rule tile in left siblings.
     * Left siblings are in document order, so reverse to
     * find the nearest one first. */
    let enclosing_rule =
      fst(z.relatives.siblings) |> List.rev |> List.find_opt(is_rule_tile);
    switch (enclosing_rule) {
    | Some(p) => select_as_parent(Piece.id(p), z, term_data, info_map)
    | None => select_as_parent(parent_id, z, term_data, info_map)
    };
  };

/* Select the smallest term strictly enclosing the current
 * selection (or cursor position if no selection). For empty
 * selections, this is the indicated term. For non-empty
 * selections, we find the root term of the selection using
 * measured ranges, then either select it (if it's bigger
 * than the selection) or escalate to its parent (if the
 * selection already covers it). */
let select_enclosing_term =
    (
      term_data: TermData.t,
      measured: Measured.t,
      info_map: Language.Statics.Map.t,
      z: t,
    )
    : option(t) => {
  switch (z.selection.content) {
  | [] =>
    current_term(term_data, ~defs_exclude_bodies=true, ~case_rules=true, z)
  | sel =>
    let z0 = Zipper.unselect(z);
    if (List.exists(is_rule_tile, sel)) {
      /* Rule → case: rules aren't terms in term_data,
       * so escalate to the parent case expression */
      let* p = Zipper.parent(z0);
      select_as_parent(Piece.id(p), z0, term_data, info_map);
    } else {
      let* root_id =
        TermData.get_root_id_using_ranges(sel, term_data, measured);
      let root_sel =
        term(
          ~defs_exclude_bodies=false,
          ~case_rules=true,
          term_data,
          root_id,
          z0,
        );
      let sel_matches =
        switch (root_sel) {
        | Some(z') => z'.selection.content == sel
        | None => false
        };
      if (sel_matches) {
        let* info = Id.Map.find_opt(root_id, info_map);
        let* parent_id = Language.Info.ancestors_of(info) |> ListUtil.hd_opt;
        escalate_from_term(root_id, parent_id, z0, term_data, info_map);
      } else {
        /* Selection is partial or matches only def header:
         * round up to root term (including body) */
        root_sel;
      };
    };
  };
};

let smart = (term_data, info_map, n, z: t): option(t) => {
  switch (n) {
  | 2 => indicated_token(z)
  | 3 =>
    /* Use the selected piece from Smart(2) to determine what
     * term to select. This avoids the fragile unselect-then-
     * re-indicate pattern, which fails when reassembly after
     * unselect changes the cursor's structural position (e.g.
     * creating ancestors from multi-shard tile shards). */
    switch (z.selection.content) {
    | [p] when Piece.is_term(p) =>
      /* Single-token term: Smart(2) already selected the whole
       * term, so Smart(3) escalates to the parent term. Unselect
       * to anchor side (safe for single-token terms). */
      let z0 =
        Zipper.directional_unselect(Direction.toggle(z.selection.focus), z);
      parent_of_indicated(z0, term_data, info_map);
    | [p] =>
      /* Non-term token (operator, delimiter, multi-shard tile):
       * select the term containing this token. Jump to the piece
       * by ID, which navigates independently of the current
       * cursor position. */
      let id = Piece.id(p);
      let z = Zipper.unselect(z);
      term(~defs_exclude_bodies=true, ~case_rules=true, term_data, id, z);
    | _ => None
    }
  | _ => None
  };
};

let vertical =
    (
      d: Action.vertical,
      ~col_target: int,
      ~measured: Measured.t,
      ~chunkiness: Action.chunkiness=ByChar,
      z: t,
    )
    : option(t) => {
  let goal =
    Point.{
      col: col_target,
      row: Zipper.Caret.point(measured, z).row + (d == Down ? 1 : (-1)),
    };
  let step =
    switch (chunkiness) {
    | ByChar => local_by_char
    | ByToken => local
    | BySmart => local_smart
    };
  Zipper.do_towards_point(~measured, ~force_progress=true, step, goal, z);
};

let to_point =
    (
      ~chunkiness: Action.chunkiness=ByChar,
      ~measured: Measured.t,
      ~goal: Point.t,
      z: t,
    )
    : option(t) => {
  let anchor = z |> toggle_focus |> Zipper.Caret.point(measured);
  let step =
    switch (chunkiness) {
    | ByChar => local_by_char
    | ByToken => local
    | BySmart => local_smart
    };
  switch (Zipper.do_towards_point(~measured, ~anchor, step, goal, z)) {
  | None => Some(z)
  | Some(z) => Some(z)
  };
};

let to_start: t => t = Zipper.do_to_extreme(local(Left));

let to_end: t => t = Zipper.do_to_extreme(local(Right));

let all = (z: t): t => z |> Move.to_start |> to_end;

let to_linebreak = (d: Direction.t, z: t): option(t) =>
  Zipper.do_until_linebreak(local(d), d, z);
