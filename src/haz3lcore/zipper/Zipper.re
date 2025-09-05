open Util;
open OptUtil.Syntax;
include ZipperBase;

let init: unit => t =
  () => {
    selection: Selection.mk([]),
    relatives: {
      siblings: (
        [],
        [
          Grout({
            id: Id.mk(),
            shape: Convex,
          }),
        ],
      ),
      ancestors: [],
    },
    caret: Outer,
  };

let next_blank = _ => Id.mk();

let delete_parent = (z: t): t => {
  ...z,
  relatives: Relatives.delete_parent(z.relatives),
};

let zip = (z: t): Segment.t =>
  Relatives.zip(~sel=z.selection.content, z.relatives);

let unzip = (seg: Segment.t): t => {
  selection: Selection.mk([]),
  relatives: {
    siblings: (seg, []),
    ancestors: [],
  },
  caret: Outer,
};

let regrout = (d: Direction.t, z: t): t => {
  assert(Selection.is_empty(z.selection));
  let relatives = Relatives.regrout(d, z.relatives);
  {
    ...z,
    relatives,
  };
};

let remold = (z: t): t => {
  assert(Selection.is_empty(z.selection));
  {
    ...z,
    relatives: Relatives.remold(z.relatives),
  };
};

let remold_regrout = (d: Direction.t, z: t): t => z |> remold |> regrout(d);

let clear_unparsed_buffer = (z: t) =>
  switch (z.selection.mode) {
  | Buffer(Unparsed) => {
      ...z,
      selection: Selection.empty,
    }
  | _ => z
  };

let unselect = (~erase_buffer=false, z: t): t => {
  /* NOTE(andrew): Erase buffer flag only applies to unparsed buffer,
   * that is, the buffer style that just contains a single flat token.
   * Erasing a buffer that contains arbitrary tiles would be more complex
   * as we can't just empty the selection without regrouting */
  let z = erase_buffer ? clear_unparsed_buffer(z) : z;
  let relatives =
    z.relatives
    |> Relatives.prepend(z.selection.focus, z.selection.content)
    |> Relatives.reassemble;
  let selection = Selection.empty;
  {
    ...z,
    selection,
    relatives,
  };
};

let destroy_selection: t => t =
  z =>
    unselect({
      ...z,
      selection: Selection.empty,
    });

let unselect_and_zip = (~erase_buffer=false, z: t): Segment.t =>
  z |> unselect(~erase_buffer) |> zip;

let replace_selection = (focus, segment, z: t): t => {
  ...z,
  selection: Selection.mk(~focus, segment),
};

let grow_selection = (z: t): option(t) => {
  let+ (p, relatives) = Relatives.pop(z.selection.focus, z.relatives);
  let selection = Selection.push(p, z.selection);
  {
    ...z,
    selection,
    relatives,
  };
};

// toggles focus and grows if selection is empty
let shrink_selection = (z: t): option(t) => {
  switch (Selection.pop(z.selection)) {
  | None =>
    let selection = Selection.toggle_focus(z.selection);
    grow_selection({
      ...z,
      selection,
    });
  | Some((p, selection)) =>
    let relatives =
      z.relatives
      |> Relatives.push(selection.focus, p)
      |> Relatives.reassemble;
    Some({
      ...z,
      selection,
      relatives,
    });
  };
};

let toggle_focus = (z: t): t => {
  ...z,
  selection: Selection.toggle_focus(z.selection),
};

let set_focus = (z: t, d: Direction.t): t => {
  let selection = {
    ...z.selection,
    focus: d,
  };
  {
    ...z,
    selection,
  };
};

let directional_unselect = (d: Direction.t, z: t): t => {
  let selection = {
    ...z.selection,
    focus: Direction.toggle(d),
  };
  unselect({
    ...z,
    selection,
  });
};

let unselect = (z: t): t =>
  z.selection.content == [] ? z : directional_unselect(z.selection.focus, z);

let move = (d: Direction.t, z: t): option(t) =>
  if (Selection.is_empty(z.selection)) {
    let+ (p, relatives) = Relatives.pop(d, z.relatives);
    let relatives =
      relatives
      |> Relatives.push(Direction.toggle(d), p)
      |> Relatives.reassemble;
    {
      ...z,
      relatives,
    };
  } else {
    Some(directional_unselect(d, z));
  };

let select = (d: Direction.t, z: t): option(t) =>
  d == z.selection.focus ? grow_selection(z) : shrink_selection(z);

/* As opposed to the Siblings.neighbor functions, which simply returns
 * the adjacent piece (if any) in the focal segment, this function is a
 * more general notion of 'the token to the left/right' of the cursor'.
 * It agrees with Sibling.neighbor whenever you are in the middle of
 * the focal segment; it returns None only if you are at the start/end
 * of the entire program, and if you are at an extreme of the focal
 * segment it returns the ADJACENT SHARD of the containing parent.
 * Note that this last case necessarily returns an incomplete tile and
 * thus does not retain knowledge of the tile's in-situ completeness */
let generalized_neighbor = (d: Direction.t, z: t): option(Piece.t) => {
  let* z = select(d, unselect(z));
  switch (z.selection.content) {
  | [p] => Some(p)
  | _ => None
  };
};

type neighbors = (option(Piece.t), option(Piece.t));

let generalized_neighbors = (z: t): neighbors => (
  generalized_neighbor(Left, z),
  generalized_neighbor(Right, z),
);

let neighbor_token = (d: Direction.t, z: t): option(Token.t) => {
  let* p = generalized_neighbor(d, z);
  Piece.token_of(p);
};

let neighbor_tokens = (z: t): (option(Token.t), option(Token.t)) => (
  neighbor_token(Left, z),
  neighbor_token(Right, z),
);

let rec do_until_piece =
        (action: t => option(t), p_n: neighbors => bool, z: t): option(t) => {
  let* z = action(z);
  if (p_n(Siblings.neighbors(z.relatives.siblings))) {
    Some(z);
  } else {
    do_until_piece(action, p_n, z);
  };
};

/* Do `action` until the predicate on the generalized neigbors of the
   caret becomes true. A generalized neighbor is the neighboring piece, unless
   the neighbor is a polytile, in which case it's the relevant shard, or
   we are at the edge of a segment, in which case it's the relevant shard
   of the parent. The None case strictly means the beginning/end of the program.
   If no such piece is found, don't move. Does not check predicate before
   moving; caller should handle that case if necessary */
let rec do_until =
        (action: t => option(t), p_n: neighbors => bool, z: t): option(t) => {
  let* z = action(z);
  if (p_n(generalized_neighbors(z))) {
    Some(z);
  } else {
    do_until(action, p_n, z);
  };
};

let do_to_extreme = (action: t => option(t), z: t): t =>
  do_until(
    action,
    (neighbors: neighbors) =>
      switch (neighbors) {
      | (None, _) => true
      | (_, None) => true
      | _ => false
      },
    z,
  )
  |> Option.value(~default=z);

let linebreak_on = (d: Direction.t, neighbors: neighbors): bool =>
  switch (neighbors) {
  | (_, Some(Secondary(s))) when d == Right && Secondary.is_linebreak(s) =>
    true
  | (_, None) when d == Right => true
  | (Some(Secondary(s)), _) when d == Left && Secondary.is_linebreak(s) =>
    true
  | (None, _) when d == Left => true
  | _ => false
  };

let do_until_linebreak =
    (f: t => option(t), d: Direction.t, z: t): option(t) =>
  linebreak_on(d, generalized_neighbors(z))
    ? Some(z) : do_until(f, linebreak_on(d), z);

let adj_pos = (d: Direction.t, z: t): t =>
  switch (d) {
  | Left => z
  | Right =>
    switch (move(Left, z)) {
    | None => z
    | Some(z) => z
    }
  };

let insert_segment = (z: t, segment: Segment.t): t =>
  replace_selection(Right, segment, z) |> unselect |> remold_regrout(Right);

let put_down_core = (seg: Segment.t, z: t): t =>
  z |> replace_selection(Right, seg) |> unselect;

let put_down_seg = (d: Direction.t, seg: Segment.t, z: t): t =>
  z |> put_down_core(seg) |> adj_pos(d);

let local_backpack = (z: t): list(Tile.t) =>
  Relatives.local_missing_shards(z.relatives);

let can_put_down = z =>
  switch (local_backpack(z)) {
  | [] => false
  | _ => z.caret == Outer
  };

let put_down_regrout_target = (d: Direction.t, target: Tile.t, z: t): t => {
  let z = put_down_core([Tile(target)], z);
  let z = z |> remold |> regrout(Left);
  adj_pos(d, z);
};

let backpack_hd = (z: t): option(Tile.t) =>
  z |> local_backpack |> ListUtil.hd_opt;

let put_down_remold_regrout = (d: Direction.t, z: t): option(t) => {
  let+ target = backpack_hd(z);
  put_down_regrout_target(d, target, z);
};

let backpack_find = (tok: Token.t, z: t): option(Tile.t) =>
  if (Form.is_ambiguous_polymorph(tok)) {
    /* Special case for ambiguous polymorphs. These tokens
       occur both on their own as infix ops and as delimiters of
       multi-delimiter forms. To give the singleton form a chance, we
       only match these to incomplete tiles to form their multi forms
       when they're on the top of the stack */
    backpack_hd(z) |> Option.map(Tile.effective_label) == Some([tok])
      ? backpack_hd(z) : None;
  } else {
    List.find_map(
      t => Tile.effective_label(t) == [tok] ? Some(t) : None,
      local_backpack(z),
    );
  };

let will_glom = (tok: Token.t, z: t): bool => backpack_find(tok, z) != None;

let glom = (d: Direction.t, tok: Token.t, z: t): option(t) => {
  let+ target = backpack_find(tok, z);
  put_down_regrout_target(d, target, z);
};

let delete = (d: Direction.t, z: t): option(t) =>
  z |> select(d) |> Option.map(destroy_selection);

let glom_prev = (z: t) =>
  switch (neighbor_token(Left, z)) {
  | Some(t) when will_glom(t, z) =>
    switch (delete(Left, z)) {
    | Some(z) => glom(Left, t, z)
    | None => Some(z)
    }
  | _ => None
  };

let put_down_glom = (z: t): option(t) =>
  switch (z.caret) {
  | Inner(_) => None
  | Outer =>
    switch (glom_prev(z)) {
    | Some(z) => Some(z)
    | None => put_down_remold_regrout(Left, z)
    }
  };

let adjacent_monotile_id = (d: Direction.t, z: t): option(Id.t) =>
  switch (Siblings.neighbors(z.relatives.siblings)) {
  | (Some(Tile({id, label: [_], _})), _) when d == Left => Some(id)
  | (_, Some(Tile({id, label: [_], _}))) when d == Right => Some(id)
  | _ => None
  };

let adjacent_monotile_or_new_id = (d, z) =>
  switch (adjacent_monotile_id(d, z)) {
  | Some(id) => id
  | None => Id.mk()
  };

let representative_piece = (z: t): option((Piece.t, Direction.t)) => {
  /* The piece to the left of the caret, or if none exists, the piece to the right */
  switch (Siblings.neighbors(sibs_with_sel(z))) {
  | (Some(l), _) => Some((l, Left))
  | (_, Some(r)) => Some((r, Right))
  | _ => None
  };
};

let base_point = (measured: Measured.t, z: t): Point.t => {
  switch (representative_piece(z)) {
  | Some((p, d)) =>
    let seg = Piece.disassemble(p);
    switch (d) {
    | Left =>
      let p = ListUtil.last(seg);
      let m = Measured.find_p(~msg="base_point", p, measured);
      m.last;
    | Right =>
      let p = List.hd(seg);
      let m = Measured.find_p(~msg="base_point", p, measured);
      m.origin;
    };
  | None => {
      row: 0,
      col: 0,
    }
  };
};

module Caret = {
  let offset: caret => int =
    fun
    | Outer => 0
    | Inner(idx) => idx + 1;

  let set = (caret: caret, z: t): t => {
    ...z,
    caret,
  };

  /* Max internal index of the shard the caret is adjacent to */
  let nhbr_max_idx = (d: Direction.t, z: t): option(int) => {
    let* t =
      switch (d, neighbor_tokens(z)) {
      | (Left, (Some(t), _)) => Some(t)
      | (Right, (_, Some(t))) => Some(t)
      | _ => None
      };
    let max_idx = Token.length(t) - 2;
    max_idx < 0 ? None : Some(max_idx);
  };

  /* Returns the delimiter index that the caret is adjacent to.
   * For non-tiles and monotiles this is always zero */
  let delim_idx = (z: t) =>
    switch (snd(z.relatives.siblings), z.relatives.ancestors) {
    | ([], [({shards: (l, _), _}, _), ..._]) => List.length(l)
    | _ => 0
    };

  /* Direction the caret is facing in */
  let direction = (z: t): option(Direction.t) =>
    switch (z.caret) {
    | Inner(_) => None
    | Outer =>
      switch (Siblings.neighbors(sibs_with_sel(z))) {
      | (Some(l), Some(r))
          when
            Piece.is_secondary(l)
            && Piece.is_secondary(r)
            && Selection.is_empty(z.selection) =>
        None
      | _ => Siblings.direction_between(sibs_with_sel(z))
      }
    };

  /* Grid position of the caret */
  let point = (measured: Measured.t, z: t): Point.t => {
    let Point.{row, col} = base_point(measured, z);
    {
      row,
      col: col + offset(z.caret),
    };
  };

  type t = ZipperBase.caret;
};

let do_towards_point =
    (
      ~anchor: option(Measured.Point.t)=?,
      ~measured: Measured.t,
      ~force_progress: bool=false,
      f: (Direction.t, t) => option(t),
      goal: Measured.Point.t,
      z: t,
    )
    : option(t) => {
  let caret_point = Caret.point(measured);

  let is_at_side_of_row = (d: Direction.t, z: t) => {
    let Point.{row, col} = caret_point(z);
    switch (move(d, z)) {
    | None => true
    | Some(z) =>
      let Point.{row: rowp, col: colp} = caret_point(z);
      row != rowp || col == colp;
    };
  };

  let direction_to_from = (p1: Point.t, p2: Point.t): Direction.t => {
    let before_row = p1.row < p2.row;
    let at_row = p1.row == p2.row;
    let before_col = p1.col < p2.col;
    before_row || at_row && before_col ? Left : Right;
  };

  let closer_to_prev = (curr, prev, goal: Point.t) =>
    /* Default to true if equal */
    abs(caret_point(prev).col - goal.col)
    < abs(caret_point(curr).col - goal.col);

  let init = caret_point(z);
  let d_to_goal = direction_to_from(goal, init);
  let rec go = (prev: t, curr: t) => {
    let curr_p = caret_point(curr);
    let x_progress = Point.dcomp(d_to_goal, curr_p.col, goal.col);
    let y_progress = Point.dcomp(d_to_goal, curr_p.row, goal.row);
    switch (y_progress, x_progress) {
    /* If we're not there yet, keep going */
    | (Under, Over | Exact | Under)
    | (Exact, Under) =>
      switch (f(d_to_goal, curr)) {
      | Some(next) => go(curr, next)
      | None => curr /* Should only occur at start/end of program */
      }
    /* If we're there, stop */
    | (Exact, Exact) => curr
    /* If we've overshot, meaning the exact goal is inaccessible,
     * we choose between current and previous (undershot) positions */
    | (Over, Over | Exact | Under) =>
      switch (force_progress) {
      /* Ideally we would use the same logic as from the below
       * anchor case here; however that results in strange
       * behavior when accidentally starting a drag at the end
       * of a line, which triggers the (invisible) selection of
       * a linebreak, making it appear that the caret has jumped
       * to the next line. The downside of leaving this as-is is
       * that multiline tokens (projectors) do not become part of
       * the selection when dragging until you're all the way
       * over them, which is slightly visually jarring */
      | false => prev
      /* Up/down kb movement works by setting a goal one row
       * below the current. When adjacent to a multiline token,
       * the nearest next caret position may be multiple lines down.
       * We must allow this overshoot in order to make progress. */
      | true => caret_point(prev) == init ? curr : prev
      }
    | (Exact, Over) =>
      switch (anchor) {
      | None =>
        /* If you're trying to (eg) move down at the end of a row
         * but the first position of the next row is further right
         * than the currentrow's end, we want to make progress
         * regardless of whether the new position would be closer
         * or further from the goal.  Otherwise, we try to just
         * get as close as we can  */
        is_at_side_of_row(Direction.toggle(d_to_goal), curr)
          ? curr : closer_to_prev(curr, prev, goal) ? prev : curr
      | Some(anchor) =>
        /* If we're dragging to make a selection, decide whether or
         * not to force progress based on the relative position of the
         * anchor (the position where the drag was started) */
        direction_to_from(goal, anchor) == d_to_goal ? curr : prev
      }
    };
  };
  let res = go(z, z);
  Measured.Point.equals(caret_point(res), caret_point(z))
    ? None : Some(res);
};

let selection_anchor_point = (measured, z: t): option(Point.t) => {
  switch (z.selection) {
  | {content: [], _} => None
  | {content, focus: Right, _} =>
    Some(
      Measured.find_p(
        ~msg="selection_anchor_point",
        List.hd(content),
        measured,
      ).
        origin,
    )
  | {content, focus: Left, _} =>
    Some(
      Measured.find_p(
        ~msg="selection_anchor_point",
        ListUtil.last(content),
        measured,
      ).
        last,
    )
  };
};

let serialize = (z: t): string => {
  sexp_of_t(z) |> Sexplib.Sexp.to_string;
};

let to_sexp = (z: t): Sexplib.Sexp.t => sexp_of_t(z);

let deserialize = (data: string): t => {
  Sexplib.Sexp.of_string(data) |> t_of_sexp;
};

let set_buffer = (z: t, ~mode: Selection.buffer, ~content: Segment.t): t => {
  ...z,
  selection: Selection.mk_buffer(mode, content),
};

let is_linebreak_to_right_of_caret =
    ({relatives: {siblings: (_, r), _}, _}: t): bool => {
  switch (r) {
  | [Secondary(s), ..._] when Secondary.is_linebreak(s) => true
  | _ => false
  };
};
