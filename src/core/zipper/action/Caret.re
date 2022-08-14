open Util;
open Zipper;

[@deriving (show({with_path: false}), sexp, yojson)]
type chunkiness =
  | ByChar
  | MonoByChar
  | ByToken;

let update = (f: caret => caret, z: t): t => {...z, caret: f(z.caret)};

let set = (caret: caret): (t => t) => update(_ => caret);

let decrement: caret => caret =
  fun
  | Outer
  | Inner(_, 0) => Outer
  | Inner(d, c) => Inner(d, c - 1);

let caret_offset: caret => int =
  fun
  | Outer => 0
  | Inner(_, c) => c + 1;

let direction = (z: t): option(Direction.t) =>
  /* Direction the caret is facing in */
  switch (z.caret) {
  | Inner(_) => None
  | Outer =>
    switch (Siblings.neighbors(sibs_with_sel(z))) {
    | (Some(l), Some(r))
        when Piece.is_whitespace(l) && Piece.is_whitespace(r) =>
      None
    | _ => Siblings.direction_between(sibs_with_sel(z))
    }
  };

let representative_piece = (z: t): option((Piece.t, Direction.t)) => {
  /* The piece to the left of the caret, or if none exists, the piece to the right */
  switch (Siblings.neighbors(sibs_with_sel(z))) {
  | (Some(l), _) => Some((l, Left))
  | (_, Some(r)) => Some((r, Right))
  | _ => None
  };
};

let base_point = (map: Measured.t, z: t): Measured.point => {
  switch (representative_piece(z)) {
  | Some((p, d)) =>
    let seg = Piece.disassemble(p);
    switch (d) {
    | Left =>
      let p = ListUtil.last(seg);
      let m = Measured.find_p(p, map);
      m.last;
    | Right =>
      let p = List.hd(seg);
      let m = Measured.find_p(p, map);
      m.origin;
    };
  | None => {row: 0, col: 0}
  };
};

let point = (map: Measured.t, z: t): Measured.point => {
  let Measured.{row, col} = base_point(map, z);
  {row, col: col + caret_offset(z.caret)};
};

let update_target = (z: t): t =>
  //NOTE(andrew): $$$ this recomputes all measures
  {
    ...z,
    caret_col_target:
      point(Measured.of_segment(unselect_and_zip(z)), z).col,
  };

type comparison =
  | Exact
  | Under
  | Over;

let comp = (current, target): comparison =>
  switch () {
  | _ when current == target => Exact
  | _ when current < target => Under
  | _ => Over
  };

let dcomp = (direction: Direction.t, a, b) =>
  switch (direction) {
  | Right => comp(a, b)
  | Left => comp(b, a)
  };

let do_towards =
    (
      ~anchor: option(Measured.point)=?,
      f: (Direction.t, t) => option(t),
      goal: Measured.point,
      z: t,
    )
    : option(t) => {
  let cursorpos = point(Measured.of_segment(unselect_and_zip(z)));
  let init = cursorpos(z);
  let d =
    goal.row < init.row || goal.row == init.row && goal.col < init.col
      ? Direction.Left : Right;

  let rec go = (prev: t, curr: t) => {
    let curr_p = cursorpos(curr);
    switch (dcomp(d, curr_p.col, goal.col), dcomp(d, curr_p.row, goal.row)) {
    | (Exact, Exact) => curr
    | (_, Over) => prev
    | (_, Under)
    | (Under, Exact) =>
      switch (f(d, curr)) {
      | None => curr
      | Some(next) => go(curr, next)
      }
    | (Over, Exact) =>
      switch (anchor) {
      | None =>
        let d_curr = abs(curr_p.col - goal.col);
        let d_prev = abs(cursorpos(prev).col - goal.col);
        // default to going over when equal
        d_prev < d_curr ? prev : curr;
      | Some(anchor) =>
        let anchor_d =
          goal.row < anchor.row
          || goal.row == anchor.row
          && goal.col < anchor.col
            ? Direction.Left : Right;
        anchor_d == d ? curr : prev;
      }
    };
  };

  let res = go(z, z);
  Measured.point_equals(cursorpos(res), cursorpos(z)) ? None : Some(res);
};

let do_vertical =
    (f: (Direction.t, t) => option(t), d: Direction.t, z: t): option(t) => {
  /* Here f should be a function which results in strict d-wards
     movement of the caret. Iterate f until we get to the closet
     caret position to a target derived from the initial position */
  let cursorpos = point(Measured.of_segment(unselect_and_zip(z)));
  let cur_p = cursorpos(z);
  let goal =
    Measured.{
      col: z.caret_col_target,
      row: cur_p.row + (d == Right ? 1 : (-1)),
    };
  // Printf.printf("Caret.do_vertical: cur: %s\n", Measured.show_point(cur_p));
  // Printf.printf("Caret.do_vertical: goal: %s\n", Measured.show_point(goal));
  do_towards(f, goal, z);
};

let do_extreme =
    (f: (Direction.t, t) => option(t), d: planar, z: t): option(t) => {
  let cursorpos = point(Measured.of_segment(unselect_and_zip(z)));
  let cur_p = cursorpos(z);
  let goal: Measured.point =
    switch (d) {
    | Right(_) => {col: Int.max_int, row: cur_p.row}
    | Left(_) => {col: 0, row: cur_p.row}
    | Up => {col: 0, row: 0}
    | Down => {col: Int.max_int, row: Int.max_int}
    };
  do_towards(f, goal, z);
};
