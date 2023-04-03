open Zipper;
open Util;
open OptUtil.Syntax;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type movability =
  | CanEnter(int, int)
  | CanPass
  | CantEven;

let movability = (chunkiness: chunkiness, label, delim_idx): movability => {
  assert(delim_idx < List.length(label));
  switch (chunkiness, label, delim_idx) {
  | (ByChar, _, _)
  | (MonoByChar, [_], 0) =>
    let char_max = Token.length(List.nth(label, delim_idx)) - 2;
    char_max < 0 ? CanPass : CanEnter(delim_idx, char_max);
  | (ByToken, _, _)
  | (MonoByChar, _, _) => CanPass
  };
};

let neighbor_movability =
    (chunkiness: chunkiness, {relatives: {siblings, ancestors}, _}: t)
    : (movability, movability) => {
  let movability = movability(chunkiness);
  let (supernhbr_l, supernhbr_r) =
    switch (ancestors) {
    | [] => (CantEven, CantEven)
    | [({children: (l_kids, _), label, _}, _), ..._] => (
        movability(label, List.length(l_kids)),
        movability(label, List.length(l_kids) + 1),
      )
    };
  let (l_nhbr, r_nhbr) = Siblings.neighbors(siblings);
  let l =
    switch (l_nhbr) {
    | Some(Tile({label, _})) => movability(label, List.length(label) - 1)
    | Some(Secondary(w)) when Secondary.is_comment(w) =>
      // Comments are always length >= 2
      let content_string = Secondary.get_string(w.content);
      CanEnter(
        Unicode.length(content_string) - 1,
        Unicode.length(content_string) - 2,
      );
    | Some(_) => CanPass
    | _ => supernhbr_l
    };
  let r =
    switch (r_nhbr) {
    | Some(Tile({label, _})) => movability(label, 0)
    | Some(Secondary(w)) when Secondary.is_comment(w) =>
      // Comments are always length >= 2
      let content_string = Secondary.get_string(w.content);
      CanEnter(0, Unicode.length(content_string) - 2);
    | Some(_) => CanPass
    | _ => supernhbr_r
    };
  (l, r);
};

module Make = (M: Editor.Meta.S) => {
  let caret_point = Zipper.caret_point(M.measured);

  let pop_out = z => Some(z |> Zipper.set_caret(Outer));
  let pop_move = (d, z) => z |> Zipper.set_caret(Outer) |> Zipper.move(d);
  let inner_incr = (delim, c, z) =>
    Some(Zipper.set_caret(Inner(delim, c + 1), z));
  let inner_decr = z => Some(Zipper.update_caret(Zipper.Caret.decrement, z));
  let inner_start = (d_init, z) =>
    Some(Zipper.set_caret(Inner(d_init, 0), z));
  let inner_end = (d, d_init, c_max, z) =>
    z |> Zipper.set_caret(Inner(d_init, c_max)) |> Zipper.move(d);

  let primary = (chunkiness: chunkiness, d: Direction.t, z: t): option(t) => {
    switch (d, z.caret, neighbor_movability(chunkiness, z)) {
    /* this case maybe shouldn't be necessary but currently covers an edge
       (select an open parens to left of a multichar token and press left) */
    | _ when z.selection.content != [] => pop_move(d, z)
    | (Left, Outer, (CanEnter(dlm, c_max), _)) =>
      inner_end(d, dlm, c_max, z)
    | (Left, Outer, _) => Zipper.move(d, z)
    | (Left, Inner(_), _) when chunkiness == ByToken => pop_out(z)
    | (Left, Inner(_), _) =>
      Some(Zipper.update_caret(Zipper.Caret.decrement, z))
    | (Right, Outer, (_, CanEnter(d_init, _))) => inner_start(d_init, z)
    | (Right, Outer, _) => Zipper.move(d, z)
    | (Right, Inner(_, c), (_, CanEnter(_, c_max))) when c == c_max =>
      pop_move(d, z)
    | (Right, Inner(_), _) when chunkiness == ByToken => pop_move(d, z)
    | (Right, Inner(delim, c), _) => inner_incr(delim, c, z)
    };
  };

  let do_towards =
      (
        ~anchor: option(Measured.Point.t)=?,
        f: (Direction.t, t) => option(t),
        goal: Measured.Point.t,
        z: t,
      )
      : option(t) => {
    let init = caret_point(z);
    let d =
      goal.row < init.row || goal.row == init.row && goal.col < init.col
        ? Direction.Left : Right;

    let rec go = (prev: t, curr: t) => {
      let curr_p = caret_point(curr);
      switch (
        Measured.Point.dcomp(d, curr_p.col, goal.col),
        Measured.Point.dcomp(d, curr_p.row, goal.row),
      ) {
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
          let d_prev = abs(caret_point(prev).col - goal.col);
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
    Measured.Point.equals(caret_point(res), caret_point(z))
      ? None : Some(res);
  };

  let do_vertical =
      (f: (Direction.t, t) => option(t), d: Direction.t, z: t): option(t) => {
    /* Here f should be a function which results in strict d-wards
       movement of the caret. Iterate f until we get to the closet
       caret position to a target derived from the initial position */
    let cur_p = caret_point(z);
    let goal =
      Measured.Point.{
        col: M.col_target,
        row: cur_p.row + (d == Right ? 1 : (-1)),
      };
    do_towards(f, goal, z);
  };

  let do_extreme =
      (f: (Direction.t, t) => option(t), d: planar, z: t): option(t) => {
    let cur_p = caret_point(z);
    let goal: Measured.Point.t =
      switch (d) {
      | Right(_) => {col: Int.max_int, row: cur_p.row}
      | Left(_) => {col: 0, row: cur_p.row}
      | Up => {col: 0, row: 0}
      | Down => {col: Int.max_int, row: Int.max_int}
      };
    do_towards(f, goal, z);
  };

  let to_start = do_extreme(primary(ByToken), Up);

  let jump_to_id = (z: t, id: Id.t): option(t) => {
    let* {origin, _} = Measured.find_by_id(id, M.measured);
    let z =
      switch (to_start(z)) {
      | None => z
      | Some(z) => z
      };
    switch (do_towards(primary(ByChar), origin, z)) {
    | None => Some(z)
    | Some(z) => Some(z)
    };
  };

  let vertical = (d: Direction.t, z: t): option(t) =>
    z.selection.content == []
      ? do_vertical(primary(ByChar), d, z)
      : Some(Zipper.directional_unselect(d, z));

  let targets_within_row = (z: t): list(t) => {
    let init = caret_point(z);
    let rec go = (d: Direction.t, z: t) => {
      switch (primary(ByChar, d, z)) {
      | None => []
      | Some(z) =>
        if (caret_point(z).row != init.row) {
          [];
        } else {
          switch (pop_backpack(z)) {
          | None => go(d, z)
          | Some(_) => [z, ...go(d, z)]
          };
        }
      };
    };
    let curr =
      switch (pop_backpack(z)) {
      | None => []
      | Some(_) => [z]
      };
    List.rev(go(Left, z)) @ curr @ go(Right, z);
  };

  // TODO(d): unify this logic with rest of movement logic
  let rec to_backpack_target = (d: planar, z: t): option(t) => {
    let done_or_try_again = (d, z) =>
      switch (pop_backpack(z)) {
      | None => to_backpack_target(d, z)
      | Some(_) => Some(z)
      };
    switch (d) {
    | Left(chunk) =>
      let* z = primary(chunk, Left, z);
      done_or_try_again(d, z);
    | Right(chunk) =>
      let* z = primary(chunk, Right, z);
      done_or_try_again(d, z);
    | Up =>
      let* z = vertical(Left, z);
      let zs =
        targets_within_row(z)
        |> List.sort((z1, z2) => {
             let dist1 = caret_point(z1).col - M.col_target;
             let dist2 = caret_point(z2).col - M.col_target;
             let c = Int.compare(abs(dist1), abs(dist2));
             // favor left
             c != 0 ? c : Int.compare(dist1, dist2);
           });
      switch (zs) {
      | [] => to_backpack_target(d, z)
      | [z, ..._] => Some(z)
      };
    | Down =>
      let* z = vertical(Right, z);
      let zs =
        targets_within_row(z)
        |> List.sort((z1, z2) => {
             let dist1 = caret_point(z1).col - M.col_target;
             let dist2 = caret_point(z2).col - M.col_target;
             let c = Int.compare(abs(dist1), abs(dist2));
             // favor right
             c != 0 ? c : - Int.compare(dist1, dist2);
           });
      switch (zs) {
      | [] => to_backpack_target(d, z)
      | [z, ..._] => Some(z)
      };
    };
  };

  let go = (d: Action.move, z: Zipper.t): option(Zipper.t) =>
    switch (d) {
    | Goal(goal) =>
      let z = Zipper.unselect(z);
      do_towards(primary(ByChar), goal, z);
    | Extreme(d) => do_extreme(primary(ByToken), d, z)
    | Local(d) =>
      z
      |> (
        switch (d) {
        | Left(chunk) => primary(chunk, Left)
        | Right(chunk) => primary(chunk, Right)
        | Up => vertical(Left)
        | Down => vertical(Right)
        }
      )
    };
};

let is_linebreak_to_right_of_caret =
    ({relatives: {siblings: (_, r), _}, _}: Zipper.t): bool => {
  switch (r) {
  | [Secondary(s), ..._] when Secondary.is_linebreak(s) => true
  | _ => false
  };
};

//TODO(andrew): document
//TODO(andrew): generalize to actually get everything down
//TODO(andrew): for going back and adding lets... maybe incorporate newlines
let semantics_push = (zipper: Zipper.t) => {
  let can_put_down = z =>
    switch (Zipper.pop_backpack(z)) {
    | Some(_) => z.caret == Outer
    | None => false
    };
  let rec move_until_cant_put_down = (z_last, z: Zipper.t) =>
    if (can_put_down(z /*&& !is_linebreak_to_right_of_caret(z)*/)) {
      switch (Zipper.move(Right, z)) {
      | None => z
      | Some(z_new) => move_until_cant_put_down(z, z_new)
      };
    } else {
      z_last;
    };
  let rec move_until_can_put_down = (z: Zipper.t) =>
    if (!can_put_down(z)) {
      switch (Zipper.move(Right, z)) {
      | None => z
      | Some(z_new) => move_until_can_put_down(z_new)
      };
    } else {
      z;
    };
  let rec go: Zipper.t => Zipper.t =
    z =>
      if (can_put_down(z)) {
        let z_can = move_until_cant_put_down(z, z);
        switch (Zipper.put_down(Right, z_can)) {
        | None =>
          //print_endline("cse shouldldny????");
          z_can
        | Some(z) =>
          let (z, _id) = Zipper.regrout(Right, z, 1000000);
          go(z);
        };
      } else {
        let z_can = move_until_can_put_down(z);
        let z_can = move_until_cant_put_down(z_can, z_can);
        switch (Zipper.put_down(Right, z_can)) {
        | None =>
          // print_endline("cse shouldldny????2");
          z_can
        | Some(z) =>
          let (z, _id) = Zipper.regrout(Right, z, 1000000);
          go(z);
        };
      };
  go(zipper);
};
