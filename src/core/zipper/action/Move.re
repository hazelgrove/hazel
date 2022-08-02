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
    | Some(_) => CanPass
    | _ => supernhbr_l
    };
  let r =
    switch (r_nhbr) {
    | Some(Tile({label, _})) => movability(label, 0)
    | Some(_) => CanPass
    | _ => supernhbr_r
    };
  (l, r);
};

let pop_out = z => Some(z |> Caret.set(Outer));
let pop_move = (d, z) => z |> Caret.set(Outer) |> Outer.move(d);
let inner_incr = (delim, c, z) => Some(Caret.set(Inner(delim, c + 1), z));
let inner_decr = z => Some(Caret.update(Caret.decrement, z));
let inner_start = (d_init, z) => Some(Caret.set(Inner(d_init, 0), z));
let inner_end = (d, d_init, c_max, z) =>
  z |> Caret.set(Inner(d_init, c_max)) |> Outer.move(d);

let primary = (chunkiness: chunkiness, d: Direction.t, z: t): option(t) => {
  switch (d, z.caret, neighbor_movability(chunkiness, z)) {
  /* this case maybe shouldn't be necessary but currently covers an edge
     (select an open parens to left of a multichar token and press left) */
  | _ when z.selection.content != [] => pop_move(d, z)
  | (Left, Outer, (CanEnter(dlm, c_max), _)) => inner_end(d, dlm, c_max, z)
  | (Left, Outer, _) => Outer.move(d, z)
  | (Left, Inner(_), _) when chunkiness == ByToken => pop_out(z)
  | (Left, Inner(_), _) => Some(Caret.update(Caret.decrement, z))
  | (Right, Outer, (_, CanEnter(d_init, _))) => inner_start(d_init, z)
  | (Right, Outer, _) => Outer.move(d, z)
  | (Right, Inner(_, c), (_, CanEnter(_, c_max))) when c == c_max =>
    pop_move(d, z)
  | (Right, Inner(_), _) when chunkiness == ByToken => pop_move(d, z)
  | (Right, Inner(delim, c), _) => inner_incr(delim, c, z)
  };
};

let vertical = (d: Direction.t, z: t): option(t) =>
  z.selection.content == []
    ? Caret.do_vertical(primary(ByChar, d), d, z)
    : Some(Outer.directional_unselect(d, z));

let targets_within_row = (map: Measured.t, z: t): list(t) => {
  let caret = Caret.point(map);
  let init = caret(z);
  let rec go = (d: Direction.t, z: t) => {
    switch (primary(ByChar, d, z)) {
    | None => []
    | Some(z) =>
      if (caret(z).row != init.row) {
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
let rec to_backpack_target = (d: planar, map, z: t): option(t) => {
  let caret_point = Caret.point(map);
  let done_or_try_again = (d, z) =>
    switch (pop_backpack(z)) {
    | None => to_backpack_target(d, map, z)
    | Some(_) => Some(z)
    };
  switch (d) {
  | Left(chunk) =>
    let* z = Option.map(Caret.update_target, primary(chunk, Left, z));
    done_or_try_again(d, z);
  | Right(chunk) =>
    let* z = Option.map(Caret.update_target, primary(chunk, Right, z));
    done_or_try_again(d, z);
  | Up =>
    let* z = vertical(Left, z);
    let zs =
      targets_within_row(map, z)
      |> List.sort((z1, z2) => {
           let dist1 = caret_point(z1).col - z.caret_col_target;
           let dist2 = caret_point(z2).col - z.caret_col_target;
           let c = Int.compare(abs(dist1), abs(dist2));
           // favor left
           c != 0 ? c : Int.compare(dist1, dist2);
         });
    switch (zs) {
    | [] => to_backpack_target(d, map, z)
    | [z, ..._] => Some(z)
    };
  | Down =>
    let* z = vertical(Right, z);
    let zs =
      targets_within_row(map, z)
      |> List.sort((z1, z2) => {
           let dist1 = caret_point(z1).col - z.caret_col_target;
           let dist2 = caret_point(z2).col - z.caret_col_target;
           let c = Int.compare(abs(dist1), abs(dist2));
           // favor right
           c != 0 ? c : - Int.compare(dist1, dist2);
         });
    switch (zs) {
    | [] => to_backpack_target(d, map, z)
    | [z, ..._] => Some(z)
    };
  };
};

let to_start = z =>
  switch (Caret.do_extreme(primary(ByToken, Zipper.from_plane(Up)), Up, z)) {
  | Some(z) => Caret.update_target(z)
  | None => z
  };
