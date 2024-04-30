open Zipper;
open Util;
open OptUtil.Syntax;

let get_id_before = (seg, ancestors, projectors) =>
  switch (Projector.split_seg(seg, projectors)) {
  | Some(([_, ..._] as xs, _, _, _)) => Piece.id(ListUtil.last(xs))
  | Some(([], _, _, _)) =>
    switch (Ancestors.parent(ancestors)) {
    | Some(a) =>
      print_endline("prev_id: empty pre using parent");
      a.id;
    | None =>
      print_endline("prev_id: empty pre no ancestor");
      Id.invalid; //TODO(andrew)
    }
  | None =>
    print_endline("prev_id: None");
    Id.invalid; //TODO(andrew)
  };

let get_id_after = (seg, projectors) =>
  switch (Projector.split_seg(seg, projectors)) {
  | Some((_, _, [hd, ..._], _)) => Piece.id(hd)
  | _ =>
    print_endline("next_id: empty post");
    Id.invalid; //TODO(andrew)
  };

let neighbor_is =
    (
      start_map: Projector.start_map,
      last_map: Projector.start_map,
      {relatives: {siblings, ancestors}, projectors, _}: t,
    )
    : (option(Id.t), option(Id.t)) => {
  let (l_nhbr, r_nhbr) = Siblings.neighbors(siblings);
  let seg = (siblings |> fst) @ (siblings |> snd);
  let l =
    switch (l_nhbr) {
    | Some(p) when Projector.Map.mem(Piece.id(p), last_map) =>
      Some(get_id_before(seg, ancestors, projectors))
    | _ => None
    };
  let r =
    switch (r_nhbr) {
    | Some(p) when Projector.Map.mem(Piece.id(p), start_map) =>
      Some(get_id_after(seg, projectors))
    | _ => None
    };
  (l, r);
};

/* Do move_action until the indicated piece is such that piece_p is true.
   If no such piece is found, don't move. */
let rec do_until_sib =
        (move: t => option(t), z_pred: Zipper.t => bool, z: t): option(t) =>
  z_pred(z)
    ? Some(z)
    : {
      let* z = move(z);
      do_until_sib(move, z_pred, z);
    };

let is_right_of = (pid: Id.t, z) =>
  switch (z.relatives.siblings, z.relatives.ancestors) {
  | ((_, [r, ..._]), _) => Piece.id(r) == pid
  | ((_, []), []) => true // end of program
  | ((_, []), _) => false
  };

let is_left_of = (pid: Id.t, z) =>
  switch (z.relatives.siblings, z.relatives.ancestors) {
  | (([_, ..._] as ls, _), _) => Piece.id(ListUtil.last(ls)) == pid
  | (([], _), []) => true // beginning of program
  | (([], _), _) => false
  };

let is_on = (d: Direction.t, pid: Id.t) =>
  switch (d) {
  | Left => is_left_of(pid)
  | Right => is_right_of(pid)
  };

let skip_to = (d: Direction.t, proj_id, z) => {
  // print_endline("ProjectorAction.skip_to");
  do_until_sib(
    Zipper.move(d),
    is_on(d, proj_id),
    z,
  );
};

//TODO(andrew): relocalize
let primary' = (d: Direction.t, z: Zipper.t): option(Zipper.t) =>
  if (z.caret == Outer) {
    Zipper.select(d, z);
  } else if (d == Left) {
    z
    |> Zipper.set_caret(Outer)
    |> Zipper.move(Right)
    |> OptUtil.and_then(Zipper.select(d));
  } else {
    z |> Zipper.set_caret(Outer) |> Zipper.select(d);
  };

let skip_select_to = (d: Direction.t, proj_id, z) => {
  // print_endline("ProjectorAction.skip_to");
  do_until_sib(
    z => z |> primary'(d),
    is_on(d, proj_id),
    z,
  );
};
