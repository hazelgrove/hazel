open Zipper;
open Util;
//open OptUtil.Syntax;

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

let is_right_of = (id: Id.t, z) =>
  switch (z.relatives.siblings, z.relatives.ancestors) {
  | ((_, [r, ..._]), _) => Piece.id(r) == id
  | ((_, []), []) => true // end of program
  | ((_, []), _) => false
  };

let is_left_of = (id: Id.t, z) =>
  switch (z.relatives.siblings, z.relatives.ancestors) {
  | (([_, ..._] as ls, _), _) => Piece.id(ListUtil.last(ls)) == id
  | (([], _), []) => true // beginning of program
  | (([], _), _) => false
  };

let is_on = (d: Direction.t, id: Id.t) =>
  switch (d) {
  | Left => is_left_of(id)
  | Right => is_right_of(id)
  };

let skip_to = (d: Direction.t, id, z) => {
  // print_endline("ProjectorAction.skip_to");
  Zipper.do_until(
    Zipper.move(d),
    is_on(d, id),
    z,
  );
};

let skip_select_to = (d: Direction.t, id, z) => {
  // print_endline("ProjectorAction.skip_select_to");
  Zipper.do_until(
    Zipper.select_caret(d),
    is_on(d, id),
    z,
  );
};
