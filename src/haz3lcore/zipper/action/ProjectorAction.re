open Zipper;
open Util;
//open OptUtil.Syntax;

[@deriving (show({with_path: false}), sexp, yojson)]
type relation =
  | Parent
  | Sibling;

[@deriving (show({with_path: false}), sexp, yojson)]
type thing = {
  id: Id.t,
  relation,
};

let get_id_before = (seg, ancestors, projectors): option(thing) =>
  switch (Projector.split_seg(seg, projectors)) {
  | Some(([_, ..._] as xs, _, _, _)) =>
    Some({id: Piece.id(ListUtil.last(xs)), relation: Sibling})
  | Some(([], _, _, _)) =>
    switch (Ancestors.parent(ancestors)) {
    | Some(a) =>
      print_endline("prev_id: empty pre using parent");
      print_endline("parent id: " ++ Id.show(a.id));
      Some({id: a.id, relation: Parent});
    | None =>
      print_endline("prev_id: empty pre no ancestor");
      None; //TODO(andrew) //Id.invalid;
    }
  | None =>
    print_endline("prev_id: None");
    None; //TODO(andrew) //Id.invalid;
  };

let get_id_after = (seg, ancestors, projectors): option(thing) =>
  switch (Projector.split_seg(seg, projectors)) {
  | Some((_, _, [hd, ..._], _)) =>
    Some({id: Piece.id(hd), relation: Sibling})
  | Some((_, _, [], _)) =>
    switch (Ancestors.parent(ancestors)) {
    | Some(a) =>
      print_endline("next_id: empty pre using parent");
      print_endline("parent id: " ++ Id.show(a.id));
      Some({id: a.id, relation: Parent});
    | None =>
      print_endline("next_id: empty pre no ancestor");
      None; //TODO(andrew) //Id.invalid;
    }
  | None =>
    print_endline("next_id: empty post");
    None; //TODO(andrew) //Id.invalid;
  };

let neighbor_is =
    (
      start_map: Projector.start_map,
      last_map: Projector.start_map,
      {relatives: {siblings, ancestors}, projectors, _}: t,
    )
    : (option(thing), option(thing)) => {
  let (l_nhbr, r_nhbr) = Siblings.neighbors(siblings);
  let seg = (siblings |> fst) @ (siblings |> snd);
  let l =
    switch (l_nhbr) {
    | Some(p) when Projector.Map.mem(Piece.id(p), last_map) =>
      get_id_before(seg, ancestors, projectors)
    | _ => None
    };
  let r =
    switch (r_nhbr) {
    | Some(p) when Projector.Map.mem(Piece.id(p), start_map) =>
      get_id_after(seg, ancestors, projectors)
    | _ => None
    };
  (l, r);
};

let id_right_of_z = (id: Id.t, z) =>
  switch (z.relatives.siblings, z.relatives.ancestors) {
  | ((_, [r, ..._]), _) => Piece.id(r) == id
  | ((_, []), []) => true // end of program
  | ((_, []), _) => false
  };

let id_left_of_z = (id: Id.t, z) =>
  switch (z.relatives.siblings, z.relatives.ancestors) {
  | (([_, ..._] as ls, _), _) => Piece.id(ListUtil.last(ls)) == id
  | (([], _), []) => true // beginning of program
  | (([], _), _) => false
  };

let id_on = (d: Direction.t, id: Id.t) =>
  switch (d) {
  | Left => id_left_of_z(id)
  | Right => id_right_of_z(id)
  };

let d2 = (relation, d) =>
  switch (relation) {
  | Sibling => d
  | Parent => Direction.toggle(d)
  };

let skip_to = (d: Direction.t, {id, relation}, z) => {
  // print_endline("ProjectorAction.skip_to");
  Zipper.do_until(
    Zipper.move(d),
    id_on(d2(relation, d), id),
    z,
  );
};

let skip_select_to = (d: Direction.t, {id, relation}, z) => {
  // print_endline("ProjectorAction.skip_select_to");
  Zipper.do_until(
    Zipper.select_caret(d),
    id_on(d2(relation, d), id),
    z,
  );
};
