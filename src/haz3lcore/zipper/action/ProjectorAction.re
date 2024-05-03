open Zipper;
open Util;

let piece_is = (projectors, nhbr) =>
  switch (nhbr) {
  | Some(p) when Projector.Map.mem(Piece.id(p), projectors) =>
    Projector.Map.mem(Piece.id(p), projectors) ? Some(Piece.id(p)) : None
  | _ => None
  };

let neighbor_is =
    ({relatives: {siblings, _}, projectors, _}: t)
    : (option(Id.t), option(Id.t)) => (
  piece_is(projectors, Siblings.left_neighbor(siblings)),
  piece_is(projectors, Siblings.right_neighbor(siblings)),
);

let id_on = (d: Direction.t, id: Id.t, z: Zipper.t): bool =>
  switch (d) {
  | Left =>
    switch (z.relatives.siblings, z.relatives.ancestors) {
    | (([_, ..._] as ls, _), _) => Piece.id(ListUtil.last(ls)) == id
    | _ => false
    }
  | Right =>
    switch (z.relatives.siblings, z.relatives.ancestors) {
    | ((_, [r, ..._]), _) => Piece.id(r) == id
    | _ => false
    }
  };

let skip_to = (d: Direction.t, id: Id.t, z: Zipper.t): option(Zipper.t) => {
  // print_endline("ProjectorAction.skip_to");
  Zipper.do_until(
    Zipper.move(d),
    id_on(Direction.toggle(d), id),
    z,
  );
};

let skip_select_to =
    (d: Direction.t, id: Id.t, z: Zipper.t): option(Zipper.t) => {
  // print_endline("ProjectorAction.skip_select_to");
  Zipper.do_until(
    Zipper.select_caret(d),
    id_on(Direction.toggle(d), id),
    z,
  );
};
