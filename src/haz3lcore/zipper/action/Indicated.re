open Util;
open Zipper;

type relation =
  | Parent
  | Sibling;

let piece' =
    (~no_ws: bool, ~ign: Piece.t => bool, z: Zipper.t)
    : option((Piece.t, Direction.t, relation)) => {
  /* Returns the piece currently indicated (if any) and which side of
     that piece the caret is on. We favor indicating the piece to the
     (R)ight, but may end up indicating the (P)arent or the (L)eft.
     We don't indicate secondary tiles. This function ignores whether
     or not there is a selection so this can be used to get the caret
     direction, but the caller shouldn't indicate if there's a selection */
  switch (Siblings.neighbors(sibs_with_sel(z)), parent(z)) {
  /* Non-empty selection => no indication */
  //| _ when z.selection.content != [] => None
  /* Empty syntax => no indication */
  | ((None, None), None) => None
  /* L not secondary, R is secondary => indicate L */
  | ((Some(l), Some(r)), _) when !ign(l) && ign(r) =>
    Some((l, Left, Sibling))
  /* L and R are secondarys => no indication */
  | ((Some(l), Some(r)), _) when ign(l) && ign(r) =>
    no_ws ? None : Some((l, Left, Sibling))
  /* At right end of syntax and L is secondary => no indication */
  | ((Some(l), None), None) when ign(l) =>
    no_ws ? None : Some((l, Left, Sibling))
  /* At left end of syntax and R is secondary => no indication */
  | ((None, Some(r)), None) when ign(r) =>
    no_ws ? None : Some((r, Right, Sibling))
  /* No L and R is a secondary and there is a P => indicate P */
  | ((None, Some(r)), Some(parent)) when ign(r) =>
    Some((parent, Left, Parent))
  /* L is not secondary and caret is outer => indicate L */
  | ((Some(l), _), _) when !ign(l) && z.caret == Outer =>
    Some((l, Left, Sibling))
  /* No L, some P, and caret is outer => indicate R */
  | ((None, _), Some(parent)) when z.caret == Outer =>
    Some((parent, Left, Parent))
  /* R is not secondary, either no L or L is secondary or caret is inner => indicate R */
  | ((_, Some(r)), _) => Some((r, Right, Sibling))
  /* No R and there is a P => indicate P */
  | ((_, None), Some(parent)) => Some((parent, Right, Parent))
  /* There is an L but no R and no P => indicate L */
  //TODO(andrew): Right below seems wrong but it gets fucky otherwise
  | ((Some(l), None), None) => Some((l, Right, Sibling))
  };
};

let piece =
  piece'(~no_ws=true, ~ign=p => Piece.(is_secondary(p) || is_grout(p)));

let shard_index = (z: Zipper.t): option(int) =>
  switch (piece(z)) {
  | None => None
  | Some((p, side, relation)) =>
    switch (relation) {
    | Parent =>
      switch (Ancestors.parent(z.relatives.ancestors)) {
      | None => failwith("indicated_shard_index impossible")
      | Some({children: (before, _), _}) =>
        let before = List.length(before);
        switch (Siblings.neighbors(z.relatives.siblings)) {
        | (_, None) => Some(before + 1)
        | _ => Some(before)
        };
      }
    | Sibling =>
      switch (p) {
      | Secondary(_)
      | Grout(_) => Some(0)
      | Tile(t) =>
        switch (side) {
        | Left => Some(List.length(t.children))
        | Right => Some(0)
        }
      }
    }
  };

let index = (z: Zipper.t): option(int) =>
  switch (piece'(~no_ws=false, ~ign=Piece.is_secondary, z)) {
  | None => None
  | Some((p, _, _)) =>
    switch (p) {
    | Secondary({id, _}) => Some(id)
    | Grout({id, _}) => Some(id)
    | Tile({id, _}) => Some(id)
    }
  };
