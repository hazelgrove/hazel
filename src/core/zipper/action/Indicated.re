open Util;
open Zipper;

type relation =
  | Parent
  | Sibling;

let piece = (z: Zipper.t): option((Piece.t, Direction.t, relation)) => {
  let ws = p => Piece.(is_whitespace(p) || is_grout(p));
  /* Returns the piece currently indicated (if any) and which side of
     that piece the caret is on. We favor indicating the piece to the
     (R)ight, but may end up indicating the (P)arent or the (L)eft.
     We don't indicate whitespace tiles. This function ignores whether
     or not there is a selection so this can be used to get the caret
     direction, but the caller shouldn't indicate if there's a selection */
  switch (Siblings.neighbors(sibs_with_sel(z)), parent(z)) {
  /* Non-empty selection => no indication */
  //| _ when z.selection.content != [] => None
  /* Empty syntax => no indication */
  | ((None, None), None) => None
  /* L not whitespace, R is whitespace => indicate L */
  | ((Some(l), Some(r)), _) when !ws(l) && ws(r) =>
    Some((l, Left, Sibling))
  /* L and R are whitespaces => no indication */
  | ((Some(l), Some(r)), _) when ws(l) && ws(r) => None
  /* At right end of syntax and L is whitespace => no indication */
  | ((Some(l), None), None) when ws(l) => None
  /* At left end of syntax and R is whitespace => no indication */
  | ((None, Some(r)), None) when ws(r) => None
  /* No L and R is a whitespace and there is a P => indicate P */
  | ((None, Some(r)), Some(parent)) when ws(r) =>
    Some((parent, Left, Parent))
  /* L is not whitespace and caret is outer => indicate L */
  | ((Some(l), _), _) when !ws(l) && z.caret == Outer =>
    Some((l, Left, Sibling))
  /* No L, some P, and caret is outer => indicate R */
  | ((None, _), Some(parent)) when z.caret == Outer =>
    Some((parent, Left, Parent))
  /* R is not whitespace, either no L or L is whitespace or caret is inner => indicate R */
  | ((_, Some(r)), _) => Some((r, Right, Sibling))
  /* No R and there is a P => indicate P */
  | ((_, None), Some(parent)) => Some((parent, Right, Parent))
  /* There is an L but no R and no P => indicate L */
  //TODO(andrew): Right below seems wrong but it gets fucky otherwise
  | ((Some(l), None), None) => Some((l, Right, Sibling))
  };
};

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
      | Whitespace(_)
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
  switch (piece(z)) {
  | None => None
  | Some((p, _, _)) =>
    switch (p) {
    | Whitespace(_) => None
    | Grout({id, _}) => Some(id)
    | Tile({id, _}) => Some(id)
    }
  };
