open Sexplib.Std;
open GeneralUtil;

[@deriving sexp]
type in_hole_reason =
  | TypeInconsistent
  | WrongLength;

[@deriving sexp]
type err_status =
  | NotInHole
  | InHole(in_hole_reason, MetaVar.t);

let err_status_to_string =
  fun
  | NotInHole => "NotInHole"
  | InHole(_, _) => "InHole";

[@deriving sexp]
type keyword =
  | Let
  | Case;

[@deriving sexp]
type in_vhole_reason =
  | Free
  | Keyword(keyword);

[@deriving sexp]
type var_err_status =
  | NotInVHole
  | InVHole(in_vhole_reason, MetaVar.t);

exception FreeVarInPat;

[@deriving (show({with_path: false}), sexp)]
type inj_side =
  | L
  | R;

let pick_side = (side, l, r) =>
  switch (side) {
  | L => l
  | R => r
  };

[@deriving (show({with_path: false}), sexp)]
type side =
  | Before
  | After;

[@deriving (show({with_path: false}), sexp)]
type cursor_pos = (int, side);

/**
 * Cursor on outer node.
 * j is fencepost index
 * e.g. a|bc -> n == 1
 * e.g. abc| -> n == 3
 */
let outer_cursor = (j: int): cursor_pos => (j, Before);
let outer_cursors = (len: int): list(cursor_pos) =>
  range(len + 1) |> List.map(j => outer_cursor(j));

/**
 * Cursor on inner node.
 * k is delimiter index, side is delimiter side
 * e.g. case| xs ... end -> (0, After)
 * e.g. let _ |= _       -> (1, Before)
 */
let inner_cursor = (k: int, side: side): cursor_pos => (k, side);
let inner_cursors_k = (k: int): list(cursor_pos) => [
  inner_cursor(k, Before),
  inner_cursor(k, After),
];
let inner_cursors = (num_delim: int): list(cursor_pos) =>
  range(num_delim) |> List.map(k => inner_cursors_k(k)) |> List.flatten;

let default_nih = (e: option(err_status)): err_status =>
  switch (e) {
  | None => NotInHole
  | Some(e) => e
  };
