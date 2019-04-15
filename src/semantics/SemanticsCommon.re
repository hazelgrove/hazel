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

[@deriving show({with_path: false})]
type side =
  | Before
  | After;

[@deriving show({with_path: false})]
type delimiter_side = side;

[@deriving show({with_path: false})]
type inner_cursor =
  | BeforeChild(int, delimiter_side)
  | ClosingDelimiter(delimiter_side);

[@deriving show({with_path: false})]
type outer_cursor =
  | Char(int);

[@deriving show({with_path: false})]
type cursor_pos =
  | O(outer_cursor)
  | I(inner_cursor);

let default_nih = (e: option(err_status)): err_status =>
  switch (e) {
  | None => NotInHole
  | Some(e) => e
  };
