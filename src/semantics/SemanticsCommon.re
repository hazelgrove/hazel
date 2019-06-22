open Sexplib.Std;
open GeneralUtil;

[@deriving sexp]
type in_hole_reason =
  | TypeInconsistent
  | WrongLength;

[@deriving (sexp, show)]
type err_status =
  | NotInHole
  | InHole(in_hole_reason, MetaVar.t);

let err_status_to_string =
  fun
  | NotInHole => "NotInHole"
  | InHole(_, _) => "InHole";

[@deriving (sexp, show)]
type keyword =
  | Let
  | Case
  | Forall
  | Type
  | Num
  | Bool
  | List
  | Fn;

[@deriving (sexp, show)]
type in_vhole_reason =
  | Free
  | Keyword(keyword);

[@deriving (sexp, show)]
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
type delim_index = int;
[@deriving (show({with_path: false}), sexp)]
type op_index = int;
[@deriving (show({with_path: false}), sexp)]
type char_index = int;
[@deriving (show({with_path: false}), sexp)]
type child_index = int;

[@deriving (show({with_path: false}), sexp)]
type side =
  | Before
  | After;

let toggle_side =
  fun
  | Before => After
  | After => Before;

[@deriving (show({with_path: false}), sexp)]
type cursor_position =
  | OnText(char_index)
  | OnDelim(delim_index, side)
  | Staging(delim_index);

let text_cursors = (len: int): list(cursor_position) =>
  range(len + 1) |> List.map(j => OnText(j));

let delim_cursors_k = (k: int): list(cursor_position) => [
  OnDelim(k, Before),
  OnDelim(k, After),
  Staging(k),
];
let delim_cursors = (num_delim: int): list(cursor_position) =>
  range(num_delim) |> List.map(k => delim_cursors_k(k)) |> List.flatten;
