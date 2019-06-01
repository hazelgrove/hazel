open Sexplib.Std;

[@deriving (sexp, show)]
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
  | Forall;

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
type cursor_side =
  | Before
  | After
  | In(int);

let default_nih = (e: option(err_status)): err_status =>
  switch (e) {
  | None => NotInHole
  | Some(e) => e
  };
