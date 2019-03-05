[@deriving show({with_path: false})]
type nat = int;

type in_hole_reason =
  | TypeInconsistent
  | WrongLength;

type err_status =
  | NotInHole
  | InHole(in_hole_reason, MetaVar.t);

let err_status_to_string =
  fun
  | NotInHole => "NotInHole"
  | InHole(reason, u) => "InHole";

type var_err_status =
  | NotInVHole
  | InVHole(MetaVar.t);

[@deriving show({with_path: false})]
type inj_side =
  | L
  | R;

let pick_side = (side, l, r) =>
  switch (side) {
  | L => l
  | R => r
  };

[@deriving show({with_path: false})]
type cursor_side =
  | Before
  | After
  | In(nat);

exception SkelInconsistentWithOpSeq;
