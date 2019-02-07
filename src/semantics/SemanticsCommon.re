type nat = int;

type in_hole_reason =
  | TypeInconsistent
  | WrongLength;

type err_status =
  | NotInHole
  | InHole(in_hole_reason, MetaVar.t);

type var_err_status =
  | NotInVHole
  | InVHole(MetaVar.t);

type inj_side =
  | L
  | R;

let pick_side = (side, l, r) =>
  switch (side) {
  | L => l
  | R => r
  };

type cursor_side =
  | Before
  | After
  | In(nat);
