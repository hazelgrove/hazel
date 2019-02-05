module SemanticsCommon =
 struct
  type in_hole_reason =
  | TypeInconsistent
  | WrongLength

  type err_status =
  | NotInHole
  | InHole of in_hole_reason * MetaVar.t

  type var_err_status =
  | NotInVHole
  | InVHole of MetaVar.t

  type inj_side =
  | L
  | R

  let pick_side side l r =
    match side with
    | L -> l
    | R -> r
 end
