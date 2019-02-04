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
 end
