let meet_of = (j: Mark.meet_type, ty: Typ.t): Typ.t =>
  switch (j) {
  | Id => ty
  | PolyEq => ty
  | List => List(ty) |> Typ.fresh
  };

let unknown_internal = () => Unknown(Internal) |> Typ.temp;
