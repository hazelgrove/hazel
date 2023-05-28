let const =
  Molds.bindings(Molds.v)
  |> List.map(fst)
  |> List.filter_map(
       fun
       | Label.Const(t) => Some(t)
       | _ => None,
     );

let get = (t: Token.t): option(Label.t) =>
  switch (t) {
  // check const first for reserved keywords
  | _ when List.mem(t, const) => Some(Const(t))
  | _ when Label.is_int_lit(t) => Some(Int_lit)
  | _ when Label.is_float_lit(t) => Some(Float_lit)
  | _ when Label.is_id_lower(t) => Some(Id_lower)
  | _ when Label.is_id_upper(t) => Some(Id_upper)
  | _ => None
  };