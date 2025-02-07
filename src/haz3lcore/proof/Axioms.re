let v: ProofCtx.t =
  []
  |> ProofCtx.add_entry(
       "Iden(+)L",
       Fun(
         Var("x") |> Pat.fresh,
         BinOp(
           Int(Equals),
           BinOp(
             Int(Plus),
             Var("x") |> Exp.fresh,
             Atom(Int(Bigint.zero)) |> Exp.fresh,
           )
           |> Exp.fresh,
           Var("x") |> Exp.fresh,
         )
         |> Exp.fresh,
         Some(Atom(Int) |> Typ.fresh),
         None,
       )
       |> Exp.fresh,
     )
  |> ProofCtx.add_entry(
       "Comm(+)",
       Fun(
         Var("x") |> Pat.fresh,
         Fun(
           Var("y") |> Pat.fresh,
           BinOp(
             Int(Equals),
             BinOp(Int(Plus), Var("x") |> Exp.fresh, Var("y") |> Exp.fresh)
             |> Exp.fresh,
             BinOp(Int(Plus), Var("y") |> Exp.fresh, Var("x") |> Exp.fresh)
             |> Exp.fresh,
           )
           |> Exp.fresh,
           Some(Atom(Int) |> Typ.fresh),
           None,
         )
         |> Exp.fresh,
         Some(Atom(Int) |> Typ.fresh),
         None,
       )
       |> Exp.fresh,
     );
