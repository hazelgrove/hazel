let v: ProofCtx.t =
  []
  |> ProofCtx.add_exp(
       "Reflexive(==)",
       Forall(
         Var("x") |> Pat.fresh,
         BinOp(
           Poly(Equals),
           BinOp(
             Poly(Equals),
             Var("x") |> Exp.fresh,
             Var("x") |> Exp.fresh,
           )
           |> Exp.fresh,
           Atom(Bool(true)) |> Exp.fresh,
         )
         |> Exp.fresh,
       )
       |> Exp.fresh,
     );
