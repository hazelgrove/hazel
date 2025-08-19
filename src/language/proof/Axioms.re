let v: ProofCtx.t =
  []
  |> ProofCtx.add_exp(
       "Iden(+)L",
       // Fun is being used as a stand-in for Forall
       Forall(
         Var("x") |> Pat.fresh,
         BinOp(
           Poly(Equals),
           BinOp(
             Int(Plus),
             Var("x") |> Exp.fresh,
             Atom(Int(Bigint.zero)) |> Exp.fresh,
           )
           |> Exp.fresh,
           Var("x") |> Exp.fresh,
         )
         |> Exp.fresh,
       )
       |> Exp.fresh,
     )
  |> ProofCtx.add_exp(
       "Zero(*)",
       Forall(
         Var("x") |> Pat.fresh,
         BinOp(
           Poly(Equals),
           BinOp(
             Int(Times),
             Var("x") |> Exp.fresh,
             Atom(Int(Bigint.zero)) |> Exp.fresh,
           )
           |> Exp.fresh,
           Atom(Int(Bigint.zero)) |> Exp.fresh,
         )
         |> Exp.fresh,
       )
       |> Exp.fresh,
     )
  |> ProofCtx.add_exp(
       "Comm(+)",
       Forall(
         Var("x") |> Pat.fresh,
         Forall(
           Var("y") |> Pat.fresh,
           BinOp(
             Poly(Equals),
             BinOp(Int(Plus), Var("x") |> Exp.fresh, Var("y") |> Exp.fresh)
             |> Exp.fresh,
             BinOp(Int(Plus), Var("y") |> Exp.fresh, Var("x") |> Exp.fresh)
             |> Exp.fresh,
           )
           |> Exp.fresh,
         )
         |> Exp.fresh,
       )
       |> Exp.fresh,
     )
  |> ProofCtx.add_exp(
       "Assoc(+)",
       Forall(
         Var("x") |> Pat.fresh,
         Forall(
           Var("y") |> Pat.fresh,
           Forall(
             Var("z") |> Pat.fresh,
             BinOp(
               Poly(Equals),
               BinOp(
                 Int(Plus),
                 Var("x") |> Exp.fresh,
                 BinOp(
                   Int(Plus),
                   Var("y") |> Exp.fresh,
                   Var("z") |> Exp.fresh,
                 )
                 |> Exp.fresh,
               )
               |> Exp.fresh,
               BinOp(
                 Int(Plus),
                 BinOp(
                   Int(Plus),
                   Var("x") |> Exp.fresh,
                   Var("y") |> Exp.fresh,
                 )
                 |> Exp.fresh,
                 Var("z") |> Exp.fresh,
               )
               |> Exp.fresh,
             )
             |> Exp.fresh,
           )
           |> Exp.fresh,
         )
         |> Exp.fresh,
       )
       |> Exp.fresh,
     )
  |> ProofCtx.add_exp(
       "Comm(*)",
       Forall(
         Var("x") |> Pat.fresh,
         Forall(
           Var("y") |> Pat.fresh,
           BinOp(
             Poly(Equals),
             BinOp(
               Int(Times),
               Var("x") |> Exp.fresh,
               Var("y") |> Exp.fresh,
             )
             |> Exp.fresh,
             BinOp(
               Int(Times),
               Var("y") |> Exp.fresh,
               Var("x") |> Exp.fresh,
             )
             |> Exp.fresh,
           )
           |> Exp.fresh,
         )
         |> Exp.fresh,
       )
       |> Exp.fresh,
     )
  |> ProofCtx.add_exp(
       "Assoc(*)",
       Forall(
         Var("x") |> Pat.fresh,
         Forall(
           Var("y") |> Pat.fresh,
           Forall(
             Var("z") |> Pat.fresh,
             BinOp(
               Poly(Equals),
               BinOp(
                 Int(Times),
                 Var("x") |> Exp.fresh,
                 BinOp(
                   Int(Times),
                   Var("y") |> Exp.fresh,
                   Var("z") |> Exp.fresh,
                 )
                 |> Exp.fresh,
               )
               |> Exp.fresh,
               BinOp(
                 Int(Times),
                 BinOp(
                   Int(Times),
                   Var("x") |> Exp.fresh,
                   Var("y") |> Exp.fresh,
                 )
                 |> Exp.fresh,
                 Var("z") |> Exp.fresh,
               )
               |> Exp.fresh,
             )
             |> Exp.fresh,
           )
           |> Exp.fresh,
         )
         |> Exp.fresh,
       )
       |> Exp.fresh,
     )
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
