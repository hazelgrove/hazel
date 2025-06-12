let v: ProofCtx.t =
  []
  |> ProofCtx.add_entry(
       "Iden(+)L",
       // Fun is being used as a stand-in for Forall
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
       "Zero(*)",
       Fun(
         Var("x") |> Pat.fresh,
         BinOp(
           Int(Equals),
           BinOp(
             Int(Times),
             Var("x") |> Exp.fresh,
             Atom(Int(Bigint.zero)) |> Exp.fresh,
           )
           |> Exp.fresh,
           Atom(Int(Bigint.zero)) |> Exp.fresh,
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
     )
  |> ProofCtx.add_entry(
       "Assoc(+)",
       Fun(
         Var("x") |> Pat.fresh,
         Fun(
           Var("y") |> Pat.fresh,
           Fun(
             Var("z") |> Pat.fresh,
             BinOp(
               Int(Equals),
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
             Some(Atom(Int) |> Typ.fresh),
             None,
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
     )
  |> ProofCtx.add_entry(
       "Comm(*)",
       Fun(
         Var("x") |> Pat.fresh,
         Fun(
           Var("y") |> Pat.fresh,
           BinOp(
             Int(Equals),
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
           Some(Atom(Int) |> Typ.fresh),
           None,
         )
         |> Exp.fresh,
         Some(Atom(Int) |> Typ.fresh),
         None,
       )
       |> Exp.fresh,
     )
  |> ProofCtx.add_entry(
       "Assoc(*)",
       Fun(
         Var("x") |> Pat.fresh,
         Fun(
           Var("y") |> Pat.fresh,
           Fun(
             Var("z") |> Pat.fresh,
             BinOp(
               Int(Equals),
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
             Some(Atom(Int) |> Typ.fresh),
             None,
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
