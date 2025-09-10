let v: ProofCtx.t =
  []
  |> ProofCtx.add_exp(
       "Iden(+)L",
       // Fun is being used as a stand-in for Forall
       Forall(
         Asc(Var("x") |> Pat.fresh, Atom(Int) |> Typ.fresh) |> Pat.fresh,
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
         Asc(Var("x") |> Pat.fresh, Atom(Int) |> Typ.fresh) |> Pat.fresh,
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
         Asc(Var("x") |> Pat.fresh, Atom(Int) |> Typ.fresh) |> Pat.fresh,
         Forall(
           Asc(Var("x") |> Pat.fresh, Atom(Int) |> Typ.fresh) |> Pat.fresh,
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
         Asc(Var("x") |> Pat.fresh, Atom(Int) |> Typ.fresh) |> Pat.fresh,
         Forall(
           Asc(Var("x") |> Pat.fresh, Atom(Int) |> Typ.fresh) |> Pat.fresh,
           Forall(
             Asc(Var("x") |> Pat.fresh, Atom(Int) |> Typ.fresh) |> Pat.fresh,
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
         Asc(Var("x") |> Pat.fresh, Atom(Int) |> Typ.fresh) |> Pat.fresh,
         Forall(
           Asc(Var("x") |> Pat.fresh, Atom(Int) |> Typ.fresh) |> Pat.fresh,
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
         Asc(Var("x") |> Pat.fresh, Atom(Int) |> Typ.fresh) |> Pat.fresh,
         Forall(
           Asc(Var("x") |> Pat.fresh, Atom(Int) |> Typ.fresh) |> Pat.fresh,
           Forall(
             Asc(Var("x") |> Pat.fresh, Atom(Int) |> Typ.fresh) |> Pat.fresh,
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
     )
  |> ProofCtx.add_exp(
       "Identity(@)R",
       Forall(
         Asc(
           Var("xs") |> Pat.fresh,
           List(Unknown(Internal) |> Typ.temp) |> Typ.fresh,
         )
         |> Pat.fresh,
         BinOp(
           Poly(Equals),
           ListConcat(Var("xs") |> Exp.fresh, ListLit([]) |> Exp.fresh)
           |> Exp.fresh,
           Var("xs") |> Exp.fresh,
         )
         |> Exp.fresh,
       )
       |> Exp.fresh,
     )
  |> ProofCtx.add_exp(
       "Identity(@)L",
       Forall(
         Asc(
           Var("xs") |> Pat.fresh,
           List(Unknown(Internal) |> Typ.temp) |> Typ.fresh,
         )
         |> Pat.fresh,
         BinOp(
           Poly(Equals),
           ListConcat(ListLit([]) |> Exp.fresh, Var("xs") |> Exp.fresh)
           |> Exp.fresh,
           Var("xs") |> Exp.fresh,
         )
         |> Exp.fresh,
       )
       |> Exp.fresh,
     )
  |> ProofCtx.add_exp(
       "Assoc(::, @)",
       Forall(
         Asc(Var("x") |> Pat.fresh, Unknown(Internal) |> Typ.fresh)
         |> Pat.fresh,
         Forall(
           Asc(
             Var("xs") |> Pat.fresh,
             List(Unknown(Internal) |> Typ.temp) |> Typ.fresh,
           )
           |> Pat.fresh,
           Forall(
             Asc(
               Var("ys") |> Pat.fresh,
               List(Unknown(Internal) |> Typ.temp) |> Typ.fresh,
             )
             |> Pat.fresh,
             BinOp(
               Poly(Equals),
               ListConcat(
                 Cons(Var("x") |> Exp.fresh, Var("xs") |> Exp.fresh)
                 |> Exp.fresh,
                 Var("ys") |> Exp.fresh,
               )
               |> Exp.fresh,
               Cons(
                 Var("x") |> Exp.fresh,
                 ListConcat(Var("xs") |> Exp.fresh, Var("ys") |> Exp.fresh)
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
       |> Exp.fresh,
     )
  |> ProofCtx.add_exp(
       "Assoc(@)",
       Forall(
         Asc(
           Var("xs") |> Pat.fresh,
           List(Unknown(Internal) |> Typ.temp) |> Typ.fresh,
         )
         |> Pat.fresh,
         Forall(
           Asc(
             Var("ys") |> Pat.fresh,
             List(Unknown(Internal) |> Typ.temp) |> Typ.fresh,
           )
           |> Pat.fresh,
           Forall(
             Asc(
               Var("zs") |> Pat.fresh,
               List(Unknown(Internal) |> Typ.temp) |> Typ.fresh,
             )
             |> Pat.fresh,
             BinOp(
               Poly(Equals),
               ListConcat(
                 Var("xs") |> Exp.fresh,
                 ListConcat(Var("ys") |> Exp.fresh, Var("zs") |> Exp.fresh)
                 |> Exp.fresh,
               )
               |> Exp.fresh,
               ListConcat(
                 ListConcat(Var("xs") |> Exp.fresh, Var("ys") |> Exp.fresh)
                 |> Exp.fresh,
                 Var("zs") |> Exp.fresh,
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
     );
