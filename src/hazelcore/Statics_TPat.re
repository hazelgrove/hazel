let rec syn = (ctx: Contexts.t, tp: TPat.t): option((Kind.t, Contexts.t)) =>
  syn_operand(ctx, tp)
and syn_operand =
    (ctx: Contexts.t, operand: TPat.operand): option((Kind.t, Contexts.t)) =>
  switch (operand) {
  | EmptyHole(_)
  | TyVar(InVarHole(_), _) => Some((KHole, ctx))
  | TyVar(NotInVarHole, id) =>
    TyId.check_valid(
      id,
      Some((Kind.KHole, Contexts.extend_tyvarctx(ctx, (id, KHole)))),
    )
  }
and ana = (ctx: Contexts.t, p: TPat.t, hty: HTyp.t): option(Contexts.t) =>
  ana_operand(ctx, p, hty)
and ana_operand =
    (ctx: Contexts.t, operand: TPat.operand, hty: HTyp.t): option(Contexts.t) =>
  switch (operand) {
  | EmptyHole(_)
  | TyVar(InVarHole(_), _) => Some(ctx)
  | TyVar(NotInVarHole, id) =>
    TyId.check_valid(
      id,
      Some(Contexts.extend_tyvarctx(ctx, (id, Singleton(hty)))),
    )
  };
