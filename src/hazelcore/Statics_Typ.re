let rec syn = (ctx: Contexts.t, ty: HTyp.t): option(Kind.t) =>
  switch (ty) {
  | TyVar(idx, _) =>
    let (_, k) = TyVarCtx.tyvar_with_idx(Contexts.tyvars(ctx), idx);
    Some(k);
  | TyVarHole(_, _)
  | Hole => Some(KHole)
  | Int
  | Float
  | Bool
  | Prod([]) => Some(Type)
  | Arrow(ty1, ty2)
  | Sum(ty1, ty2) =>
    ana(ctx, ty1, Kind.Type) && ana(ctx, ty2, Kind.Type) ? Some(Type) : None
  | Prod(lst) =>
    List.fold_left(
      (b1, b2) => b1 && b2,
      true,
      List.map(hty => ana(ctx, hty, Kind.Type), lst),
    )
      ? Some(Type) : None
  | List(t) => ana(ctx, t, Kind.Type) ? Some(Type) : None
  }
and ana = (ctx: Contexts.t, ty: HTyp.t, k: Kind.t): bool =>
  switch (syn(ctx, ty)) {
  | None => false
  | Some(syn_k) => KindUtil.consistent(ctx, k, syn_k)
  };

let rec fix_holes =
        (
          ctx: Contexts.t,
          u_gen: MetaVarGen.t,
          ~renumber_empty_holes=false,
          ty: UHTyp.t,
        )
        : (UHTyp.t, MetaVarGen.t) => {
  fix_holes_opseq(ctx, u_gen, ~renumber_empty_holes, ty);
}
and fix_holes_opseq =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      OpSeq(skel, seq): UHTyp.opseq,
    )
    : (UHTyp.opseq, MetaVarGen.t) => {
  let (skel, seq, u_gen) =
    fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel, seq);
  (OpSeq(skel, seq), u_gen);
}
and fix_holes_skel =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      skel: UHTyp.skel,
      seq: UHTyp.seq,
    )
    : (UHTyp.skel, UHTyp.seq, MetaVarGen.t) => {
  switch (skel) {
  | Placeholder(n) =>
    let en = seq |> Seq.nth_operand(n);
    let (en, u_gen) =
      fix_holes_operand(ctx, u_gen, ~renumber_empty_holes, en);
    let seq = seq |> Seq.update_nth_operand(n, en);
    (skel, seq, u_gen);
  | BinOp(_, op, skel1, skel2) =>
    let (skel1, seq, u_gen) =
      fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel1, seq);
    let (skel2, seq, u_gen) =
      fix_holes_skel(ctx, u_gen, ~renumber_empty_holes, skel2, seq);
    (BinOp(NotInHole, op, skel1, skel2), seq, u_gen);
  };
}
and fix_holes_operand =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      e: UHTyp.operand,
    )
    : (UHTyp.operand, MetaVarGen.t) => {
  switch (e) {
  | Hole
  | Unit
  | Int
  | Float
  | Bool => (e, u_gen)
  | TyVar(InVarHole(Keyword(_), _), _) => (e, u_gen)
  | TyVar(_, id) =>
    if (Contexts.tyvars_contains(ctx, id)) {
      (TyVar(NotInVarHole, id), u_gen);
    } else {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      (TyVar(InVarHole(Free, u), id), u_gen);
    }
  | Parenthesized(t) =>
    let (t, u_gen) = fix_holes(ctx, u_gen, ~renumber_empty_holes, t);
    (Parenthesized(t), u_gen);
  | List(t) =>
    let (t, u_gen) = fix_holes(ctx, u_gen, ~renumber_empty_holes, t);
    (List(t), u_gen);
  };
};
