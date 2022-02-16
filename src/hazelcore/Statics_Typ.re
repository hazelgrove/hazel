let rec syn = (tyvars: TyVarCtx.t, ty: HTyp.t): option(Kind.t) =>
  switch (ty) {
  | Hole => Some(KHole)
  | TyVar(i, _) => TyVarCtx.kind(tyvars, i)
  | TyVarHole(_) => Some(KHole)
  | Int
  | Float
  | Bool => Some(Singleton(ty))
  | Arrow(ty1, ty2)
  | Sum(ty1, ty2) =>
    open OptUtil.Syntax;
    let* () = ana(tyvars, ty1, Kind.Type);
    let+ () = ana(tyvars, ty2, Kind.Type);
    Kind.Singleton(ty);
  | Prod(tys) =>
    open OptUtil.Syntax;
    let+ () =
      List.fold_left(
        (opt, ty) => Option.bind(opt, _ => ana(tyvars, ty, Kind.Type)),
        Some(),
        tys,
      );
    Kind.Singleton(ty);
  | List(ty1) =>
    open OptUtil.Syntax;
    let+ _ = ana(tyvars, ty1, Kind.Type);
    Kind.Singleton(ty);
  }

and ana = (tyvars: TyVarCtx.t, ty: HTyp.t, k: Kind.t): option(unit) =>
  switch (ty) {
  | Hole
  | TyVarHole(_) => Some()
  // subsumption
  | Sum(_)
  | Prod(_)
  | TyVar(_)
  | Arrow(_)
  | Int
  | Float
  | Bool
  | List(_) =>
    open OptUtil.Syntax;
    let* k' = syn(tyvars, ty);
    Kind.consistent_subkind(tyvars, k', k) ? Some() : None;
  };

let rec syn_fix_holes:
  (TyVarCtx.t, MetaVarGen.t, UHTyp.t) => (UHTyp.t, Kind.t, MetaVarGen.t) =
  (tyvars, u_gen, ty) =>
    switch (ty) {
    | OpSeq(skel, seq) =>
      let (skel, seq, k, u_gen) =
        syn_fix_holes_skel(tyvars, u_gen, skel, seq);
      (OpSeq(skel, seq), k, u_gen);
    }
and syn_fix_holes_skel = (tyvars, u_gen, skel, seq) =>
  switch (skel) {
  | Placeholder(n) =>
    let ty_n = seq |> Seq.nth_operand(n);
    let (ty_n, k, u_gen) = syn_fix_holes_operand(tyvars, u_gen, ty_n);
    let seq = seq |> Seq.update_nth_operand(n, ty_n);
    (skel, seq, k, u_gen);
  | BinOp(_, op, skel1, skel2) =>
    let (skel1, seq, _, u_gen) =
      ana_fix_holes_skel(tyvars, u_gen, skel1, seq, Kind.Type);
    let (skel2, seq, _, u_gen) =
      ana_fix_holes_skel(tyvars, u_gen, skel2, seq, Kind.Type);
    let skel = Skel.BinOp(NotInHole, op, skel1, skel2);
    let ty' = UHTyp.expand(UHTyp.mk_OpSeq(seq));
    switch (syn(tyvars, ty')) {
    | Some(k) => (skel, seq, k, u_gen)
    | None =>
      failwith("TODO: Add inconsistent kind hole (this can't happen now) 1")
    };
  }
and syn_fix_holes_operand =
    (tyvars: TyVarCtx.t, u_gen: MetaVarGen.t, operand: UHTyp.operand)
    : (UHTyp.operand, Kind.t, MetaVarGen.t) => {
  switch (operand) {
  | Hole => (Hole, Kind.KHole, u_gen)
  | TyVar(NotInHole(_), name) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    let ty = UHTyp.TyVar(InHole(Unbound, u), name);
    (ty, Kind.Singleton(TyVarHole(Unbound, u, name)), u_gen);
  | TyVar(InHole(reason, u), name) => (
      UHTyp.TyVar(InHole(reason, u), name),
      Kind.Singleton(HTyp.TyVarHole(reason, u, name)),
      u_gen,
    )
  | Unit
  | Int
  | Float
  | Bool => (operand, Kind.Singleton(UHTyp.expand_operand(operand)), u_gen)
  | Parenthesized(body) =>
    let (block, k, u_gen) = syn_fix_holes(tyvars, u_gen, body);
    (Parenthesized(block), k, u_gen);
  | List(opseq) =>
    let (opseq, k, u_gen) = syn_fix_holes(tyvars, u_gen, opseq);
    (List(opseq), k, u_gen);
  };
}

and ana_fix_holes:
  (TyVarCtx.t, MetaVarGen.t, UHTyp.t, Kind.t) =>
  (UHTyp.t, Kind.t, MetaVarGen.t) =
  (tyvars, u_gen, opseq, k) =>
    switch (opseq) {
    | OpSeq(skel, seq) =>
      let (skel, seq, k', u_gen) =
        ana_fix_holes_skel(tyvars, u_gen, skel, seq, k);
      if (Kind.consistent_subkind(tyvars, k', k)) {
        (OpSeq(skel, seq), k', u_gen);
      } else {
        failwith(
          "TODO: Add inconsistent kind hole (this can't happen now) 2",
        );
      };
    }
and ana_fix_holes_skel = (tyvars, u_gen, skel, seq, k) =>
  switch (skel) {
  | Placeholder(n) =>
    let ty_n = seq |> Seq.nth_operand(n);
    let (ty_n, k', u_gen) = ana_fix_holes_operand(tyvars, u_gen, ty_n, k);
    let seq = seq |> Seq.update_nth_operand(n, ty_n);
    (skel, seq, k', u_gen);
  | BinOp(_, _, _, _) =>
    let (skel, seq, k', u_gen) =
      syn_fix_holes_skel(tyvars, u_gen, skel, seq);
    if (Kind.consistent_subkind(tyvars, k', k)) {
      (skel, seq, k', u_gen);
    } else {
      failwith("TODO: Add inconsistent kind hole (this can't happen now) 3");
    };
  }
and ana_fix_holes_operand = (tyvars, u_gen, operand, k) => {
  switch (operand) {
  | UHTyp.Hole => (Hole, Kind.KHole, u_gen)
  | TyVar(InHole(_), _) => (operand, Kind.KHole, u_gen)
  | Parenthesized(body) =>
    let (block, k', u_gen) = ana_fix_holes(tyvars, u_gen, body, k);
    if (Kind.consistent_subkind(tyvars, k', k)) {
      (Parenthesized(block), k', u_gen);
    } else {
      failwith("TODO: Add inconsistent kind hole (this can't happen now) 4");
    };
  // subsumption
  | TyVar(NotInHole(_), _)
  | Unit
  | Int
  | Float
  | Bool
  | List(_) =>
    let (ty, k', u_gen) = syn_fix_holes_operand(tyvars, u_gen, operand);
    if (Kind.consistent_subkind(tyvars, k', k)) {
      (ty, k', u_gen);
    } else {
      failwith("TODO: Add inconsistent kind hole (this can't happen now) 5");
    };
  };
};
