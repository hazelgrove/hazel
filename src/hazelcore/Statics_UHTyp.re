let rec syn_fix_holes:
  (Context.t, MetaVarGen.t, UHTyp.t) => (UHTyp.t, Kind.t, MetaVarGen.t) =
  (ctx, u_gen, ty) =>
    switch (ty) {
    | OpSeq(skel, seq) =>
      let (skel, seq, k, u_gen) = syn_fix_holes_skel(ctx, u_gen, skel, seq);
      (OpSeq(skel, seq), k, u_gen);
    }
and syn_fix_holes_skel = (ctx, u_gen, skel, seq) =>
  switch (skel) {
  | Placeholder(n) =>
    let ty_n = seq |> Seq.nth_operand(n);
    let (ty_n, k, u_gen) = syn_fix_holes_operand(ctx, u_gen, ty_n);
    let seq = seq |> Seq.update_nth_operand(n, ty_n);
    (skel, seq, k, u_gen);
  | BinOp(_, op, skel1, skel2) =>
    let (skel1, seq, _, u_gen) =
      ana_fix_holes_skel(ctx, u_gen, skel1, seq, Kind.Type);
    let (skel2, seq, _, u_gen) =
      ana_fix_holes_skel(ctx, u_gen, skel2, seq, Kind.Type);
    switch (Elaborator_Typ.syn_elab(ctx, Delta.empty, UHTyp.mk_OpSeq(seq))) {
    | Some((_, k, _)) =>
      let skel = Skel.BinOp(NotInHole, op, skel1, skel2);
      (skel, seq, k, u_gen);
    | None =>
      failwith("TODO: Add inconsistent kind hole (this can't happen now) 1")
    };
  }
and syn_fix_holes_operand =
    (ctx: Context.t, u_gen: MetaVarGen.t, operand: UHTyp.operand)
    : (UHTyp.operand, Kind.t, MetaVarGen.t) => {
  switch (operand) {
  | Hole => (Hole, Kind.Hole, u_gen)
  | TyVar(NotInTyVarHole(index, stamp), t) =>
    let cref = KindSystem.ContextRef.{index, stamp};
    let k' = Kind.singleton(HTyp.tyvar(ctx, index, t));
    switch (Context.tyvar_kind(ctx, {index, stamp})) {
    | Some(k)
        when
          Context.tyvar(ctx, cref) == Some(t)
          && Kind.consistent_subkind(ctx, k', k) => (
        operand,
        k',
        u_gen,
      )
    | Some(_)
    | None =>
      let reason: TyVarErrStatus.HoleReason.t =
        if (TyVar.reserved_word(t)) {
          Reserved;
        } else if (TyVar.valid_name(t)) {
          Unbound;
        } else {
          InvalidName;
        };
      let (u, u_gen) = MetaVarGen.next(u_gen);
      let ty = UHTyp.TyVar(InHole(reason, u), t);
      (ty, Kind.S(TyVarHole(reason, u, t)), u_gen);
    };
  | TyVar(InHole(_, u), t) =>
    if (TyVar.reserved_word(t)) {
      let ty = UHTyp.TyVar(InHole(Reserved, u), t);
      (ty, Kind.S(TyVarHole(Reserved, u, t)), u_gen);
    } else if (TyVar.valid_name(t)) {
      switch (Context.tyvar_ref(ctx, t)) {
      | None =>
        let ty = UHTyp.TyVar(InHole(Unbound, u), t);
        (ty, Kind.S(TyVarHole(Unbound, u, t)), u_gen);
      | Some(cref) =>
        let ty = UHTyp.TyVar(NotInTyVarHole(cref.index, cref.stamp), t);
        (ty, Kind.S(TyVar(cref, t)), u_gen);
      };
    } else {
      let ty = UHTyp.TyVar(InHole(InvalidName, u), t);
      (ty, Kind.S(TyVarHole(InvalidName, u, t)), u_gen);
    }
  | Unit
  | Int
  | Float
  | Bool => (operand, Kind.singleton(UHTyp.expand_operand(operand)), u_gen)
  | Parenthesized(body) =>
    let (block, k, u_gen) = syn_fix_holes(ctx, u_gen, body);
    (Parenthesized(block), k, u_gen);
  | List(opseq) =>
    let (opseq, k, u_gen) = syn_fix_holes(ctx, u_gen, opseq);
    (List(opseq), k, u_gen);
  | Forall(tpat, ty) =>
    let (ty, k, u_gen) = syn_fix_holes(ctx, u_gen, ty);
    (Forall(tpat, ty), k, u_gen);
  };
}

and ana_fix_holes:
  (Context.t, MetaVarGen.t, UHTyp.t, Kind.t) =>
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
  | UHTyp.Hole => (Hole, Kind.Hole, u_gen)
  | Parenthesized(body) =>
    let (block, k', u_gen) = ana_fix_holes(tyvars, u_gen, body, k);
    if (Kind.consistent_subkind(tyvars, k', k)) {
      (Parenthesized(block), k', u_gen);
    } else {
      failwith("TODO: Add inconsistent kind hole (this can't happen now) 4");
    };
  // subsumption
  | TyVar(_)
  | Unit
  | Int
  | Float
  | Bool
  | List(_)
  | Forall(_) =>
    let (ty, k', u_gen) = syn_fix_holes_operand(tyvars, u_gen, operand);
    if (Kind.consistent_subkind(tyvars, k', k)) {
      (ty, k', u_gen);
    } else {
      failwith("TODO: Add inconsistent kind hole (this can't happen now) 5");
    };
  };
};
