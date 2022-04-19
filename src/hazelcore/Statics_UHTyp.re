let rec syn_fix_holes:
  (TyCtx.t, MetaVarGen.t, UHTyp.t) => (UHTyp.t, Kind.t, MetaVarGen.t) =
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
      ana_fix_holes_skel(tyvars, u_gen, skel1, seq, KindCore.T);
    let (skel2, seq, _, u_gen) =
      ana_fix_holes_skel(tyvars, u_gen, skel2, seq, KindCore.T);
    switch (
      Elaborator_Typ.syn_elab(tyvars, Delta.empty, UHTyp.mk_OpSeq(seq))
    ) {
    | Some((_, k, _)) =>
      let skel = Skel.BinOp(NotInHole, op, skel1, skel2);
      (skel, seq, k, u_gen);
    | None =>
      failwith("TODO: Add inconsistent kind hole (this can't happen now) 1")
    };
  }
and syn_fix_holes_operand =
    (tyvars: TyCtx.t, u_gen: MetaVarGen.t, operand: UHTyp.operand)
    : (UHTyp.operand, Kind.t, MetaVarGen.t) => {
  switch (operand) {
  | Hole => (Hole, KindCore.KHole, u_gen)
  | TyVar(NotInTyVarHole(i), name) =>
    let k' = Kind.singleton(HTyp.tyvar(i, name));
    switch (TyCtx.tyvar_kind(tyvars, i)) {
    | Some(k)
        when
          TyCtx.tyvar_name(tyvars, i) == Some(name)
          && Kind.consistent_subkind(tyvars, k', k) => (
        operand,
        k',
        u_gen,
      )
    | Some(_)
    | None =>
      let reason: TyVarErrStatus.HoleReason.t =
        if (TyVar.reserved_word(name)) {
          Reserved;
        } else if (TyVar.valid_name(name)) {
          Unbound;
        } else {
          InvalidName;
        };
      let (u, u_gen) = MetaVarGen.next(u_gen);
      let ty = UHTyp.TyVar(InHole(reason, u), name);
      (ty, KindCore.Singleton(TyVarHole(reason, u, name)), u_gen);
    };
  | TyVar(InHole(_, u), name) =>
    if (TyVar.reserved_word(name)) {
      let ty = UHTyp.TyVar(InHole(Reserved, u), name);
      (ty, KindCore.Singleton(TyVarHole(Reserved, u, name)), u_gen);
    } else if (TyVar.valid_name(name)) {
      switch (TyCtx.tyvar_index(tyvars, name)) {
      | None =>
        let ty = UHTyp.TyVar(InHole(Unbound, u), name);
        (ty, KindCore.Singleton(TyVarHole(Unbound, u, name)), u_gen);
      | Some(i) =>
        let ty = UHTyp.TyVar(NotInTyVarHole(i), name);
        (ty, KindCore.Singleton(TyVar(i, name)), u_gen);
      };
    } else {
      let ty = UHTyp.TyVar(InHole(InvalidName, u), name);
      (ty, KindCore.Singleton(TyVarHole(InvalidName, u, name)), u_gen);
    }
  | Unit
  | Int
  | Float
  | Bool => (operand, Kind.singleton(UHTyp.expand_operand(operand)), u_gen)
  | Parenthesized(body) =>
    let (block, k, u_gen) = syn_fix_holes(tyvars, u_gen, body);
    (Parenthesized(block), k, u_gen);
  | List(opseq) =>
    let (opseq, k, u_gen) = syn_fix_holes(tyvars, u_gen, opseq);
    (List(opseq), k, u_gen);
  };
}

and ana_fix_holes:
  (TyCtx.t, MetaVarGen.t, UHTyp.t, Kind.t) => (UHTyp.t, Kind.t, MetaVarGen.t) =
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
  | UHTyp.Hole => (Hole, KindCore.KHole, u_gen)
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
  | List(_) =>
    let (ty, k', u_gen) = syn_fix_holes_operand(tyvars, u_gen, operand);
    if (Kind.consistent_subkind(tyvars, k', k)) {
      (ty, k', u_gen);
    } else {
      failwith("TODO: Add inconsistent kind hole (this can't happen now) 5");
    };
  };
};
