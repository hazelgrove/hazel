module ElaborationResult = {
  type t = option((HTyp.t, Kind.t, Delta.t));
};

open OptUtil.Syntax;

let rec get_prod_elements: UHTyp.skel => list(UHTyp.skel) =
  fun
  | BinOp(_, Prod, skel1, skel2) =>
    get_prod_elements(skel1) @ get_prod_elements(skel2)
  | skel => [skel];

let rec syn_elab: (TyVarCtx.t, Delta.t, UHTyp.t) => ElaborationResult.t =
  (tyvars, delta) =>
    fun
    | OpSeq(skel, seq) => syn_elab_skel(tyvars, delta, skel, seq)
and syn_elab_skel = (tyvars, delta, skel, seq) =>
  switch (skel) {
  | Placeholder(n) =>
    seq |> Seq.nth_operand(n) |> syn_elab_operand(tyvars, delta)
  | BinOp(_, Prod, _, _) =>
    let skels = get_prod_elements(skel);
    let+ (tys, deltas) =
      List.fold_right(
        (skel, acc_opt) => {
          let* (tys, deltas) = acc_opt;
          let+ (ty, _, delta) =
            ana_elab_skel(tyvars, delta, skel, seq, Kind.Type);
          ([ty, ...tys], [delta, ...deltas]);
        },
        skels,
        Some(([], [])),
      );
    let delta =
      deltas |> List.fold_left((d1, d2) => Delta.union(d1, d2), Delta.empty);
    let ty = HTyp.product(tys);
    (ty, Kind.singleton(ty), delta);
  | BinOp(_, op, skel1, skel2) =>
    let* (ty1, _, delta1) =
      ana_elab_skel(tyvars, delta, skel1, seq, Kind.Type);
    let+ (ty2, _, delta2) =
      ana_elab_skel(tyvars, delta, skel2, seq, Kind.Type);
    let ty: HTyp.t =
      switch (op) {
      | Arrow => HTyp.arrow(ty1, ty2)
      | Sum => HTyp.sum(ty1, ty2)
      | Prod => failwith("Impossible, Prod is matched first")
      };
    (ty, Kind.singleton(ty), Delta.union(delta1, delta2));
  }
and syn_elab_operand =
    (tyvars: TyVarCtx.t, delta: Delta.t, operand: UHTyp.operand)
    : ElaborationResult.t => {
  let const = (ty: HTyp.t) => Some((ty, Kind.singleton(ty), delta));
  switch (operand) {
  | Hole => Some((HTyp.hole, Kind.KHole, delta))
  | TyVar(NotInHole(i), name) => const(HTyp.tyvar(i, name))
  | TyVar(InHole(reason, u), name) =>
    let ty = HTyp.tyvarhole(reason, u, name);
    Some((ty, Kind.KHole, Delta.add(u, Delta.Hole.Type, delta)));
  | Unit => const(HTyp.product([]))
  | Int => const(HTyp.int)
  | Float => const(HTyp.float)
  | Bool => const(HTyp.bool)
  | Parenthesized(ty) => syn_elab(tyvars, delta, ty)
  | List(ty) =>
    let+ (ty_elt, _, delta) = syn_elab(tyvars, delta, ty);
    let ty = HTyp.list(ty_elt);
    (ty, Kind.singleton(ty), delta);
  };
}

and ana_elab: (TyVarCtx.t, Delta.t, UHTyp.t, Kind.t) => ElaborationResult.t =
  (tyvars, delta, opseq, k) =>
    switch (opseq) {
    | OpSeq(skel, seq) => ana_elab_skel(tyvars, delta, skel, seq, k)
    }
and ana_elab_skel = (tyvars, delta, skel, seq, k): ElaborationResult.t =>
  switch (skel) {
  | Placeholder(n) =>
    let ty_n = seq |> Seq.nth_operand(n);
    ana_elab_operand(tyvars, delta, ty_n, k);
  | BinOp(_, _, _, _) =>
    let* (ty, k', delta) = syn_elab_skel(tyvars, delta, skel, seq);
    Kind.consistent_subkind(tyvars, k', k) ? Some((ty, k', delta)) : None;
  }
and ana_elab_operand =
    (tyvars: TyVarCtx.t, delta: Delta.t, operand: UHTyp.operand, k: Kind.t)
    : ElaborationResult.t => {
  switch (operand) {
  | Hole => Some((HTyp.hole, Kind.KHole, delta))
  | TyVar(InHole(reason, u), t) =>
    Some((HTyp.tyvarhole(reason, u, t), Kind.KHole, delta))
  | Parenthesized(opseq) => ana_elab(tyvars, delta, opseq, k)
  // subsumption
  | TyVar(NotInHole(_), _)
  | Unit
  | Int
  | Float
  | Bool
  | List(_) =>
    let* (ty, k', delta) = syn_elab_operand(tyvars, delta, operand);
    Kind.consistent_subkind(tyvars, k', k) ? Some((ty, k', delta)) : None;
  };
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
    switch (syn_elab(tyvars, Delta.empty, UHTyp.mk_OpSeq(seq))) {
    | Some((_, k, _)) =>
      let skel = Skel.BinOp(NotInHole, op, skel1, skel2);
      (skel, seq, k, u_gen);
    | None =>
      failwith("TODO: Add inconsistent kind hole (this can't happen now) 1")
    };
  }
and syn_fix_holes_operand =
    (tyvars: TyVarCtx.t, u_gen: MetaVarGen.t, operand: UHTyp.operand)
    : (UHTyp.operand, Kind.t, MetaVarGen.t) => {
  switch (operand) {
  | Hole => (Hole, Kind.KHole, u_gen)
  | TyVar(NotInHole(i), name) =>
    let k' = Kind.singleton(HTyp.tyvar(i, name));
    switch (TyVarCtx.kind(tyvars, i)) {
    | Some(k)
        when
          TyVarCtx.name(tyvars, i) == Some(name)
          && Kind.consistent_subkind(tyvars, k', k) => (
        operand,
        k',
        u_gen,
      )
    | Some(_)
    | None =>
      let reason: TyVar.HoleReason.t =
        if (TyVar.reserved_word(name)) {
          Reserved;
        } else if (TyVar.valid_name(name)) {
          Unbound;
        } else {
          InvalidName;
        };
      let (u, u_gen) = MetaVarGen.next(u_gen);
      let ty = UHTyp.TyVar(InHole(reason, u), name);
      (ty, Kind.Singleton(TyVarHole(reason, u, name)), u_gen);
    };
  | TyVar(InHole(_, u), name) =>
    if (TyVar.reserved_word(name)) {
      let ty = UHTyp.TyVar(InHole(Reserved, u), name);
      (ty, Kind.Singleton(TyVarHole(Reserved, u, name)), u_gen);
    } else if (TyVar.valid_name(name)) {
      switch (TyVarCtx.index(tyvars, name)) {
      | None =>
        let ty = UHTyp.TyVar(InHole(Unbound, u), name);
        (ty, Kind.Singleton(TyVarHole(Unbound, u, name)), u_gen);
      | Some(i) =>
        let ty = UHTyp.TyVar(NotInHole(i), name);
        (ty, Kind.Singleton(TyVar(i, name)), u_gen);
      };
    } else {
      let ty = UHTyp.TyVar(InHole(InvalidName, u), name);
      (ty, Kind.Singleton(TyVarHole(InvalidName, u, name)), u_gen);
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
