module ElaborationResult = {
  type t = option((HTyp.t, Delta.t));
};

open OptUtil.Syntax;

let rec get_prod_elements: UHTyp.skel => list(UHTyp.skel) =
  fun
  | BinOp(_, Prod, skel1, skel2) =>
    get_prod_elements(skel1) @ get_prod_elements(skel2)
  | skel => [skel];

let rec syn: (Contexts.t, Delta.t, UHTyp.t) => ElaborationResult.t =
  (ctx, delta) =>
    fun
    | OpSeq(skel, seq) => syn_skel(ctx, delta, skel, seq)
and syn_skel = (ctx, delta, skel, seq) =>
  switch (skel) {
  | Placeholder(n) => seq |> Seq.nth_operand(n) |> syn_operand(ctx, delta)
  | BinOp(_, Prod, _, _) =>
    let skels = get_prod_elements(skel);
    let+ (tys, deltas) =
      List.fold_left(
        (acc_opt, skel) => {
          let* (tys, deltas) = acc_opt;
          let+ (ty, delta) = ana_skel(ctx, delta, skel, seq);
          ([ty, ...tys], [delta, ...deltas]);
        },
        Some(([], [])),
        skels,
      );
    let delta =
      deltas |> List.fold_left((d1, d2) => Delta.union(d1, d2), Delta.empty);
    let ty = HTyp.Prod(tys);
    (ty, delta);
  | BinOp(_, op, skel1, skel2) =>
    let* (ty1, d1) = ana_skel(ctx, delta, skel1, seq);
    let+ (ty2, d2) = ana_skel(ctx, delta, skel2, seq);
    let ty: HTyp.t =
      switch (op) {
      | Arrow => Arrow(ty1, ty2)
      | Sum => Sum(ty1, ty2)
      | Prod => failwith("Impossible, Prod is matched first")
      };
    (ty, Delta.union(d1, d2));
  }
and syn_operand =
    (ctx: Contexts.t, delta: Delta.t, operand: UHTyp.operand)
    : ElaborationResult.t => {
  let const = (ty: HTyp.t) => Some((ty, delta));
  switch (operand) {
  | Hole => Some((Hole, delta))
  | TyVar(NotInHole(i), name) => const(TyVar(i, name))
  | TyVar(InHole(reason, u), name) =>
    let ty = HTyp.TyVarHole(reason, u, name);
    let delta' = Delta.(add(u, Hole.Type, delta));
    Some((ty, delta'));
  | Unit => const(Prod([]))
  | Int => const(Int)
  | Float => const(Float)
  | Bool => const(Bool)
  | Parenthesized(opseq) => syn(ctx, delta, opseq)
  | List(opseq) =>
    let+ (ty, delta) = ana(ctx, delta, opseq);
    (ty, delta);
  };
}

and ana: (Contexts.t, Delta.t, UHTyp.t) => ElaborationResult.t =
  (ctx, delta, opseq) =>
    switch (opseq) {
    | OpSeq(skel, seq) => ana_skel(ctx, delta, skel, seq)
    }
and ana_skel = (ctx, delta, skel, seq): ElaborationResult.t =>
  switch (skel) {
  | Placeholder(n) => seq |> Seq.nth_operand(n) |> ana_operand(ctx, delta)
  | BinOp(_, _, _, _) =>
    let+ (ty, delta) = syn_skel(ctx, delta, skel, seq);
    (ty, delta);
  }
and ana_operand =
    (ctx: Contexts.t, delta: Delta.t, operand: UHTyp.operand)
    : ElaborationResult.t => {
  switch (operand) {
  | Hole => Some((Hole, delta))
  | TyVar(InHole(reason, u), t) =>
    let ty = HTyp.TyVarHole(reason, u, t);
    Some((ty, Delta.add(u, Delta.Hole.Type, delta)));
  | Parenthesized(opseq) => ana(ctx, delta, opseq)
  | TyVar(NotInHole(_), _)
  | Unit
  | Int
  | Float
  | Bool
  | List(_) =>
    let+ (ty, delta) = syn_operand(ctx, delta, operand);
    (ty, delta);
  };
};

let rec syn_fix_holes:
  (Contexts.t, MetaVarGen.t, UHTyp.t) => (UHTyp.t, MetaVarGen.t) =
  (ctx, u_gen, ty) =>
    switch (ty) {
    | OpSeq(skel, seq) =>
      let (skel, seq, u_gen) = syn_fix_holes_skel(ctx, u_gen, skel, seq);
      (OpSeq(skel, seq), u_gen);
    }
and syn_fix_holes_skel = (ctx, u_gen, skel, seq) =>
  switch (skel) {
  | Placeholder(n) =>
    let en = seq |> Seq.nth_operand(n);
    let (en, u_gen) = syn_fix_holes_operand(ctx, u_gen, en);
    let seq = seq |> Seq.update_nth_operand(n, en);
    (skel, seq, u_gen);
  | BinOp(_, op, skel1, skel2) =>
    let (skel1, seq, u_gen) = ana_fix_holes_skel(ctx, u_gen, skel1, seq);
    let (skel2, seq, u_gen) = ana_fix_holes_skel(ctx, u_gen, skel2, seq);

    let skel = Skel.BinOp(NotInHole, op, skel1, skel2);
    (skel, seq, u_gen);
  }
and syn_fix_holes_operand =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, operand: UHTyp.operand)
    : (UHTyp.operand, MetaVarGen.t) => {
  switch (operand) {
  | Hole => (Hole, u_gen)
  | TyVar(NotInHole(_), name) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    let ty = UHTyp.TyVar(InHole(Unbound, u), name);
    (ty, u_gen);
  | TyVar(InHole(reason, u), name) => (
      UHTyp.TyVar(InHole(reason, u), name),
      u_gen,
    )
  | Unit
  | Int
  | Float
  | Bool => (operand, u_gen)
  | Parenthesized(body) =>
    let (block, u_gen) = syn_fix_holes(ctx, u_gen, body);
    (Parenthesized(block), u_gen);
  | List(opseq) =>
    /* TElabSList */
    let (opseq, u_gen) = ana_fix_holes(ctx, u_gen, opseq);
    (List(opseq), u_gen);
  };
}
and ana_fix_holes:
  (Contexts.t, MetaVarGen.t, UHTyp.t) => (UHTyp.t, MetaVarGen.t) =
  (ctx, u_gen, opseq) =>
    switch (opseq) {
    | OpSeq(skel, seq) =>
      let (skel, seq, u_gen) = ana_fix_holes_skel(ctx, u_gen, skel, seq);
      (OpSeq(skel, seq), u_gen);
    }
and ana_fix_holes_skel = (ctx, u_gen, skel, seq) =>
  switch (skel) {
  | Placeholder(n) =>
    let en = seq |> Seq.nth_operand(n);
    let (en, u_gen) = ana_fix_holes_operand(ctx, u_gen, en);
    let seq = seq |> Seq.update_nth_operand(n, en);
    (skel, seq, u_gen);
  | BinOp(_, _, _, _) =>
    let (skel, seq, u_gen) = syn_fix_holes_skel(ctx, u_gen, skel, seq);
    (skel, seq, u_gen);
  }
and ana_fix_holes_operand = (ctx, u_gen, operand) => {
  switch (operand) {
  | UHTyp.Hole => (Hole, u_gen)
  | TyVar(InHole(_), _) => (operand, u_gen)
  | Parenthesized(body) =>
    let (block, u_gen) = ana_fix_holes(ctx, u_gen, body);
    (Parenthesized(block), u_gen);
  | TyVar(NotInHole(_), _)
  | Unit
  | Int
  | Float
  | Bool
  | List(_) => syn_fix_holes_operand(ctx, u_gen, operand)
  };
};

let syn_fix_holes_z = (ctx: Contexts.t, u_gen: MetaVarGen.t, zty: ZTyp.t) => {
  let path = CursorPath_Typ.of_z(zty);
  let (ty, u_gen) = syn_fix_holes(ctx, u_gen, ZTyp.erase(zty));
  let zty =
    CursorPath_Typ.follow(path, ty)
    |> OptUtil.get(() =>
         failwith(
           "fix_holes did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
         )
       );
  (zty, u_gen);
};
