module ElaborationResult = {
  include OptUtil;

  module Syn = {
    type t = option((HTyp.t, Kind.t, Delta.t));
  };

  module Ana = {
    type t = option((HTyp.t, Delta.t));
  };
};

open ElaborationResult.Syntax;

let rec get_prod_elements: UHTyp.skel => list(UHTyp.skel) =
  fun
  | BinOp(_, Prod, skel1, skel2) =>
    get_prod_elements(skel1) @ get_prod_elements(skel2)
  | skel => [skel];

let rec syn: (Contexts.t, Delta.t, UHTyp.t) => ElaborationResult.Syn.t =
  (ctx, delta) =>
    fun
    | OpSeq(skel, seq) => syn_skel(ctx, delta, skel, seq)
and syn_skel = (ctx, delta, skel, seq) =>
  switch (skel) {
  | Placeholder(n) => seq |> Seq.nth_operand(n) |> syn_operand(ctx, delta)
  | BinOp(_, Prod, _, _) =>
    /* TElabSBinOp */
    let+ rs =
      skel
      |> get_prod_elements
      |> List.map(skel => ana_skel(ctx, delta, Kind.Type, skel, seq))
      |> OptUtil.sequence;
    let (tys, ds) = ListUtil.unzip(rs);
    let delta =
      ds |> List.fold_left((d1, d2) => Delta.union(d1, d2), Delta.empty);
    let tau = HTyp.Prod(tys);
    (tau, Kind.Singleton(tau), delta);
  | BinOp(_, op, skel1, skel2) =>
    /* TElabSBinOp */
    let* (ty1, d1) = ana_skel(ctx, delta, Kind.Type, skel1, seq);
    let+ (ty2, d2) = ana_skel(ctx, delta, Kind.Type, skel2, seq);
    let ty =
      switch (op) {
      | Arrow => HTyp.Arrow(ty1, ty2)
      | Sum => HTyp.Sum(ty1, ty2)
      | Prod => failwith("Impossible, Prod is matched first")
      };
    (ty, Kind.Singleton(ty), Delta.union(d1, d2));
  }
and syn_operand = (ctx, delta, operand) => {
  let const = (c: HTyp.t) => {
    /* TElabSConst */
    Some((c, Kind.Singleton(c), delta));
  };

  switch (operand) {
  | Hole(u) =>
    /* TElabSHole */
    Some((
      HTyp.Hole,
      Kind.KHole,
      Delta.add(
        u,
        Delta.Hole.Type(Kind.KHole, Contexts.tyvars(ctx)),
        delta,
      ),
    ))
  // TODO: NEHole case
  | TyVar(NotInVarHole, t) =>
    /* TElabSVar */
    let+ idx = TyVarCtx.index_of(Contexts.tyvars(ctx), t);
    let tau = HTyp.TyVar(idx, t);
    (tau, Kind.Singleton(tau), delta);
  | TyVar(InVarHole(_, u), t) =>
    /* TElabSUVar */
    // TODO: id(\Phi) in TyVarHole
    Some((
      HTyp.TyVarHole(u, t),
      Kind.KHole,
      Delta.add(
        u,
        Delta.Hole.Type(Kind.KHole, Contexts.tyvars(ctx)),
        delta,
      ),
    ))
  | Unit => const(Prod([]))
  | Int => const(Int)
  | Float => const(Float)
  | Bool => const(Bool)
  | Parenthesized(opseq) => syn(ctx, delta, opseq)
  | List(opseq) =>
    /* TElabSList */
    let+ (ty, delta) = ana(ctx, delta, opseq, Kind.Type);
    let tau = HTyp.List(ty);
    (tau, Kind.Singleton(tau), delta);
  };
}

and ana: (Contexts.t, Delta.t, UHTyp.t, Kind.t) => ElaborationResult.Ana.t =
  (ctx, delta, opseq, kind) =>
    switch (opseq) {
    | OpSeq(skel, seq) => ana_skel(ctx, delta, kind, skel, seq)
    }
and ana_skel = (ctx, delta, kind, skel, seq) =>
  switch (skel) {
  | Placeholder(n) =>
    seq |> Seq.nth_operand(n) |> ana_operand(ctx, delta, kind)
  | BinOp(_, _, _, _) =>
    /* TElabASubsume */
    let+ (ty, _, delta) = syn_skel(ctx, delta, skel, seq);
    (ty, delta);
  }
and ana_operand = (ctx, delta, kind, operand) => {
  switch (operand) {
  | UHTyp.Hole(u) =>
    /* TElabAHole */
    Some((
      Hole,
      Delta.add(
        u,
        Delta.Hole.Type(Kind.KHole, Contexts.tyvars(ctx)),
        delta,
      ),
    ))
  // TODO: Add an NEHole case when it's possible to have an arbitrary type hole
  | TyVar(InVarHole(_, u), t) =>
    /* TElabAUVar */
    // TODO: id(\Phi) in TyVarHole
    Some((
      TyVarHole(u, t),
      Delta.add(
        u,
        Delta.Hole.Type(Kind.KHole, Contexts.tyvars(ctx)),
        delta,
      ),
    ))
  | Parenthesized(opseq) => ana(ctx, delta, opseq, kind)
  | TyVar(NotInVarHole, _)
  | Unit
  | Int
  | Float
  | Bool
  | List(_) =>
    /* TElabASubsume */
    let+ (ty, _, delta) = syn_operand(ctx, delta, operand);
    (ty, delta);
  };
};

let syn_kind = (ctx, uhty) =>
  syn(ctx, Delta.empty, uhty) |> Option.map(((_, k, _)) => k);
let syn_kind_skel = (ctx, skel, seq) =>
  syn_skel(ctx, Delta.empty, skel, seq) |> Option.map(((_, k, _)) => k);
let syn_kind_operand = (ctx, operand) =>
  syn_operand(ctx, Delta.empty, operand) |> Option.map(((_, k, _)) => k);

let rec syn_fix_holes:
  (Contexts.t, MetaVarGen.t, UHTyp.t) => (UHTyp.t, Kind.t, MetaVarGen.t) =
  (ctx, u_gen) =>
    fun
    | OpSeq(skel, seq) => {
        let (skel, seq, kind, u_gen) =
          syn_fix_holes_skel(ctx, u_gen, skel, seq);
        (OpSeq(skel, seq), kind, u_gen);
      }
and syn_fix_holes_skel = (ctx, u_gen, skel, seq) =>
  switch (skel) {
  | Placeholder(n) =>
    let en = seq |> Seq.nth_operand(n);
    let (en, ty, u_gen) = syn_fix_holes_operand(ctx, u_gen, en);
    let seq = seq |> Seq.update_nth_operand(n, en);
    (skel, seq, ty, u_gen);
  | BinOp(_, op, skel1, skel2) =>
    /* TElabSBinOp */
    let (skel1, seq, u_gen) =
      ana_fix_holes_skel(ctx, u_gen, Kind.Type, skel1, seq);
    let (skel2, seq, u_gen) =
      ana_fix_holes_skel(ctx, u_gen, Kind.Type, skel2, seq);

    let skel = Skel.BinOp(NotInHole, op, skel1, skel2);
    // TODO: Is there a better way around this than force-unwrapping?
    let (_, k, _) = syn_skel(ctx, Delta.empty, skel, seq) |> Option.get;
    (skel, seq, k, u_gen);
  }
and syn_fix_holes_operand = (ctx, u_gen, operand) =>
  switch (operand) {
  | UHTyp.Hole(u) =>
    /* TElabSHole */
    (Hole(u), Kind.KHole, u_gen)
  // TODO: NEHole case
  | TyVar(NotInVarHole, t) =>
    /* TElabSVar */
    let idx_opt = TyVarCtx.index_of(Contexts.tyvars(ctx), t);
    switch (idx_opt) {
    | Some(idx) =>
      let (_, k) = TyVarCtx.tyvar_with_idx(Contexts.tyvars(ctx), idx);
      (TyVar(NotInVarHole, t), k, u_gen);
    | None =>
      let (u, u_gen) = u_gen |> MetaVarGen.next;
      (
        UHTyp.TyVar(InVarHole(VarErrStatus.HoleReason.Free, u), t),
        Kind.KHole,
        u_gen,
      );
    };
  | TyVar(InVarHole(r, u), t) =>
    /* TElabSUVar */
    // TODO: id(\Phi) in TyVarHole
    (UHTyp.TyVar(InVarHole(r, u), t), Kind.KHole, u_gen)
  | Unit => (Unit, Kind.Singleton(Prod([])), u_gen)
  | Int => (Int, Kind.Singleton(Int), u_gen)
  | Float => (Float, Kind.Singleton(Float), u_gen)
  | Bool => (Bool, Kind.Singleton(Bool), u_gen)
  | Parenthesized(body) =>
    let (block, kind, u_gen) = syn_fix_holes(ctx, u_gen, body);
    (Parenthesized(block), kind, u_gen);
  | List(opseq) =>
    /* TElabSList */
    let (opseq, u_gen) = ana_fix_holes(ctx, u_gen, opseq, Kind.Type);

    // TODO: Is there a better way around this than force-unwrapping?
    let (_, k, _) = syn(ctx, Delta.empty, opseq) |> Option.get;
    (List(opseq), k, u_gen);
  }
and ana_fix_holes:
  (Contexts.t, MetaVarGen.t, UHTyp.t, Kind.t) => (UHTyp.t, MetaVarGen.t) =
  (ctx, u_gen, opseq, kind) =>
    switch (opseq) {
    | OpSeq(skel, seq) =>
      let (skel, seq, u_gen) =
        ana_fix_holes_skel(ctx, u_gen, kind, skel, seq);
      (OpSeq(skel, seq), u_gen);
    }
and ana_fix_holes_skel = (ctx, u_gen, kind, skel, seq) =>
  switch (skel) {
  | Placeholder(n) =>
    let en = seq |> Seq.nth_operand(n);
    let (en, u_gen) = ana_fix_holes_operand(ctx, u_gen, kind, en);
    let seq = seq |> Seq.update_nth_operand(n, en);
    (skel, seq, u_gen);
  | BinOp(_, _, _, _) =>
    /* TElabASubsume */
    let (skel, seq, k', u_gen) = syn_fix_holes_skel(ctx, u_gen, skel, seq);
    if (Construction.consistent_subkind(ctx, k', kind)) {
      (skel, seq, u_gen);
    } else {
      failwith("TODO: Add inconsistent kind hole (this can't happen now)");
    };
  }
and ana_fix_holes_operand = (ctx, u_gen, kind, operand) => {
  switch (operand) {
  | UHTyp.Hole(u) =>
    /* TElabAHole */
    (Hole(u), u_gen)
  | TyVar(InVarHole(r, u), t) =>
    /* TElabAUVar */
    // TODO: id(\Phi) in TyVarHole
    (TyVar(InVarHole(r, u), t), u_gen)
  | Parenthesized(body) =>
    let (block, u_gen) = ana_fix_holes(ctx, u_gen, body, kind);
    (Parenthesized(block), u_gen);
  | TyVar(NotInVarHole, _)
  | Unit
  | Int
  | Float
  | Bool
  | List(_) =>
    /* TElabASubsume */
    let (ty, k', u_gen) = syn_fix_holes_operand(ctx, u_gen, operand);
    if (Construction.consistent_subkind(ctx, k', kind)) {
      (ty, u_gen);
    } else {
      failwith("TODO: Add inconsistent kind hole (this can't happen now)");
    };
  };
};

let syn_fix_holes_z = (ctx: Contexts.t, u_gen: MetaVarGen.t, zty: ZTyp.t) => {
  let path = CursorPath_Typ.of_z(zty);
  let (ty, k, u_gen) = syn_fix_holes(ctx, u_gen, ZTyp.erase(zty));
  let zty =
    CursorPath_Typ.follow(path, ty)
    |> OptUtil.get(() =>
         failwith(
           "fix_holes did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
         )
       );
  (zty, k, u_gen);
};
