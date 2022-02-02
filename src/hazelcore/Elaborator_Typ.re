module ElaborationResult = {
  include OptUtil;

  module Syn = {
    type t = option((HTyp.t, Kind.t, Delta.t, MetaVarGen.t));
  };

  module Ana = {
    type t = option((HTyp.t, Delta.t, MetaVarGen.t));
  };
};

open ElaborationResult.Syntax;

let rec get_prod_elements: UHTyp.skel => list(UHTyp.skel) =
  fun
  | BinOp(_, Prod, skel1, skel2) =>
    get_prod_elements(skel1) @ get_prod_elements(skel2)
  | skel => [skel];

let rec syn:
  (Contexts.t, MetaVarGen.t, Delta.t, UHTyp.t) => ElaborationResult.Syn.t =
  (ctx, u_gen, delta) =>
    fun
    | OpSeq(skel, seq) => syn_skel(ctx, u_gen, delta, skel, seq)
and syn_skel = (ctx, u_gen, delta, skel, seq) =>
  switch (skel) {
  | Placeholder(n) =>
    seq |> Seq.nth_operand(n) |> syn_operand(ctx, u_gen, delta)
  | BinOp(_, Prod, _, _) =>
    /* TElabSBinOp */
    let+ (tys, deltas, u_gen) =
      skel
      |> get_prod_elements
      |> List.fold_left(
           (results, skel) => {
             let* (tys, deltas, u_gen) = results;
             let+ (ty, delta, u_gen) =
               ana_skel(ctx, u_gen, delta, Kind.Type, skel, seq);
             ([ty, ...tys], [delta, ...deltas], u_gen);
           },
           Some(([], [], u_gen)),
         );
    let delta =
      deltas |> List.fold_left((d1, d2) => Delta.union(d1, d2), Delta.empty);
    let ty = HTyp.Prod(tys);
    (ty, Kind.Singleton(Type, ty), delta, u_gen);
  | BinOp(_, op, skel1, skel2) =>
    /* TElabSBinOp */
    let* (ty1, d1, u_gen) =
      ana_skel(ctx, u_gen, delta, Kind.Type, skel1, seq);
    let+ (ty2, d2, u_gen) =
      ana_skel(ctx, u_gen, delta, Kind.Type, skel2, seq);
    let ty: HTyp.t =
      switch (op) {
      | Arrow => Arrow(ty1, ty2)
      | Sum => Sum(ty1, ty2)
      | Prod => failwith("Impossible, Prod is matched first")
      };
    let k = Kind.Singleton(Type, ty);
    (ty, k, Delta.union(d1, d2), u_gen);
  }
and syn_operand = (ctx, u_gen, delta, operand) => {
  let const = (ty, u_gen) => {
    /* TElabSConst */
    let k = Kind.Singleton(Type, ty);
    Some((ty, k, delta, u_gen));
  };
  let tyctx = Contexts.typing(ctx);
  switch (operand) {
  | Hole(u) =>
    /* TElabSHole */
    let ty = HTyp.Hole(u);
    let delta = Delta.(add(u, Hole.Type(KHole, tyctx), delta));
    Some((ty, KHole, delta, u_gen));
  // TODO: NEHole case
  | TyVar(NotInHole(i), name) =>
    /* TElabSVar */
    let* _ = tyctx |> TyCtx.var_kind(i);
    const(TyVar(i, name), u_gen);
  | TyVar(InHole(reason, u), name) =>
    /* TElabSUVar */
    // TODO: id(\Phi) in TyVarHole
    let ty = HTyp.TyVarHole(reason, u, name);
    let k = Kind.KHole;
    let delta' = Delta.(add(u, Hole.Type(k, tyctx), delta));
    Some((ty, k, delta', u_gen));
  | Unit => const(Prod([]), u_gen)
  | Int => const(Int, u_gen)
  | Float => const(Float, u_gen)
  | Bool => const(Bool, u_gen)
  | Parenthesized(opseq) => syn(ctx, u_gen, delta, opseq)
  | List(opseq) =>
    /* TElabSList */
    let+ (ty, delta, u_gen) = ana(ctx, u_gen, delta, opseq, Kind.Type);
    (ty, Kind.Singleton(Type, ty), delta, u_gen);
  };
}

and ana:
  (Contexts.t, MetaVarGen.t, Delta.t, UHTyp.t, Kind.t) =>
  ElaborationResult.Ana.t =
  (ctx, u_gen, delta, opseq, k) =>
    switch (opseq) {
    | OpSeq(skel, seq) => ana_skel(ctx, u_gen, delta, k, skel, seq)
    }
and ana_skel =
    (ctx, u_gen, delta, k, skel, seq): option((HTyp.t, Delta.t, MetaVar.t)) =>
  switch (skel) {
  | Placeholder(n) =>
    seq |> Seq.nth_operand(n) |> ana_operand(ctx, u_gen, delta, k)
  | BinOp(_, _, _, _) =>
    /* TElabASubsume */
    let+ (ty, _, delta, u_gen) = syn_skel(ctx, u_gen, delta, skel, seq);
    (ty, delta, u_gen);
  }
and ana_operand = (ctx, u_gen, delta, k, operand) => {
  let tyctx = Contexts.typing(ctx);
  switch (operand) {
  | UHTyp.Hole(u) =>
    /* TElabAHole */
    let ty = HTyp.Hole(u);
    Some((ty, delta |> Delta.add(u, Delta.Hole.Type(KHole, tyctx)), u_gen));
  // TODO: Add an NEHole case when it's possible to have an arbitrary type hole
  | TyVar(InHole(reason, u), t) =>
    /* TElabAUVar */
    // TODO: id(\Phi) in TyVarHole
    let ty = HTyp.TyVarHole(reason, u, t);
    Some((ty, Delta.add(u, Delta.Hole.Type(KHole, tyctx), delta), u_gen));
  | Parenthesized(opseq) => ana(ctx, u_gen, delta, opseq, k)
  | TyVar(NotInHole(_), _)
  | Unit
  | Int
  | Float
  | Bool
  | List(_) =>
    /* TElabASubsume */
    let+ (ty, _, delta, u_gen) = syn_operand(ctx, u_gen, delta, operand);
    (ty, delta, u_gen);
  };
};

let syn_kind = (ctx, u_gen, uhty) =>
  syn(ctx, u_gen, Delta.empty, uhty)
  |> Option.map(((_, k, _, u_gen)) => (k, u_gen));
let syn_kind_skel = (ctx, u_gen, skel, seq) =>
  syn_skel(ctx, u_gen, Delta.empty, skel, seq)
  |> Option.map(((_, k, _, u_gen)) => (k, u_gen));
let syn_kind_operand = (ctx, u_gen, operand) =>
  syn_operand(ctx, u_gen, Delta.empty, operand)
  |> Option.map(((_, k, _, u_gen)) => (k, u_gen));

// let ana_kind = (ctx, uhty, k: Kind.t): option(unit) => {
//   open OptUtil.Syntax;
//   let* dk' = syn_kind(ctx, uhty);
//   ctx |> Contexts.typing |> Kind.consistent_subkind(dk', k) ? Some() : None;
// };

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
    let (_, k, _, u_gen) =
      syn_skel(ctx, u_gen, Delta.empty, skel, seq) |> Option.get;
    (skel, seq, k, u_gen);
  }
and syn_fix_holes_operand = (ctx, u_gen, operand) => {
  let tyctx = Contexts.typing(ctx);
  switch (operand) {
  | UHTyp.Hole(u) =>
    /* TElabSHole */
    (Hole(u), KHole, u_gen)
  // TODO: NEHole case
  | TyVar(NotInHole(i), name) =>
    /* TElabSVar */
    switch (tyctx |> TyCtx.var_kind(i)) {
    | Some(k) =>
      let ty = UHTyp.TyVar(NotInHole(i), name);
      (ty, k, u_gen);
    | None =>
      let (u, u_gen) = MetaVarGen.next(u_gen);
      let ty = UHTyp.TyVar(InHole(Unbound, u), name);
      (ty, KHole, u_gen);
    }
  | TyVar(InHole(reason, u), name) =>
    /* TElabSUVar */
    // TODO: id(\Phi) in TyVarHole
    (UHTyp.TyVar(InHole(reason, u), name), Kind.KHole, u_gen)
  | Unit => (Unit, Singleton(Type, Prod([])), u_gen)
  | Int => (Int, Singleton(Type, Int), u_gen)
  | Float => (Float, Singleton(Type, Float), u_gen)
  | Bool => (Bool, Singleton(Type, Bool), u_gen)
  | Parenthesized(body) =>
    let (block, kind, u_gen) = syn_fix_holes(ctx, u_gen, body);
    (Parenthesized(block), kind, u_gen);
  | List(opseq) =>
    /* TElabSList */
    let (opseq, u_gen) = ana_fix_holes(ctx, u_gen, opseq, Kind.Type);
    // TODO: Is there a better way around this than force-unwrapping?
    let (_, k, _, u_gen) = syn(ctx, u_gen, Delta.empty, opseq) |> Option.get;
    (List(opseq), k, u_gen);
  };
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
and ana_fix_holes_skel = (ctx, u_gen, k, skel, seq) =>
  switch (skel) {
  | Placeholder(n) =>
    let en = seq |> Seq.nth_operand(n);
    let (en, u_gen) = ana_fix_holes_operand(ctx, u_gen, k, en);
    let seq = seq |> Seq.update_nth_operand(n, en);
    (skel, seq, u_gen);
  | BinOp(_, _, _, _) =>
    /* TElabASubsume */
    let (skel, seq, _k', u_gen) = syn_fix_holes_skel(ctx, u_gen, skel, seq);
    // if (ctx |> Contexts.typing |> Kind.consistent_subkind(k', k)) {
    (skel, seq, u_gen);
  // } else {
  //   failwith("TODO: Add inconsistent kind hole (this can't happen now)");
  // };
  }
and ana_fix_holes_operand = (ctx, u_gen, k, operand) => {
  switch (operand) {
  | UHTyp.Hole(u) =>
    /* TElabAHole */
    (Hole(u), u_gen)
  | TyVar(InHole(_), _) =>
    /* TElabAUVar */
    // TODO: id(\Phi) in TyVarHole
    (operand, u_gen)
  | Parenthesized(body) =>
    let (block, u_gen) = ana_fix_holes(ctx, u_gen, body, k);
    (Parenthesized(block), u_gen);
  | TyVar(NotInHole(_), _)
  | Unit
  | Int
  | Float
  | Bool
  | List(_) =>
    /* TElabASubsume */
    let (ty, _k', u_gen) = syn_fix_holes_operand(ctx, u_gen, operand);
    // if (ctx |> Contexts.typing |> Kind.consistent_subkind(k', k)) {
    (ty, u_gen);
  // } else {
  //   failwith("TODO: Add inconsistent kind hole (this can't happen now)");
  // };
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
