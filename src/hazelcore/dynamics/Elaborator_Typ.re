module ElaborationResult = {
  type t = option((HTyp.t, Kind.t, Delta.t));
};

open OptUtil.Syntax;

let rec get_prod_elements: UHTyp.skel => list(UHTyp.skel) =
  fun
  | BinOp(_, Prod, skel1, skel2) =>
    get_prod_elements(skel1) @ get_prod_elements(skel2)
  | skel => [skel];

let rec syn_elab: (Contexts.t, Delta.t, UHTyp.t) => ElaborationResult.t =
  (ctx, delta) =>
    fun
    | OpSeq(skel, seq) => syn_elab_skel(ctx, delta, skel, seq)
and syn_elab_skel = (ctx, delta, skel, seq) =>
  switch (skel) {
  | Placeholder(n) =>
    seq |> Seq.nth_operand(n) |> syn_elab_operand(ctx, delta)
  | BinOp(_, Prod, _, _) =>
    let skels = get_prod_elements(skel);
    let+ (tys, deltas) =
      List.fold_left(
        (acc_opt, skel) => {
          let* (tys, deltas) = acc_opt;
          let+ (ty, _, delta) =
            ana_elab_skel(ctx, delta, skel, seq, Kind.Type);
          ([ty, ...tys], [delta, ...deltas]);
        },
        Some(([], [])),
        skels,
      );
    let delta =
      deltas |> List.fold_left((d1, d2) => Delta.union(d1, d2), Delta.empty);
    let ty = HTyp.Prod(tys);
    (ty, Kind.Singleton(ty), delta);
  | BinOp(_, op, skel1, skel2) =>
    let* (ty1, _, delta1) = ana_elab_skel(ctx, delta, skel1, seq, Kind.Type);
    let+ (ty2, _, delta2) = ana_elab_skel(ctx, delta, skel2, seq, Kind.Type);
    let ty: HTyp.t =
      switch (op) {
      | Arrow => Arrow(ty1, ty2)
      | Sum => Sum(ty1, ty2)
      | Prod => failwith("Impossible, Prod is matched first")
      };
    (ty, Kind.Singleton(ty), Delta.union(delta1, delta2));
  }
and syn_elab_operand =
    (ctx: Contexts.t, delta: Delta.t, operand: UHTyp.operand)
    : ElaborationResult.t => {
  let const = (ty: HTyp.t) => Some((ty, Kind.Singleton(ty), delta));
  switch (operand) {
  | Hole => Some((Hole, Kind.KHole, delta))
  | TyVar(NotInHole(i), name) => const(TyVar(i, name))
  | TyVar(InHole(reason, u), name) =>
    let ty = HTyp.TyVarHole(reason, u, name);
    Some((ty, Kind.KHole, Delta.add(u, Delta.Hole.Type, delta)));
  | Unit => const(Prod([]))
  | Int => const(Int)
  | Float => const(Float)
  | Bool => const(Bool)
  | Parenthesized(ty) => syn_elab(ctx, delta, ty)
  | List(ty) => syn_elab(ctx, delta, ty)
  };
}

and ana_elab: (Contexts.t, Delta.t, UHTyp.t, Kind.t) => ElaborationResult.t =
  (ctx, delta, opseq, k) =>
    switch (opseq) {
    | OpSeq(skel, seq) => ana_elab_skel(ctx, delta, skel, seq, k)
    }
and ana_elab_skel = (ctx, delta, skel, seq, k): ElaborationResult.t =>
  switch (skel) {
  | Placeholder(n) =>
    let ty_n = seq |> Seq.nth_operand(n);
    ana_elab_operand(ctx, delta, ty_n, k);
  | BinOp(_, _, _, _) =>
    let* (ty, k', delta) = syn_elab_skel(ctx, delta, skel, seq);
    Kind.consistent_subkind(Contexts.tyvars(ctx), k', k)
      ? Some((ty, k', delta)) : None;
  }
and ana_elab_operand =
    (ctx: Contexts.t, delta: Delta.t, operand: UHTyp.operand, k: Kind.t)
    : ElaborationResult.t => {
  switch (operand) {
  | Hole => Some((HTyp.Hole, Kind.KHole, delta))
  | TyVar(InHole(reason, u), t) =>
    Some((HTyp.TyVarHole(reason, u, t), Kind.KHole, delta))
  | Parenthesized(opseq) => ana_elab(ctx, delta, opseq, k)
  // subsumption
  | TyVar(NotInHole(_), _)
  | Unit
  | Int
  | Float
  | Bool
  | List(_) =>
    let* (ty, k', delta) = syn_elab_operand(ctx, delta, operand);
    Kind.consistent_subkind(Contexts.tyvars(ctx), k', k)
      ? Some((ty, k', delta)) : None;
  };
};

// and ana_fix_holes:
//   (Contexts.t, MetaVarGen.t, UHTyp.t) => (UHTyp.t, MetaVarGen.t) =
//   (ctx, u_gen, opseq) =>
//     switch (opseq) {
//     | OpSeq(skel, seq) =>
//       let (skel, seq, u_gen) = ana_fix_holes_skel(ctx, u_gen, skel, seq);
//       (OpSeq(skel, seq), u_gen);
//     }
// and ana_fix_holes_skel = (ctx, u_gen, skel, seq) =>
//   switch (skel) {
//   | Placeholder(n) =>
//     let en = seq |> Seq.nth_operand(n);
//     let (en, u_gen) = ana_fix_holes_operand(ctx, u_gen, en);
//     let seq = seq |> Seq.update_nth_operand(n, en);
//     (skel, seq, u_gen);
//   | BinOp(_, _, _, _) =>
//     let (skel, seq, u_gen) = syn_fix_holes_skel(ctx, u_gen, skel, seq);
//     (skel, seq, u_gen);
//   }
// and ana_fix_holes_operand = (ctx, u_gen, operand) => {
//   switch (operand) {
//   | UHTyp.Hole => (Hole, u_gen)
//   | TyVar(InHole(_), _) => (operand, u_gen)
//   | Parenthesized(body) =>
//     let (block, u_gen) = ana_fix_holes(ctx, u_gen, body);
//     (Parenthesized(block), u_gen);
//   | TyVar(NotInHole(_), _)
//   | Unit
//   | Int
//   | Float
//   | Bool
//   | List(_) => syn_fix_holes_operand(ctx, u_gen, operand)
//   };
// };

// let syn_fix_holes_z = (ctx: Contexts.t, u_gen: MetaVarGen.t, zty: ZTyp.t) => {
//   let path = CursorPath_Typ.of_z(zty);
//   let (ty, u_gen) = syn_fix_holes(ctx, u_gen, ZTyp.erase(zty));
//   let zty =
//     CursorPath_Typ.follow(path, ty)
//     |> OptUtil.get(() =>
//          failwith(
//            "fix_holes did not preserve path "
//            ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
//          )
//        );
//   (zty, u_gen);
// };
