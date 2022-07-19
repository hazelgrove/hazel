open Sexplib.Std;

module ElaborationResult = {
  [@deriving sexp]
  type t = option((HTyp.t, Kind.t, Delta.t));
};

let rec get_prod_elements: UHTyp.skel => list(UHTyp.skel) =
  fun
  | BinOp(_, Prod, skel1, skel2) =>
    get_prod_elements(skel1) @ get_prod_elements(skel2)
  | skel => [skel];

let rec syn_elab: (Context.t, Delta.t, UHTyp.t) => ElaborationResult.t =
  (ctx, delta, OpSeq(skel, seq)) => syn_elab_skel(ctx, delta, skel, seq)

and syn_elab_skel = (ctx, delta, skel, seq) =>
  switch (skel) {
  | Placeholder(n) =>
    seq |> Seq.nth_operand(n) |> syn_elab_operand(ctx, delta)
  | BinOp(_, Prod, _, _) =>
    let skels = get_prod_elements(skel);
    open OptUtil.Syntax;
    let+ (tys, deltas) =
      List.fold_right(
        (skel, acc_opt) => {
          let* (tys, deltas) = acc_opt;
          let+ (ty, _, delta) =
            ana_elab_skel(ctx, delta, skel, seq, Kind.Type);
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
    open OptUtil.Syntax;
    let* (ty1, _, delta1) = ana_elab_skel(ctx, delta, skel1, seq, Kind.Type);
    let+ (ty2, _, delta2) = ana_elab_skel(ctx, delta, skel2, seq, Kind.Type);
    let ty: HTyp.t =
      switch (op) {
      | Arrow => HTyp.arrow(ty1, ty2)
      | Sum => HTyp.sum(ty1, ty2)
      | Prod => failwith("Impossible, Prod is matched first")
      };
    (ty, Kind.singleton(ty), Delta.union(delta1, delta2));
  }

and syn_elab_operand =
    (ctx: Context.t, delta: Delta.t, operand: UHTyp.operand)
    : ElaborationResult.t => {
  let const = (ty: HTyp.t) => Some((ty, Kind.singleton(ty), delta));
  switch (operand) {
  | Hole => Some((HTyp.hole(), Kind.Hole, delta))
  | Unit => const(HTyp.product([]))
  | Int => const(HTyp.int())
  | Float => const(HTyp.float())
  | Bool => const(HTyp.bool())
  | Parenthesized(ty) => syn_elab(ctx, delta, ty)
  | List(ty) =>
    open OptUtil.Syntax;
    let+ (ty_elt, _, delta) = syn_elab(ctx, delta, ty);
    let ty = HTyp.list(ty_elt);
    (ty, Kind.singleton(ty), delta);
  | TyVar(NotInTyVarHole, t) =>
    open OptUtil.Syntax;
    let* cref = Context.tyvar_ref(ctx, t);
    let ty = HTyp.rescope(ctx, HTyp.tyvar(ctx, cref.index, t));
    Some((ty, Kind.singleton(ty), delta));
  | TyVar(InHole(reason, u), t) =>
    let ty = HTyp.tyvarhole(reason, u, t);
    Some((ty, Kind.Hole, Delta.add(u, Delta.Hole.Type, delta)));
  | InvalidText(u, t) =>
    let ty = HTyp.invalid_text(u, t);
    Some((ty, Kind.Hole, Delta.add(u, Delta.Hole.Type, delta)));
  };
}

and ana_elab: (Context.t, Delta.t, UHTyp.t, Kind.t) => ElaborationResult.t =
  (ctx, delta, OpSeq(skel, seq), k) =>
    ana_elab_skel(ctx, delta, skel, seq, k)

and ana_elab_skel = (ctx: Context.t, delta, skel, seq, k): ElaborationResult.t =>
  switch (skel) {
  | Placeholder(n) =>
    let ty_n = seq |> Seq.nth_operand(n);
    ana_elab_operand(ctx, delta, ty_n, k);
  | BinOp(_, _, _, _) =>
    open OptUtil.Syntax;
    let* (ty, k', delta) = syn_elab_skel(ctx, delta, skel, seq);
    Kind.consistent_subkind(ctx, k', k) ? Some((ty, k', delta)) : None;
  }

and ana_elab_operand =
    (ctx: Context.t, delta: Delta.t, operand: UHTyp.operand, k: Kind.t)
    : ElaborationResult.t =>
  switch (operand) {
  | Hole => Some((HTyp.hole(), Kind.Hole, delta))
  | TyVar(InHole(reason, u), t) =>
    Some((HTyp.tyvarhole(reason, u, t), Kind.Hole, delta))
  | InvalidText(u, t) => Some((HTyp.invalid_text(u, t), Kind.Hole, delta))
  | Parenthesized(opseq) => ana_elab(ctx, delta, opseq, k)
  // subsumption
  | TyVar(NotInTyVarHole, _)
  | Unit
  | Int
  | Float
  | Bool
  | List(_) =>
    open OptUtil.Syntax;
    let* (ty, k', delta) = syn_elab_operand(ctx, delta, operand);
    Kind.consistent_subkind(ctx, k', k) ? Some((ty, k', delta)) : None;
  };
