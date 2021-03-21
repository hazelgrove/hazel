module ElaborationResult = {
  include OptUtil;

  type t = option((HTyp.t, Kind.t, Delta.t));
};
open ElaborationResult.Syntax;

let rec get_prod_elements: UHTyp.skel => list(UHTyp.skel) =
  fun
  | BinOp(_, Prod, skel1, skel2) =>
    get_prod_elements(skel1) @ get_prod_elements(skel2)
  | skel => [skel];

let syn: (Contexts.t, Delta.t, UHTyp.t) => ElaborationResult.t =
  (ctx, delta) =>  fun
  | OpSeq(skel, seq) => syn_skel(ctx, delta, skel, seq)
and syn_skel = (ctx, delta, skel, seq) =>
    switch (skel) {
    | Placeholder(n) => seq |> Seq.nth_operand(n) |> syn_operand(ctx)
    | BinOp(_, op, skel1, skel2) =>
      /* TElabSBinOp */
      let* (ty1, k1, d1) = ana_skel(ctx, delta, Kind.Type, skel1, seq);
      let* (ty2, k2, d2) = ana_skel(ctx, delta, Kind.Type, skel2, seq);
      switch (k1, k2) {
      | (Kind.Type, Kind.Type) =>
        let ty =
          switch (op) {
          | Operators_Typ.Arrow => Arrow(ty1, ty2)
          | Sum => Sum(ty1, ty2)
          };
        Some((ty, Kind.Type, Delta.union(d1, d2)))
      | (_, _) => None
      }
    | BinOp(_, Prod, _, _) =>
      /* TElabSBinOp */
      let* rs =
        skel
        |> get_prod_elements
        |> List.map(skel => ana_skel(ctx, delta, Kind.Type, skel, seq))
        |> OptUtil.sequence;
      let (tys, ks, ds) = ListUtil.unzip3(rs);
      let+ () = ks |> List.for_all(k => k == Kind.Type) |> OptUtil.of_bool
      let delta = ds |> List.fold_left((d1, d2) => Delta.union(d1, d2), Delta.empty);
      (Prod(tys), Kind.Type, delta)
    }
  and syn_operand = (ctx, delta) =>
    fun
    | Hole => Hole
    | TyVar(NotInVarHole, t) =>
      TyVar(TyVarCtx.index_of_exn(ctx, t), TyId.to_string(t))
    | TyVar(InVarHole(_, u), t) => TyVarHole(u, TyId.to_string(t))
    | Unit => Prod([])
    | Int => Int
    | Float => Float
    | Bool => Bool
    | Parenthesized(opseq) => expand(ctx, opseq)
    | List(opseq) => List(expand(ctx, opseq));

and ana: (Contexts.t, Delta.t, UHTyp.t, Kind.t) => ElaborationResult.t =
  (ctx, delta) =>  fun
  | OpSeq(skel, seq) => ana_skel(ctx, delta, kind, skel, seq)
and ana_skel = (ctx, delta, kind, skel, seq) => failwith("TODO")
and ana_operand = failwith("TODO")


