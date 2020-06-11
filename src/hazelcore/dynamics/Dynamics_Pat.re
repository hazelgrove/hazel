module ExpandResult = {
  type t =
    | Expands(DHPat.t, HTyp.t, Contexts.t, Delta.t)
    | DoesNotExpand;

  let to_option =
    fun
    | DoesNotExpand => None
    | Expands(pat, ty, ctx, delta) => Some((pat, ty, ctx, delta));

  let from_option =
    fun
    | None => DoesNotExpand
    | Some((pat, ty, ctx, delta)) => Expands(pat, ty, ctx, delta);

  let bind = (x: t, ~f: ((DHPat.t, HTyp.t, Contexts.t, Delta.t)) => t): t =>
    switch (x) {
    | DoesNotExpand => DoesNotExpand
    | Expands(dp, ty, ctx, delta) => f((dp, ty, ctx, delta))
    };
};

module Let_syntax = ExpandResult;

let rec syn_expand =
        (ctx: Contexts.t, delta: Delta.t, p: UHPat.t): ExpandResult.t =>
  syn_expand_opseq(ctx, delta, p)
and syn_expand_opseq =
    (ctx: Contexts.t, delta: Delta.t, OpSeq(skel, seq): UHPat.opseq)
    : ExpandResult.t =>
  syn_expand_skel(ctx, delta, skel, seq)
and syn_expand_skel =
    (ctx: Contexts.t, delta: Delta.t, skel: UHPat.skel, seq: UHPat.seq)
    : ExpandResult.t =>
  switch (skel) {
  | Placeholder(n) =>
    syn_expand_operand(ctx, delta, seq |> Seq.nth_operand(n))
  | BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2)
  | BinOp(InHole(WrongLength as reason, u), Comma as op, skel1, skel2) =>
    let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
    switch (syn_expand_skel(ctx, delta, skel_not_in_hole, seq)) {
    | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
    | Expands(dp, _, ctx, delta) =>
      let gamma = Contexts.gamma(ctx);
      let delta =
        MetaVarMap.extend_unique(delta, (u, (PatternHole, Hole, gamma)));
      Expands(NonEmptyHole(reason, u, 0, dp), Hole, ctx, delta);
    };
  | BinOp(InHole(WrongLength, _), _, _, _) => ExpandResult.DoesNotExpand
  | BinOp(NotInHole, Comma, _, _) =>
    switch (UHPat.get_tuple_elements(skel)) {
    | [skel1, skel2, ...tail] =>
      let%bind (dp1, ty1, ctx, delta) =
        syn_expand_skel(ctx, delta, skel1, seq);
      let%bind (dp2, ty2, ctx, delta) =
        syn_expand_skel(ctx, delta, skel2, seq);
      tail
      |> ListUtil.map_with_accumulator_opt(
           ((dp_acc, ctx, delta), skel) => {
             syn_expand_skel(ctx, delta, skel, seq)
             |> ExpandResult.to_option
             |> Option.map(((dp, ty, ctx, delta)) =>
                  ((DHPat.Pair(dp_acc, dp), ctx, delta), ty)
                )
           },
           (DHPat.Pair(dp1, dp2), ctx, delta),
         )
      |> Option.map((((dp_acc, ctx, delta), tys)) =>
           (dp_acc, HTyp.Prod([ty1, ty2, ...tys]), ctx, delta)
         )
      |> ExpandResult.from_option;
    | _ =>
      raise(
        Invalid_argument(
          "Encountered tuple pattern type with less than 2 elements!",
        ),
      )
    }
  | BinOp(NotInHole, Space, skel1, skel2) =>
    switch (syn_expand_skel(ctx, delta, skel1, seq)) {
    | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
    | Expands(dp1, _, ctx, delta) =>
      switch (syn_expand_skel(ctx, delta, skel2, seq)) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(dp2, _, ctx, delta) =>
        let dp = DHPat.Ap(dp1, dp2);
        Expands(dp, Hole, ctx, delta);
      }
    }
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    switch (syn_expand_skel(ctx, delta, skel1, seq)) {
    | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
    | Expands(dp1, ty1, ctx, delta) =>
      let ty = HTyp.List(ty1);
      switch (ana_expand_skel(ctx, delta, skel2, seq, ty)) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(dp2, _, ctx, delta) =>
        let dp = DHPat.Cons(dp1, dp2);
        Expands(dp, ty, ctx, delta);
      };
    }
  }
and syn_expand_operand =
    (ctx: Contexts.t, delta: Delta.t, operand: UHPat.operand): ExpandResult.t =>
  switch (operand) {
  | Wild(InHole(TypeInconsistent as reason, u))
  | Var(InHole(TypeInconsistent as reason, u), _, _)
  | IntLit(InHole(TypeInconsistent as reason, u), _)
  | FloatLit(InHole(TypeInconsistent as reason, u), _)
  | BoolLit(InHole(TypeInconsistent as reason, u), _)
  | ListNil(InHole(TypeInconsistent as reason, u))
  | Inj(InHole(TypeInconsistent as reason, u), _, _) =>
    let operand' = operand |> UHPat.set_err_status_operand(NotInHole);
    switch (syn_expand_operand(ctx, delta, operand')) {
    | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
    | Expands(dp, _, ctx, delta) =>
      let gamma = Contexts.gamma(ctx);
      let delta =
        MetaVarMap.extend_unique(delta, (u, (PatternHole, Hole, gamma)));
      Expands(NonEmptyHole(reason, u, 0, dp), Hole, ctx, delta);
    };
  | Wild(InHole(WrongLength, _))
  | Var(InHole(WrongLength, _), _, _)
  | IntLit(InHole(WrongLength, _), _)
  | FloatLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _))
  | Inj(InHole(WrongLength, _), _, _) => ExpandResult.DoesNotExpand
  | EmptyHole(u) =>
    let gamma = Contexts.gamma(ctx);
    let dp = DHPat.EmptyHole(u, 0);
    let ty = HTyp.Hole;
    let delta =
      MetaVarMap.extend_unique(delta, (u, (PatternHole, ty, gamma)));
    Expands(dp, ty, ctx, delta);
  | Wild(NotInHole) => Expands(Wild, Hole, ctx, delta)
  | Var(NotInHole, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
  | Var(NotInHole, InVarHole(Keyword(k), u), _) =>
    Expands(Keyword(u, 0, k), Hole, ctx, delta)
  | Var(NotInHole, NotInVarHole, x) =>
    let ctx = Contexts.extend_gamma(ctx, (x, Hole));
    Expands(Var(x), Hole, ctx, delta);
  | IntLit(NotInHole, n) =>
    switch (int_of_string_opt(n)) {
    | Some(n) => Expands(IntLit(n), Int, ctx, delta)
    | None => DoesNotExpand
    }
  | FloatLit(NotInHole, f) =>
    switch (TextShape.hazel_float_of_string_opt(f)) {
    | Some(f) => Expands(FloatLit(f), Float, ctx, delta)
    | None => DoesNotExpand
    }
  | BoolLit(NotInHole, b) => Expands(BoolLit(b), Bool, ctx, delta)
  | ListNil(NotInHole) => Expands(ListNil, List(Hole), ctx, delta)
  | Parenthesized(p1) => syn_expand(ctx, delta, p1)
  | Inj(NotInHole, side, p) =>
    switch (syn_expand(ctx, delta, p)) {
    | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
    | Expands(dp1, ty1, ctx, delta) =>
      let dp = DHPat.Inj(side, dp1);
      let ty =
        switch (side) {
        | L => HTyp.Sum(ty1, Hole)
        | R => HTyp.Sum(Hole, ty1)
        };
      Expands(dp, ty, ctx, delta);
    }
  }
and ana_expand =
    (ctx: Contexts.t, delta: Delta.t, p: UHPat.t, ty: HTyp.t): ExpandResult.t =>
  ana_expand_opseq(ctx, delta, p, ty)
and ana_expand_opseq =
    (
      ctx: Contexts.t,
      delta: Delta.t,
      OpSeq(skel, seq) as opseq: UHPat.opseq,
      ty: HTyp.t,
    )
    : ExpandResult.t => {
  // handle n-tuples
  switch (Statics_Pat.tuple_zip(skel, ty)) {
  | Some(skel_tys) =>
    skel_tys
    |> List.fold_left(
         (
           acc: option((list(DHPat.t), Contexts.t, Delta.t)),
           (skel: UHPat.skel, ty: HTyp.t),
         ) =>
           switch (acc) {
           | None => None
           | Some((rev_dps, ctx, delta)) =>
             switch (ana_expand_skel(ctx, delta, skel, seq, ty)) {
             | ExpandResult.DoesNotExpand => None
             | Expands(dp, _, ctx, delta) =>
               Some(([dp, ...rev_dps], ctx, delta))
             }
           },
         Some(([], ctx, delta)),
       )
    |> (
      fun
      | None => ExpandResult.DoesNotExpand
      | Some((rev_dps, ctx, delta)) => {
          let dp = rev_dps |> List.rev |> DHPat.mk_tuple;
          Expands(dp, ty, ctx, delta);
        }
    )
  | None =>
    if (List.length(HTyp.get_prod_elements(ty)) == 1) {
      skel
      |> UHPat.get_tuple_elements
      |> List.fold_left(
           (
             acc: option((list(DHPat.t), Contexts.t, Delta.t)),
             skel: UHPat.skel,
           ) =>
             switch (acc) {
             | None => None
             | Some((rev_dps, ctx, delta)) =>
               switch (syn_expand_skel(ctx, delta, skel, seq)) {
               | DoesNotExpand => None
               | Expands(dp, _, ctx, delta) =>
                 Some(([dp, ...rev_dps], ctx, delta))
               }
             },
           Some(([], ctx, delta)),
         )
      |> (
        fun
        | None => ExpandResult.DoesNotExpand
        | Some((rev_dps, ctx, delta)) => {
            let dp = DHPat.mk_tuple(List.rev(rev_dps));
            Expands(dp, ty, ctx, delta);
          }
      );
    } else {
      switch (opseq |> UHPat.get_err_status_opseq) {
      | NotInHole
      | InHole(TypeInconsistent, _) => ExpandResult.DoesNotExpand
      | InHole(WrongLength as reason, u) =>
        switch (
          syn_expand_opseq(
            ctx,
            delta,
            opseq |> UHPat.set_err_status_opseq(NotInHole),
          )
        ) {
        | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
        | Expands(dp, _, _, delta) =>
          let gamma = ctx |> Contexts.gamma;
          let delta =
            MetaVarMap.extend_unique(delta, (u, (PatternHole, ty, gamma)));
          Expands(NonEmptyHole(reason, u, 0, dp), ty, ctx, delta);
        }
      };
    }
  };
}
and ana_expand_skel =
    (
      ctx: Contexts.t,
      delta: Delta.t,
      skel: UHPat.skel,
      seq: UHPat.seq,
      ty: HTyp.t,
    )
    : ExpandResult.t =>
  switch (skel) {
  | BinOp(_, Comma, _, _)
  | BinOp(InHole(WrongLength, _), _, _, _) =>
    // tuples handled at opseq level
    ExpandResult.DoesNotExpand
  | Placeholder(n) =>
    let pn = seq |> Seq.nth_operand(n);
    ana_expand_operand(ctx, delta, pn, ty);
  | BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2) =>
    let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
    switch (syn_expand_skel(ctx, delta, skel_not_in_hole, seq)) {
    | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
    | Expands(dp1, _, ctx, delta) =>
      let dp = DHPat.NonEmptyHole(reason, u, 0, dp1);
      let gamma = Contexts.gamma(ctx);
      let delta =
        MetaVarMap.extend_unique(delta, (u, (PatternHole, ty, gamma)));
      Expands(dp, ty, ctx, delta);
    };
  | BinOp(NotInHole, Space, skel1, skel2) =>
    switch (ana_expand_skel(ctx, delta, skel1, seq, Hole)) {
    | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
    | Expands(dp1, _ty1, ctx, delta) =>
      switch (ana_expand_skel(ctx, delta, skel2, seq, Hole)) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(dp2, _ty2, ctx, delta) =>
        let dp = DHPat.Ap(dp1, dp2);
        Expands(dp, Hole, ctx, delta);
      }
    }
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    switch (HTyp.matched_list(ty)) {
    | None => ExpandResult.DoesNotExpand
    | Some(ty_elt) =>
      switch (ana_expand_skel(ctx, delta, skel1, seq, ty_elt)) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(dp1, _, ctx, delta) =>
        let ty_list = HTyp.List(ty_elt);
        switch (ana_expand_skel(ctx, delta, skel2, seq, ty_list)) {
        | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
        | Expands(dp2, _, ctx, delta) =>
          let dp = DHPat.Cons(dp1, dp2);
          Expands(dp, ty, ctx, delta);
        };
      }
    }
  }
and ana_expand_operand =
    (ctx: Contexts.t, delta: Delta.t, operand: UHPat.operand, ty: HTyp.t)
    : ExpandResult.t =>
  switch (operand) {
  | Wild(InHole(TypeInconsistent as reason, u))
  | Var(InHole(TypeInconsistent as reason, u), _, _)
  | IntLit(InHole(TypeInconsistent as reason, u), _)
  | FloatLit(InHole(TypeInconsistent as reason, u), _)
  | BoolLit(InHole(TypeInconsistent as reason, u), _)
  | ListNil(InHole(TypeInconsistent as reason, u))
  | Inj(InHole(TypeInconsistent as reason, u), _, _) =>
    let operand' = operand |> UHPat.set_err_status_operand(NotInHole);
    switch (syn_expand_operand(ctx, delta, operand')) {
    | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
    | Expands(dp1, _, ctx, delta) =>
      let dp = DHPat.NonEmptyHole(reason, u, 0, dp1);
      let gamma = Contexts.gamma(ctx);
      let delta =
        MetaVarMap.extend_unique(delta, (u, (PatternHole, ty, gamma)));
      Expands(dp, ty, ctx, delta);
    };
  | Wild(InHole(WrongLength, _))
  | Var(InHole(WrongLength, _), _, _)
  | IntLit(InHole(WrongLength, _), _)
  | FloatLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _))
  | Inj(InHole(WrongLength, _), _, _) => ExpandResult.DoesNotExpand
  | EmptyHole(u) =>
    let gamma = Contexts.gamma(ctx);
    let dp = DHPat.EmptyHole(u, 0);
    let delta =
      MetaVarMap.extend_unique(delta, (u, (PatternHole, ty, gamma)));
    Expands(dp, ty, ctx, delta);
  | Var(NotInHole, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
  | Var(NotInHole, InVarHole(Keyword(k), u), _) =>
    Expands(Keyword(u, 0, k), ty, ctx, delta)
  | Var(NotInHole, NotInVarHole, x) =>
    let ctx = Contexts.extend_gamma(ctx, (x, ty));
    Expands(Var(x), ty, ctx, delta);
  | Wild(NotInHole) => Expands(Wild, ty, ctx, delta)
  | IntLit(NotInHole, _)
  | FloatLit(NotInHole, _)
  | BoolLit(NotInHole, _) => syn_expand_operand(ctx, delta, operand)
  | ListNil(NotInHole) =>
    switch (HTyp.matched_list(ty)) {
    | None => ExpandResult.DoesNotExpand
    | Some(ty_elt) => Expands(ListNil, HTyp.List(ty_elt), ctx, delta)
    }
  | Parenthesized(p) => ana_expand(ctx, delta, p, ty)
  | Inj(NotInHole, side, p1) =>
    switch (HTyp.matched_sum(ty)) {
    | None => ExpandResult.DoesNotExpand
    | Some((tyL, tyR)) =>
      let ty1 = InjSide.pick(side, tyL, tyR);
      switch (ana_expand(ctx, delta, p1, ty1)) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(dp1, ty1, ctx, delta) =>
        let ty =
          switch (side) {
          | L => HTyp.Sum(ty1, tyR)
          | R => HTyp.Sum(tyL, ty1)
          };
        Expands(Inj(side, dp1), ty, ctx, delta);
      };
    }
  };

let rec renumber_result_only =
        (path: InstancePath.t, hii: HoleInstanceInfo.t, dp: DHPat.t)
        : (DHPat.t, HoleInstanceInfo.t) =>
  switch (dp) {
  | Wild
  | Var(_)
  | IntLit(_)
  | FloatLit(_)
  | BoolLit(_)
  | ListNil
  | Triv => (dp, hii)
  | EmptyHole(u, _) =>
    let sigma = Environment.empty;
    let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
    (EmptyHole(u, i), hii);
  | NonEmptyHole(reason, u, _, dp1) =>
    /* TODO: see above */
    let sigma = Environment.empty;
    let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
    let (dp1, hii) = renumber_result_only(path, hii, dp1);
    (NonEmptyHole(reason, u, i, dp1), hii);
  | Keyword(u, _, k) =>
    /* TODO: see above */
    let sigma = Environment.empty;
    let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
    (Keyword(u, i, k), hii);
  | Inj(side, dp1) =>
    let (dp1, hii) = renumber_result_only(path, hii, dp1);
    (Inj(side, dp1), hii);
  | Pair(dp1, dp2) =>
    let (dp1, hii) = renumber_result_only(path, hii, dp1);
    let (dp2, hii) = renumber_result_only(path, hii, dp2);
    (Pair(dp1, dp2), hii);
  | Cons(dp1, dp2) =>
    let (dp1, hii) = renumber_result_only(path, hii, dp1);
    let (dp2, hii) = renumber_result_only(path, hii, dp2);
    (Cons(dp1, dp2), hii);
  | Ap(dp1, dp2) =>
    let (dp1, hii) = renumber_result_only(path, hii, dp1);
    let (dp2, hii) = renumber_result_only(path, hii, dp2);
    (Pair(dp1, dp2), hii);
  };
