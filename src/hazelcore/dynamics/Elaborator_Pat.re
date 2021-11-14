module ElaborationResult = {
  type t =
    | Elaborates(DHPat.t, HTyp.t, Contexts.t, Delta.t)
    | DoesNotElaborate;

  let to_option =
    fun
    | DoesNotElaborate => None
    | Elaborates(pat, ty, ctx, delta) => Some((pat, ty, ctx, delta));

  let from_option =
    fun
    | None => DoesNotElaborate
    | Some((pat, ty, ctx, delta)) => Elaborates(pat, ty, ctx, delta);

  let bind = (x: t, ~f: ((DHPat.t, HTyp.t, Contexts.t, Delta.t)) => t): t =>
    switch (x) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(dp, ty, ctx, delta) => f((dp, ty, ctx, delta))
    };
};

module Let_syntax = ElaborationResult;

let rec syn_elab =
        (ctx: Contexts.t, delta: Delta.t, p: UHPat.t): ElaborationResult.t =>
  syn_elab_opseq(ctx, delta, p)
and syn_elab_opseq =
    (ctx: Contexts.t, delta: Delta.t, OpSeq(skel, seq): UHPat.opseq)
    : ElaborationResult.t =>
  syn_elab_skel(ctx, delta, skel, seq)
and syn_elab_skel =
    (ctx: Contexts.t, delta: Delta.t, skel: UHPat.skel, seq: UHPat.seq)
    : ElaborationResult.t =>
  switch (skel) {
  | Placeholder(n) => syn_elab_operand(ctx, delta, seq |> Seq.nth_operand(n))
  | BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2)
  | BinOp(InHole(WrongLength as reason, u), Comma as op, skel1, skel2) =>
    let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
    switch (syn_elab_skel(ctx, delta, skel_not_in_hole, seq)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(dp, _, ctx, delta) =>
      let gamma = Contexts.gamma(ctx);
      let delta =
        MetaVarMap.add(u, (Delta.PatternHole, HTyp.Hole, gamma), delta);
      Elaborates(NonEmptyHole(reason, u, 0, dp), Hole, ctx, delta);
    };
  | BinOp(InHole(WrongLength, _), _, _, _) => DoesNotElaborate
  | BinOp(NotInHole, Comma, _, _) =>
    switch (UHPat.get_tuple_elements(skel)) {
    | [skel1, skel2, ...tail] =>
      let%bind (dp1, ty1, ctx, delta) =
        syn_elab_skel(ctx, delta, skel1, seq);
      let%bind (dp2, ty2, ctx, delta) =
        syn_elab_skel(ctx, delta, skel2, seq);
      tail
      |> ListUtil.map_with_accumulator_opt(
           ((dp_acc, ctx, delta), skel) => {
             syn_elab_skel(ctx, delta, skel, seq)
             |> ElaborationResult.to_option
             |> Option.map(((dp, ty, ctx, delta)) =>
                  ((DHPat.Pair(dp_acc, dp), ctx, delta), ty)
                )
           },
           (DHPat.Pair(dp1, dp2), ctx, delta),
         )
      |> Option.map((((dp_acc, ctx, delta), tys)) =>
           (dp_acc, HTyp.Prod([ty1, ty2, ...tys]), ctx, delta)
         )
      |> ElaborationResult.from_option;
    | _ =>
      raise(
        Invalid_argument(
          "Encountered tuple pattern type with less than 2 elements!",
        ),
      )
    }
  | BinOp(NotInHole, Space, skel1, skel2) =>
    switch (syn_elab_skel(ctx, delta, skel1, seq)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(dp1, _, ctx, delta) =>
      switch (syn_elab_skel(ctx, delta, skel2, seq)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(dp2, _, ctx, delta) =>
        let dp = DHPat.Ap(dp1, dp2);
        Elaborates(dp, Hole, ctx, delta);
      }
    }
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    switch (syn_elab_skel(ctx, delta, skel1, seq)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(dp1, ty1, ctx, delta) =>
      let ty = HTyp.List(ty1);
      switch (ana_elab_skel(ctx, delta, skel2, seq, ty)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(dp2, _, ctx, delta) =>
        let dp = DHPat.Cons(dp1, dp2);
        Elaborates(dp, ty, ctx, delta);
      };
    }
  }
and syn_elab_operand =
    (ctx: Contexts.t, delta: Delta.t, operand: UHPat.operand)
    : ElaborationResult.t => {
  Sexplib.Sexp.(
    {
      print_endline("PAT SYN_ELAB_OPERAND");
      print_endline(to_string_hum(UHPat.sexp_of_operand(operand)));
    }
  );
  switch (operand) {
  | Wild(InHole(TypeInconsistent as reason, u))
  | Var(InHole(TypeInconsistent as reason, u), _, _)
  | IntLit(InHole(TypeInconsistent as reason, u), _)
  | FloatLit(InHole(TypeInconsistent as reason, u), _)
  | BoolLit(InHole(TypeInconsistent as reason, u), _)
  | ListNil(InHole(TypeInconsistent as reason, u)) =>
    let operand' = operand |> UHPat.set_err_status_operand(NotInHole);
    switch (syn_elab_operand(ctx, delta, operand')) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(dp, _, ctx, delta) =>
      let gamma = Contexts.gamma(ctx);
      let delta =
        MetaVarMap.add(u, (Delta.PatternHole, HTyp.Hole, gamma), delta);
      Elaborates(NonEmptyHole(reason, u, 0, dp), Hole, ctx, delta);
    };

  // adapted from ESInjErr
  | Inj(InHole(InjectionInSyntheticPosition as reason, u), tag, arg_opt) =>
    let gamma = Contexts.gamma(ctx);
    let ty_opt = arg_opt |> Option.map(_ => HTyp.Hole);
    switch (ana_elab_inj_body(ctx, delta, arg_opt, ty_opt)) {
    | Some(DoesNotElaborate) => DoesNotElaborate
    | Some(Elaborates(dp, _, ctx, delta')) =>
      let inj = (tag, Some(dp));
      let delta'' =
        MetaVarMap.add(u, (Delta.PatternHole, HTyp.Hole, gamma), delta');
      Elaborates(InjError(reason, u, 0, inj), Hole, ctx, delta'');
    | None =>
      let delta' =
        MetaVarMap.add(u, (Delta.PatternHole, HTyp.Hole, gamma), delta);
      Elaborates(InjError(reason, u, 0, (tag, None)), Hole, ctx, delta');
    };
  | Inj(InHole(_), _, _) => DoesNotElaborate

  | Wild(InHole(WrongLength, _))
  | Var(InHole(WrongLength, _), _, _)
  | IntLit(InHole(WrongLength, _), _)
  | FloatLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _)) => DoesNotElaborate
  | EmptyHole(u) =>
    let gamma = Contexts.gamma(ctx);
    let dp = DHPat.EmptyHole(u, 0);
    let ty = HTyp.Hole;
    let delta = MetaVarMap.add(u, (Delta.PatternHole, ty, gamma), delta);
    Elaborates(dp, ty, ctx, delta);
  | InvalidText(u, t) =>
    let gamma = Contexts.gamma(ctx);
    let dp = DHPat.InvalidText(u, 0, t);
    let ty = HTyp.Hole;
    let delta = MetaVarMap.add(u, (Delta.PatternHole, ty, gamma), delta);
    Elaborates(dp, ty, ctx, delta);
  | Wild(NotInHole) => Elaborates(Wild, Hole, ctx, delta)
  | Var(NotInHole, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
  | Var(NotInHole, InVarHole(Keyword(k), u), _) =>
    Elaborates(Keyword(u, 0, k), Hole, ctx, delta)
  | Var(NotInHole, NotInVarHole, x) =>
    let ctx = Contexts.extend_gamma(ctx, (x, Hole));
    Elaborates(Var(x), Hole, ctx, delta);
  | IntLit(NotInHole, n) =>
    switch (int_of_string_opt(n)) {
    | Some(n) => Elaborates(IntLit(n), Int, ctx, delta)
    | None => DoesNotElaborate
    }
  | FloatLit(NotInHole, f) =>
    switch (TextShape.hazel_float_of_string_opt(f)) {
    | Some(f) => Elaborates(FloatLit(f), Float, ctx, delta)
    | None => DoesNotElaborate
    }
  | BoolLit(NotInHole, b) => Elaborates(BoolLit(b), Bool, ctx, delta)
  | ListNil(NotInHole) => Elaborates(ListNil, List(Hole), ctx, delta)
  | Parenthesized(p1) => syn_elab(ctx, delta, p1)
  | Inj(NotInHole, _, _) => DoesNotElaborate
  | TypeAnn(_, op, _) => syn_elab_operand(ctx, delta, op)
  };
}
and ana_elab =
    (ctx: Contexts.t, delta: Delta.t, p: UHPat.t, ty: HTyp.t)
    : ElaborationResult.t =>
  ana_elab_opseq(ctx, delta, p, ty)
and ana_elab_opseq =
    (
      ctx: Contexts.t,
      delta: Delta.t,
      OpSeq(skel, seq) as opseq: UHPat.opseq,
      ty: HTyp.t,
    )
    : ElaborationResult.t => {
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
             switch (ana_elab_skel(ctx, delta, skel, seq, ty)) {
             | DoesNotElaborate => None
             | Elaborates(dp, _, ctx, delta) =>
               Some(([dp, ...rev_dps], ctx, delta))
             }
           },
         Some(([], ctx, delta)),
       )
    |> (
      fun
      | None => ElaborationResult.DoesNotElaborate
      | Some((rev_dps, ctx, delta)) => {
          let dp = rev_dps |> List.rev |> DHPat.mk_tuple;
          Elaborates(dp, ty, ctx, delta);
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
               switch (syn_elab_skel(ctx, delta, skel, seq)) {
               | DoesNotElaborate => None
               | Elaborates(dp, _, ctx, delta) =>
                 Some(([dp, ...rev_dps], ctx, delta))
               }
             },
           Some(([], ctx, delta)),
         )
      |> (
        fun
        | None => ElaborationResult.DoesNotElaborate
        | Some((rev_dps, ctx, delta)) => {
            let dp = DHPat.mk_tuple(List.rev(rev_dps));
            Elaborates(dp, ty, ctx, delta);
          }
      );
    } else {
      switch (opseq |> UHPat.get_err_status_opseq) {
      | NotInHole
      | InHole(TypeInconsistent, _) => DoesNotElaborate
      | InHole(WrongLength as reason, u) =>
        switch (
          syn_elab_opseq(
            ctx,
            delta,
            opseq |> UHPat.set_err_status_opseq(NotInHole),
          )
        ) {
        | DoesNotElaborate => DoesNotElaborate
        | Elaborates(dp, _, _, delta) =>
          let gamma = ctx |> Contexts.gamma;
          let delta =
            MetaVarMap.add(u, (Delta.PatternHole, ty, gamma), delta);
          Elaborates(NonEmptyHole(reason, u, 0, dp), ty, ctx, delta);
        }
      };
    }
  };
}
and ana_elab_skel =
    (
      ctx: Contexts.t,
      delta: Delta.t,
      skel: UHPat.skel,
      seq: UHPat.seq,
      ty: HTyp.t,
    )
    : ElaborationResult.t =>
  switch (skel) {
  | BinOp(_, Comma, _, _)
  | BinOp(InHole(WrongLength, _), _, _, _) =>
    // tuples handled at opseq level
    DoesNotElaborate
  | Placeholder(n) =>
    let pn = seq |> Seq.nth_operand(n);
    ana_elab_operand(ctx, delta, pn, ty);
  | BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2) =>
    let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
    switch (syn_elab_skel(ctx, delta, skel_not_in_hole, seq)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(dp1, _, ctx, delta) =>
      let dp = DHPat.NonEmptyHole(reason, u, 0, dp1);
      let gamma = Contexts.gamma(ctx);
      let delta = MetaVarMap.add(u, (Delta.PatternHole, ty, gamma), delta);
      Elaborates(dp, ty, ctx, delta);
    };
  | BinOp(NotInHole, Space, skel1, skel2) =>
    switch (ana_elab_skel(ctx, delta, skel1, seq, Hole)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(dp1, _ty1, ctx, delta) =>
      switch (ana_elab_skel(ctx, delta, skel2, seq, Hole)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(dp2, _ty2, ctx, delta) =>
        let dp = DHPat.Ap(dp1, dp2);
        Elaborates(dp, Hole, ctx, delta);
      }
    }
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    switch (HTyp.matched_list(ty)) {
    | None => DoesNotElaborate
    | Some(ty_elt) =>
      switch (ana_elab_skel(ctx, delta, skel1, seq, ty_elt)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(dp1, _, ctx, delta) =>
        let ty_list = HTyp.List(ty_elt);
        switch (ana_elab_skel(ctx, delta, skel2, seq, ty_list)) {
        | DoesNotElaborate => DoesNotElaborate
        | Elaborates(dp2, _, ctx, delta) =>
          let dp = DHPat.Cons(dp1, dp2);
          Elaborates(dp, ty, ctx, delta);
        };
      }
    }
  }
and ana_elab_operand =
    (ctx: Contexts.t, delta: Delta.t, operand: UHPat.operand, ty: HTyp.t)
    : ElaborationResult.t => {
  Sexplib.Sexp.(
    {
      print_endline("PAT ANA_ELAB_OPERAND");
      print_endline(to_string_hum(UHPat.sexp_of_operand(operand)));
      print_endline(to_string_hum(HTyp.sexp_of_t(ty)));
    }
  );
  switch (operand) {
  | Wild(InHole(TypeInconsistent as reason, u))
  | Var(InHole(TypeInconsistent as reason, u), _, _)
  | IntLit(InHole(TypeInconsistent as reason, u), _)
  | FloatLit(InHole(TypeInconsistent as reason, u), _)
  | BoolLit(InHole(TypeInconsistent as reason, u), _)
  | ListNil(InHole(TypeInconsistent as reason, u))
  | TypeAnn(InHole(TypeInconsistent as reason, u), _, _) =>
    let operand' = operand |> UHPat.set_err_status_operand(NotInHole);
    switch (syn_elab_operand(ctx, delta, operand')) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(dp1, _, ctx, delta) =>
      let dp = DHPat.NonEmptyHole(reason, u, 0, dp1);
      let gamma = Contexts.gamma(ctx);
      let delta = MetaVarMap.add(u, (Delta.PatternHole, ty, gamma), delta);
      Elaborates(dp, ty, ctx, delta);
    };

  | Inj(InHole(InjectionInSyntheticPosition, _), _, _) => DoesNotElaborate

  // adapted from ESInjErr
  | Inj(InHole(ExpectedTypeNotConsistentWithSums as reason, u), tag, arg_opt) =>
    let gamma = Contexts.gamma(ctx);
    let ty_opt = arg_opt |> Option.map(_ => HTyp.Hole);
    switch (ana_elab_inj_body(ctx, delta, arg_opt, ty_opt)) {
    | Some(DoesNotElaborate) => DoesNotElaborate
    | Some(Elaborates(dp, _, ctx', delta')) =>
      let inj = (tag, Some(dp));
      let delta'' =
        MetaVarMap.add(u, (Delta.PatternHole, HTyp.Hole, gamma), delta');
      Elaborates(InjError(reason, u, 0, inj), Hole, ctx', delta'');
    | None =>
      let inj = (tag, None);
      let delta' =
        MetaVarMap.add(u, (Delta.PatternHole, HTyp.Hole, gamma), delta);
      Elaborates(InjError(reason, u, 0, inj), Hole, ctx, delta');
    };

  | Inj(InHole(UnexpectedArg as reason, u), tag, Some(arg)) =>
    switch (ty) {
    | Sum(tymap) =>
      switch (TagMap.find_opt(tag, tymap)) {
      | None
      | Some(Some(_)) => DoesNotElaborate
      | Some(None) =>
        switch (ana_elab(ctx, delta, arg, Hole)) {
        | DoesNotElaborate => DoesNotElaborate
        | Elaborates(dp, _, ctx', delta') =>
          let gamma = Contexts.gamma(ctx');
          let inj = (tag, Some(dp));
          let delta'' =
            MetaVarMap.add(u, (Delta.PatternHole, ty, gamma), delta');
          Elaborates(InjError(reason, u, 0, inj), ty, ctx', delta'');
        }
      }
    | _ => DoesNotElaborate
    }
  | Inj(InHole(UnexpectedArg, _), _, None) => DoesNotElaborate

  | Inj(InHole(ExpectedArg as reason, u), tag, None) =>
    switch (ty) {
    | Sum(tymap) =>
      switch (TagMap.find_opt(tag, tymap)) {
      | None
      | Some(None) => DoesNotElaborate
      | Some(Some(_)) =>
        let gamma = Contexts.gamma(ctx);
        let inj = (tag, None);
        let delta' =
          MetaVarMap.add(u, (Delta.PatternHole, ty, gamma), delta);
        Elaborates(InjError(reason, u, 0, inj), ty, ctx, delta');
      }
    | _ => DoesNotElaborate
    }
  | Inj(InHole(ExpectedArg, _), _, Some(_)) => DoesNotElaborate

  | Wild(InHole(WrongLength, _))
  | Var(InHole(WrongLength, _), _, _)
  | IntLit(InHole(WrongLength, _), _)
  | FloatLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _))
  | TypeAnn(InHole(WrongLength, _), _, _) => DoesNotElaborate
  | EmptyHole(u) =>
    let gamma = Contexts.gamma(ctx);
    let dp = DHPat.EmptyHole(u, 0);
    let delta = MetaVarMap.add(u, (Delta.PatternHole, ty, gamma), delta);
    Elaborates(dp, ty, ctx, delta);
  | Var(NotInHole, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
  | Var(NotInHole, InVarHole(Keyword(k), u), _) =>
    Elaborates(Keyword(u, 0, k), ty, ctx, delta)
  | Var(NotInHole, NotInVarHole, x) =>
    let ctx = Contexts.extend_gamma(ctx, (x, ty));
    Elaborates(Var(x), ty, ctx, delta);
  | Wild(NotInHole) => Elaborates(Wild, ty, ctx, delta)
  | InvalidText(_, _)
  | IntLit(NotInHole, _)
  | FloatLit(NotInHole, _)
  | BoolLit(NotInHole, _) => syn_elab_operand(ctx, delta, operand)
  | ListNil(NotInHole) =>
    switch (HTyp.matched_list(ty)) {
    | None => DoesNotElaborate
    | Some(ty_elt) => Elaborates(ListNil, HTyp.List(ty_elt), ctx, delta)
    }
  | Parenthesized(p) => ana_elab(ctx, delta, p, ty)

  | Inj(NotInHole, tag, arg_opt) =>
    switch (ty) {
    | Hole =>
      let ty_opt = arg_opt |> Option.map(_ => HTyp.Hole);
      switch (ana_elab_inj_body(ctx, delta, arg_opt, ty_opt)) {
      | Some(DoesNotElaborate) => DoesNotElaborate
      | Some(Elaborates(dp, dp_ty, ctx', delta')) =>
        let tymap = TagMap.singleton(tag, Some(dp_ty));
        Elaborates(Inj((tag, Some(dp))), HTyp.Sum(tymap), ctx', delta');
      | None =>
        let tymap = TagMap.singleton(tag, None);
        Elaborates(Inj((tag, None)), HTyp.Sum(tymap), ctx, delta);
      };
    | Sum(tymap) =>
      switch (TagMap.find_opt(tag, tymap)) {
      | Some(ty1_opt) =>
        switch (ty1_opt) {
        | Some(_) =>
          switch (ana_elab_inj_body(ctx, delta, arg_opt, ty1_opt)) {
          | None
          | Some(DoesNotElaborate) => DoesNotElaborate
          | Some(Elaborates(dp, _, ctx', delta')) =>
            Elaborates(Inj((tag, Some(dp))), ty, ctx', delta')
          }
        | None =>
          switch (ana_elab_inj_body(ctx, delta, arg_opt, ty1_opt)) {
          | Some(_) => DoesNotElaborate
          | None => Elaborates(Inj((tag, None)), ty, ctx, delta)
          }
        }
      | None =>
        let ty_opt = arg_opt |> Option.map(_ => HTyp.Hole);
        switch (ana_elab_inj_body(ctx, delta, arg_opt, ty_opt)) {
        | Some(DoesNotElaborate) => DoesNotElaborate
        | Some(Elaborates(dp, _, ctx', delta')) =>
          Elaborates(Inj((tag, Some(dp))), ty, ctx', delta')
        | None => Elaborates(Inj((tag, None)), ty, ctx, delta)
        };
      }
    | _ => DoesNotElaborate
    }

  | TypeAnn(NotInHole, op, _) => ana_elab_operand(ctx, delta, op, ty)
  };
}
and ana_elab_inj_body =
    (
      ctx: Contexts.t,
      delta: Delta.t,
      p_opt: option(UHPat.t),
      ty_opt: option(HTyp.t),
    )
    : option(ElaborationResult.t) =>
  switch (p_opt, ty_opt) {
  | (None, None) => None
  | (Some(p), Some(ty)) => Some(ana_elab(ctx, delta, p, ty))
  | (_, _) => Some(DoesNotElaborate)
  };

let rec renumber_result_only =
        (path: InstancePath.t, hii: HoleInstanceInfo.t, dp: DHPat.t)
        : (DHPat.t, HoleInstanceInfo.t) =>
  switch (dp) {
  | Wild
  | Var(_)
  | IntLit(_)
  | FloatLit(_)
  | InvalidText(_)
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
  | Inj((tag, Some(p))) =>
    let (dp, hii) = renumber_result_only(path, hii, p);
    (Inj((tag, Some(dp))), hii);
  | Inj((_, None)) => (dp, hii)
  | InjError(reason, u, _, (tag, Some(p))) =>
    let sigma = Environment.empty;
    let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
    let (dp, hii) = renumber_result_only(path, hii, p);
    (InjError(reason, u, i, (tag, Some(dp))), hii);
  | InjError(reason, u, _, (_, None) as inj) =>
    let sigma = Environment.empty;
    let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
    (InjError(reason, u, i, inj), hii);
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
