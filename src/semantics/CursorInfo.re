open SemanticsCommon;

type cursor_mode =
  /* 
   *  # cursor in analytic position 
   */

  | AnaAnnotatedLambda(HTyp.t, HTyp.t)
  /* cursor is on a lambda with an argument type annotation */

  | AnaTypeInconsistent(HTyp.t, HTyp.t)
  /* cursor is on a type inconsistent expression */

  | AnaWrongLength(
      nat /* expected length */,
      nat, /* got length */
      HTyp.t,
    ) /* expected type */
  /* cursor is on a tuple of the wrong length */

  | AnaFree(HTyp.t)
  /* cursor is on a free variable */

  | Analyzed(HTyp.t)
  /* none of the above and didn't go through subsumption */

  | AnaSubsumed(HTyp.t, HTyp.t)
  /* none of the above and went through subsumption */

  /* 
   *  # cursor in synthetic position 
   */

  | SynErrorArrow(HTyp.t /* expected */, HTyp.t) /* got */
  /* cursor is on the function position of an ap,
     and that expression does not synthesize a type
     with a matched arrow type */

  | SynMatchingArrow(HTyp.t, HTyp.t)
  /* cursor is on the function position of an ap,
     and that expression does synthesize a type
     with a matched arrow type */

  | SynFreeArrow(HTyp.t)
  /* cursor is on a free variable in the function
     position of an ap */

  | SynFree
  /* none of the above, cursor is on a free variable */

  | Synthesized(HTyp.t)
  /* none of the above */

  /* 
   * # cursor in type position 
   */

  | TypePosition
  /* (we will have a richer structure here later) */

  /* 
   *  # cursor in analytic pattern position 
   */

  | PatAnaTypeInconsistent(HTyp.t, HTyp.t)
  /* cursor is on a type inconsistent pattern */

  | PatAnaWrongLength(
      nat /* expected length */,
      nat, /* got length */
      HTyp.t,
    ) /* expected type */
  /* cursor is on a tuple pattern of the wrong length */

  | PatAnalyzed(HTyp.t)
  /* none of the above and didn't go through subsumption */

  | PatAnaSubsumed(HTyp.t, HTyp.t)
  /* none of the above and went through subsumption */

  /* 
   * # cursor in synthetic pattern position
   */

  | PatSynthesized(HTyp.t)
;

type cursor_sort =
  | IsExpr(UHExp.t)
  | IsPat(UHPat.t)
  | IsType;

type t = {
  mode: cursor_mode,
  sort: cursor_sort,
  side: cursor_side,
  ctx: Contexts.t,
};

let mk_cursor_info = (mode, sort, side, ctx) => {mode, sort, side, ctx};

let update_sort = (ci: t, sort: cursor_sort): t => {
  let {mode, sort: _, side, ctx} = ci;
  {mode, sort, side, ctx};
};

let rec ana_pat_cursor_found =
        (ctx: Contexts.t, p: UHPat.t, ty: HTyp.t, side: cursor_side)
        : option(t) =>
  switch (p) {
  | UHPat.Parenthesized(p1) =>
    switch (ana_pat_cursor_found(ctx, p1, ty, side)) {
    | None => None
    | Some(ci) => Some(update_sort(ci, IsPat(p)))
    }
  | UHPat.Pat(InHole(TypeInconsistent, _), p') =>
    switch (Statics.syn_pat'(ctx, p')) {
    | None => None
    | Some((ty', _)) =>
      Some(
        mk_cursor_info(
          PatAnaTypeInconsistent(ty, ty'),
          IsPat(p),
          side,
          ctx,
        ),
      )
    }
  | UHPat.Pat(NotInHole, UHPat.EmptyHole(_)) =>
    Some(
      mk_cursor_info(PatAnaSubsumed(ty, HTyp.Hole), IsPat(p), side, ctx),
    )
  | UHPat.Pat(NotInHole, UHPat.Wild)
  | UHPat.Pat(NotInHole, UHPat.Var(_)) =>
    Some(mk_cursor_info(PatAnalyzed(ty), IsPat(p), side, ctx))
  | UHPat.Pat(NotInHole, UHPat.NumLit(_)) =>
    Some(mk_cursor_info(PatAnaSubsumed(ty, HTyp.Num), IsPat(p), side, ctx))
  | UHPat.Pat(NotInHole, UHPat.BoolLit(_)) =>
    Some(
      mk_cursor_info(PatAnaSubsumed(ty, HTyp.Bool), IsPat(p), side, ctx),
    )
  | UHPat.Pat(NotInHole, UHPat.Inj(_, p1)) =>
    Some(mk_cursor_info(PatAnalyzed(ty), IsPat(p), side, ctx))
  | UHPat.Pat(NotInHole, UHPat.ListNil) =>
    Some(mk_cursor_info(PatAnalyzed(ty), IsPat(p), side, ctx))
  | UHPat.Pat(
      NotInHole,
      UHPat.OpSeq(Skel.BinOp(NotInHole, Comma, skel1, skel2), seq),
    )
  | UHPat.Pat(
      NotInHole,
      UHPat.OpSeq(Skel.BinOp(NotInHole, Cons, skel1, skel2), seq),
    ) =>
    Some(mk_cursor_info(PatAnalyzed(ty), IsPat(p), side, ctx))
  | UHPat.Pat(
      InHole(WrongLength, _),
      UHPat.OpSeq(
        Skel.BinOp(InHole(WrongLength, _), Comma, skel1, skel2),
        _,
      ),
    ) =>
    switch (ty) {
    | HTyp.Prod(ty1, ty2) =>
      let n_elts = List.length(UHPat.get_tuple(skel1, skel2));
      let n_types = List.length(HTyp.get_tuple(ty1, ty2));
      Some(
        mk_cursor_info(
          PatAnaWrongLength(n_types, n_elts, ty),
          IsPat(p),
          side,
          ctx,
        ),
      );
    | _ => None
    }
  | UHPat.Pat(InHole(WrongLength, _), _) => None
  | UHPat.Pat(
      NotInHole,
      UHPat.OpSeq(Skel.BinOp(InHole(_, _), _, skel1, skel2), seq),
    ) =>
    None
  | UHPat.Pat(NotInHole, UHPat.OpSeq(Skel.Placeholder(_), _)) => None
  | UHPat.Pat(NotInHole, UHPat.OpSeq(Skel.BinOp(_, Space, _, _), _)) => None
  };

let rec syn_pat_cursor_info =
        (ctx: Contexts.t, zp: ZPat.t): option(t) =>
  switch (zp) {
  | ZPat.CursorP(side, p) =>
    switch (Statics.syn_pat(ctx, p)) {
    | None => None
    | Some((ty, _)) =>
      Some(mk_cursor_info(PatSynthesized(ty), IsPat(p), side, ctx))
    }
  | ZPat.Deeper(_, zp') => syn_pat_cursor_info'(ctx, zp')
  | ZPat.ParenthesizedZ(zp1) => syn_pat_cursor_info(ctx, zp1)
  }
and syn_pat_cursor_info' =
    (ctx: Contexts.t, zp': ZPat.t'): option(t) =>
  switch (zp') {
  | ZPat.InjZ(side, zp1) => syn_pat_cursor_info(ctx, zp1)
  | ZPat.OpSeqZ(skel, zp1, surround) =>
    let p1 = ZPat.erase(zp1);
    let seq = OperatorSeq.opseq_of_exp_and_surround(p1, surround);
    let n = OperatorSeq.surround_prefix_length(surround);
    syn_skel_pat_cursor_info(ctx, skel, seq, n, zp1);
  }
and syn_skel_pat_cursor_info =
    (
      ctx: Contexts.t,
      skel: UHPat.skel_t,
      seq: UHPat.opseq,
      n: nat,
      zp1: ZPat.t,
    )
    : option(t) =>
  switch (skel) {
  | Skel.Placeholder(n') =>
    if (n == n') {
      syn_pat_cursor_info(ctx, zp1);
    } else {
      None;
    }
  | Skel.BinOp(_, UHPat.Comma, skel1, skel2) =>
    switch (syn_skel_pat_cursor_info(ctx, skel1, seq, n, zp1)) {
    | Some(_) as result => result
    | None => syn_skel_pat_cursor_info(ctx, skel2, seq, n, zp1)
    }
  | Skel.BinOp(_, UHPat.Space, skel1, skel2) =>
    switch (syn_skel_pat_cursor_info(ctx, skel1, seq, n, zp1)) {
    | Some(_) as result => result
    | None => syn_skel_pat_cursor_info(ctx, skel2, seq, n, zp1)
    }
  | Skel.BinOp(_, UHPat.Cons, skel1, skel2) =>
    switch (syn_skel_pat_cursor_info(ctx, skel1, seq, n, zp1)) {
    | Some(_) as result => result
    | None =>
      switch (Statics.syn_skel_pat(ctx, skel1, seq, None)) {
      | None => None
      | Some((ty_elt, ctx, _)) =>
        let list_ty = HTyp.List(ty_elt);
        ana_skel_pat_cursor_info(ctx, skel2, seq, n, zp1, list_ty);
      }
    }
  }
and ana_pat_cursor_info =
    (ctx: Contexts.t, zp: ZPat.t, ty: HTyp.t): option(t) =>
  switch (zp) {
  | ZPat.CursorP(side, p) => ana_pat_cursor_found(ctx, p, ty, side)
  | ZPat.Deeper(InHole(TypeInconsistent, u), zp') =>
    syn_pat_cursor_info'(ctx, zp')
  | ZPat.Deeper(NotInHole, zp')
  | ZPat.Deeper(
      InHole(WrongLength, _),
      ZPat.OpSeqZ(Skel.BinOp(_, UHPat.Comma, _, _), _, _) as zp',
    ) =>
    ana_pat_cursor_info'(ctx, zp', ty)
  | ZPat.Deeper(InHole(WrongLength, _), _) => None
  | ZPat.ParenthesizedZ(zp) => ana_pat_cursor_info(ctx, zp, ty)
  }
and ana_pat_cursor_info' =
    (ctx: Contexts.t, zp': ZPat.t', ty: HTyp.t): option(t) =>
  switch (zp') {
  | ZPat.InjZ(side, zp1) =>
    switch (HTyp.matched_sum(ty)) {
    | None => None
    | Some((tyL, tyR)) =>
      let ty1 = pick_side(side, tyL, tyR);
      ana_pat_cursor_info(ctx, zp1, ty1);
    }
  | ZPat.OpSeqZ(skel, zp1, surround) =>
    let p1 = ZPat.erase(zp1);
    let seq = OperatorSeq.opseq_of_exp_and_surround(p1, surround);
    let n = OperatorSeq.surround_prefix_length(surround);
    ana_skel_pat_cursor_info(ctx, skel, seq, n, zp1, ty);
  }
and ana_skel_pat_cursor_info =
    (
      ctx: Contexts.t,
      skel: UHPat.skel_t,
      seq: UHPat.opseq,
      n: nat,
      zp1: ZPat.t,
      ty: HTyp.t,
    )
    : option(t) =>
  switch (skel) {
  | Skel.Placeholder(n') =>
    if (n == n') {
      ana_pat_cursor_info(ctx, zp1, ty);
    } else {
      None;
    }
  | Skel.BinOp(InHole(TypeInconsistent, _), _, skel1, skel2) =>
    syn_skel_pat_cursor_info(ctx, skel, seq, n, zp1)
  | Skel.BinOp(NotInHole, UHPat.Comma, skel1, skel2) =>
    switch (ty) {
    | HTyp.Hole =>
      switch (ana_skel_pat_cursor_info(ctx, skel1, seq, n, zp1, HTyp.Hole)) {
      | Some(_) as result => result
      | None => ana_skel_pat_cursor_info(ctx, skel2, seq, n, zp1, HTyp.Hole)
      }
    | HTyp.Prod(ty1, ty2) =>
      let types = HTyp.get_tuple(ty1, ty2);
      let skels = UHPat.get_tuple(skel1, skel2);
      switch (Util.zip_eq(skels, types)) {
      | None => None
      | Some(zipped) =>
        List.fold_left(
          (opt_result, skel_ty: (UHPat.skel_t, HTyp.t)) =>
            switch (opt_result) {
            | Some(_) as result => result
            | None =>
              let (skel, ty) = skel_ty;
              ana_skel_pat_cursor_info(ctx, skel, seq, n, zp1, ty);
            },
          None,
          zipped,
        )
      };
    | _ => None
    }
  | Skel.BinOp(InHole(WrongLength, _), UHPat.Comma, skel1, skel2) =>
    switch (ty) {
    | HTyp.Prod(ty1, ty2) =>
      let types = HTyp.get_tuple(ty1, ty2);
      let skels = UHPat.get_tuple(skel1, skel2);
      let (zipped, remainder) = HTyp.zip_with_skels(skels, types);
      let ana_zipped =
        List.fold_left(
          (opt_result, skel_ty: (UHPat.skel_t, HTyp.t)) =>
            switch (opt_result) {
            | Some(_) as result => result
            | None =>
              let (skel, ty) = skel_ty;
              ana_skel_pat_cursor_info(ctx, skel, seq, n, zp1, ty);
            },
          None,
          zipped,
        );
      switch (ana_zipped) {
      | Some(_) as result => result
      | None =>
        List.fold_left(
          (opt_result, skel) =>
            switch (opt_result) {
            | Some(_) as result => result
            | None => syn_skel_pat_cursor_info(ctx, skel, seq, n, zp1)
            },
          None,
          remainder,
        )
      };
    | _ => None
    }
  | Skel.BinOp(InHole(WrongLength, _), _, _, _) => None
  | Skel.BinOp(NotInHole, UHPat.Cons, skel1, skel2) =>
    switch (HTyp.matched_list(ty)) {
    | None => None
    | Some(ty_elt) =>
      switch (ana_skel_pat_cursor_info(ctx, skel1, seq, n, zp1, ty_elt)) {
      | Some(_) as result => result
      | None =>
        let ty_list = HTyp.List(ty_elt);
        ana_skel_pat_cursor_info(ctx, skel2, seq, n, zp1, ty_list);
      }
    }
  | Skel.BinOp(NotInHole, UHPat.Space, _, _) =>
    syn_skel_pat_cursor_info(ctx, skel, seq, n, zp1)
  };

let rec ana_cursor_found =
        (ctx: Contexts.t, e: UHExp.t, ty: HTyp.t, side: cursor_side)
        : option(t) =>
  switch (e) {
  | UHExp.Parenthesized(e') =>
    switch (ana_cursor_found(ctx, e', ty, side)) {
    | None => None
    | Some(ci) => Some(update_sort(ci, IsExpr(e)))
    }
  | UHExp.Tm(
      InHole(TypeInconsistent, _),
      UHExp.OpSeq(Skel.BinOp(_, op, skel1, skel2), surround),
    ) =>
    let e' = UHExp.OpSeq(Skel.BinOp(NotInHole, op, skel1, skel2), surround);
    switch (Statics.syn'(ctx, e')) {
    | None => None
    | Some(ty') =>
      Some(
        mk_cursor_info(AnaTypeInconsistent(ty, ty'), IsExpr(e), side, ctx),
      )
    };
  | UHExp.Tm(InHole(TypeInconsistent, _), e') =>
    switch (Statics.syn'(ctx, e')) {
    | None => 
      None
    | Some(ty') =>
      Some(
        mk_cursor_info(AnaTypeInconsistent(ty, ty'), IsExpr(e), side, ctx),
      )
    }
  | UHExp.Tm(_, UHExp.Var(InVHole(_), _)) =>
    Some(mk_cursor_info(AnaFree(ty), IsExpr(e), side, ctx))
  | UHExp.Tm(NotInHole, UHExp.Case(_, _)) => 
    Some(mk_cursor_info(Analyzed(ty), IsExpr(e), side, ctx))
  | UHExp.Tm(NotInHole, UHExp.Let(_, _, _, _))
  | UHExp.Tm(NotInHole, UHExp.ListNil) 
  | UHExp.Tm(
      NotInHole,
      UHExp.OpSeq(Skel.BinOp(NotInHole, UHExp.Comma, _, _), _),
    )
  | UHExp.Tm(
      NotInHole,
      UHExp.OpSeq(Skel.BinOp(NotInHole, UHExp.Cons, _, _), _),
    ) =>
      Some(mk_cursor_info(Analyzed(ty), IsExpr(e), side, ctx))
  | UHExp.Tm(
      InHole(WrongLength, _),
      UHExp.OpSeq(
        Skel.BinOp(InHole(WrongLength, _), UHExp.Comma, skel1, skel2),
        _,
      ),
    ) =>
    switch (ty) {
    | HTyp.Prod(ty1, ty2) =>
      let n_elts = List.length(UHExp.get_tuple(skel1, skel2));
      let n_types = List.length(HTyp.get_tuple(ty1, ty2));
      Some(
        mk_cursor_info(
          AnaWrongLength(n_types, n_elts, ty),
          IsExpr(e),
          side,
          ctx,
        ),
      );
    | _ => None
    }
  | UHExp.Tm(InHole(WrongLength, _), _) => None
  | UHExp.Tm(
      NotInHole,
      UHExp.OpSeq(Skel.BinOp(InHole(WrongLength, _), _, _, _), _),
    ) =>
    None
  | UHExp.Tm(NotInHole, UHExp.Lam(_, ann, _)) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => None
    | Some((ty1_given, ty2)) =>
      switch (ann) {
      | Some(uty1) =>
        let ty1_ann = UHTyp.expand(uty1);
        switch (HTyp.consistent(ty1_ann, ty1_given)) {
        | false => None
        | true =>
          Some(
            mk_cursor_info(
              AnaAnnotatedLambda(ty, HTyp.Arrow(ty1_ann, ty2)),
              IsExpr(e),
              side,
              ctx,
            ),
          )
        };
      | None => 
        Some(mk_cursor_info(Analyzed(ty), IsExpr(e), side, ctx))
      }
    }
  | UHExp.Tm(NotInHole, UHExp.Inj(_, _)) =>
    Some(mk_cursor_info(Analyzed(ty), IsExpr(e), side, ctx))
  | UHExp.Tm(
      NotInHole,
      UHExp.OpSeq(
        Skel.BinOp(InHole(TypeInconsistent, _), _, _, _),
        surround,
      ),
    ) =>
    None
  | UHExp.Tm(NotInHole, UHExp.OpSeq(Skel.Placeholder(_), surround)) => None
  | UHExp.Tm(
      NotInHole,
      UHExp.OpSeq(Skel.BinOp(NotInHole, UHExp.Plus, _, _), _),
    )
  | UHExp.Tm(
      NotInHole,
      UHExp.OpSeq(Skel.BinOp(NotInHole, UHExp.Times, _, _), _),
    )
  | UHExp.Tm(
      NotInHole,
      UHExp.OpSeq(Skel.BinOp(NotInHole, UHExp.LessThan, _, _), _),
    )
  | UHExp.Tm(
      NotInHole,
      UHExp.OpSeq(Skel.BinOp(NotInHole, UHExp.Space, _, _), _),
    )
  | UHExp.Tm(NotInHole, UHExp.EmptyHole(_))
  | UHExp.Tm(NotInHole, UHExp.Asc(_, _))
  | UHExp.Tm(NotInHole, UHExp.Var(NotInVHole, _))
  | UHExp.Tm(NotInHole, UHExp.NumLit(_))
  | UHExp.Tm(NotInHole, UHExp.BoolLit(_))
  | UHExp.Tm(NotInHole, UHExp.ApPalette(_, _, _)) =>
    switch (Statics.syn(ctx, e)) {
    | None => None
    | Some(ty') =>
      Some(mk_cursor_info(AnaSubsumed(ty, ty'), IsExpr(e), side, ctx));
    } 
  };

let rec syn_cursor_info = (ctx: Contexts.t, ze: ZExp.t): option(t) =>
  switch (ze) {
  | ZExp.CursorE(side, UHExp.Tm(_, UHExp.Var(InVHole(_), _)) as e) =>
    Some(mk_cursor_info(SynFree, IsExpr(e), side, ctx))
  | ZExp.CursorE(side, e) =>
    switch (Statics.syn(ctx, e)) {
    | Some(ty) => Some(mk_cursor_info(Synthesized(ty), IsExpr(e), side, ctx))
    | None => None
    }
  | ZExp.ParenthesizedZ(ze1) => syn_cursor_info(ctx, ze1)
  | ZExp.Deeper(_, ze1') => syn_cursor_info'(ctx, ze1')
  }
and ana_cursor_info =
    (ctx: Contexts.t, ze: ZExp.t, ty: HTyp.t): option(t) =>
  switch (ze) {
  | ZExp.CursorE(side, e) => ana_cursor_found(ctx, e, ty, side)
  | ZExp.ParenthesizedZ(ze1) => ana_cursor_info(ctx, ze1, ty)
  | ZExp.Deeper(InHole(TypeInconsistent, u), ze1') => syn_cursor_info'(ctx, ze1')
  | ZExp.Deeper(
      InHole(WrongLength, _),
      ZExp.OpSeqZ(Skel.BinOp(_, UHExp.Comma, _, _), _, _) as ze1',
    )
  | ZExp.Deeper(NotInHole, ze1') => ana_cursor_info'(ctx, ze1', ty)
  | ZExp.Deeper(InHole(WrongLength, _), _) => None
  }
and syn_cursor_info' = (ctx: Contexts.t, ze: ZExp.t'): option(t) =>
  switch (ze) {
  | ZExp.AscZ1(ze1, uty) =>
    let ty = UHTyp.expand(uty);
    let e1 = ZExp.erase(ze1);
    if (UHExp.bidelimited(e1)) {
      ana_cursor_info(ctx, ze1, ty);
    } else {
      None;
    };
  | ZExp.AscZ2(e1, zty) =>
    Some(
      mk_cursor_info(
        TypePosition,
        IsType,
        Before, 
        ctx,
      ),
    )
  | ZExp.LetZP(zp, ann, e1, e2) =>
    switch (ann) {
    | Some(uty1) =>
      let ty1 = UHTyp.expand(uty1);
      ana_pat_cursor_info(ctx, zp, ty1);
    | None =>
      switch (Statics.syn(ctx, e1)) {
      | None => None
      | Some(ty1) => ana_pat_cursor_info(ctx, zp, ty1)
      }
    }
  | ZExp.LetZA(p, zann, e1, e2) =>
    Some(
      mk_cursor_info(
        TypePosition,
        IsType,
        Before, 
        ctx,
      ),
    )
  | ZExp.LetZE1(p, ann, ze1, e2) =>
    switch (ann) {
    | Some(uty1) =>
      let ty1 = UHTyp.expand(uty1);
      let ctx1 = Statics.ctx_for_let(ctx, p, ty1, ZExp.erase(ze1));
      ana_cursor_info(ctx1, ze1, ty1);
    | None => syn_cursor_info(ctx, ze1)
    }
  | ZExp.LetZE2(p, ann, e1, ze2) =>
    switch (ann) {
    | Some(uty1) =>
      let ty1 = UHTyp.expand(uty1);
      switch (Statics.ana_pat(ctx, p, ty1)) {
      | None => None
      | Some(ctx2) => syn_cursor_info(ctx2, ze2)
      };
    | None =>
      switch (Statics.syn(ctx, e1)) {
      | None => None
      | Some(ty1) =>
        switch (Statics.ana_pat(ctx, p, ty1)) {
        | None => None
        | Some(ctx2) => syn_cursor_info(ctx2, ze2)
        }
      }
    }
  | ZExp.LamZP(zp, ann, _) =>
    let ty1 =
      switch (ann) {
      | Some(uty1) => UHTyp.expand(uty1)
      | None => HTyp.Hole
      };
    ana_pat_cursor_info(ctx, zp, ty1);
  | ZExp.LamZA(_, zann, _) =>
    Some(
      mk_cursor_info(
        TypePosition,
        IsType,
        Before, 
        ctx,
      ),
    )
  | ZExp.LamZE(p, ann, ze1) =>
    let ty1 =
      switch (ann) {
      | Some(uty1) => UHTyp.expand(uty1)
      | None => HTyp.Hole
      };
    switch (Statics.ana_pat(ctx, p, ty1)) {
    | None => None
    | Some(ctx1) => syn_cursor_info(ctx1, ze1)
    };
  | ZExp.InjZ(side, ze1) => syn_cursor_info(ctx, ze1)
  | ZExp.CaseZE(_, _)
  | ZExp.CaseZR(_, _) => None
  | ZExp.OpSeqZ(skel, ze0, surround) =>
    let e0 = ZExp.erase(ze0);
    let seq = OperatorSeq.opseq_of_exp_and_surround(e0, surround);
    let n = OperatorSeq.surround_prefix_length(surround);
    syn_skel_cursor_info(ctx, skel, seq, n, ze0);
  | ZExp.ApPaletteZ(_, _, zpsi) => 
    let (ty, ze) = Util.ZNatMap.prj_z_v(zpsi.zsplice_map);
    ana_cursor_info(ctx, ze, ty);
  }
and ana_cursor_info' =
    (ctx: Contexts.t, ze: ZExp.t', ty: HTyp.t): option(t) =>
  switch (ze) {
  | ZExp.LetZP(zp, ann, e1, e2) =>
    switch (ann) {
    | Some(uty1) =>
      let ty1 = UHTyp.expand(uty1);
      ana_pat_cursor_info(ctx, zp, ty1);
    | None =>
      switch (Statics.syn(ctx, e1)) {
      | None => None
      | Some(ty1) => ana_pat_cursor_info(ctx, zp, ty1)
      }
    }
  | ZExp.LetZA(_, zann, e1, e2) =>
    Some(
      mk_cursor_info(
        TypePosition,
        IsType,
        Before, 
        ctx,
      ),
    )
  | ZExp.LetZE1(p, ann, ze1, e2) =>
    switch (ann) {
    | Some(uty1) =>
      let ty1 = UHTyp.expand(uty1);
      let ctx1 = Statics.ctx_for_let(ctx, p, ty1, ZExp.erase(ze1));
      ana_cursor_info(ctx1, ze1, ty1);
    | None => syn_cursor_info(ctx, ze1)
    }
  | ZExp.LetZE2(p, ann, e1, ze2) =>
    switch (ann) {
    | Some(uty1) =>
      let ty1 = UHTyp.expand(uty1);
      switch (Statics.ana_pat(ctx, p, ty1)) {
      | None => None
      | Some(ctx2) => ana_cursor_info(ctx2, ze2, ty)
      };
    | None =>
      switch (Statics.syn(ctx, e1)) {
      | None => None
      | Some(ty1) =>
        switch (Statics.ana_pat(ctx, p, ty1)) {
        | None => None
        | Some(ctx2) => ana_cursor_info(ctx2, ze2, ty)
        }
      }
    }
  | ZExp.LamZP(p, ann, e) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => None
    | Some((ty1_given, ty2)) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => ty1_given
        };
      ana_pat_cursor_info(ctx, p, ty1);
    }
  | ZExp.LamZA(_, zann, _) =>
    Some(
      mk_cursor_info(
        TypePosition,
        IsType,
        Before, 
        ctx,
      ),
    )
  | ZExp.LamZE(p, ann, ze1) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => None
    | Some((ty1_given, ty2)) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => ty1_given
        };
      switch (Statics.ana_pat(ctx, p, ty1)) {
      | None => None
      | Some(ctx) => 
        ana_cursor_info(ctx, ze1, ty2)
      };
    }
  | ZExp.InjZ(side, ze1) =>
    switch (HTyp.matched_sum(ty)) {
    | None => None
    | Some((ty1, ty2)) =>
      ana_cursor_info(ctx, ze1, pick_side(side, ty1, ty2))
    }
  | ZExp.CaseZE(ze1, rules) => syn_cursor_info(ctx, ze1)
  | ZExp.CaseZR(e1, zrules) =>
    switch (Statics.syn(ctx, e1)) {
    | None => None
    | Some(ty1) =>
      let zrule = Util.ZList.prj_z(zrules);
      ana_rule_cursor_info(ctx, zrule, ty1, ty);
    }
  | ZExp.OpSeqZ(skel, ze0, surround) =>
    let e0 = ZExp.erase(ze0);
    let seq = OperatorSeq.opseq_of_exp_and_surround(e0, surround);
    let n = OperatorSeq.surround_prefix_length(surround);
    ana_skel_cursor_info(ctx, skel, seq, n, ze0, ty);
  | ZExp.AscZ1(_, _)
  | ZExp.AscZ2(_, _)
  | ZExp.ApPaletteZ(_, _, _) => syn_cursor_info'(ctx, ze)
  }
and ana_rule_cursor_info =
    (ctx: Contexts.t, zrule: ZExp.zrule, pat_ty: HTyp.t, clause_ty: HTyp.t)
    : option(t) =>
  switch (zrule) {
  | ZExp.RuleZP(zp, e) => ana_pat_cursor_info(ctx, zp, pat_ty)
  | ZExp.RuleZE(p, ze) =>
    switch (Statics.ana_pat(ctx, p, pat_ty)) {
    | None => None
    | Some(ctx) => ana_cursor_info(ctx, ze, clause_ty)
    }
  }
and syn_skel_cursor_info =
    (ctx: Contexts.t, skel: UHExp.skel_t, seq: UHExp.opseq, n: nat, ze_n: ZExp.t)
    : option(t) =>
  switch (skel) {
  | Skel.Placeholder(n') =>
    if (n == n') {
      syn_cursor_info(ctx, ze_n);
    } else {
      None;
    }
  | Skel.BinOp(_, UHExp.Plus, skel1, skel2)
  | Skel.BinOp(_, UHExp.Times, skel1, skel2)
  | Skel.BinOp(_, UHExp.LessThan, skel1, skel2) =>
    switch (ana_skel_cursor_info(ctx, skel1, seq, n, ze_n, HTyp.Num)) {
    | Some(_) as result => result
    | None =>
      switch (ana_skel_cursor_info(ctx, skel2, seq, n, ze_n, HTyp.Num)) {
      | Some(_) as result => result
      | None => None
      }
    }
  | Skel.BinOp(_, UHExp.Space, Skel.Placeholder(n') as skel1, skel2) =>
    if (n == n') {
      switch (ZExp.cursor_on_outer_expr(ze_n)) {
      | Some((UHExp.Tm(InHole(TypeInconsistent, u), e_n') as e_n, side)) =>
        switch (Statics.syn'(ctx, e_n')) {
        | Some(ty) =>
          Some(
            mk_cursor_info(
              SynErrorArrow(HTyp.Arrow(HTyp.Hole, HTyp.Hole), ty),
              IsExpr(e_n),
              side,
              ctx,
            ),
          )
        | None => None
        }
      | Some((UHExp.Tm(_, UHExp.Var(InVHole(_), _)) as e_n, side)) =>
        Some(
          mk_cursor_info(
            SynFreeArrow(HTyp.Arrow(HTyp.Hole, HTyp.Hole)),
            IsExpr(e_n),
            side,
            ctx,
          ),
        )
      | Some((e_n, side)) =>
        switch (Statics.syn(ctx, e_n)) {
        | Some(ty) =>
          switch (HTyp.matched_arrow(ty)) {
          | Some((ty1, ty2)) =>
            Some(
              mk_cursor_info(
                SynMatchingArrow(ty, HTyp.Arrow(ty1, ty2)),
                IsExpr(e_n),
                side,
                ctx,
              ),
            )
          | None => None
          }
        | None => None
        }
      | None => syn_cursor_info(ctx, ze_n)
      };
    } else {
      switch (Statics.syn_skel(ctx, skel1, seq, None)) {
      | None => None
      | Some((ty, _)) =>
        switch (HTyp.matched_arrow(ty)) {
        | Some((ty1, ty2)) =>
          ana_skel_cursor_info(ctx, skel2, seq, n, ze_n, ty1)
        | None => None
        }
      };
    }
  | Skel.BinOp(_, UHExp.Space, skel1, skel2) =>
    switch (syn_skel_cursor_info(ctx, skel1, seq, n, ze_n)) {
    | Some(_) as result => result
    | None =>
      switch (Statics.syn_skel(ctx, skel1, seq, None)) {
      | None => None
      | Some((ty, _)) =>
        switch (HTyp.matched_arrow(ty)) {
        | None => None
        | Some((ty1, ty2)) =>
          ana_skel_cursor_info(ctx, skel2, seq, n, ze_n, ty1)
        }
      }
    }
  | Skel.BinOp(_, UHExp.Comma, skel1, skel2) =>
    switch (syn_skel_cursor_info(ctx, skel1, seq, n, ze_n)) {
    | Some(_) as result => result
    | None => syn_skel_cursor_info(ctx, skel2, seq, n, ze_n)
    }
  | Skel.BinOp(_, UHExp.Cons, skel1, skel2) =>
    switch (syn_skel_cursor_info(ctx, skel1, seq, n, ze_n)) {
    | Some(_) as result => result
    | None =>
      switch (Statics.syn_skel(ctx, skel1, seq, None)) {
      | None => None
      | Some((ty_elt, _)) =>
        let ty_list = HTyp.List(ty_elt);
        ana_skel_cursor_info(ctx, skel2, seq, n, ze_n, ty_list);
      }
    }
  }
and ana_skel_cursor_info =
    (
      ctx: Contexts.t,
      skel: UHExp.skel_t,
      seq: UHExp.opseq,
      n: nat,
      ze_n: ZExp.t,
      ty: HTyp.t,
    )
    : option(t) =>
  switch (skel) {
  | Skel.Placeholder(n') =>
    if (n == n') {
      ana_cursor_info(ctx, ze_n, ty);
    } else {
      None;
    }
  | Skel.BinOp(InHole(TypeInconsistent, _), _, _, _) =>
    syn_skel_cursor_info(ctx, skel, seq, n, ze_n)
  | Skel.BinOp(NotInHole, UHExp.Comma, skel1, skel2) =>
    switch (ty) {
    | HTyp.Hole =>
      switch (ana_skel_cursor_info(ctx, skel1, seq, n, ze_n, HTyp.Hole)) {
      | Some(_) as result => result
      | None => ana_skel_cursor_info(ctx, skel2, seq, n, ze_n, HTyp.Hole)
      }
    | HTyp.Prod(ty1, ty2) =>
      let types = HTyp.get_tuple(ty1, ty2);
      let skels = UHExp.get_tuple(skel1, skel2);
      switch (Util.zip_eq(skels, types)) {
      | None => None
      | Some(zipped) =>
        List.fold_left(
          (opt_result, skel_ty: (UHExp.skel_t, HTyp.t)) =>
            switch (opt_result) {
            | Some(_) as result => result
            | None =>
              let (skel, ty) = skel_ty;
              ana_skel_cursor_info(ctx, skel, seq, n, ze_n, ty);
            },
          None,
          zipped,
        )
      };
    | _ => None
    }
  | Skel.BinOp(InHole(WrongLength, _), UHExp.Comma, skel1, skel2) =>
    switch (ty) {
    | HTyp.Prod(ty1, ty2) =>
      let types = HTyp.get_tuple(ty1, ty2);
      let skels = UHExp.get_tuple(skel1, skel2);
      let (zipped, remainder) = HTyp.zip_with_skels(skels, types);
      let ana_zipped =
        List.fold_left(
          (opt_result, skel_ty: (UHExp.skel_t, HTyp.t)) =>
            switch (opt_result) {
            | Some(_) as result => result
            | None =>
              let (skel, ty) = skel_ty;
              ana_skel_cursor_info(ctx, skel, seq, n, ze_n, ty);
            },
          None,
          zipped,
        );
      switch (ana_zipped) {
      | Some(_) as result => result
      | None =>
        List.fold_left(
          (opt_result, skel) =>
            switch (opt_result) {
            | Some(_) as result => result
            | None => syn_skel_cursor_info(ctx, skel, seq, n, ze_n)
            },
          None,
          remainder,
        )
      };
    | _ => None
    }
  | Skel.BinOp(InHole(WrongLength, _), _, _, _) => None
  | Skel.BinOp(NotInHole, UHExp.Cons, skel1, skel2) =>
    switch (HTyp.matched_list(ty)) {
    | None => None
    | Some(ty_elt) =>
      switch (ana_skel_cursor_info(ctx, skel1, seq, n, ze_n, ty_elt)) {
      | Some(_) as result => result
      | None =>
        let ty_list = HTyp.List(ty_elt);
        ana_skel_cursor_info(ctx, skel2, seq, n, ze_n, ty_list);
      }
    }
  | Skel.BinOp(_, UHExp.Plus, _, _)
  | Skel.BinOp(_, UHExp.Times, _, _)
  | Skel.BinOp(_, UHExp.LessThan, _, _)
  | Skel.BinOp(_, UHExp.Space, _, _) =>
    syn_skel_cursor_info(ctx, skel, seq, n, ze_n)
  };

