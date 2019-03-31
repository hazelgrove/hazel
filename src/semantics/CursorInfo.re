open SemanticsCommon;
open GeneralUtil;

type cursor_mode =
  /*
   *  # cursor in analytic position
   */
  | AnaAnnotatedLambda(HTyp.t, HTyp.t)
  /* cursor is on a lambda with an argument type annotation */
  | AnaTypeInconsistent(HTyp.t, HTyp.t)
  /* cursor is on a type inconsistent expression */
  | AnaWrongLength(
      int /* expected length */,
      int, /* got length */
      HTyp.t,
    ) /* expected type */
  /* cursor is on a tuple of the wrong length */
  | AnaFree(HTyp.t)
  /* cursor is on a free variable */
  | Analyzed(HTyp.t)
  /* none of the above and didn't go through subsumption */
  | AnaSubsumed(HTyp.t, HTyp.t)
  /* none of the above and went through subsumption */
  | AnaKeyword(HTyp.t, keyword)
  /* cursor is on a keyword */
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
  | SynKeywordArrow(HTyp.t, keyword)
  /* cursor is on a keyword in the function position of an ap */
  | SynFree
  /* none of the above, cursor is on a free variable */
  | SynKeyword(keyword)
  /* cursor is on a keyword */
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
      int /* expected length */,
      int, /* got length */
      HTyp.t,
    ) /* expected type */
  /* cursor is on a tuple pattern of the wrong length */
  | PatAnalyzed(HTyp.t)
  /* none of the above and didn't go through subsumption */
  | PatAnaSubsumed(HTyp.t, HTyp.t)
  /* none of the above and went through subsumption */
  | PatAnaKeyword(HTyp.t, keyword)
  /* cursor is on a keyword */
  /*
   * # cursor in synthetic pattern position
   */
  | PatSynthesized(HTyp.t)
  /* cursor is on a keyword */
  | PatSynKeyword(keyword)
  /*
   *  # cursor on line
   */
  | Line;

type cursor_sort =
  | IsExpr(UHExp.t)
  | IsPat(UHPat.t)
  | IsType
  | IsLine(UHExp.line)
  | IsBlock(UHExp.block);

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

let rec ana_cursor_found_pat =
        (ctx: Contexts.t, p: UHPat.t, ty: HTyp.t, side: cursor_side)
        : option(t) =>
  switch (p) {
  | Parenthesized(p1) =>
    switch (ana_cursor_found_pat(ctx, p1, ty, side)) {
    | None => None
    | Some(ci) => Some(update_sort(ci, IsPat(p)))
    }
  | Pat(InHole(TypeInconsistent, _), _) =>
    let p_nih = UHPat.set_err_status_t(NotInHole, p);
    switch (Statics.syn_pat(ctx, p_nih)) {
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
    };
  | EmptyHole(_) =>
    Some(mk_cursor_info(PatAnaSubsumed(ty, Hole), IsPat(p), side, ctx))
  | Pat(NotInHole, Wild)
  | Pat(NotInHole, Var(_)) =>
    Some(mk_cursor_info(PatAnalyzed(ty), IsPat(p), side, ctx))
  | Pat(NotInHole, NumLit(_)) =>
    Some(mk_cursor_info(PatAnaSubsumed(ty, Num), IsPat(p), side, ctx))
  | Pat(NotInHole, BoolLit(_)) =>
    Some(mk_cursor_info(PatAnaSubsumed(ty, Bool), IsPat(p), side, ctx))
  | Pat(NotInHole, Inj(_, _)) =>
    Some(mk_cursor_info(PatAnalyzed(ty), IsPat(p), side, ctx))
  | Pat(NotInHole, ListNil) =>
    Some(mk_cursor_info(PatAnalyzed(ty), IsPat(p), side, ctx))
  | OpSeq(BinOp(NotInHole, Comma, _, _), _)
  | OpSeq(BinOp(NotInHole, Cons, _, _), _) =>
    Some(mk_cursor_info(PatAnalyzed(ty), IsPat(p), side, ctx))
  | OpSeq(BinOp(InHole(WrongLength, _), Comma, skel1, skel2), _) =>
    switch (ty) {
    | Prod(ty1, ty2) =>
      let n_elts = ListMinTwo.length(UHPat.get_tuple(skel1, skel2));
      let n_types = ListMinTwo.length(HTyp.get_tuple(ty1, ty2));
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
  | Pat(InHole(WrongLength, _), _) => None
  | OpSeq(BinOp(InHole(_, _), _, _, _), _) => None
  | OpSeq(Placeholder(_), _) => None
  | OpSeq(BinOp(_, Space, _, _), _) => None
  };

let rec syn_cursor_info_pat = (ctx: Contexts.t, zp: ZPat.t): option(t) =>
  switch (zp) {
  | CursorP(side, Pat(_, Var(InVHole(Keyword(k), _), _)) as p) =>
    Some(mk_cursor_info(PatSynKeyword(k), IsPat(p), side, ctx))
  | CursorP(side, p) =>
    switch (Statics.syn_pat(ctx, p)) {
    | None => None
    | Some((ty, _)) =>
      Some(mk_cursor_info(PatSynthesized(ty), IsPat(p), side, ctx))
    }
  | Deeper(_, zp') => syn_cursor_info_pat'(ctx, zp')
  | ParenthesizedZ(zp1) => syn_cursor_info_pat(ctx, zp1)
  | OpSeqZ(skel, zp1, surround) =>
    let p1 = ZPat.erase(zp1);
    let seq = OperatorSeq.opseq_of_exp_and_surround(p1, surround);
    let n = OperatorSeq.surround_prefix_length(surround);
    syn_cursor_info_pat_skel(ctx, skel, seq, n, zp1);
  }
and syn_cursor_info_pat' = (ctx: Contexts.t, zp': ZPat.t'): option(t) =>
  switch (zp') {
  | InjZ(_, zp1) => syn_cursor_info_pat(ctx, zp1)
  }
and syn_cursor_info_pat_skel =
    (
      ctx: Contexts.t,
      skel: UHPat.skel_t,
      seq: UHPat.opseq,
      n: int,
      zp1: ZPat.t,
    )
    : option(t) =>
  switch (skel) {
  | Placeholder(n') =>
    if (n == n') {
      syn_cursor_info_pat(ctx, zp1);
    } else {
      None;
    }
  | BinOp(_, Comma, skel1, skel2) =>
    switch (syn_cursor_info_pat_skel(ctx, skel1, seq, n, zp1)) {
    | Some(_) as result => result
    | None => syn_cursor_info_pat_skel(ctx, skel2, seq, n, zp1)
    }
  | BinOp(_, Space, skel1, skel2) =>
    switch (ana_cursor_info_pat_skel(ctx, skel1, seq, n, zp1, HTyp.Hole)) {
    | Some(_) as result => result
    | None => ana_cursor_info_pat_skel(ctx, skel2, seq, n, zp1, Hole)
    }
  | BinOp(_, Cons, skel1, skel2) =>
    switch (syn_cursor_info_pat_skel(ctx, skel1, seq, n, zp1)) {
    | Some(_) as result => result
    | None =>
      switch (Statics.syn_skel_pat(ctx, skel1, seq, None)) {
      | None => None
      | Some((ty_elt, ctx, _)) =>
        let list_ty = HTyp.List(ty_elt);
        ana_cursor_info_pat_skel(ctx, skel2, seq, n, zp1, list_ty);
      }
    }
  }
and ana_cursor_info_pat =
    (ctx: Contexts.t, zp: ZPat.t, ty: HTyp.t): option(t) =>
  switch (zp) {
  | CursorP(side, Pat(_, Var(InVHole(Keyword(k), _), _)) as p) =>
    Some(mk_cursor_info(PatAnaKeyword(ty, k), IsPat(p), side, ctx))
  | CursorP(side, p) => ana_cursor_found_pat(ctx, p, ty, side)
  | Deeper(InHole(TypeInconsistent, _), zp') =>
    syn_cursor_info_pat'(ctx, zp')
  | OpSeqZ(skel, zp1, surround) =>
    let p1 = ZPat.erase(zp1);
    let seq = OperatorSeq.opseq_of_exp_and_surround(p1, surround);
    let n = OperatorSeq.surround_prefix_length(surround);
    ana_cursor_info_pat_skel(ctx, skel, seq, n, zp1, ty);
  | Deeper(NotInHole, zp') => ana_cursor_info_pat'(ctx, zp', ty)
  | Deeper(InHole(WrongLength, _), _) => None
  | ParenthesizedZ(zp) => ana_cursor_info_pat(ctx, zp, ty)
  }
and ana_cursor_info_pat' =
    (ctx: Contexts.t, zp': ZPat.t', ty: HTyp.t): option(t) =>
  switch (zp') {
  | InjZ(side, zp1) =>
    switch (HTyp.matched_sum(ty)) {
    | None => None
    | Some((tyL, tyR)) =>
      let ty1 = pick_side(side, tyL, tyR);
      ana_cursor_info_pat(ctx, zp1, ty1);
    }
  }
and ana_cursor_info_pat_skel =
    (
      ctx: Contexts.t,
      skel: UHPat.skel_t,
      seq: UHPat.opseq,
      n: int,
      zp1: ZPat.t,
      ty: HTyp.t,
    )
    : option(t) =>
  switch (skel) {
  | Placeholder(n') =>
    if (n == n') {
      ana_cursor_info_pat(ctx, zp1, ty);
    } else {
      None;
    }
  | BinOp(InHole(TypeInconsistent, _), _, _, _) =>
    syn_cursor_info_pat_skel(ctx, skel, seq, n, zp1)
  | BinOp(NotInHole, Comma, skel1, skel2) =>
    switch (ty) {
    | Hole =>
      switch (ana_cursor_info_pat_skel(ctx, skel1, seq, n, zp1, Hole)) {
      | Some(_) as result => result
      | None => ana_cursor_info_pat_skel(ctx, skel2, seq, n, zp1, Hole)
      }
    | Prod(ty1, ty2) =>
      let types = HTyp.get_tuple(ty1, ty2);
      let skels = UHPat.get_tuple(skel1, skel2);
      switch (ListMinTwo.zip_eq(skels, types)) {
      | None => None
      | Some(zipped) =>
        List.fold_left(
          (opt_result, skel_ty: (UHPat.skel_t, HTyp.t)) =>
            switch (opt_result) {
            | Some(_) as result => result
            | None =>
              let (skel, ty) = skel_ty;
              ana_cursor_info_pat_skel(ctx, skel, seq, n, zp1, ty);
            },
          None,
          ListMinTwo.to_list(zipped),
        )
      };
    | _ => None
    }
  | BinOp(InHole(WrongLength, _), Comma, skel1, skel2) =>
    switch (ty) {
    | Prod(ty1, ty2) =>
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
              ana_cursor_info_pat_skel(ctx, skel, seq, n, zp1, ty);
            },
          None,
          ListMinTwo.to_list(zipped),
        );
      switch (ana_zipped) {
      | Some(_) as result => result
      | None =>
        List.fold_left(
          (opt_result, skel) =>
            switch (opt_result) {
            | Some(_) as result => result
            | None => syn_cursor_info_pat_skel(ctx, skel, seq, n, zp1)
            },
          None,
          remainder,
        )
      };
    | _ => None
    }
  | BinOp(InHole(WrongLength, _), _, _, _) => None
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    switch (HTyp.matched_list(ty)) {
    | None => None
    | Some(ty_elt) =>
      switch (ana_cursor_info_pat_skel(ctx, skel1, seq, n, zp1, ty_elt)) {
      | Some(_) as result => result
      | None =>
        let ty_list = HTyp.List(ty_elt);
        ana_cursor_info_pat_skel(ctx, skel2, seq, n, zp1, ty_list);
      }
    }
  | BinOp(NotInHole, Space, _, _) =>
    syn_cursor_info_pat_skel(ctx, skel, seq, n, zp1)
  };

let rec ana_cursor_found_block =
        (
          ctx: Contexts.t,
          Block(lines, e): UHExp.block,
          ty: HTyp.t,
          side: cursor_side,
        )
        : option(cursor_mode) =>
  switch (Statics.syn_lines(ctx, lines)) {
  | None => None
  | Some(ctx) =>
    switch (ana_cursor_found_exp(ctx, e, ty, side)) {
    | None => None
    | Some(ci) =>
      let {mode, sort: _, side: _, ctx: _} = ci;
      Some(mode);
    }
  }
and ana_cursor_found_exp =
    (ctx: Contexts.t, e: UHExp.t, ty: HTyp.t, side: cursor_side): option(t) =>
  switch (e) {
  | Parenthesized(block) =>
    switch (ana_cursor_found_block(ctx, block, ty, side)) {
    | None => None
    | Some(mode) => Some(mk_cursor_info(mode, IsExpr(e), side, ctx))
    }
  | Tm(InHole(TypeInconsistent, _), _)
  | OpSeq(BinOp(InHole(TypeInconsistent, _), _, _, _), _) =>
    let e_nih = UHExp.set_err_status_t(NotInHole, e);
    switch (Statics.syn_exp(ctx, e_nih)) {
    | None => None
    | Some(ty') =>
      Some(
        mk_cursor_info(AnaTypeInconsistent(ty, ty'), IsExpr(e), side, ctx),
      )
    };
  | Tm(_, Var(InVHole(Keyword(k), _), _)) =>
    Some(mk_cursor_info(AnaKeyword(ty, k), IsExpr(e), side, ctx))
  | Tm(_, Var(InVHole(Free, _), _)) =>
    Some(mk_cursor_info(AnaFree(ty), IsExpr(e), side, ctx))
  | Tm(NotInHole, Case(_, _, _)) =>
    Some(mk_cursor_info(Analyzed(ty), IsExpr(e), side, ctx))
  | Tm(NotInHole, ListNil) =>
    Some(mk_cursor_info(Analyzed(ty), IsExpr(e), side, ctx))
  | Tm(InHole(WrongLength, _), _) => None
  | Tm(NotInHole, Lam(_, ann, _)) =>
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
              AnaAnnotatedLambda(ty, Arrow(ty1_ann, ty2)),
              IsExpr(e),
              side,
              ctx,
            ),
          )
        };
      | None => Some(mk_cursor_info(Analyzed(ty), IsExpr(e), side, ctx))
      }
    }
  | Tm(NotInHole, Inj(_, _)) =>
    Some(mk_cursor_info(Analyzed(ty), IsExpr(e), side, ctx))
  | OpSeq(BinOp(NotInHole, Comma, _, _), _)
  | OpSeq(BinOp(NotInHole, Cons, _, _), _) =>
    Some(mk_cursor_info(Analyzed(ty), IsExpr(e), side, ctx))
  | OpSeq(BinOp(InHole(WrongLength, _), Comma, skel1, skel2), _) =>
    switch (ty) {
    | Prod(ty1, ty2) =>
      let n_elts = ListMinTwo.length(UHExp.get_tuple(skel1, skel2));
      let n_types = ListMinTwo.length(HTyp.get_tuple(ty1, ty2));
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
  | OpSeq(BinOp(InHole(WrongLength, _), _, _, _), _) => None
  | OpSeq(Placeholder(_), _) => None
  | OpSeq(BinOp(NotInHole, Plus, _, _), _)
  | OpSeq(BinOp(NotInHole, Times, _, _), _)
  | OpSeq(BinOp(NotInHole, LessThan, _, _), _)
  | OpSeq(BinOp(NotInHole, Space, _, _), _)
  | EmptyHole(_)
  | Tm(NotInHole, Var(NotInVHole, _))
  | Tm(NotInHole, NumLit(_))
  | Tm(NotInHole, BoolLit(_))
  | Tm(NotInHole, ApPalette(_, _, _)) =>
    switch (Statics.syn_exp(ctx, e)) {
    | None => None
    | Some(ty') =>
      Some(mk_cursor_info(AnaSubsumed(ty, ty'), IsExpr(e), side, ctx))
    }
  };

let rec syn_cursor_info_block =
        (ctx: Contexts.t, zblock: ZExp.zblock): option(t) =>
  switch (zblock) {
  | BlockZL((prefix, zline, _), _) =>
    switch (Statics.syn_lines(ctx, prefix)) {
    | None => None
    | Some(ctx) => syn_cursor_info_line(ctx, zline)
    }
  | BlockZE(lines, ze) =>
    switch (Statics.syn_lines(ctx, lines)) {
    | None => None
    | Some(ctx) => syn_cursor_info(ctx, ze)
    }
  }
and syn_cursor_info_line = (ctx: Contexts.t, zli: ZExp.zline): option(t) =>
  switch (zli) {
  | CursorL(side, li) => Some(mk_cursor_info(Line, IsLine(li), side, ctx))
  | DeeperL(zli') => syn_cursor_info_line'(ctx, zli')
  }
and syn_cursor_info_line' = (ctx: Contexts.t, zli': ZExp.zline'): option(t) =>
  switch (zli') {
  | ExpLineZ(ze) => syn_cursor_info(ctx, ze)
  | LetLineZP(zp, ann, block) =>
    switch (ann) {
    | Some(uty1) =>
      let ty1 = UHTyp.expand(uty1);
      ana_cursor_info_pat(ctx, zp, ty1);
    | None =>
      switch (Statics.syn_block(ctx, block)) {
      | None => None
      | Some(ty1) => ana_cursor_info_pat(ctx, zp, ty1)
      }
    }
  | LetLineZA(_, _, _) =>
    Some(mk_cursor_info(TypePosition, IsType, Before, ctx))
  | LetLineZE(p, ann, zblock) =>
    switch (ann) {
    | Some(uty1) =>
      let ty1 = UHTyp.expand(uty1);
      let ctx1 = Statics.ctx_for_let(ctx, p, ty1, ZExp.erase_block(zblock));
      ana_cursor_info_block(ctx1, zblock, ty1);
    | None => syn_cursor_info_block(ctx, zblock)
    }
  }
and syn_cursor_info = (ctx: Contexts.t, ze: ZExp.t): option(t) =>
  switch (ze) {
  | CursorE(side, Tm(_, Var(InVHole(Keyword(k), _), _)) as e) =>
    Some(mk_cursor_info(SynKeyword(k), IsExpr(e), side, ctx))
  | CursorE(side, Tm(_, Var(InVHole(Free, _), _)) as e) =>
    Some(mk_cursor_info(SynFree, IsExpr(e), side, ctx))
  | CursorE(side, e) =>
    switch (Statics.syn_exp(ctx, e)) {
    | Some(ty) =>
      Some(mk_cursor_info(Synthesized(ty), IsExpr(e), side, ctx))
    | None => None
    }
  | ParenthesizedZ(zblock) => syn_cursor_info_block(ctx, zblock)
  | DeeperE(_, ze1') => syn_cursor_info'(ctx, ze1')
  | OpSeqZ(skel, ze0, surround) =>
    let e0 = ZExp.erase(ze0);
    let seq = OperatorSeq.opseq_of_exp_and_surround(e0, surround);
    let n = OperatorSeq.surround_prefix_length(surround);
    syn_cursor_info_skel(ctx, skel, seq, n, ze0);
  }
and syn_cursor_info' = (ctx: Contexts.t, ze: ZExp.t'): option(t) =>
  switch (ze) {
  | LamZP(zp, ann, _) =>
    let ty1 =
      switch (ann) {
      | Some(uty1) => UHTyp.expand(uty1)
      | None => Hole
      };
    ana_cursor_info_pat(ctx, zp, ty1);
  | LamZA(_, _, _) =>
    Some(mk_cursor_info(TypePosition, IsType, Before, ctx))
  | LamZE(p, ann, zblock) =>
    let ty1 =
      switch (ann) {
      | Some(uty1) => UHTyp.expand(uty1)
      | None => Hole
      };
    switch (Statics.ana_pat(ctx, p, ty1)) {
    | None => None
    | Some(ctx1) => syn_cursor_info_block(ctx1, zblock)
    };
  | InjZ(_, zblock) => syn_cursor_info_block(ctx, zblock)
  | CaseZE(_, _, None)
  | CaseZR(_, _, None) => None
  | CaseZE(zblock, _, Some(_)) => syn_cursor_info_block(ctx, zblock)
  | CaseZR(block, zrules, Some(uty)) =>
    let ty = UHTyp.expand(uty);
    switch (Statics.syn_block(ctx, block)) {
    | None => None
    | Some(ty1) =>
      let zrule = GeneralUtil.ZList.prj_z(zrules);
      ana_cursor_info_rule(ctx, zrule, ty1, ty);
    };
  | CaseZA(_, _, _) =>
    Some(mk_cursor_info(TypePosition, IsType, Before, ctx))
  | ApPaletteZ(_, _, zpsi) =>
    let (ty, zblock) = GeneralUtil.ZNatMap.prj_z_v(zpsi.zsplice_map);
    ana_cursor_info_block(ctx, zblock, ty);
  }
and ana_cursor_info_block =
    (ctx: Contexts.t, zblock: ZExp.zblock, ty: HTyp.t): option(t) =>
  switch (zblock) {
  | BlockZL((prefix, zline, _), _) =>
    switch (Statics.syn_lines(ctx, prefix)) {
    | None => None
    | Some(ctx) => syn_cursor_info_line(ctx, zline)
    }
  | BlockZE(lines, ze) =>
    switch (Statics.syn_lines(ctx, lines)) {
    | None => None
    | Some(ctx) => ana_cursor_info(ctx, ze, ty)
    }
  }
and ana_cursor_info = (ctx: Contexts.t, ze: ZExp.t, ty: HTyp.t): option(t) =>
  switch (ze) {
  | CursorE(side, e) => ana_cursor_found_exp(ctx, e, ty, side)
  | ParenthesizedZ(zblock) => ana_cursor_info_block(ctx, zblock, ty)
  | DeeperE(InHole(TypeInconsistent, _), ze1') =>
    syn_cursor_info'(ctx, ze1')
  | DeeperE(NotInHole, ze1') => ana_cursor_info'(ctx, ze1', ty)
  | DeeperE(InHole(WrongLength, _), _) => None
  | OpSeqZ(skel, ze0, surround) =>
    let e0 = ZExp.erase(ze0);
    let seq = OperatorSeq.opseq_of_exp_and_surround(e0, surround);
    let n = OperatorSeq.surround_prefix_length(surround);
    ana_cursor_info_skel(ctx, skel, seq, n, ze0, ty);
  }
and ana_cursor_info' = (ctx: Contexts.t, ze: ZExp.t', ty: HTyp.t): option(t) =>
  switch (ze) {
  | LamZP(zp, ann, _) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => None
    | Some((ty1_given, _)) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => ty1_given
        };
      ana_cursor_info_pat(ctx, zp, ty1);
    }
  | LamZA(_, _, _) =>
    Some(mk_cursor_info(TypePosition, IsType, Before, ctx))
  | LamZE(p, ann, zblock) =>
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
      | Some(ctx) => ana_cursor_info_block(ctx, zblock, ty2)
      };
    }
  | InjZ(side, zblock) =>
    switch (HTyp.matched_sum(ty)) {
    | None => None
    | Some((ty1, ty2)) =>
      ana_cursor_info_block(ctx, zblock, pick_side(side, ty1, ty2))
    }
  | CaseZE(zblock, _, _) => syn_cursor_info_block(ctx, zblock)
  | CaseZR(block, zrules, _) =>
    switch (Statics.syn_block(ctx, block)) {
    | None => None
    | Some(ty1) =>
      let zrule = ZList.prj_z(zrules);
      ana_cursor_info_rule(ctx, zrule, ty1, ty);
    }
  | CaseZA(_, _, _) =>
    Some(mk_cursor_info(TypePosition, IsType, Before, ctx))
  | ApPaletteZ(_, _, _) => syn_cursor_info'(ctx, ze)
  }
and ana_cursor_info_rule =
    (ctx: Contexts.t, zrule: ZExp.zrule, pat_ty: HTyp.t, clause_ty: HTyp.t)
    : option(t) =>
  switch (zrule) {
  | RuleZP(zp, _) => ana_cursor_info_pat(ctx, zp, pat_ty)
  | RuleZE(p, zblock) =>
    switch (Statics.ana_pat(ctx, p, pat_ty)) {
    | None => None
    | Some(ctx) => ana_cursor_info_block(ctx, zblock, clause_ty)
    }
  }
and syn_cursor_info_skel =
    (
      ctx: Contexts.t,
      skel: UHExp.skel_t,
      seq: UHExp.opseq,
      n: int,
      ze_n: ZExp.t,
    )
    : option(t) =>
  switch (skel) {
  | Placeholder(n') =>
    if (n == n') {
      syn_cursor_info(ctx, ze_n);
    } else {
      None;
    }
  | BinOp(_, Plus, skel1, skel2)
  | BinOp(_, Times, skel1, skel2)
  | BinOp(_, LessThan, skel1, skel2) =>
    switch (ana_cursor_info_skel(ctx, skel1, seq, n, ze_n, Num)) {
    | Some(_) as result => result
    | None =>
      switch (ana_cursor_info_skel(ctx, skel2, seq, n, ze_n, Num)) {
      | Some(_) as result => result
      | None => None
      }
    }
  | BinOp(_, Space, Placeholder(n') as skel1, skel2) =>
    if (n == n') {
      /*
       switch (ZExp.cursor_on_outer_expr(ze_n)) {
       | Some((Tm(InHole(TypeInconsistent, u), e_n') as e_n, side)) =>
         let e_n_nih = UHExp.set_err_status_t(NotInHole, e_n);
         switch (Statics.syn_exp(ctx, e_n_nih)) {
         | Some(ty) =>
           Some(
             mk_cursor_info(
               SynErrorArrow(Arrow(Hole, Hole), ty),
               IsExpr(e_n),
               side,
               ctx,
             ),
           )
         | None => None
         };
       | Some((Tm(_, Var(InVHole(Keyword(k), _), _)) as e_n, side)) =>
         Some(
           mk_cursor_info(
             SynKeywordArrow(Arrow(Hole, Hole), k),
             IsExpr(e_n),
             side,
             ctx,
           ),
         )
       | Some((Tm(_, Var(InVHole(Free, _), _)) as e_n, side)) =>
         Some(
           mk_cursor_info(
             SynFreeArrow(Arrow(Hole, Hole)),
             IsExpr(e_n),
             side,
             ctx,
           ),
         )
       | Some((e_n, side)) =>
         switch (Statics.syn_exp(ctx, e_n)) {
         | Some(ty) =>
           switch (HTyp.matched_arrow(ty)) {
           | Some((ty1, ty2)) =>
             Some(
               mk_cursor_info(
                 SynMatchingArrow(ty, Arrow(ty1, ty2)),
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
       */
      syn_cursor_info(
        ctx,
        ze_n,
      );
    } else {
      switch (Statics.syn_skel(ctx, skel1, seq, None)) {
      | None => None
      | Some((ty, _)) =>
        switch (HTyp.matched_arrow(ty)) {
        | Some((ty1, _)) =>
          ana_cursor_info_skel(ctx, skel2, seq, n, ze_n, ty1)
        | None => None
        }
      };
    }
  | BinOp(_, Space, skel1, skel2) =>
    switch (syn_cursor_info_skel(ctx, skel1, seq, n, ze_n)) {
    | Some(_) as result => result
    | None =>
      switch (Statics.syn_skel(ctx, skel1, seq, None)) {
      | None => None
      | Some((ty, _)) =>
        switch (HTyp.matched_arrow(ty)) {
        | None => None
        | Some((ty1, _)) =>
          ana_cursor_info_skel(ctx, skel2, seq, n, ze_n, ty1)
        }
      }
    }
  | BinOp(_, Comma, skel1, skel2) =>
    switch (syn_cursor_info_skel(ctx, skel1, seq, n, ze_n)) {
    | Some(_) as result => result
    | None => syn_cursor_info_skel(ctx, skel2, seq, n, ze_n)
    }
  | BinOp(_, Cons, skel1, skel2) =>
    switch (syn_cursor_info_skel(ctx, skel1, seq, n, ze_n)) {
    | Some(_) as result => result
    | None =>
      switch (Statics.syn_skel(ctx, skel1, seq, None)) {
      | None => None
      | Some((ty_elt, _)) =>
        let ty_list = HTyp.List(ty_elt);
        ana_cursor_info_skel(ctx, skel2, seq, n, ze_n, ty_list);
      }
    }
  }
and ana_cursor_info_skel =
    (
      ctx: Contexts.t,
      skel: UHExp.skel_t,
      seq: UHExp.opseq,
      n: int,
      ze_n: ZExp.t,
      ty: HTyp.t,
    )
    : option(t) =>
  switch (skel) {
  | Placeholder(n') =>
    if (n == n') {
      ana_cursor_info(ctx, ze_n, ty);
    } else {
      None;
    }
  | BinOp(InHole(TypeInconsistent, _), _, _, _) =>
    syn_cursor_info_skel(ctx, skel, seq, n, ze_n)
  | BinOp(NotInHole, Comma, skel1, skel2) =>
    switch (ty) {
    | Hole =>
      switch (ana_cursor_info_skel(ctx, skel1, seq, n, ze_n, Hole)) {
      | Some(_) as result => result
      | None => ana_cursor_info_skel(ctx, skel2, seq, n, ze_n, Hole)
      }
    | Prod(ty1, ty2) =>
      let types = HTyp.get_tuple(ty1, ty2);
      let skels = UHExp.get_tuple(skel1, skel2);
      switch (ListMinTwo.zip_eq(skels, types)) {
      | None => None
      | Some(zipped) =>
        List.fold_left(
          (opt_result, skel_ty: (UHExp.skel_t, HTyp.t)) =>
            switch (opt_result) {
            | Some(_) as result => result
            | None =>
              let (skel, ty) = skel_ty;
              ana_cursor_info_skel(ctx, skel, seq, n, ze_n, ty);
            },
          None,
          ListMinTwo.to_list(zipped),
        )
      };
    | _ => None
    }
  | BinOp(InHole(WrongLength, _), Comma, skel1, skel2) =>
    switch (ty) {
    | Prod(ty1, ty2) =>
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
              ana_cursor_info_skel(ctx, skel, seq, n, ze_n, ty);
            },
          None,
          ListMinTwo.to_list(zipped),
        );
      switch (ana_zipped) {
      | Some(_) as result => result
      | None =>
        List.fold_left(
          (opt_result, skel) =>
            switch (opt_result) {
            | Some(_) as result => result
            | None => syn_cursor_info_skel(ctx, skel, seq, n, ze_n)
            },
          None,
          remainder,
        )
      };
    | _ => None
    }
  | BinOp(InHole(WrongLength, _), _, _, _) => None
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    switch (HTyp.matched_list(ty)) {
    | None => None
    | Some(ty_elt) =>
      switch (ana_cursor_info_skel(ctx, skel1, seq, n, ze_n, ty_elt)) {
      | Some(_) as result => result
      | None =>
        let ty_list = HTyp.List(ty_elt);
        ana_cursor_info_skel(ctx, skel2, seq, n, ze_n, ty_list);
      }
    }
  | BinOp(_, Plus, _, _)
  | BinOp(_, Times, _, _)
  | BinOp(_, LessThan, _, _)
  | BinOp(_, Space, _, _) => syn_cursor_info_skel(ctx, skel, seq, n, ze_n)
  };
