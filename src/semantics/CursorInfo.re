open Sexplib.Std;
open SemanticsCommon;
open GeneralUtil;

[@deriving sexp]
type typed =
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
   * # cursor in type position
   */
  | OnType
  /* (we will have a richer structure here later) */
  | OnLine
  | OnRule
  | OnOp;

[@deriving sexp]
type node =
  | Typ(UHTyp.t)
  | Pat(UHPat.t)
  | Exp(UHExp.t)
  | Line(UHExp.line)
  | Rule(UHExp.rule);

[@deriving sexp]
type frame =
  | TypFrame(option(ZTyp.opseq_surround))
  | PatFrame(option(ZPat.opseq_surround))
  | ExpFrame(UHExp.lines, option(ZExp.opseq_surround), option(UHExp.block));

[@deriving sexp]
type term =
  | Type(UHTyp.t)
  | Pattern(UHPat.t)
  | Expression(UHExp.block);

[@deriving sexp]
type child_term = (child_index, term);

[@deriving sexp]
type t = {
  typed,
  node,
  frame,
  ctx: Contexts.t,
  position: cursor_position,
  node_steps: Path.steps,
  // not quite the term steps because we don't have an indexing
  // scheme into all semantic terms (e.g. subblock beginning with
  // a let line, subseq rooted at an op), so we overapproximate
  // (e.g. give the steps of the whole block, of the whole opseq)
  term_steps: Path.steps,
};

let mk_cursor_info =
    (typed, node, frame, position, ctx, node_steps, term_steps) => {
  typed,
  node,
  frame,
  position,
  ctx,
  node_steps,
  term_steps,
};

let update_node = (ci: t, node): t => {
  let {typed, node: _, frame, position, ctx, node_steps, term_steps} = ci;
  {typed, node, frame, position, ctx, node_steps, term_steps};
};

let update_position = (position, ci: t): t => {...ci, position};

let is_concluding_let_line = ci =>
  switch (ci.node, ci.frame) {
  | (
      Line(LetLine(_, _, _)),
      ExpFrame(_, None, Some(Block([], EmptyHole(_)))),
    ) =>
    true
  | (_, _) => false
  };

let force_get_exp_frame =
    (ci): (UHExp.lines, option(ZExp.opseq_surround), option(UHExp.block)) =>
  switch (ci.frame) {
  | TypFrame(_)
  | PatFrame(_) => assert(false)
  | ExpFrame(prefix, surround, suffix_block) => (
      prefix,
      surround,
      suffix_block,
    )
  };

let is_staging = ci =>
  switch (ci.position) {
  | Staging(_) => true
  | _ => false
  };

let is_before_node = ci =>
  switch (ci.node) {
  | Line(li) => ZExp.is_before_line(CursorL(ci.position, li))
  | Exp(e) => ZExp.is_before_exp(CursorE(ci.position, e))
  | Rule(rule) => ZExp.is_before_rule(CursorR(ci.position, rule))
  | Pat(p) => ZPat.is_before(CursorP(ci.position, p))
  | Typ(ty) => ZTyp.is_before(CursorT(ci.position, ty))
  };

let is_after_node = ci =>
  switch (ci.node) {
  | Line(li) => ZExp.is_after_line(CursorL(ci.position, li))
  | Exp(e) => ZExp.is_after_exp(CursorE(ci.position, e))
  | Rule(rule) => ZExp.is_after_rule(CursorR(ci.position, rule))
  | Pat(p) => ZPat.is_after(CursorP(ci.position, p))
  | Typ(ty) => ZTyp.is_after(CursorT(ci.position, ty))
  };

let staging_left_border = ci =>
  switch (ci.position, ci.node) {
  | (
      Staging(0),
      Line(LetLine(_, _, _)) |
      Exp(
        Inj(_, _, _) | Parenthesized(_) | Case(_, _, _, _) | Lam(_, _, _, _),
      ) |
      Pat(Inj(_, _, _) | Parenthesized(_)) |
      Typ(List(_) | Parenthesized(_)),
    ) =>
    true
  | _ => false
  };

let staging_right_border = ci =>
  switch (ci.position, ci.node) {
  | (Staging(3), Line(LetLine(_, _, _)))
  | (
      Staging(1),
      Exp(Inj(_, _, _) | Parenthesized(_) | Case(_, _, _, _)) |
      Pat(Inj(_, _, _) | Parenthesized(_)) |
      Typ(List(_) | Parenthesized(_)),
    ) =>
    true
  | _ => false
  };

let child_indices_of_current_node = ci =>
  switch (ci.node) {
  | Line(li) => UHExp.child_indices_line(li)
  | Exp(e) => UHExp.child_indices_exp(e)
  | Rule(rule) => UHExp.child_indices_rule(rule)
  | Pat(p) => UHPat.child_indices(p)
  | Typ(ty) => UHTyp.child_indices(ty)
  };

let preserved_child_term_of_node = ci =>
  switch (ci.node) {
  | Line(li) =>
    li
    |> UHExp.favored_child_of_line
    |> Opt.map_default(
         ~default=None,
         fun
         | (_, UHExp.Block([], EmptyHole(_))) => None
         | (i, block) => Some((i, Expression(block))),
       )
  | Exp(e) =>
    switch (e |> UHExp.favored_child_of_exp, ci.frame) {
    | (None, _) => None
    | (_, TypFrame(_) | PatFrame(_)) => None
    | (Some((_, Block([], EmptyHole(_)))), _) => None
    | (Some((i, block)), ExpFrame(_, None, _)) =>
      Some((i, Expression(block)))
    | (
        Some((i, Block([], OpSeq(_, _)) as block)),
        ExpFrame(_, Some(_surround), _),
      ) =>
      Some((i, Expression(block)))
    | (
        Some((i, Block([], conclusion) as block)),
        ExpFrame(_, Some(_surround), _),
      ) =>
      switch (e, conclusion |> UHExp.bidelimited) {
      | (Parenthesized(_), false) => None
      | _ => Some((i, Expression(block)))
      }
    | (Some((_, Block(_, _))), ExpFrame(_, Some(_surround), _)) => None
    }
  | Pat(p) =>
    p |> UHPat.favored_child |> Opt.map(((i, p)) => (i, Pattern(p)))
  | Typ(ty) =>
    ty |> UHTyp.favored_child |> Opt.map(((i, ty)) => (i, Type(ty)))
  | Rule(_) => None
  };

let rec cursor_info_typ =
        (
          ~node_steps: Path.steps,
          ~term_steps: Path.steps,
          ~frame: option(ZTyp.opseq_surround)=?,
          ctx: Contexts.t,
          zty: ZTyp.t,
        )
        : option(t) =>
  switch (zty) {
  | CursorT(cursor, ty) =>
    Some(
      mk_cursor_info(
        OnType,
        Typ(ty),
        TypFrame(frame),
        cursor,
        ctx,
        node_steps,
        term_steps,
      ),
    )
  | ParenthesizedZ(zty1)
  | ListZ(zty1) => cursor_info_typ(~node_steps, ~term_steps, ctx, zty1)
  | OpSeqZ(_, zty1, surround) =>
    cursor_info_typ(~node_steps, ~term_steps, ~frame=surround, ctx, zty1)
  };

let rec _ana_cursor_found_pat =
        (ctx: Contexts.t, p: UHPat.t, ty: HTyp.t)
        : option((typed, node, Contexts.t)) =>
  switch (p) {
  /* in hole */
  | EmptyHole(_) => Some((PatAnaSubsumed(ty, Hole), Pat(p), ctx))
  | Wild(InHole(TypeInconsistent, _))
  | Var(InHole(TypeInconsistent, _), _, _)
  | NumLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | Inj(InHole(TypeInconsistent, _), _, _)
  | OpSeq(BinOp(InHole(TypeInconsistent, _), _, _, _), _) =>
    let p_nih = UHPat.set_err_status_t(NotInHole, p);
    switch (Statics.syn_pat(ctx, p_nih)) {
    | None => None
    | Some((ty', _)) =>
      Some((PatAnaTypeInconsistent(ty, ty'), Pat(p), ctx))
    };
  | Wild(InHole(WrongLength, _))
  | Var(InHole(WrongLength, _), _, _)
  | NumLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _))
  | Inj(InHole(WrongLength, _), _, _) => None
  /* not in hole */
  | Var(NotInHole, InVHole(Keyword(k), _), _) =>
    Some((PatAnaKeyword(ty, k), Pat(p), ctx))
  | Wild(NotInHole)
  | Var(NotInHole, _, _)
  | ListNil(NotInHole) => Some((PatAnalyzed(ty), Pat(p), ctx))
  | NumLit(NotInHole, _) => Some((PatAnaSubsumed(ty, Num), Pat(p), ctx))
  | BoolLit(NotInHole, _) => Some((PatAnaSubsumed(ty, Bool), Pat(p), ctx))
  | Inj(NotInHole, _, _) => Some((PatAnalyzed(ty), Pat(p), ctx))
  | Parenthesized(p1) =>
    switch (_ana_cursor_found_pat(ctx, p1, ty)) {
    | None => None
    | Some((mode, _, ctx)) => Some((mode, Pat(p), ctx))
    }
  | OpSeq(BinOp(NotInHole, Comma, _, _), _)
  | OpSeq(BinOp(NotInHole, Cons, _, _), _) =>
    Some((PatAnalyzed(ty), Pat(p), ctx))
  | OpSeq(BinOp(InHole(reason, _), Comma, skel1, skel2), _) =>
    switch (ty, reason) {
    | (Prod(ty1, ty2), WrongLength) =>
      let n_elts = ListMinTwo.length(UHPat.get_tuple(skel1, skel2));
      let n_types = ListMinTwo.length(HTyp.get_tuple(ty1, ty2));
      Some((PatAnaWrongLength(n_types, n_elts, ty), Pat(p), ctx));
    | (_, TypeInconsistent) =>
      let n_elts = ListMinTwo.length(UHPat.get_tuple(skel1, skel2));
      Some((PatAnaWrongLength(1, n_elts, ty), Pat(p), ctx));
    | (_, _) => None
    }
  | OpSeq(BinOp(InHole(_, _), Cons, _, _), _) => None
  | OpSeq(Placeholder(_), _) => None
  | OpSeq(BinOp(_, Space, _, _), _) => None
  };

let ana_cursor_found_pat =
    (
      ~node_steps,
      ~term_steps,
      ~frame,
      ctx: Contexts.t,
      p: UHPat.t,
      ty: HTyp.t,
      cursor: cursor_position,
    )
    : option(t) =>
  switch (_ana_cursor_found_pat(ctx, p, ty)) {
  | None => None
  | Some((typed, node, ctx)) =>
    Some(
      mk_cursor_info(
        typed,
        node,
        PatFrame(frame),
        cursor,
        ctx,
        node_steps,
        term_steps,
      ),
    )
  };

let rec _syn_cursor_info_pat =
        (~node_steps, ~term_steps, ~frame=None, ctx: Contexts.t, zp: ZPat.t)
        : option(t) =>
  switch (zp) {
  // TODO special case OpSeq
  | CursorP(cursor, Var(_, InVHole(Keyword(k), _), _) as p) =>
    Some(
      mk_cursor_info(
        PatSynKeyword(k),
        Pat(p),
        PatFrame(frame),
        cursor,
        ctx,
        node_steps,
        term_steps,
      ),
    )
  | CursorP(cursor, p) =>
    switch (Statics.syn_pat(ctx, p)) {
    | None => None
    | Some((ty, _)) =>
      Some(
        mk_cursor_info(
          PatSynthesized(ty),
          Pat(p),
          PatFrame(frame),
          cursor,
          ctx,
          node_steps,
          term_steps,
        ),
      )
    }
  | InjZ(_, _, zp1) =>
    _syn_cursor_info_pat(~node_steps, ~term_steps, ctx, zp1)
  | ParenthesizedZ(zp1) =>
    _syn_cursor_info_pat(~node_steps, ~term_steps, ctx, zp1)
  | OpSeqZ(skel, zp1, surround) =>
    let p1 = ZPat.erase(zp1);
    let seq = OperatorSeq.opseq_of_exp_and_surround(p1, surround);
    let n = OperatorSeq.surround_prefix_length(surround);
    _syn_cursor_info_pat_skel(
      ~node_steps,
      ~term_steps,
      ~frame=Some(surround),
      ctx,
      skel,
      seq,
      n,
      zp1,
    );
  }
and _syn_cursor_info_pat_skel =
    (
      ~node_steps,
      ~term_steps,
      ~frame: option(ZPat.opseq_surround),
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
      _syn_cursor_info_pat(~node_steps, ~term_steps, ctx, zp1);
    } else {
      None;
    }
  | BinOp(_, Comma, skel1, skel2) =>
    switch (
      _syn_cursor_info_pat_skel(
        ~node_steps,
        ~term_steps,
        ~frame,
        ctx,
        skel1,
        seq,
        n,
        zp1,
      )
    ) {
    | Some(_) as result => result
    | None =>
      _syn_cursor_info_pat_skel(
        ~node_steps,
        ~term_steps,
        ~frame,
        ctx,
        skel2,
        seq,
        n,
        zp1,
      )
    }
  | BinOp(_, Space, skel1, skel2) =>
    switch (
      _ana_cursor_info_pat_skel(
        ~node_steps,
        ~term_steps,
        ~frame,
        ctx,
        skel1,
        seq,
        n,
        zp1,
        HTyp.Hole,
      )
    ) {
    | Some(_) as result => result
    | None =>
      _ana_cursor_info_pat_skel(
        ~node_steps,
        ~term_steps,
        ~frame,
        ctx,
        skel2,
        seq,
        n,
        zp1,
        Hole,
      )
    }
  | BinOp(_, Cons, skel1, skel2) =>
    switch (
      _syn_cursor_info_pat_skel(
        ~node_steps,
        ~term_steps,
        ~frame,
        ctx,
        skel1,
        seq,
        n,
        zp1,
      )
    ) {
    | Some(_) as result => result
    | None =>
      switch (Statics.syn_skel_pat(ctx, skel1, seq, None)) {
      | None => None
      | Some((ty_elt, ctx, _)) =>
        let list_ty = HTyp.List(ty_elt);
        _ana_cursor_info_pat_skel(
          ~node_steps,
          ~term_steps,
          ~frame,
          ctx,
          skel2,
          seq,
          n,
          zp1,
          list_ty,
        );
      }
    }
  }
and _ana_cursor_info_pat =
    (
      ~node_steps,
      ~term_steps,
      ~frame=None,
      ctx: Contexts.t,
      zp: ZPat.t,
      ty: HTyp.t,
    )
    : option(t) =>
  switch (zp) {
  /* TODO special case OpSeq */
  | CursorP(cursor, Var(_, InVHole(Keyword(k), _), _) as p) =>
    Some(
      mk_cursor_info(
        PatAnaKeyword(ty, k),
        Pat(p),
        PatFrame(frame),
        cursor,
        ctx,
        node_steps,
        term_steps,
      ),
    )
  | CursorP(cursor, p) =>
    ana_cursor_found_pat(~node_steps, ~term_steps, ~frame, ctx, p, ty, cursor)
  | InjZ(InHole(WrongLength, _), _, _) => None
  | InjZ(InHole(TypeInconsistent, _), _, _) =>
    _syn_cursor_info_pat(~node_steps, ~term_steps, ctx, zp)
  | InjZ(NotInHole, position, zp1) =>
    switch (HTyp.matched_sum(ty)) {
    | None => None
    | Some((tyL, tyR)) =>
      let ty1 = pick_side(position, tyL, tyR);
      _ana_cursor_info_pat(~node_steps, ~term_steps, ctx, zp1, ty1);
    }
  | OpSeqZ(skel, zp1, surround) =>
    let p1 = ZPat.erase(zp1);
    let seq = OperatorSeq.opseq_of_exp_and_surround(p1, surround);
    let n = OperatorSeq.surround_prefix_length(surround);
    _ana_cursor_info_pat_skel(
      ~node_steps,
      ~term_steps,
      ~frame=Some(surround),
      ctx,
      skel,
      seq,
      n,
      zp1,
      ty,
    );
  | ParenthesizedZ(zp) =>
    _ana_cursor_info_pat(~node_steps, ~term_steps, ctx, zp, ty)
  }
and _ana_cursor_info_pat_skel =
    (
      ~node_steps,
      ~term_steps,
      ~frame: option(ZPat.opseq_surround),
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
      _ana_cursor_info_pat(~node_steps, ~term_steps, ~frame, ctx, zp1, ty);
    } else {
      None;
    }
  | BinOp(InHole(TypeInconsistent, _), _, _, _) =>
    _syn_cursor_info_pat_skel(
      ~node_steps,
      ~term_steps,
      ~frame,
      ctx,
      skel,
      seq,
      n,
      zp1,
    )
  | BinOp(NotInHole, Comma, skel1, skel2) =>
    switch (ty) {
    | Hole =>
      switch (
        _ana_cursor_info_pat_skel(
          ~node_steps,
          ~term_steps,
          ~frame,
          ctx,
          skel1,
          seq,
          n,
          zp1,
          Hole,
        )
      ) {
      | Some(_) as result => result
      | None =>
        _ana_cursor_info_pat_skel(
          ~node_steps,
          ~term_steps,
          ~frame,
          ctx,
          skel2,
          seq,
          n,
          zp1,
          Hole,
        )
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
              _ana_cursor_info_pat_skel(
                ~node_steps,
                ~term_steps,
                ~frame,
                ctx,
                skel,
                seq,
                n,
                zp1,
                ty,
              );
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
              _ana_cursor_info_pat_skel(
                ~node_steps,
                ~term_steps,
                ~frame,
                ctx,
                skel,
                seq,
                n,
                zp1,
                ty,
              );
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
            | None =>
              _syn_cursor_info_pat_skel(
                ~node_steps,
                ~term_steps,
                ~frame,
                ctx,
                skel,
                seq,
                n,
                zp1,
              )
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
      switch (
        _ana_cursor_info_pat_skel(
          ~node_steps,
          ~term_steps,
          ~frame,
          ctx,
          skel1,
          seq,
          n,
          zp1,
          ty_elt,
        )
      ) {
      | Some(_) as result => result
      | None =>
        let ty_list = HTyp.List(ty_elt);
        _ana_cursor_info_pat_skel(
          ~node_steps,
          ~term_steps,
          ~frame,
          ctx,
          skel2,
          seq,
          n,
          zp1,
          ty_list,
        );
      }
    }
  | BinOp(NotInHole, Space, _, _) =>
    _syn_cursor_info_pat_skel(
      ~node_steps,
      ~term_steps,
      ~frame,
      ctx,
      skel,
      seq,
      n,
      zp1,
    )
  };

let rec _ana_cursor_found_block =
        (ctx: Contexts.t, Block(lines, e): UHExp.block, ty: HTyp.t)
        : option(typed) =>
  switch (Statics.syn_lines(ctx, lines)) {
  | None => None
  | Some(ctx) =>
    switch (_ana_cursor_found_exp(ctx, e, ty)) {
    | None => None
    | Some((typed, _, _)) => Some(typed)
    }
  }
and _ana_cursor_found_exp =
    (ctx: Contexts.t, e: UHExp.t, ty: HTyp.t)
    : option((typed, node, Contexts.t)) =>
  switch (e) {
  /* in hole */
  | Var(InHole(TypeInconsistent, _), _, _)
  | NumLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | Lam(InHole(TypeInconsistent, _), _, _, _)
  | Inj(InHole(TypeInconsistent, _), _, _)
  | Case(InHole(TypeInconsistent, _), _, _, _)
  | ApPalette(InHole(TypeInconsistent, _), _, _, _)
  | OpSeq(BinOp(InHole(TypeInconsistent, _), _, _, _), _) =>
    let e_nih = UHExp.set_err_status_t(NotInHole, e);
    switch (Statics.syn_exp(ctx, e_nih)) {
    | None => None
    | Some(ty') => Some((AnaTypeInconsistent(ty, ty'), Exp(e), ctx))
    };
  | Var(InHole(WrongLength, _), _, _)
  | NumLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _)) => None
  | Lam(InHole(WrongLength, _), _, _, _)
  | Inj(InHole(WrongLength, _), _, _)
  | Case(InHole(WrongLength, _), _, _, _)
  | ApPalette(InHole(WrongLength, _), _, _, _) => None
  | OpSeq(BinOp(InHole(reason, _), Comma, skel1, skel2), _) =>
    switch (ty, reason) {
    | (Prod(ty1, ty2), WrongLength) =>
      let n_elts = ListMinTwo.length(UHExp.get_tuple(skel1, skel2));
      let n_types = ListMinTwo.length(HTyp.get_tuple(ty1, ty2));
      Some((AnaWrongLength(n_types, n_elts, ty), Exp(e), ctx));
    | (_, TypeInconsistent) =>
      let n_elts = ListMinTwo.length(UHExp.get_tuple(skel1, skel2));
      Some((AnaWrongLength(1, n_elts, ty), Exp(e), ctx));
    | _ => None
    }
  | OpSeq(BinOp(InHole(WrongLength, _), _, _, _), _) => None
  /* not in hole */
  | Var(_, InVHole(Keyword(k), _), _) =>
    Some((AnaKeyword(ty, k), Exp(e), ctx))
  | Var(_, InVHole(Free, _), _) => Some((AnaFree(ty), Exp(e), ctx))
  | ListNil(NotInHole) => Some((Analyzed(ty), Exp(e), ctx))
  | EmptyHole(_)
  | Var(NotInHole, NotInVHole, _)
  | NumLit(NotInHole, _)
  | BoolLit(NotInHole, _) =>
    switch (Statics.syn_exp(ctx, e)) {
    | None => None
    | Some(ty') => Some((AnaSubsumed(ty, ty'), Exp(e), ctx))
    }
  | Case(NotInHole, _, _, _) => Some((Analyzed(ty), Exp(e), ctx))
  | Lam(NotInHole, _, ann, _) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => None
    | Some((ty1_given, ty2)) =>
      switch (ann) {
      | Some(uty1) =>
        let ty1_ann = UHTyp.expand(uty1);
        switch (HTyp.consistent(ty1_ann, ty1_given)) {
        | false => None
        | true =>
          Some((AnaAnnotatedLambda(ty, Arrow(ty1_ann, ty2)), Exp(e), ctx))
        };
      | None => Some((Analyzed(ty), Exp(e), ctx))
      }
    }
  | Inj(NotInHole, _, _) => Some((Analyzed(ty), Exp(e), ctx))
  | OpSeq(BinOp(NotInHole, Comma, _, _), _)
  | OpSeq(BinOp(NotInHole, Cons, _, _), _) =>
    Some((Analyzed(ty), Exp(e), ctx))
  | OpSeq(Placeholder(_), _) => None
  | OpSeq(BinOp(NotInHole, And | Or, _, _), _)
  | OpSeq(BinOp(NotInHole, Minus, _, _), _)
  | OpSeq(BinOp(NotInHole, Plus, _, _), _)
  | OpSeq(BinOp(NotInHole, Times, _, _), _)
  | OpSeq(BinOp(NotInHole, LessThan, _, _), _)
  | OpSeq(BinOp(NotInHole, Space, _, _), _)
  | ApPalette(NotInHole, _, _, _) =>
    switch (Statics.syn_exp(ctx, e)) {
    | None => None
    | Some(ty') => Some((AnaSubsumed(ty, ty'), Exp(e), ctx))
    }
  | Parenthesized(block) =>
    switch (_ana_cursor_found_block(ctx, block, ty)) {
    | None => None
    | Some(typed) => Some((typed, Exp(e), ctx))
    }
  };

let ana_cursor_found_exp =
    (
      ~node_steps,
      ~term_steps,
      ~frame,
      ctx: Contexts.t,
      e: UHExp.t,
      ty: HTyp.t,
      cursor: cursor_position,
    )
    : option(t) =>
  switch (_ana_cursor_found_exp(ctx, e, ty)) {
  | None => None
  | Some((mode, sort, ctx)) =>
    let (prefix, surround, suffix) = frame;
    Some(
      mk_cursor_info(
        mode,
        sort,
        ExpFrame(prefix, surround, suffix),
        cursor,
        ctx,
        node_steps,
        term_steps,
      ),
    );
  };

let rec _syn_cursor_info_block =
        (~node_steps, ~term_steps, ctx: Contexts.t, zblock: ZExp.zblock)
        : option(t) =>
  switch (zblock) {
  | BlockZL((prefix, zline, suffix), conclusion) =>
    switch (Statics.syn_lines(ctx, prefix)) {
    | None => None
    | Some(ctx) =>
      _syn_cursor_info_line(
        ~node_steps,
        ~term_steps,
        ~frame=(prefix, None, Some(UHExp.Block(suffix, conclusion))),
        ctx,
        zline,
      )
    }
  | BlockZE(lines, ze) =>
    switch (Statics.syn_lines(ctx, lines)) {
    | None => None
    | Some(ctx) =>
      _syn_cursor_info(
        ~node_steps,
        ~term_steps,
        ~frame=(lines, None, None),
        ctx,
        ze,
      )
    }
  }
and _syn_cursor_info_line =
    (~node_steps, ~term_steps, ~frame, ctx: Contexts.t, zli: ZExp.zline)
    : option(t) =>
  switch (zli) {
  | CursorL(cursor, line) =>
    let (prefix, _, suffix) = frame;
    Some(
      mk_cursor_info(
        OnLine,
        Line(line),
        ExpFrame(prefix, None, suffix),
        cursor,
        ctx,
        node_steps,
        term_steps,
      ),
    );
  | ExpLineZ(ze) =>
    _syn_cursor_info(~node_steps, ~term_steps, ~frame, ctx, ze)
  | LetLineZP(zp, ann, block) =>
    switch (ann) {
    | Some(uty1) =>
      let ty1 = UHTyp.expand(uty1);
      _ana_cursor_info_pat(~node_steps, ~term_steps, ctx, zp, ty1);
    | None =>
      switch (Statics.syn_block(ctx, block)) {
      | None => None
      | Some(ty1) =>
        _ana_cursor_info_pat(~node_steps, ~term_steps, ctx, zp, ty1)
      }
    }
  | LetLineZA(_, zann, _) =>
    cursor_info_typ(~node_steps, ~term_steps, ctx, zann)
  | LetLineZE(p, ann, zblock) =>
    switch (ann) {
    | Some(uty1) =>
      let ty1 = UHTyp.expand(uty1);
      let ctx1 = Statics.ctx_for_let(ctx, p, ty1, ZExp.erase_block(zblock));
      _ana_cursor_info_block(~node_steps, ~term_steps, ctx1, zblock, ty1);
    | None => _syn_cursor_info_block(~node_steps, ~term_steps, ctx, zblock)
    }
  }
and _syn_cursor_info =
    (~node_steps, ~term_steps, ~frame, ctx: Contexts.t, ze: ZExp.t)
    : option(t) => {
  let (prefix, surround, suffix) = frame;
  switch (ze) {
  | CursorE(cursor, Var(_, InVHole(Keyword(k), _), _) as e) =>
    Some(
      mk_cursor_info(
        SynKeyword(k),
        Exp(e),
        ExpFrame(prefix, surround, suffix),
        cursor,
        ctx,
        node_steps,
        term_steps,
      ),
    )
  | CursorE(cursor, Var(_, InVHole(Free, _), _) as e) =>
    Some(
      mk_cursor_info(
        SynFree,
        Exp(e),
        ExpFrame(prefix, surround, suffix),
        cursor,
        ctx,
        node_steps,
        term_steps,
      ),
    )
  | CursorE(cursor, e) =>
    switch (Statics.syn_exp(ctx, e)) {
    | None => None
    | Some(ty) =>
      Some(
        mk_cursor_info(
          Synthesized(ty),
          Exp(e),
          ExpFrame(prefix, surround, suffix),
          cursor,
          ctx,
          node_steps,
          term_steps,
        ),
      )
    }
  | OpSeqZ(skel, ze0, surround) =>
    let e0 = ZExp.erase(ze0);
    let seq = OperatorSeq.opseq_of_exp_and_surround(e0, surround);
    let n = OperatorSeq.surround_prefix_length(surround);
    _syn_cursor_info_skel(
      ~node_steps,
      ~term_steps,
      ~frame=(prefix, Some(surround), suffix),
      ctx,
      skel,
      seq,
      n,
      ze0,
    );
  | ParenthesizedZ(zblock) =>
    _syn_cursor_info_block(~node_steps, ~term_steps, ctx, zblock)
  | LamZP(_, zp, ann, _) =>
    let ty1 =
      switch (ann) {
      | Some(uty1) => UHTyp.expand(uty1)
      | None => Hole
      };
    _ana_cursor_info_pat(~node_steps, ~term_steps, ctx, zp, ty1);
  | LamZA(_, _, zann, _) =>
    cursor_info_typ(~node_steps, ~term_steps, ctx, zann)
  | LamZE(_, p, ann, zblock) =>
    let ty1 =
      switch (ann) {
      | Some(uty1) => UHTyp.expand(uty1)
      | None => Hole
      };
    switch (Statics.ana_pat(ctx, p, ty1)) {
    | None => None
    | Some(ctx1) =>
      _syn_cursor_info_block(~node_steps, ~term_steps, ctx1, zblock)
    };
  | InjZ(_, _, zblock) =>
    _syn_cursor_info_block(~node_steps, ~term_steps, ctx, zblock)
  | CaseZE(_, _, _, None)
  | CaseZR(_, _, _, None) => None
  | CaseZE(_, zblock, _, Some(_)) =>
    _syn_cursor_info_block(~node_steps, ~term_steps, ctx, zblock)
  | CaseZR(_, block, zrules, Some(uty)) =>
    let ty = UHTyp.expand(uty);
    switch (Statics.syn_block(ctx, block)) {
    | None => None
    | Some(ty1) =>
      let zrule = GeneralUtil.ZList.prj_z(zrules);
      _ana_cursor_info_rule(~node_steps, ~term_steps, ctx, zrule, ty1, ty);
    };
  | CaseZA(_, _, _, zann) =>
    cursor_info_typ(~node_steps, ~term_steps, ctx, zann)
  | ApPaletteZ(_, _, _, zpsi) =>
    let (ty, zblock) = GeneralUtil.ZNatMap.prj_z_v(zpsi.zsplice_map);
    _ana_cursor_info_block(~node_steps, ~term_steps, ctx, zblock, ty);
  };
}
and _ana_cursor_info_block =
    (
      ~node_steps,
      ~term_steps,
      ctx: Contexts.t,
      zblock: ZExp.zblock,
      ty: HTyp.t,
    )
    : option(t) =>
  switch (zblock) {
  | BlockZL((prefix, zline, suffix), conclusion) =>
    switch (Statics.syn_lines(ctx, prefix)) {
    | None => None
    | Some(ctx) =>
      _syn_cursor_info_line(
        ~node_steps,
        ~term_steps,
        ~frame=(prefix, None, Some(Block(suffix, conclusion))),
        ctx,
        zline,
      )
    }
  | BlockZE(lines, ze) =>
    switch (Statics.syn_lines(ctx, lines)) {
    | None => None
    | Some(ctx) =>
      _ana_cursor_info(
        ~node_steps,
        ~term_steps,
        ~frame=(lines, None, None),
        ctx,
        ze,
        ty,
      )
    }
  }
and _ana_cursor_info =
    (
      ~node_steps,
      ~term_steps,
      ~frame,
      ctx: Contexts.t,
      ze: ZExp.t,
      ty: HTyp.t,
    )
    : option(t) =>
  switch (ze) {
  | CursorE(cursor, e) =>
    ana_cursor_found_exp(~node_steps, ~term_steps, ~frame, ctx, e, ty, cursor)
  /* zipper cases */
  | ParenthesizedZ(zblock) =>
    _ana_cursor_info_block(~node_steps, ~term_steps, ctx, zblock, ty)
  | OpSeqZ(skel, ze0, surround) =>
    let e0 = ZExp.erase(ze0);
    let seq = OperatorSeq.opseq_of_exp_and_surround(e0, surround);
    let n = OperatorSeq.surround_prefix_length(surround);
    let (prefix, _, suffix) = frame;
    _ana_cursor_info_skel(
      ~node_steps,
      ~term_steps,
      ~frame=(prefix, Some(surround), suffix),
      ctx,
      skel,
      seq,
      n,
      ze0,
      ty,
    );
  /* zipper in hole */
  | LamZP(InHole(WrongLength, _), _, _, _)
  | LamZA(InHole(WrongLength, _), _, _, _)
  | LamZE(InHole(WrongLength, _), _, _, _)
  | InjZ(InHole(WrongLength, _), _, _)
  | CaseZE(InHole(WrongLength, _), _, _, _)
  | CaseZR(InHole(WrongLength, _), _, _, _)
  | CaseZA(InHole(WrongLength, _), _, _, _)
  | ApPaletteZ(InHole(WrongLength, _), _, _, _) => None
  | LamZP(InHole(TypeInconsistent, _), _, _, _)
  | LamZA(InHole(TypeInconsistent, _), _, _, _)
  | LamZE(InHole(TypeInconsistent, _), _, _, _)
  | InjZ(InHole(TypeInconsistent, _), _, _)
  | CaseZE(InHole(TypeInconsistent, _), _, _, _)
  | CaseZR(InHole(TypeInconsistent, _), _, _, _)
  | CaseZA(InHole(TypeInconsistent, _), _, _, _)
  | ApPaletteZ(InHole(TypeInconsistent, _), _, _, _) =>
    _syn_cursor_info(~node_steps, ~term_steps, ~frame, ctx, ze)
  /* zipper not in hole */
  | LamZP(NotInHole, zp, ann, _) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => None
    | Some((ty1_given, _)) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => ty1_given
        };
      _ana_cursor_info_pat(~node_steps, ~term_steps, ctx, zp, ty1);
    }
  | LamZA(NotInHole, _, zann, _) =>
    cursor_info_typ(~node_steps, ~term_steps, ctx, zann)
  | LamZE(NotInHole, p, ann, zblock) =>
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
        _ana_cursor_info_block(~node_steps, ~term_steps, ctx, zblock, ty2)
      };
    }
  | InjZ(NotInHole, position, zblock) =>
    switch (HTyp.matched_sum(ty)) {
    | None => None
    | Some((ty1, ty2)) =>
      _ana_cursor_info_block(
        ~node_steps,
        ~term_steps,
        ctx,
        zblock,
        pick_side(position, ty1, ty2),
      )
    }
  | CaseZE(NotInHole, zblock, _, _) =>
    _syn_cursor_info_block(~node_steps, ~term_steps, ctx, zblock)
  | CaseZR(NotInHole, block, zrules, _) =>
    switch (Statics.syn_block(ctx, block)) {
    | None => None
    | Some(ty1) =>
      let zrule = ZList.prj_z(zrules);
      _ana_cursor_info_rule(~node_steps, ~term_steps, ctx, zrule, ty1, ty);
    }
  | CaseZA(NotInHole, _, _, zann) =>
    cursor_info_typ(~node_steps, ~term_steps, ctx, zann)
  | ApPaletteZ(NotInHole, _, _, _) =>
    _syn_cursor_info(~node_steps, ~term_steps, ~frame, ctx, ze)
  }
and _ana_cursor_info_rule =
    (
      ~node_steps,
      ~term_steps,
      ctx: Contexts.t,
      zrule: ZExp.zrule,
      pat_ty: HTyp.t,
      clause_ty: HTyp.t,
    )
    : option(t) =>
  switch (zrule) {
  | CursorR(cursor, rule) =>
    Some(
      mk_cursor_info(
        OnRule,
        Rule(rule),
        ExpFrame([], None, None),
        cursor,
        ctx,
        node_steps,
        term_steps,
      ),
    )
  | RuleZP(zp, _) =>
    _ana_cursor_info_pat(~node_steps, ~term_steps, ctx, zp, pat_ty)
  | RuleZE(p, zblock) =>
    switch (Statics.ana_pat(ctx, p, pat_ty)) {
    | None => None
    | Some(ctx) =>
      _ana_cursor_info_block(~node_steps, ~term_steps, ctx, zblock, clause_ty)
    }
  }
and _syn_cursor_info_skel =
    (
      ~node_steps,
      ~term_steps,
      ~frame,
      ctx: Contexts.t,
      skel: UHExp.skel_t,
      seq: UHExp.opseq,
      n: int,
      ze_n: ZExp.t,
    )
    : option(t) => {
  switch (skel) {
  | Placeholder(n') =>
    if (n == n') {
      _syn_cursor_info(~node_steps, ~term_steps, ~frame, ctx, ze_n);
    } else {
      None;
    }
  | BinOp(_, Minus, skel1, skel2)
  | BinOp(_, Plus, skel1, skel2)
  | BinOp(_, Times, skel1, skel2)
  | BinOp(_, LessThan, skel1, skel2) =>
    switch (
      _ana_cursor_info_skel(
        ~node_steps,
        ~term_steps,
        ~frame,
        ctx,
        skel1,
        seq,
        n,
        ze_n,
        Num,
      )
    ) {
    | Some(_) as result => result
    | None =>
      switch (
        _ana_cursor_info_skel(
          ~node_steps,
          ~term_steps,
          ~frame,
          ctx,
          skel2,
          seq,
          n,
          ze_n,
          Num,
        )
      ) {
      | Some(_) as result => result
      | None => None
      }
    }
  | BinOp(_, And | Or, skel1, skel2) =>
    switch (
      _ana_cursor_info_skel(
        ~node_steps,
        ~term_steps,
        ~frame,
        ctx,
        skel1,
        seq,
        n,
        ze_n,
        Bool,
      )
    ) {
    | Some(_) as result => result
    | None =>
      switch (
        _ana_cursor_info_skel(
          ~node_steps,
          ~term_steps,
          ~frame,
          ctx,
          skel2,
          seq,
          n,
          ze_n,
          Bool,
        )
      ) {
      | Some(_) as result => result
      | None => None
      }
    }
  | BinOp(_, Space, Placeholder(n') as skel1, skel2) =>
    if (n == n') {
      switch (_syn_cursor_info(~node_steps, ~term_steps, ~frame, ctx, ze_n)) {
      | None => None
      | Some(ci) =>
        switch (ZExp.cursor_on_outer_expr(ze_n)) {
        | Some((
            Block(_, Var(InHole(TypeInconsistent, _), _, _)) as outer_block,
            _position,
          ))
        | Some((
            Block(_, NumLit(InHole(TypeInconsistent, _), _)) as outer_block,
            _position,
          ))
        | Some((
            Block(_, BoolLit(InHole(TypeInconsistent, _), _)) as outer_block,
            _position,
          ))
        | Some((
            Block(_, ListNil(InHole(TypeInconsistent, _))) as outer_block,
            _position,
          ))
        | Some((
            Block(_, Lam(InHole(TypeInconsistent, _), _, _, _)) as outer_block,
            _position,
          ))
        | Some((
            Block(_, Inj(InHole(TypeInconsistent, _), _, _)) as outer_block,
            _position,
          ))
        | Some((
            Block(_, Case(InHole(TypeInconsistent, _), _, _, _)) as outer_block,
            _position,
          ))
        | Some((
            Block(_, ApPalette(InHole(TypeInconsistent, _), _, _, _)) as outer_block,
            _position,
          )) =>
          let outer_block_nih =
            UHExp.set_err_status_block(NotInHole, outer_block);
          switch (Statics.syn_block(ctx, outer_block_nih)) {
          | None => None
          | Some(ty) =>
            Some({...ci, typed: SynErrorArrow(Arrow(Hole, Hole), ty)})
          };
        | Some((Block(_, Var(_, InVHole(Keyword(k), _), _)), _position)) =>
          Some({...ci, typed: SynKeywordArrow(Arrow(Hole, Hole), k)})
        | Some((Block(_, Var(_, InVHole(Free, _), _)), _position)) =>
          Some({...ci, typed: SynFreeArrow(Arrow(Hole, Hole))})
        | Some((outer_block, _position)) =>
          switch (Statics.syn_block(ctx, outer_block)) {
          | None => None
          | Some(ty) =>
            switch (HTyp.matched_arrow(ty)) {
            | None => None
            | Some((ty1, ty2)) =>
              Some({...ci, typed: SynMatchingArrow(ty, Arrow(ty1, ty2))})
            }
          }
        | None =>
          _syn_cursor_info(~node_steps, ~term_steps, ~frame, ctx, ze_n)
        }
      };
    } else {
      switch (Statics.syn_skel(ctx, skel1, seq, None)) {
      | None => None
      | Some((ty, _)) =>
        switch (HTyp.matched_arrow(ty)) {
        | None => None
        | Some((ty1, _)) =>
          _ana_cursor_info_skel(
            ~node_steps,
            ~term_steps,
            ~frame,
            ctx,
            skel2,
            seq,
            n,
            ze_n,
            ty1,
          )
        }
      };
    }
  | BinOp(_, Space, skel1, skel2) =>
    switch (
      _syn_cursor_info_skel(
        ~node_steps,
        ~term_steps,
        ~frame,
        ctx,
        skel1,
        seq,
        n,
        ze_n,
      )
    ) {
    | Some(_) as result => result
    | None =>
      switch (Statics.syn_skel(ctx, skel1, seq, None)) {
      | None => None
      | Some((ty, _)) =>
        switch (HTyp.matched_arrow(ty)) {
        | None => None
        | Some((ty1, _)) =>
          _ana_cursor_info_skel(
            ~node_steps,
            ~term_steps,
            ~frame,
            ctx,
            skel2,
            seq,
            n,
            ze_n,
            ty1,
          )
        }
      }
    }
  | BinOp(_, Comma, skel1, skel2) =>
    switch (
      _syn_cursor_info_skel(
        ~node_steps,
        ~term_steps,
        ~frame,
        ctx,
        skel1,
        seq,
        n,
        ze_n,
      )
    ) {
    | Some(_) as result => result
    | None =>
      _syn_cursor_info_skel(
        ~node_steps,
        ~term_steps,
        ~frame,
        ctx,
        skel2,
        seq,
        n,
        ze_n,
      )
    }
  | BinOp(_, Cons, skel1, skel2) =>
    switch (
      _syn_cursor_info_skel(
        ~node_steps,
        ~term_steps,
        ~frame,
        ctx,
        skel1,
        seq,
        n,
        ze_n,
      )
    ) {
    | Some(_) as result => result
    | None =>
      switch (Statics.syn_skel(ctx, skel1, seq, None)) {
      | None => None
      | Some((ty_elt, _)) =>
        let ty_list = HTyp.List(ty_elt);
        _ana_cursor_info_skel(
          ~node_steps,
          ~term_steps,
          ~frame,
          ctx,
          skel2,
          seq,
          n,
          ze_n,
          ty_list,
        );
      }
    }
  };
}
and _ana_cursor_info_skel =
    (
      ~node_steps,
      ~term_steps,
      ~frame,
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
      _ana_cursor_info(~node_steps, ~term_steps, ~frame, ctx, ze_n, ty);
    } else {
      None;
    }
  | BinOp(InHole(TypeInconsistent, _), _, _, _) =>
    _syn_cursor_info_skel(
      ~node_steps,
      ~term_steps,
      ~frame,
      ctx,
      skel,
      seq,
      n,
      ze_n,
    )
  | BinOp(NotInHole, Comma, skel1, skel2) =>
    switch (ty) {
    | Hole =>
      switch (
        _ana_cursor_info_skel(
          ~node_steps,
          ~term_steps,
          ~frame,
          ctx,
          skel1,
          seq,
          n,
          ze_n,
          Hole,
        )
      ) {
      | Some(_) as result => result
      | None =>
        _ana_cursor_info_skel(
          ~node_steps,
          ~term_steps,
          ~frame,
          ctx,
          skel2,
          seq,
          n,
          ze_n,
          Hole,
        )
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
              _ana_cursor_info_skel(
                ~node_steps,
                ~term_steps,
                ~frame,
                ctx,
                skel,
                seq,
                n,
                ze_n,
                ty,
              );
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
              _ana_cursor_info_skel(
                ~node_steps,
                ~term_steps,
                ~frame,
                ctx,
                skel,
                seq,
                n,
                ze_n,
                ty,
              );
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
            | None =>
              _syn_cursor_info_skel(
                ~node_steps,
                ~term_steps,
                ~frame,
                ctx,
                skel,
                seq,
                n,
                ze_n,
              )
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
      switch (
        _ana_cursor_info_skel(
          ~node_steps,
          ~term_steps,
          ~frame,
          ctx,
          skel1,
          seq,
          n,
          ze_n,
          ty_elt,
        )
      ) {
      | Some(_) as result => result
      | None =>
        let ty_list = HTyp.List(ty_elt);
        _ana_cursor_info_skel(
          ~node_steps,
          ~term_steps,
          ~frame,
          ctx,
          skel2,
          seq,
          n,
          ze_n,
          ty_list,
        );
      }
    }
  | BinOp(_, Minus | And | Or, _, _)
  | BinOp(_, Plus, _, _)
  | BinOp(_, Times, _, _)
  | BinOp(_, LessThan, _, _)
  | BinOp(_, Space, _, _) =>
    _syn_cursor_info_skel(
      ~node_steps,
      ~term_steps,
      ~frame,
      ctx,
      skel,
      seq,
      n,
      ze_n,
    )
  };

let syn_cursor_info_block = (ctx, zblock) => {
  let node_steps = Path.steps_of_zblock(zblock);
  let term_steps = Path.term_steps_of_zblock(zblock);
  _syn_cursor_info_block(~node_steps, ~term_steps, ctx, zblock);
};

let syn_cursor_info = (~frame, ctx, ze) => {
  let node_steps = Path.steps_of_zexp(ze);
  let term_steps = Path.term_steps_of_zexp(ze);
  _syn_cursor_info(~node_steps, ~term_steps, ~frame, ctx, ze);
};

let ana_cursor_info = (~frame, ctx, ze, ty) => {
  let node_steps = Path.steps_of_zexp(ze);
  let term_steps = Path.term_steps_of_zexp(ze);
  _ana_cursor_info(~node_steps, ~term_steps, ~frame, ctx, ze, ty);
};
