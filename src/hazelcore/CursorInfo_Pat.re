open OptUtil.Syntax;

type cursor_term = CursorInfo.cursor_term;
type zoperand = CursorInfo_common.zoperand;

let rec extract_cursor_term = (zpat: ZPat.t): cursor_term => {
  switch (zpat) {
  | ZOpSeq(_, zseq) => extract_cursor_pat_zseq(zseq)
  };
}
and extract_cursor_pat_zseq = (zseq: ZSeq.t(_, _, _, _)): cursor_term => {
  switch (zseq) {
  | ZOperand(zpat_operand, _) => extract_from_zpat_operand(zpat_operand)
  | ZOperator(zpat_operator, _) =>
    let (cursor_pos, uop) = zpat_operator;
    PatOp(cursor_pos, uop);
  };
}
and extract_from_zpat_operand = (zpat_operand: ZPat.zoperand): cursor_term => {
  switch (zpat_operand) {
  | CursorP(cursor_pos, upat_operand) => Pat(cursor_pos, upat_operand)
  | ParenthesizedZ(zpat)
  | InjZP(_, _, zpat) => extract_cursor_term(zpat)
  | InjZT(_, ztag, _) => CursorInfo_Tag.extract_cursor_term(ztag)
  | TypeAnnZP(_, zop, _) => extract_from_zpat_operand(zop)
  | TypeAnnZA(_, _, zann) => CursorInfo_Typ.extract_cursor_term(zann)
  };
};

let rec get_zoperand_from_zpat = (zpat: ZPat.t): option(zoperand) => {
  get_zoperand_from_zpat_opseq(zpat);
}
and get_zoperand_from_zpat_opseq = (zopseq: ZPat.zopseq): option(zoperand) => {
  switch (zopseq) {
  | ZOpSeq(_, zseq) =>
    switch (zseq) {
    | ZOperand(zpat_operand, _) =>
      get_zoperand_from_zpat_operand(zpat_operand)
    | ZOperator(_, _) => None
    }
  };
}
and get_zoperand_from_zpat_operand =
    (zoperand: ZPat.zoperand): option(zoperand) => {
  switch (zoperand) {
  | CursorP(_, _) => Some(ZPat(zoperand))
  | ParenthesizedZ(zpat)
  | InjZP(_, _, zpat) => get_zoperand_from_zpat(zpat)
  | InjZT(_, ztag, _) => CursorInfo_Tag.get_zoperand(ztag)
  | TypeAnnZP(_, zop, _) => get_zoperand_from_zpat_operand(zop)
  | TypeAnnZA(_, _, zann) => CursorInfo_Typ.get_zoperand_from_ztyp(zann)
  };
};

let rec syn_cursor_info =
        (~steps=[], ctx: Contexts.t, zp: ZPat.t)
        : option(CursorInfo_common.deferrable(CursorInfo.t)) =>
  syn_cursor_info_zopseq(~steps, ctx, zp)
and syn_cursor_info_zopseq =
    (
      ~steps: CursorPath.steps,
      ctx: Contexts.t,
      ZOpSeq(skel, zseq): ZPat.zopseq,
    )
    : option(CursorInfo_common.deferrable(CursorInfo.t)) => {
  // handle n-tuples:
  // cannot simply defer to syn_cursor_info_skel here
  // because it assumes binary tupling -- this would
  // cause sub-tuples to synthesize sub-product types,
  // but we want all comma operators in an opseq to
  // show the complete product type
  let seq = zseq |> ZPat.erase_zseq;
  let skels = skel |> UHPat.get_tuple_elements;
  switch (zseq) {
  | ZOperator((_, Comma), _) =>
    // cursor on tuple comma
    skels
    |> List.fold_left(
         (acc: option((list(HTyp.t), Contexts.t)), skel) =>
           switch (acc) {
           | None => None
           | Some((rev_tys, ctx)) =>
             switch (Statics_Pat.syn_skel(ctx, skel, seq)) {
             | None => None
             | Some((ty, ctx)) => Some(([ty, ...rev_tys], ctx))
             }
           },
         Some(([], ctx)),
       )
    |> Option.map(((rev_tys, _)) =>
         CursorInfo_common.CursorNotOnDeferredVarPat(
           CursorInfo_common.mk(
             PatSynthesized(Prod(rev_tys |> List.rev)),
             ctx,
             extract_cursor_pat_zseq(zseq),
           ),
         )
       )
  | _ =>
    // cursor within tuple element
    let opt_ctx =
      skels
      |> ListUtil.take_while(skel =>
           !ZOpSeq.skel_contains_cursor(skel, zseq)
         )
      |> List.fold_left(
           (opt_ctx, skel) =>
             switch (opt_ctx) {
             | None => None
             | Some(ctx) =>
               Statics_Pat.syn_skel(ctx, skel, seq)
               |> Option.map(((_, ctx)) => ctx)
             },
           Some(ctx),
         );
    switch (opt_ctx) {
    | None => None
    | Some(ctx) =>
      let cursor_skel =
        skels |> List.find(skel => ZOpSeq.skel_contains_cursor(skel, zseq));
      syn_cursor_info_skel(~steps, ctx, cursor_skel, zseq);
    };
  };
}
and syn_cursor_info_skel =
    (
      ~steps: CursorPath.steps,
      ctx: Contexts.t,
      skel: UHPat.skel,
      zseq: ZPat.zseq,
    )
    : option(CursorInfo_common.deferrable(CursorInfo.t)) => {
  let seq = zseq |> ZPat.erase_zseq;
  if (ZOpSeq.skel_is_rooted_at_cursor(skel, zseq)) {
    // found cursor
    switch (zseq) {
    | ZOperand(zoperand, (prefix, _)) =>
      // skel must be Placeholder
      syn_cursor_info_zoperand(
        ~steps=steps @ [Seq.length_of_affix(prefix)],
        ctx,
        zoperand,
      )
    | ZOperator(_) =>
      Statics_Pat.syn_skel(ctx, skel, seq)
      |> Option.map(((ty, _)) => {
           CursorInfo_common.CursorNotOnDeferredVarPat(
             CursorInfo_common.mk(
               PatSynthesized(ty),
               ctx,
               extract_cursor_pat_zseq(zseq),
             ),
           )
         })
    };
  } else {
    // recurse toward cursor
    switch (skel) {
    | Placeholder(_) => None
    | BinOp(_, Comma, _, _) =>
      failwith(
        "Pat.syn_cursor_info_skel: expected commas to be handled at opseq level",
      )
    | BinOp(_, Space, skel1, skel2) =>
      switch (ana_cursor_info_skel(~steps, ctx, skel1, zseq, HTyp.Hole)) {
      | Some(_) as res => res
      | None =>
        switch (Statics_Pat.ana_skel(ctx, skel1, seq, Hole)) {
        | None => None
        | Some(ctx) => ana_cursor_info_skel(~steps, ctx, skel2, zseq, Hole)
        }
      }
    | BinOp(_, Cons, skel1, skel2) =>
      switch (syn_cursor_info_skel(~steps, ctx, skel1, zseq)) {
      | Some(_) as res => res
      | None =>
        switch (Statics_Pat.syn_skel(ctx, skel1, seq)) {
        | None => None
        | Some((ty_elt, ctx)) =>
          ana_cursor_info_skel(~steps, ctx, skel2, zseq, HTyp.List(ty_elt))
        }
      }
    };
  };
}
and syn_cursor_info_zoperand =
    (~steps: CursorPath.steps, ctx: Contexts.t, zoperand: ZPat.zoperand)
    : option(CursorInfo_common.deferrable(CursorInfo.t)) =>
  switch (zoperand) {
  | CursorP(_, Var(_, InVarHole(Keyword(k), _), _)) =>
    Some(
      CursorNotOnDeferredVarPat(
        CursorInfo_common.mk(
          PatSynKeyword(k),
          ctx,
          extract_from_zpat_operand(zoperand),
        ),
      ),
    )
  | CursorP(_, Var(NotInHole, NotInVarHole, x) as p) =>
    Statics_Pat.syn_operand(ctx, p)
    |> Option.map(((ty, _)) => {
         CursorInfo_common.CursorOnDeferredVarPat(
           uses =>
             CursorInfo_common.mk(
               ~uses,
               PatSynthesized(ty),
               ctx,
               extract_from_zpat_operand(zoperand),
             ),
           x,
         )
       })
  | CursorP(_, p) =>
    Statics_Pat.syn_operand(ctx, p)
    |> Option.map(((ty, _)) => {
         CursorInfo_common.CursorNotOnDeferredVarPat(
           CursorInfo_common.mk(
             PatSynthesized(ty),
             ctx,
             extract_from_zpat_operand(zoperand),
           ),
         )
       })
  | ParenthesizedZ(zbody) => syn_cursor_info(~steps=steps @ [0], ctx, zbody)
  | InjZT(_, ztag, _) =>
    let+ cursor_info =
      CursorInfo_Tag.cursor_info(~steps=steps @ [0], ctx, ztag);
    CursorInfo_common.CursorNotOnDeferredVarPat(cursor_info);
  | InjZP(_, _, zpat) =>
    ana_cursor_info(~steps=steps @ [1], ctx, zpat, HTyp.Hole)
  | TypeAnnZP(_, zop, ty) =>
    ana_cursor_info_zoperand(~steps=steps @ [0], ctx, zop, UHTyp.expand(ty))
  | TypeAnnZA(_, _, zann) =>
    zann
    |> CursorInfo_Typ.cursor_info(~steps=steps @ [1], ctx)
    |> Option.map(x => CursorInfo_common.CursorNotOnDeferredVarPat(x))
  }

and ana_cursor_info =
    (~steps, ctx: Contexts.t, zp: ZPat.t, ty: HTyp.t)
    : option(CursorInfo_common.deferrable(CursorInfo.t)) => {
  ana_cursor_info_zopseq(~steps, ctx, zp, ty);
}
and ana_cursor_info_zopseq =
    (
      ~steps: CursorPath.steps,
      ctx: Contexts.t,
      ZOpSeq(skel, zseq) as zopseq: ZPat.zopseq,
      ty: HTyp.t,
    )
    : option(CursorInfo_common.deferrable(CursorInfo.t)) => {
  // handle n-tuples:
  // cannot simply defer to ana_cursor_info_skel here
  // because it assumes binary tupling -- this would
  // cause sub-tuples to synthesize sub-product types,
  // but we want all comma operators in an opseq to
  // show the complete product type
  let seq = zseq |> ZPat.erase_zseq;
  switch (zseq) {
  | ZOperator((_, Comma), _) =>
    // cursor on tuple comma
    let opseq = ZPat.erase_zopseq(zopseq);
    let err = UHPat.get_err_status_opseq(opseq);
    switch (err) {
    | NotInHole =>
      Some(
        CursorNotOnDeferredVarPat(
          CursorInfo_common.mk(
            PatAnalyzed(ty),
            ctx,
            extract_cursor_pat_zseq(zseq),
          ),
        ),
      )
    | InHole(WrongLength, _) =>
      let expected_length = List.length(HTyp.get_prod_elements(ty));
      let got_length = List.length(UHPat.get_tuple_elements(skel));
      Some(
        CursorNotOnDeferredVarPat(
          CursorInfo_common.mk(
            PatAnaWrongLength(expected_length, got_length, ty),
            ctx,
            extract_cursor_pat_zseq(zseq),
          ),
        ),
      );
    | InHole(TypeInconsistent, _) =>
      let opseq' = UHPat.set_err_status_opseq(NotInHole, opseq);
      Statics_Pat.syn_opseq(ctx, opseq')
      |> Option.map(((ty', _)) => {
           CursorInfo_common.CursorNotOnDeferredVarPat(
             CursorInfo_common.mk(
               PatAnaTypeInconsistent(ty, ty'),
               ctx,
               extract_cursor_pat_zseq(zseq),
             ),
           )
         });
    };
  | _ =>
    // cursor in tuple element
    switch (Statics_Pat.tuple_zip(skel, ty)) {
    | None =>
      // wrong length, switch to syn
      let zopseq_not_in_hole =
        zopseq |> ZPat.set_err_status_zopseq(NotInHole);
      syn_cursor_info_zopseq(~steps, ctx, zopseq_not_in_hole);
    | Some(skel_tys) =>
      let opt_ctx =
        skel_tys
        |> ListUtil.take_while(((skel, _)) =>
             !ZOpSeq.skel_contains_cursor(skel, zseq)
           )
        |> List.fold_left(
             (opt_ctx, (skel, ty)) =>
               switch (opt_ctx) {
               | None => None
               | Some(ctx) => Statics_Pat.ana_skel(ctx, skel, seq, ty)
               },
             Some(ctx),
           );
      switch (opt_ctx) {
      | None => None
      | Some(ctx) =>
        let (cursor_skel, ty) =
          skel_tys
          |> List.find(((skel, _)) =>
               ZOpSeq.skel_contains_cursor(skel, zseq)
             );
        ana_cursor_info_skel(~steps, ctx, cursor_skel, zseq, ty);
      };
    }
  };
}
and ana_cursor_info_skel =
    (
      ~steps: CursorPath.steps,
      ctx: Contexts.t,
      skel: UHPat.skel,
      zseq: ZPat.zseq,
      ty: HTyp.t,
    )
    : option(CursorInfo_common.deferrable(CursorInfo.t)) => {
  let seq = zseq |> ZPat.erase_zseq;
  if (ZOpSeq.skel_is_rooted_at_cursor(skel, zseq)) {
    // found cursor
    switch (zseq) {
    | ZOperand(zoperand, (prefix, _)) =>
      // skel must be Placeholder
      ana_cursor_info_zoperand(
        ~steps=steps @ [Seq.length_of_affix(prefix)],
        ctx,
        zoperand,
        ty,
      )
    | ZOperator(_) =>
      // skel must be BinOp
      let opseq = OpSeq.OpSeq(skel, ZPat.erase_zseq(zseq));
      let err = UHPat.get_err_status_opseq(opseq);
      switch (err) {
      | NotInHole =>
        Statics_Pat.ana_skel(ctx, skel, seq, ty)
        |> Option.map(_ =>
             CursorInfo_common.CursorNotOnDeferredVarPat(
               CursorInfo_common.mk(
                 PatAnalyzed(ty),
                 ctx,
                 extract_cursor_pat_zseq(zseq),
               ),
             )
           )
      | InHole(WrongLength, _) =>
        failwith(__LOC__ ++ ": n-tuples handled at opseq level")
      | InHole(TypeInconsistent, _) =>
        let opseq' = UHPat.set_err_status_opseq(NotInHole, opseq);
        Statics_Pat.syn_opseq(ctx, opseq')
        |> Option.map(((ty', _)) => {
             CursorInfo_common.CursorNotOnDeferredVarPat(
               CursorInfo_common.mk(
                 PatAnaTypeInconsistent(ty, ty'),
                 ctx,
                 extract_cursor_pat_zseq(zseq),
               ),
             )
           });
      };
    };
  } else {
    // recurse toward cursor
    switch (skel) {
    | Placeholder(_) => None
    | BinOp(InHole(_), _, _, _) =>
      syn_cursor_info_skel(~steps, ctx, skel, zseq)
    | BinOp(_, Comma, _, _) =>
      failwith(
        "Pat.ana_cursor_info_skel: expected commas to be handled at opseq level",
      )
    | BinOp(NotInHole, Space, skel1, skel2) =>
      switch (ana_cursor_info_skel(~steps, ctx, skel1, zseq, Hole)) {
      | Some(_) as res => res
      | None =>
        switch (Statics_Pat.ana_skel(ctx, skel1, seq, Hole)) {
        | None => None
        | Some(ctx) => ana_cursor_info_skel(~steps, ctx, skel2, zseq, Hole)
        }
      }
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (HTyp.matched_list(ty)) {
      | None => None
      | Some(ty_elt) =>
        switch (ana_cursor_info_skel(~steps, ctx, skel1, zseq, ty_elt)) {
        | Some(_) as res => res
        | None =>
          switch (Statics_Pat.ana_skel(ctx, skel1, seq, ty_elt)) {
          | None => None
          | Some(ctx) =>
            ana_cursor_info_skel(~steps, ctx, skel2, zseq, List(ty_elt))
          }
        }
      }
    };
  };
}
and ana_cursor_info_zoperand =
    (
      ~steps: CursorPath.steps,
      ctx: Contexts.t,
      zoperand: ZPat.zoperand,
      ty: HTyp.t,
    )
    : option(CursorInfo_common.deferrable(CursorInfo.t)) => {
  let cursor_term = extract_from_zpat_operand(zoperand);
  switch (zoperand) {
  | CursorP(_, operand) =>
    switch (operand) {
    // in hole
    | EmptyHole(_) =>
      Some(
        CursorNotOnDeferredVarPat(
          CursorInfo_common.mk(PatAnaSubsumed(ty, Hole), ctx, cursor_term),
        ),
      )
    | Wild(InHole(TypeInconsistent, _))
    | Var(InHole(TypeInconsistent, _), _, _)
    | IntLit(InHole(TypeInconsistent, _), _)
    | FloatLit(InHole(TypeInconsistent, _), _)
    | BoolLit(InHole(TypeInconsistent, _), _)
    | ListNil(InHole(TypeInconsistent, _))
    | TypeAnn(InHole(TypeInconsistent, _), _, _) =>
      let operand' = UHPat.set_err_status_operand(NotInHole, operand);
      switch (Statics_Pat.syn_operand(ctx, operand')) {
      | None => None
      | Some((ty', _)) =>
        Some(
          CursorNotOnDeferredVarPat(
            CursorInfo_common.mk(
              PatAnaTypeInconsistent(ty, ty'),
              ctx,
              cursor_term,
            ),
          ),
        )
      };
    | Inj(InHole(InjectionInSyntheticPosition, _), _, _) => None
    | Inj(InHole(ExpectedTypeNotConsistentWithSums, _), _, _) =>
      Some(
        CursorInfo_common.CursorNotOnDeferredVarPat(
          CursorInfo_common.mk(
            PatAnaInjExpectedTypeNotConsistentWithSums(ty),
            ctx,
            cursor_term,
          ),
        ),
      )
    | Inj(InHole(UnexpectedBody, _), _, _) =>
      Some(
        CursorInfo_common.CursorNotOnDeferredVarPat(
          CursorInfo_common.mk(PatAnaInjUnexpectedBody, ctx, cursor_term),
        ),
      )
    | Inj(InHole(ExpectedBody, _), tag, None) =>
      switch (ty) {
      | Sum(tymap) =>
        let* ty_opt = TagMap.find_opt(tag, tymap);
        let+ ty = ty_opt;
        CursorInfo_common.CursorNotOnDeferredVarPat(
          CursorInfo_common.mk(PatAnaInjExpectedBody(ty), ctx, cursor_term),
        );
      | _ => None
      }
    | Inj(InHole(BadTag, _), tag, _) =>
      Some(
        CursorInfo_common.CursorNotOnDeferredVarPat(
          CursorInfo_common.mk(PatAnaInjBadTag(tag), ctx, cursor_term),
        ),
      )
    | Inj(InHole(_), _, _) => None
    | Wild(InHole(WrongLength, _))
    | Var(InHole(WrongLength, _), _, _)
    | IntLit(InHole(WrongLength, _), _)
    | FloatLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _))
    | TypeAnn(InHole(WrongLength, _), _, _) => None
    | Var(NotInHole, InVarHole(Keyword(k), _), _) =>
      Some(
        CursorNotOnDeferredVarPat(
          CursorInfo_common.mk(PatAnaKeyword(ty, k), ctx, cursor_term),
        ),
      )
    // not in hole
    | InvalidText(_) =>
      Some(
        CursorNotOnDeferredVarPat(
          CursorInfo_common.mk(PatAnaInvalid(ty), ctx, cursor_term),
        ),
      )
    | Var(NotInHole, _, x) =>
      Some(
        CursorOnDeferredVarPat(
          uses =>
            CursorInfo_common.mk(~uses, PatAnalyzed(ty), ctx, cursor_term),
          x,
        ),
      )
    | Wild(NotInHole)
    | ListNil(NotInHole) =>
      Some(
        CursorNotOnDeferredVarPat(
          CursorInfo_common.mk(PatAnalyzed(ty), ctx, cursor_term),
        ),
      )
    | IntLit(NotInHole, _) =>
      Some(
        CursorNotOnDeferredVarPat(
          CursorInfo_common.mk(PatAnaSubsumed(ty, Int), ctx, cursor_term),
        ),
      )
    | FloatLit(NotInHole, _) =>
      Some(
        CursorNotOnDeferredVarPat(
          CursorInfo_common.mk(PatAnaSubsumed(ty, Float), ctx, cursor_term),
        ),
      )
    | BoolLit(NotInHole, _) =>
      Some(
        CursorNotOnDeferredVarPat(
          CursorInfo_common.mk(PatAnaSubsumed(ty, Bool), ctx, cursor_term),
        ),
      )
    | Inj(NotInHole, _, _) =>
      Some(
        CursorNotOnDeferredVarPat(
          CursorInfo_common.mk(PatAnalyzed(ty), ctx, cursor_term),
        ),
      )
    | Parenthesized(body) =>
      Statics_Pat.ana(ctx, body, ty)
      |> Option.map(_ =>
           CursorInfo_common.CursorNotOnDeferredVarPat(
             CursorInfo_common.mk(PatAnalyzed(ty), ctx, cursor_term),
           )
         )
    //  TODO: report bugfix off of dev
    | TypeAnn(NotInHole, op, _) =>
      Statics_Pat.ana_operand(ctx, op, ty)
      |> Option.map(_ =>
           CursorInfo_common.CursorNotOnDeferredVarPat(
             CursorInfo_common.mk(PatAnalyzed(ty), ctx, cursor_term),
           )
         )
    }
  | InjZT(_, ztag, _) =>
    let+ cursor_info =
      CursorInfo_Tag.cursor_info(~steps=steps @ [0], ctx, ztag);
    CursorInfo_common.CursorNotOnDeferredVarPat(cursor_info);
  | InjZP(_, tag, zpat) =>
    switch (ty) {
    | Sum(tymap) =>
      switch (TagMap.find_opt(tag, tymap) |> Option.join) {
      | Some(ty_arg) =>
        ana_cursor_info(~steps=steps @ [1], ctx, zpat, ty_arg)
      | None => ana_cursor_info(~steps=steps @ [1], ctx, zpat, HTyp.Hole)
      }
    | _ => None
    }
  | ParenthesizedZ(zbody) =>
    ana_cursor_info(~steps=steps @ [0], ctx, zbody, ty)
  | TypeAnnZP(err, zop, ann) =>
    switch (err) {
    | InHole(WrongLength, _) => None
    | InHole(TypeInconsistent, _) =>
      syn_cursor_info_zoperand(~steps, ctx, zoperand)
    | NotInHole =>
      let ty_ann = UHTyp.expand(ann);
      ana_cursor_info_zoperand(~steps=steps @ [0], ctx, zop, ty_ann);
    }
  | TypeAnnZA(err, _, zann) =>
    switch (err) {
    | InHole(WrongLength, _) => None
    | InHole(TypeInconsistent, _) =>
      syn_cursor_info_zoperand(~steps, ctx, zoperand)
    | NotInHole =>
      zann
      |> CursorInfo_Typ.cursor_info(~steps=steps @ [1], ctx)
      |> Option.map(x => CursorInfo_common.CursorNotOnDeferredVarPat(x))
    }
  };
};
