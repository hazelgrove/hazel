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
    PatOperator(cursor_pos, uop);
  };
}
and extract_from_zpat_operand = (zpat_operand: ZPat.zoperand): cursor_term => {
  switch (zpat_operand) {
  | CursorP(cursor_pos, upat_operand) => PatOperand(cursor_pos, upat_operand)
  | ParenthesizedZ(zpat)
  | InjZ(_, _, zpat) => extract_cursor_term(zpat)
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
  | InjZ(_, _, zpat) => get_zoperand_from_zpat(zpat)
  | TypeAnnZP(_, zop, _) => get_zoperand_from_zpat_operand(zop)
  | TypeAnnZA(_, _, zann) => CursorInfo_Typ.get_zoperand_from_ztyp(zann)
  };
};
let rec syn_cursor_info =
        (
          ~steps=[],
          ~pattern_context: option(CursorInfo.pattern_context)=None,
          ctx: Contexts.t,
          zp: ZPat.t,
        )
        : option(CursorInfo_common.deferrable(CursorInfo.t)) =>
  syn_cursor_info_zopseq(~pattern_context, ~steps, ctx, zp)
and syn_cursor_info_zopseq =
    (
      ~steps: CursorPath.steps,
      ~pattern_context: option(CursorInfo.pattern_context),
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
             ~pattern_context,
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
      syn_cursor_info_skel(~pattern_context, ~steps, ctx, cursor_skel, zseq);
    };
  };
}
and syn_cursor_info_skel =
    (
      ~steps: CursorPath.steps,
      ~pattern_context: option(CursorInfo.pattern_context),
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
        ~pattern_context,
        ~steps=steps @ [Seq.length_of_affix(prefix)],
        ctx,
        zoperand,
      )
    | ZOperator(_) =>
      Statics_Pat.syn_skel(ctx, skel, seq)
      |> Option.map(((ty, _)) => {
           CursorInfo_common.CursorNotOnDeferredVarPat(
             CursorInfo_common.mk(
               ~pattern_context,
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
      switch (
        ana_cursor_info_skel(
          ~pattern_context,
          ~steps,
          ctx,
          skel1,
          zseq,
          HTyp.Hole,
        )
      ) {
      | Some(_) as res => res
      | None =>
        switch (Statics_Pat.ana_skel(ctx, skel1, seq, Hole)) {
        | None => None
        | Some(ctx) =>
          ana_cursor_info_skel(
            ~pattern_context,
            ~steps,
            ctx,
            skel2,
            zseq,
            Hole,
          )
        }
      }
    | BinOp(_, Cons, skel1, skel2) =>
      switch (
        syn_cursor_info_skel(~pattern_context, ~steps, ctx, skel1, zseq)
      ) {
      | Some(_) as res => res
      | None =>
        switch (Statics_Pat.syn_skel(ctx, skel1, seq)) {
        | None => None
        | Some((ty_elt, ctx)) =>
          ana_cursor_info_skel(
            ~pattern_context,
            ~steps,
            ctx,
            skel2,
            zseq,
            HTyp.List(ty_elt),
          )
        }
      }
    };
  };
}
and syn_cursor_info_zoperand =
    (
      ~steps: CursorPath.steps,
      ~pattern_context: option(CursorInfo.pattern_context),
      ctx: Contexts.t,
      zoperand: ZPat.zoperand,
    )
    : option(CursorInfo_common.deferrable(CursorInfo.t)) => {
  let mk = (~uses=[], typed) =>
    CursorInfo_common.mk(
      ~pattern_context,
      ~uses,
      typed,
      ctx,
      extract_from_zpat_operand(zoperand),
    );
  switch (zoperand) {
  | CursorP(_, Var(_, InVarHole(Keyword(k), _), _)) =>
    Some(CursorNotOnDeferredVarPat(mk(PatSynKeyword(k))))
  | CursorP(_, Var(NotInHole, NotInVarHole, x) as p) =>
    Statics_Pat.syn_operand(ctx, p)
    |> Option.map(((ty, _)) => {
         CursorInfo_common.CursorOnDeferredVarPat(
           uses => mk(~uses, PatSynthesized(ty)),
           x,
         )
       })
  | CursorP(_, p) =>
    Statics_Pat.syn_operand(ctx, p)
    |> Option.map(((ty, _)) => {
         CursorInfo_common.CursorNotOnDeferredVarPat(mk(PatSynthesized(ty)))
       })
  | InjZ(_, _, zbody)
  | ParenthesizedZ(zbody) =>
    syn_cursor_info(~pattern_context, ~steps=steps @ [0], ctx, zbody)
  | TypeAnnZP(_, zop, ty) =>
    ana_cursor_info_zoperand(
      ~pattern_context,
      ~steps=steps @ [0],
      ctx,
      zop,
      UHTyp.expand(ty),
    )
  | TypeAnnZA(_, operand, zann) =>
    let annotation_types: CursorInfo.annotation_types = {
      analyzed_ty: Hole,
      pattern_ty:
        Statics_Pat.syn_operand(ctx, operand)
        |> Option.map(fst)
        |> HTyp.relax,
      ann_ty: zann |> ZTyp.erase |> UHTyp.expand,
    };
    // TODO(andrew): temporary hack. improve CursorInfo_Typ.cursor_info
    let annotation_types =
      switch (zann) {
      | ZOpSeq(_, ZOperand(CursorT(_), _)) => annotation_types
      | _ => CursorInfo.default_annotation_types
      };
    zann
    |> CursorInfo_Typ.cursor_info(
         ~annotation_types,
         ~steps=steps @ [1],
         ctx,
       )
    |> Option.map(x => CursorInfo_common.CursorNotOnDeferredVarPat(x));
  };
}
and ana_cursor_info =
    (
      ~steps,
      ~pattern_context: option(CursorInfo.pattern_context),
      ctx: Contexts.t,
      zp: ZPat.t,
      ty: HTyp.t,
    )
    : option(CursorInfo_common.deferrable(CursorInfo.t)) => {
  ana_cursor_info_zopseq(~steps, ~pattern_context, ctx, zp, ty);
}
and ana_cursor_info_zopseq =
    (
      ~steps: CursorPath.steps,
      ~pattern_context: option(CursorInfo.pattern_context),
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
  let mk = typed =>
    CursorInfo_common.mk(
      ~pattern_context,
      typed,
      ctx,
      extract_cursor_pat_zseq(zseq),
    );
  switch (zseq) {
  | ZOperator((_, Comma), _) =>
    // cursor on tuple comma
    let opseq = ZPat.erase_zopseq(zopseq);
    let err = UHPat.get_err_status_opseq(opseq);
    switch (err) {
    | NotInHole => Some(CursorNotOnDeferredVarPat(mk(PatAnalyzed(ty))))
    | InHole(WrongLength, _) =>
      let expected_length = List.length(HTyp.get_prod_elements(ty));
      let got_length = List.length(UHPat.get_tuple_elements(skel));
      Some(
        CursorNotOnDeferredVarPat(
          mk(PatAnaWrongLength(expected_length, got_length, ty)),
        ),
      );
    | InHole(TypeInconsistent, _) =>
      let opseq' = UHPat.set_err_status_opseq(NotInHole, opseq);
      Statics_Pat.syn_opseq(ctx, opseq')
      |> Option.map(((ty', _)) => {
           CursorInfo_common.CursorNotOnDeferredVarPat(
             mk(PatAnaTypeInconsistent(ty, ty')),
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
      syn_cursor_info_zopseq(
        ~pattern_context,
        ~steps,
        ctx,
        zopseq_not_in_hole,
      );
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
        ana_cursor_info_skel(
          ~pattern_context,
          ~steps,
          ctx,
          cursor_skel,
          zseq,
          ty,
        );
      };
    }
  };
}
and ana_cursor_info_skel =
    (
      ~steps: CursorPath.steps,
      ~pattern_context: option(CursorInfo.pattern_context),
      ctx: Contexts.t,
      skel: UHPat.skel,
      zseq: ZPat.zseq,
      ty: HTyp.t,
    )
    : option(CursorInfo_common.deferrable(CursorInfo.t)) => {
  let seq = zseq |> ZPat.erase_zseq;
  let mk = typed =>
    CursorInfo_common.mk(
      ~pattern_context,
      typed,
      ctx,
      extract_cursor_pat_zseq(zseq),
    );
  if (ZOpSeq.skel_is_rooted_at_cursor(skel, zseq)) {
    // found cursor
    switch (zseq) {
    | ZOperand(zoperand, (prefix, _)) =>
      // skel must be Placeholder
      ana_cursor_info_zoperand(
        ~pattern_context,
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
               mk(PatAnalyzed(ty)),
             )
           )
      | InHole(WrongLength, _) =>
        failwith(__LOC__ ++ ": n-tuples handled at opseq level")
      | InHole(TypeInconsistent, _) =>
        let opseq' = UHPat.set_err_status_opseq(NotInHole, opseq);
        Statics_Pat.syn_opseq(ctx, opseq')
        |> Option.map(((ty', _)) => {
             CursorInfo_common.CursorNotOnDeferredVarPat(
               mk(PatAnaTypeInconsistent(ty, ty')),
             )
           });
      };
    };
  } else {
    // recurse toward cursor
    switch (skel) {
    | Placeholder(_) => None
    | BinOp(InHole(_), _, _, _) =>
      syn_cursor_info_skel(~pattern_context, ~steps, ctx, skel, zseq)
    | BinOp(_, Comma, _, _) =>
      failwith(
        "Pat.ana_cursor_info_skel: expected commas to be handled at opseq level",
      )
    | BinOp(NotInHole, Space, skel1, skel2) =>
      switch (
        ana_cursor_info_skel(~pattern_context, ~steps, ctx, skel1, zseq, Hole)
      ) {
      | Some(_) as res => res
      | None =>
        switch (Statics_Pat.ana_skel(ctx, skel1, seq, Hole)) {
        | None => None
        | Some(ctx) =>
          ana_cursor_info_skel(
            ~pattern_context,
            ~steps,
            ctx,
            skel2,
            zseq,
            Hole,
          )
        }
      }
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (HTyp.matched_list(ty)) {
      | None => None
      | Some(ty_elt) =>
        switch (
          ana_cursor_info_skel(
            ~pattern_context,
            ~steps,
            ctx,
            skel1,
            zseq,
            ty_elt,
          )
        ) {
        | Some(_) as res => res
        | None =>
          switch (Statics_Pat.ana_skel(ctx, skel1, seq, ty_elt)) {
          | None => None
          | Some(ctx) =>
            ana_cursor_info_skel(
              ~pattern_context,
              ~steps,
              ctx,
              skel2,
              zseq,
              List(ty_elt),
            )
          }
        }
      }
    };
  };
}
and ana_cursor_info_zoperand =
    (
      ~steps: CursorPath.steps,
      ~pattern_context: option(CursorInfo.pattern_context),
      ctx: Contexts.t,
      zoperand: ZPat.zoperand,
      ty: HTyp.t,
    )
    : option(CursorInfo_common.deferrable(CursorInfo.t)) => {
  let cursor_term = extract_from_zpat_operand(zoperand);
  let mk = (~uses=[], typed) =>
    CursorInfo_common.mk(~pattern_context, ~uses, typed, ctx, cursor_term);
  switch (zoperand) {
  | CursorP(_, operand) =>
    switch (operand) {
    // in hole
    | EmptyHole(_) =>
      Some(CursorNotOnDeferredVarPat(mk(PatAnaSubsumed(ty, Hole))))
    | Wild(InHole(TypeInconsistent, _))
    | Var(InHole(TypeInconsistent, _), _, _)
    | IntLit(InHole(TypeInconsistent, _), _)
    | FloatLit(InHole(TypeInconsistent, _), _)
    | BoolLit(InHole(TypeInconsistent, _), _)
    | ListNil(InHole(TypeInconsistent, _))
    | TypeAnn(InHole(TypeInconsistent, _), _, _)
    | Inj(InHole(TypeInconsistent, _), _, _) =>
      let operand' = UHPat.set_err_status_operand(NotInHole, operand);
      switch (Statics_Pat.syn_operand(ctx, operand')) {
      | None => None
      | Some((ty', _)) =>
        Some(
          CursorNotOnDeferredVarPat(mk(PatAnaTypeInconsistent(ty, ty'))),
        )
      };
    | Wild(InHole(WrongLength, _))
    | Var(InHole(WrongLength, _), _, _)
    | IntLit(InHole(WrongLength, _), _)
    | FloatLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _))
    | TypeAnn(InHole(WrongLength, _), _, _)
    | Inj(InHole(WrongLength, _), _, _) => None
    | Var(NotInHole, InVarHole(Keyword(k), _), _) =>
      Some(CursorNotOnDeferredVarPat(mk(PatAnaKeyword(ty, k))))
    // not in hole
    | InvalidText(_) =>
      Some(CursorNotOnDeferredVarPat(mk(PatAnaInvalid(ty))))
    | Var(NotInHole, _, x) =>
      Some(CursorOnDeferredVarPat(uses => mk(~uses, PatAnalyzed(ty)), x))
    | Wild(NotInHole)
    | ListNil(NotInHole) =>
      Some(CursorNotOnDeferredVarPat(mk(PatAnalyzed(ty))))
    | IntLit(NotInHole, _) =>
      Some(CursorNotOnDeferredVarPat(mk(PatAnaSubsumed(ty, Int))))
    | FloatLit(NotInHole, _) =>
      Some(CursorNotOnDeferredVarPat(mk(PatAnaSubsumed(ty, Float))))
    | BoolLit(NotInHole, _) =>
      Some(CursorNotOnDeferredVarPat(mk(PatAnaSubsumed(ty, Bool))))
    | Inj(NotInHole, _, _) =>
      Some(CursorNotOnDeferredVarPat(mk(PatAnalyzed(ty))))
    | Parenthesized(body) =>
      Statics_Pat.ana(ctx, body, ty)
      |> Option.map(_ =>
           CursorInfo_common.CursorNotOnDeferredVarPat(mk(PatAnalyzed(ty)))
         )
    | TypeAnn(NotInHole, op, _) =>
      Statics_Pat.ana_operand(ctx, op, ty)
      |> Option.map(_ =>
           CursorInfo_common.CursorNotOnDeferredVarPat(mk(PatAnalyzed(ty)))
         )
    }
  | InjZ(InHole(WrongLength, _), _, _) => None
  | InjZ(InHole(TypeInconsistent, _), _, _) =>
    syn_cursor_info_zoperand(~pattern_context, ~steps, ctx, zoperand)
  | InjZ(NotInHole, position, zbody) =>
    switch (HTyp.matched_sum(ty)) {
    | None => None
    | Some((tyL, tyR)) =>
      let ty_body = InjSide.pick(position, tyL, tyR);
      ana_cursor_info(
        ~pattern_context,
        ~steps=steps @ [0],
        ctx,
        zbody,
        ty_body,
      );
    }
  | ParenthesizedZ(zbody) =>
    ana_cursor_info(~pattern_context, ~steps=steps @ [0], ctx, zbody, ty)
  | TypeAnnZP(err, zop, ann) =>
    switch (err) {
    | InHole(WrongLength, _) => None
    | InHole(TypeInconsistent, _) =>
      syn_cursor_info_zoperand(~pattern_context, ~steps, ctx, zoperand)
    | NotInHole =>
      let ty_ann = UHTyp.expand(ann);
      ana_cursor_info_zoperand(
        ~pattern_context,
        ~steps=steps @ [0],
        ctx,
        zop,
        ty_ann,
      );
    }
  | TypeAnnZA(err, operand, zann) =>
    let annotation_types: CursorInfo.annotation_types = {
      analyzed_ty: ty,
      pattern_ty:
        Statics_Pat.syn_operand(ctx, operand)
        |> Option.map(fst)
        |> HTyp.relax,
      ann_ty: zann |> ZTyp.erase |> UHTyp.expand,
    };
    // TODO(andrew): temporary hack. improve CursorInfo_Typ.cursor_info
    let annotation_types =
      switch (zann) {
      | ZOpSeq(_, ZOperand(CursorT(_), _)) => annotation_types
      | _ => CursorInfo.default_annotation_types
      };
    switch (err) {
    | InHole(WrongLength, _) => None
    | InHole(TypeInconsistent, _) =>
      syn_cursor_info_zoperand(~pattern_context, ~steps, ctx, zoperand)
    | NotInHole =>
      zann
      |> CursorInfo_Typ.cursor_info(
           ~annotation_types,
           ~steps=steps @ [1],
           ctx,
         )
      |> Option.map(x => CursorInfo_common.CursorNotOnDeferredVarPat(x))
    };
  };
};
