open OptUtil.Syntax;
type cursor_term = CursorInfo.cursor_term;
type zoperand = CursorInfo_common.zoperand;

let rec extract_cursor_term = (exp: ZExp.t): cursor_term => {
  extract_from_zline(ZList.prj_z(exp));
}
and extract_from_zline = (zline: ZExp.zline): cursor_term => {
  switch (zline) {
  | CursorL(cursor_pos, uex_line) => Line(cursor_pos, uex_line)
  | ExpLineZ(zopseq) => extract_from_zexp_opseq(zopseq)
  | LetLineZP(_, zpat, _) => CursorInfo_Pat.extract_cursor_term(zpat)
  | LivelitDefLineZExpansionType({expansion_type: ztyp, _})
  | LivelitDefLineZModelType({model_type: ztyp, _})
  | LivelitDefLineZActionType({action_type: ztyp, _}) =>
    CursorInfo_Typ.extract_cursor_term(ztyp)
  | LivelitDefLineZCaptures({captures: zdef, _})
  | LivelitDefLineZInit({init: zdef, _})
  | LivelitDefLineZUpdate({update: zdef, _})
  | LivelitDefLineZView({view: zdef, _})
  | LivelitDefLineZShape({shape: zdef, _})
  | LivelitDefLineZExpand({expand: zdef, _})
  | LetLineZE(_, _, zdef) => extract_cursor_term(zdef)
  };
}
and extract_from_zexp_operand = (zexp_operand: ZExp.zoperand): cursor_term => {
  switch (zexp_operand) {
  | CursorE(cursor_pos, operand) => Exp(cursor_pos, operand)
  | ParenthesizedZ(zexp) => extract_cursor_term(zexp)
  | LamZP(_, zpat, _) => CursorInfo_Pat.extract_cursor_term(zpat)
  | LamZE(_, _, zexp)
  | InjZ(_, _, zexp)
  | CaseZE(_, zexp, _) => extract_cursor_term(zexp)
  | CaseZR(_, _, zrules) => extract_from_zrules(zrules)
  | SubscriptZE1(_, zexp, _, _) => extract_cursor_term(zexp)
  | SubscriptZE2(_, _, zexp, _) => extract_cursor_term(zexp)
  | SubscriptZE3(_, _, _, zexp) => extract_cursor_term(zexp)
  | ApLivelitZ(_, _, _, _, _, zsplice_info) =>
    extract_cursor_term(ZSpliceInfo.prj_ze(zsplice_info))
  };
}
and extract_from_zrules = (zrules: ZExp.zrules): cursor_term => {
  let zrule = ZList.prj_z(zrules);
  extract_from_zrule(zrule);
}
and extract_from_zrule = (zrule: ZExp.zrule): cursor_term => {
  switch (zrule) {
  | CursorR(cursor_pos, uex_rule) => Rule(cursor_pos, uex_rule)
  | RuleZP(zpat, _) => CursorInfo_Pat.extract_cursor_term(zpat)
  | RuleZE(_, zexp) => extract_cursor_term(zexp)
  };
}
and extract_from_zexp_opseq = (zopseq: ZExp.zopseq): cursor_term => {
  switch (zopseq) {
  | ZOpSeq(_, zseq) => extract_from_zexp_zseq(zseq)
  };
}
and extract_from_zexp_zseq = (zseq: ZSeq.t(_, _, _, _)): cursor_term => {
  switch (zseq) {
  | ZOperand(zoperand, _) => extract_from_zexp_operand(zoperand)
  | ZOperator(zoperator, _) =>
    let (cursor_pos, uop) = zoperator;
    ExpOp(cursor_pos, uop);
  };
};

let rec get_zoperand = (zexp: ZExp.t): option(zoperand) => {
  get_zoperand_from_zexp(zexp);
}
and get_zoperand_from_zexp = (zexp: ZExp.t): option(zoperand) => {
  get_zoperand_from_zline(ZList.prj_z(zexp));
}
and get_zoperand_from_zline = (zline: ZExp.zline): option(zoperand) => {
  switch (zline) {
  | CursorL(_, _) => None
  | ExpLineZ(zopseq) => get_zoperand_from_zexp_opseq(zopseq)
  | LetLineZP(_, zpat, _) => CursorInfo_Pat.get_zoperand_from_zpat(zpat)
  | LivelitDefLineZExpansionType({expansion_type: ztyp, _})
  | LivelitDefLineZModelType({model_type: ztyp, _})
  | LivelitDefLineZActionType({action_type: ztyp, _}) =>
    CursorInfo_Typ.get_zoperand_from_ztyp(ztyp)
  | LivelitDefLineZCaptures({captures: zdef, _})
  | LivelitDefLineZInit({init: zdef, _})
  | LivelitDefLineZUpdate({update: zdef, _})
  | LivelitDefLineZView({view: zdef, _})
  | LivelitDefLineZShape({shape: zdef, _})
  | LivelitDefLineZExpand({expand: zdef, _})
  | LetLineZE(_, _, zdef) => get_zoperand_from_zexp(zdef)
  };
}
and get_zoperand_from_zexp_opseq = (zopseq: ZExp.zopseq): option(zoperand) => {
  switch (zopseq) {
  | ZOpSeq(_, zseq) =>
    switch (zseq) {
    | ZOperand(zoperand, _) => get_zoperand_from_zexp_operand(zoperand)
    | ZOperator(_, _) => None
    }
  };
}
and get_zoperand_from_zexp_operand =
    (zoperand: ZExp.zoperand): option(zoperand) =>
  switch (zoperand) {
  | CursorE(_, _) => Some(ZExp(zoperand))
  | ParenthesizedZ(zexp) => get_zoperand_from_zexp(zexp)
  | LamZP(_, zpat, _) => CursorInfo_Pat.get_zoperand_from_zpat(zpat)
  | LamZE(_, _, zexp)
  | InjZ(_, _, zexp)
  | CaseZE(_, zexp, _) => get_zoperand_from_zexp(zexp)
  | CaseZR(_, _, zrules) => get_zoperand_from_zrules(zrules)
  | SubscriptZE1(_, zexp, _, _) => get_zoperand_from_zexp(zexp)
  | SubscriptZE2(_, _, zexp, _) => get_zoperand_from_zexp(zexp)
  | SubscriptZE3(_, _, _, zexp) => get_zoperand_from_zexp(zexp)
  | ApLivelitZ(_, _, _, _, _, zsplice_info) =>
    get_zoperand_from_zexp(ZSpliceInfo.prj_ze(zsplice_info))
  }
and get_zoperand_from_zrules = (zrules: ZExp.zrules): option(zoperand) => {
  get_zoperand_from_zrule(ZList.prj_z(zrules));
}
and get_zoperand_from_zrule = (zrule: ZExp.zrule): option(zoperand) =>
  switch (zrule) {
  | CursorR(_, _) => None
  | RuleZP(zpat, _) => CursorInfo_Pat.get_zoperand_from_zpat(zpat)
  | RuleZE(_, zexp) => get_zoperand_from_zexp(zexp)
  };

let rec get_outer_zrules = (exp: ZExp.t): option(ZExp.zrules) => {
  get_outer_zrules_from_zexp(exp, None);
}
and get_outer_zrules_from_zexp =
    (exp: ZExp.t, outer_zrules: option(ZExp.zrules)): option(ZExp.zrules) => {
  get_outer_zrules_from_zline(ZList.prj_z(exp), outer_zrules);
}
and get_outer_zrules_from_zline =
    (zline: ZExp.zline, outer_zrules: option(ZExp.zrules)) => {
  switch (zline) {
  | CursorL(_, _) => outer_zrules
  | ExpLineZ(zopseq) =>
    get_outer_zrules_from_zexp_opseq(zopseq, outer_zrules)
  | LetLineZP(_) => outer_zrules
  | LivelitDefLineZExpansionType(_)
  | LivelitDefLineZModelType(_)
  | LivelitDefLineZActionType(_) => outer_zrules
  | LivelitDefLineZCaptures({captures: zdef, _})
  | LivelitDefLineZInit({init: zdef, _})
  | LivelitDefLineZUpdate({update: zdef, _})
  | LivelitDefLineZView({view: zdef, _})
  | LivelitDefLineZShape({shape: zdef, _})
  | LivelitDefLineZExpand({expand: zdef, _})
  | LetLineZE(_, _, zdef) => get_outer_zrules_from_zexp(zdef, outer_zrules)
  };
}
and get_outer_zrules_from_zexp_opseq =
    (zopseq: ZExp.zopseq, outer_zrules: option(ZExp.zrules))
    : option(ZExp.zrules) => {
  switch (zopseq) {
  | ZOpSeq(_, zseq) =>
    switch (zseq) {
    | ZOperand(zoperand, _) =>
      get_outer_zrules_from_zexp_operand(zoperand, outer_zrules)
    | ZOperator(_, _) => outer_zrules
    }
  };
}
and get_outer_zrules_from_zexp_operand =
    (zoperand: ZExp.zoperand, outer_zrules: option(ZExp.zrules))
    : option(ZExp.zrules) => {
  switch (zoperand) {
  | CursorE(_, _) => outer_zrules
  | ParenthesizedZ(zexp) => get_outer_zrules_from_zexp(zexp, outer_zrules)
  | LamZP(_) => outer_zrules
  | LamZE(_, _, zexp)
  | InjZ(_, _, zexp)
  | CaseZE(_, zexp, _) => get_outer_zrules_from_zexp(zexp, outer_zrules)
  | CaseZR(_, _, zrules) => get_outer_zrules_from_zrules(zrules)
  | SubscriptZE1(_, zexp, _, _) =>
    get_outer_zrules_from_zexp(zexp, outer_zrules)
  | SubscriptZE2(_, _, zexp, _) =>
    get_outer_zrules_from_zexp(zexp, outer_zrules)
  | SubscriptZE3(_, _, _, zexp) =>
    get_outer_zrules_from_zexp(zexp, outer_zrules)
  | ApLivelitZ(_, _, _, _, _, zsplice_info) =>
    get_outer_zrules_from_zexp(
      ZSpliceInfo.prj_ze(zsplice_info),
      outer_zrules,
    )
  };
}
and get_outer_zrules_from_zrules = (zrules: ZExp.zrules): option(ZExp.zrules) => {
  get_outer_zrules_from_zrule(ZList.prj_z(zrules), Some(zrules));
}
and get_outer_zrules_from_zrule =
    (zrule: ZExp.zrule, outer_zrules: option(ZExp.zrules))
    : option(ZExp.zrules) => {
  switch (zrule) {
  | CursorR(_, _)
  | RuleZP(_, _) => outer_zrules
  | RuleZE(_, zexp) => get_outer_zrules_from_zexp(zexp, outer_zrules)
  };
};

type err_status_result =
  | StandardErr(ErrStatus.t)
  | VarErr(VarErrStatus.HoleReason.t)
  | InconsistentBranchesErr(list(HTyp.t));

let rec cursor_on_outer_expr: ZExp.zoperand => option(err_status_result) =
  fun
  | CursorE(_, operand) => {
      let err_status =
        switch (operand) {
        | Var(_, InVarHole(reason, _), _) => VarErr(reason)
        | Case(InconsistentBranches(types, _), _, _) =>
          InconsistentBranchesErr(types)
        | _ => StandardErr(UHExp.get_err_status_operand(operand))
        };
      Some(err_status);
    }
  | ParenthesizedZ(([], ExpLineZ(ZOpSeq(skel, zseq)), [])) =>
    if (ZOpSeq.skel_is_rooted_at_cursor(skel, zseq)) {
      switch (skel, zseq) {
      | (BinOp(err, _, _, _), _) => Some(StandardErr(err))
      | (_, ZOperand(zoperand, _)) => cursor_on_outer_expr(zoperand)
      | _ => None
      };
    } else {
      None;
    }
  | _ => None;

let caret_is_after_zoperand = (zexp: ZExp.t): bool => {
  switch (get_zoperand(zexp)) {
  | None => false
  | Some(zop) =>
    switch (zop) {
    | ZExp(zoperand) => ZExp.is_after_zoperand(zoperand)
    | ZPat(zoperand) => ZPat.is_after_zoperand(zoperand)
    | ZTyp(zoperand) => ZTyp.is_after_zoperand(zoperand)
    }
  };
};
let caret_is_before_zoperand = (zexp: ZExp.t): bool => {
  switch (get_zoperand(zexp)) {
  | None => false
  | Some(zop) =>
    switch (zop) {
    | ZExp(zoperand) => ZExp.is_before_zoperand(zoperand)
    | ZPat(zoperand) => ZPat.is_before_zoperand(zoperand)
    | ZTyp(zoperand) => ZTyp.is_before_zoperand(zoperand)
    }
  };
};

let adjacent_is_emptyline = (exp: ZExp.t): (bool, bool) => {
  let prev_is_empty_line = {
    let prefix = ZList.prj_prefix(exp);
    switch (ListUtil.split_last_opt(prefix)) {
    | None => false
    | Some((_, EmptyLine)) =>
      switch (ZList.prj_z(exp)) {
      | ExpLineZ(zopseq) => ZExp.is_before_zopseq(zopseq)
      | CursorL(_, _)
      | LetLineZP(_)
      | LetLineZE(_)
      | LivelitDefLineZExpansionType(_)
      | LivelitDefLineZModelType(_)
      | LivelitDefLineZActionType(_)
      | LivelitDefLineZCaptures(_)
      | LivelitDefLineZInit(_)
      | LivelitDefLineZUpdate(_)
      | LivelitDefLineZView(_)
      | LivelitDefLineZShape(_)
      | LivelitDefLineZExpand(_) => true
      }
    | Some((_, _)) => false
    };
  };
  let next_is_empty_line = {
    let suffix = ZList.prj_suffix(exp);
    switch (suffix) {
    | [] => false
    | [EmptyLine, ..._] =>
      switch (ZList.prj_z(exp)) {
      | ExpLineZ(zopseq) => ZExp.is_after_zopseq(zopseq)
      | CursorL(_, _)
      | LetLineZP(_)
      | LetLineZE(_)
      | LivelitDefLineZExpansionType(_)
      | LivelitDefLineZCaptures(_)
      | LivelitDefLineZModelType(_)
      | LivelitDefLineZActionType(_)
      | LivelitDefLineZInit(_)
      | LivelitDefLineZUpdate(_)
      | LivelitDefLineZView(_)
      | LivelitDefLineZShape(_)
      | LivelitDefLineZExpand(_) => false
      }
    | _ => false
    };
  };
  (prev_is_empty_line, next_is_empty_line);
};

let rec syn_cursor_info =
        (~steps=[], ctx: Contexts.t, ze: ZExp.t): option(CursorInfo.t) => {
  syn_cursor_info_zblock(~steps, ctx, ze);
}
and syn_cursor_info_zblock =
    (
      ~steps: CursorPath.steps,
      ctx: Contexts.t,
      (prefix, zline, suffix): ZExp.zblock,
    )
    : option(CursorInfo.t) => {
  switch (Statics_Exp.syn_lines(ctx, prefix)) {
  | None => None
  | Some(ctx) =>
    switch (
      syn_cursor_info_line(~steps=steps @ [List.length(prefix)], ctx, zline)
    ) {
    | None => None
    | Some(CursorNotOnDeferredVarPat(ci)) => Some(ci)
    | Some(CursorOnDeferredVarPat(deferred_ci, x)) =>
      let uses =
        UsageAnalysis.find_uses_block(
          ~offset=List.length(prefix) + 1,
          ~steps,
          x,
          suffix,
        );
      Some(uses |> deferred_ci);
    }
  };
}
and syn_cursor_info_line =
    (~steps: CursorPath.steps, ctx: Contexts.t, zline: ZExp.zline)
    : option(CursorInfo_common.deferrable(CursorInfo.t)) =>
  switch (zline) {
  | CursorL(_) =>
    Some(
      CursorNotOnDeferredVarPat(
        CursorInfo_common.mk(OnLine, ctx, extract_from_zline(zline)),
      ),
    )
  | ExpLineZ(ze) =>
    switch (syn_cursor_info_zopseq(~steps, ctx, ze)) {
    | None => None
    | Some(ci) => Some(CursorNotOnDeferredVarPat(ci))
    }
  // TODO review desired behavior w.r.t. abbrev err status
  | LetLineZP(_, zp, def) =>
    switch (LLPat.of_uhpat(ZPat.erase(zp))) {
    | None =>
      let ty_def =
        switch (Statics_Exp.syn(ctx, def)) {
        | Some(ty) => ty
        | None => HTyp.Hole
        };
      switch (
        CursorInfo_Pat.ana_cursor_info_zopseq(
          ~steps=steps @ [0],
          ctx,
          zp,
          ty_def,
        )
      ) {
      | None => None
      | Some(CursorNotOnDeferredVarPat(_)) as deferrable => deferrable
      | Some(CursorOnDeferredVarPat(deferred, x)) as deferrable =>
        switch (Statics_Exp.recursive_let_id(ZPat.erase(zp), def)) {
        | None => deferrable
        | Some(_) =>
          let rec_uses = UsageAnalysis.find_uses(~steps=steps @ [1], x, def);
          Some(
            CursorOnDeferredVarPat(uses => rec_uses @ uses |> deferred, x),
          );
        }
      };
    | Some(llp) =>
      let (_, cursor_position) = CursorPath_Pat.of_z(zp);
      Some(
        CursorOnDeferredVarPat(
          uses =>
            CursorInfo_common.mk(
              ~uses,
              PatAbbrev,
              ctx,
              Pat(cursor_position, UHPat.var(llp)),
            ),
          llp,
        ),
      );
    }
  | LetLineZE(_, p, zdef) =>
    switch (LLPat.of_uhpat(p)) {
    | None =>
      let def = ZExp.erase(zdef);
      let def_ctx = Statics_Exp.extend_let_def_ctx(ctx, p, def);
      let* (ty_p, _) = Statics_Pat.syn(ctx, p);
      ana_cursor_info(~steps=steps @ [1], def_ctx, zdef, ty_p)
      |> Option.map(ci => CursorInfo_common.CursorNotOnDeferredVarPat(ci));
    | Some(_) =>
      switch (LLExp.of_uhexp(ZExp.erase(zdef))) {
      | None =>
        syn_cursor_info(~steps=steps @ [1], ctx, zdef)
        |> Option.map(ci => CursorInfo_common.CursorNotOnDeferredVarPat(ci))
      | Some({hd, _}) =>
        let (zoperand, n) =
          switch (zdef) {
          | (_, ExpLineZ(ZOpSeq(_, ZOperand(zoperand, (prefix, _)))), _) => (
              zoperand,
              Seq.length_of_affix(prefix),
            )
          | _ =>
            failwith(
              "expected livelit exp to consist of single line spaced opseq",
            )
          };
        let (_, livelit_ctx) = ctx;
        switch (LivelitCtx.lookup(livelit_ctx, hd)) {
        | None =>
          syn_cursor_info_zoperand(~steps=steps @ [0, n], ctx, zoperand)
          |> Option.map(ci => CursorInfo_common.CursorNotOnDeferredVarPat(ci))
        | Some((old_def, applied_param_tys)) =>
          let unapplied_param_tys =
            ListUtil.drop(List.length(applied_param_tys), old_def.param_tys);
          if (n == 0) {
            let (_, cursor_position) = CursorPath_Exp.of_zoperand(zoperand);
            let operand = ZExp.erase_zoperand(zoperand);
            let ty =
              List.fold_right(
                ((_, param_ty), ty) => HTyp.Arrow(param_ty, ty),
                unapplied_param_tys,
                old_def.expansion_ty,
              );
            Some(
              CursorNotOnDeferredVarPat(
                CursorInfo_common.mk(
                  ExpAbbrevHead(ty),
                  ctx,
                  Exp(cursor_position, operand),
                ),
              ),
            );
          } else {
            let ci =
              switch (List.nth_opt(unapplied_param_tys, n)) {
              | Some((_, ty)) =>
                ana_cursor_info_zoperand(
                  ~steps=steps @ [0, n],
                  ctx,
                  zoperand,
                  ty,
                )
              | None =>
                syn_cursor_info_zoperand(
                  ~steps=steps @ [0, n],
                  ctx,
                  zoperand,
                )
              };
            ci
            |> Option.map(ci =>
                 CursorInfo_common.CursorNotOnDeferredVarPat(ci)
               );
          };
        };
      }
    }
  | LivelitDefLineZExpansionType({expansion_type, _}) =>
    CursorInfo_Typ.cursor_info(~steps=steps @ [1], ctx, expansion_type)
    |> Option.map(ci => CursorInfo_common.CursorNotOnDeferredVarPat(ci))
  | LivelitDefLineZCaptures({captures, _}) =>
    syn_cursor_info(~steps=steps @ [2], ctx, captures)
    |> Option.map(ci => CursorInfo_common.CursorNotOnDeferredVarPat(ci))
  | LivelitDefLineZModelType({model_type, _}) =>
    CursorInfo_Typ.cursor_info(~steps=steps @ [3], ctx, model_type)
    |> Option.map(ci => CursorInfo_common.CursorNotOnDeferredVarPat(ci))
  | LivelitDefLineZActionType({action_type, _}) =>
    CursorInfo_Typ.cursor_info(~steps=steps @ [4], ctx, action_type)
    |> Option.map(ci => CursorInfo_common.CursorNotOnDeferredVarPat(ci))
  | LivelitDefLineZInit({init, model_type, action_type, _}) =>
    let init_ty = Statics_Exp.ll_init_ty(model_type, action_type);
    ana_cursor_info(~steps=steps @ [5], ctx, init, init_ty)
    |> Option.map(ci => CursorInfo_common.CursorNotOnDeferredVarPat(ci));
  | LivelitDefLineZUpdate({update, model_type, action_type, _}) =>
    let update_ty = Statics_Exp.ll_update_ty(model_type, action_type);
    ana_cursor_info(~steps=steps @ [6], ctx, update, update_ty)
    |> Option.map(ci => CursorInfo_common.CursorNotOnDeferredVarPat(ci));
  | LivelitDefLineZView({view, model_type, action_type, _}) =>
    let view_ty = Statics_Exp.ll_view_ty(model_type, action_type);
    ana_cursor_info(~steps=steps @ [7], ctx, view, view_ty)
    |> Option.map(ci => CursorInfo_common.CursorNotOnDeferredVarPat(ci));
  | LivelitDefLineZShape({shape, model_type, action_type, _}) =>
    let shape_ty = Statics_Exp.ll_shape_ty(model_type, action_type);
    ana_cursor_info(~steps=steps @ [8], ctx, shape, shape_ty)
    |> Option.map(ci => CursorInfo_common.CursorNotOnDeferredVarPat(ci));
  | LivelitDefLineZExpand({expand, model_type, action_type, _}) =>
    let expand_ty = Statics_Exp.ll_expand_ty(model_type, action_type);
    ana_cursor_info(~steps=steps @ [9], ctx, expand, expand_ty)
    |> Option.map(ci => CursorInfo_common.CursorNotOnDeferredVarPat(ci));
  }
and syn_cursor_info_zopseq =
    (
      ~steps: CursorPath.steps,
      ctx: Contexts.t,
      ZOpSeq(skel, zseq) as zopseq: ZExp.zopseq,
    )
    : option(CursorInfo.t) => {
  // handle n-tuples:
  // cannot simply defer to syn_cursor_info_skel here
  // because it assumes binary tupling -- this would
  // cause sub-tuples to synthesize sub-product types,
  // but we want all comma operators in an opseq to
  // show the complete product type
  switch (zseq) {
  | ZOperator((_, Comma), _) =>
    // cursor on tuple comma
    Statics_Exp.syn_opseq(ctx, zopseq |> ZExp.erase_zopseq)
    |> Option.map(ty =>
         CursorInfo_common.mk(
           Synthesized(ty, ""),
           ctx,
           extract_from_zexp_zseq(zseq),
         )
       )
  | _ =>
    // cursor within tuple element
    let cursor_skel =
      skel
      |> UHExp.get_tuple_elements
      |> List.find(skel => ZOpSeq.skel_contains_cursor(skel, zseq));
    syn_cursor_info_skel(~steps, ctx, cursor_skel, zseq);
  };
}
and syn_cursor_info_skel =
    (
      ~steps: CursorPath.steps,
      ctx: Contexts.t,
      skel: UHExp.skel,
      zseq: ZExp.zseq,
    )
    : option(CursorInfo.t) => {
  let seq = zseq |> ZExp.erase_zseq;
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
      Statics_Exp.syn_skel(ctx, skel, seq)
      |> Option.map(ty =>
           CursorInfo_common.mk(
             Synthesized(ty, ""),
             ctx,
             extract_from_zexp_zseq(zseq),
           )
         )
    };
  } else {
    let check_livelit_first = backup => {
      let livelit_check = LivelitUtil.check_livelit(ctx, seq, skel);
      switch (livelit_check) {
      | Some((ApLivelitData(_), _, _, reqd_param_tys, args)) =>
        let (_, reqd_param_tys) = List.split(reqd_param_tys);
        let arg_results =
          List.combine(args, reqd_param_tys)
          |> List.filter_map(((arg, param_ty)) =>
               if (ZOpSeq.skel_contains_cursor(arg, zseq)) {
                 ana_cursor_info_skel(~steps, ctx, arg, zseq, param_ty);
               } else {
                 None;
               }
             );
        switch (arg_results) {
        | [result] => Some(result)
        | [_, _, ..._] => None
        | [] =>
          Statics_Exp.syn_skel(ctx, skel, seq)
          |> Option.map(ty =>
               CursorInfo_common.mk(
                 Synthesized(ty, ""),
                 ctx,
                 extract_from_zexp_zseq(zseq),
               )
             )
        };
      | _ => backup()
      };
    };
    // recurse toward cursor
    switch (skel) {
    | Placeholder(_) => None
    | BinOp(_, Comma, _, _) =>
      failwith(
        "Exp.syn_cursor_info_skel: expected commas to be handled at opseq level",
      )
    | BinOp(
        _,
        Minus | Plus | Times | Divide | LessThan | GreaterThan | Equals,
        skel1,
        skel2,
      ) =>
      switch (ana_cursor_info_skel(~steps, ctx, skel1, zseq, HTyp.Int)) {
      | Some(_) as result => result
      | None => ana_cursor_info_skel(~steps, ctx, skel2, zseq, Int)
      }
    | BinOp(
        _,
        FMinus | FPlus | FTimes | FDivide | FLessThan | FGreaterThan | FEquals,
        skel1,
        skel2,
      ) =>
      switch (ana_cursor_info_skel(~steps, ctx, skel1, zseq, HTyp.Float)) {
      | Some(_) as result => result
      | None => ana_cursor_info_skel(~steps, ctx, skel2, zseq, Float)
      }
    | BinOp(_, And | Or, skel1, skel2) =>
      switch (ana_cursor_info_skel(~steps, ctx, skel1, zseq, Bool)) {
      | Some(_) as result => result
      | None => ana_cursor_info_skel(~steps, ctx, skel2, zseq, Bool)
      }
    | BinOp(_, Caret, skel1, skel2) =>
      switch (ana_cursor_info_skel(~steps, ctx, skel1, zseq, String)) {
      | Some(_) as result => result
      | None => ana_cursor_info_skel(~steps, ctx, skel2, zseq, String)
      }
    | BinOp(_, Space, Placeholder(n) as skel1, skel2) =>
      check_livelit_first(() =>
        if (ZOpSeq.skel_contains_cursor(skel1, zseq)) {
          let zoperand =
            switch (zseq) {
            | ZOperator(_) => assert(false)
            | ZOperand(zoperand, _) => zoperand
            };
          let mk = typed =>
            CursorInfo_common.mk(typed, ctx, extract_from_zexp_zseq(zseq));
          switch (cursor_on_outer_expr(zoperand)) {
          | None =>
            syn_cursor_info_zoperand(~steps=steps @ [n], ctx, zoperand)
          | Some(StandardErr(InHole(WrongLength, _))) => None
          | Some(StandardErr(InHole(TypeInconsistent(None), _))) =>
            let operand_nih =
              zoperand
              |> ZExp.erase_zoperand
              |> UHExp.set_err_status_operand(NotInHole);
            let ty = Statics_Exp.syn_operand(ctx, operand_nih);
            switch (ty) {
            | Some(String) =>
              switch (zoperand) {
              | CursorE(OnText(j), StringLit(_, s)) =>
                switch (CursorInfo_common.is_invalid_escape_sequence(j, s)) {
                | Some(s) =>
                  ty
                  |> Option.map(ty =>
                       mk(SynErrorArrow(Arrow(Hole, Hole), ty, s))
                     )
                | None =>
                  ty
                  |> Option.map(ty =>
                       mk(SynErrorArrow(Arrow(Hole, Hole), ty, ""))
                     )
                }
              | _ =>
                ty
                |> Option.map(ty =>
                     mk(SynErrorArrow(Arrow(Hole, Hole), ty, ""))
                   )
              }
            | _ =>
              ty
              |> Option.map(ty =>
                   mk(SynErrorArrow(Arrow(Hole, Hole), ty, ""))
                 )
            };
          | Some(
              StandardErr(
                InHole(
                  TypeInconsistent(Some(InsufficientParams | DecodingError)),
                  _,
                ),
              ),
            ) =>
            Statics_Exp.syn_operand(ctx, ZExp.erase_zoperand(zoperand))
            |> Option.map(ty =>
                 mk(SynErrorArrow(Arrow(Hole, Hole), ty, ""))
               )
          | Some(
              StandardErr(
                InHole(TypeInconsistent(Some(IllTypedExpansion)), _),
              ),
            ) =>
            //TODO(andrew): Do we have access to the actual lldefn here? If so, expansion types below
            Some(mk(LivelitIllTypedExpansion(Hole, Hole)))
          | Some(VarErr(Free)) =>
            Some(mk(SynFreeArrow(Arrow(Hole, Hole))))
          | Some(VarErr(Keyword(k))) =>
            Some(mk(SynKeywordArrow(Arrow(Hole, Hole), k)))
          | Some(InconsistentBranchesErr(rule_types)) =>
            Some(mk(SynInconsistentBranchesArrow(rule_types, steps @ [n])))
          | Some(StandardErr(NotInHole)) =>
            let operand_nih =
              zoperand
              |> ZExp.erase_zoperand
              |> UHExp.set_err_status_operand(NotInHole);
            switch (operand_nih) {
            | InvalidText(_) =>
              Some(mk(SynInvalidArrow(Arrow(Hole, Hole))))
            | _ =>
              switch (
                Statics_Exp.syn_operand(ctx, zoperand |> ZExp.erase_zoperand)
              ) {
              | None => None
              | Some(ty) =>
                HTyp.matched_arrow(ty)
                |> Option.map(((ty1, ty2)) =>
                     mk(SynMatchingArrow(ty, Arrow(ty1, ty2)))
                   )
              }
            };
          };
        } else {
          switch (Statics_Exp.syn_skel(ctx, skel1, seq)) {
          | None => None
          | Some(ty) =>
            switch (HTyp.matched_arrow(ty)) {
            | None => None
            | Some((ty1, _)) =>
              ana_cursor_info_skel(~steps, ctx, skel2, zseq, ty1)
            }
          };
        }
      )
    | BinOp(_, Space, skel1, skel2) =>
      check_livelit_first(() =>
        switch (syn_cursor_info_skel(~steps, ctx, skel1, zseq)) {
        | Some(_) as result => result
        | None =>
          switch (Statics_Exp.syn_skel(ctx, skel1, seq)) {
          | None => None
          | Some(ty) =>
            switch (HTyp.matched_arrow(ty)) {
            | None => None
            | Some((ty1, _)) =>
              ana_cursor_info_skel(~steps, ctx, skel2, zseq, ty1)
            }
          }
        }
      )
    | BinOp(_, Cons, skel1, skel2) =>
      switch (syn_cursor_info_skel(~steps, ctx, skel1, zseq)) {
      | Some(_) as result => result
      | None =>
        switch (Statics_Exp.syn_skel(ctx, skel1, seq)) {
        | None => None
        | Some(ty_elt) =>
          let ty_list = HTyp.List(ty_elt);
          ana_cursor_info_skel(~steps, ctx, skel2, zseq, ty_list);
        }
      }
    };
  };
}
and syn_cursor_info_zoperand =
    (~steps: CursorPath.steps, ctx: Contexts.t, zoperand: ZExp.zoperand)
    : option(CursorInfo.t) => {
  let cursor_term = extract_from_zexp_operand(zoperand);
  switch (zoperand) {
  | CursorE(_, InvalidText(_)) =>
    Some(CursorInfo_common.mk(SynInvalid, ctx, cursor_term))
  | CursorE(_, Var(_, InVarHole(Keyword(k), _), _)) =>
    Some(CursorInfo_common.mk(SynKeyword(k), ctx, cursor_term))
  | CursorE(_, Var(_, InVarHole(Free, _), _)) =>
    Some(CursorInfo_common.mk(SynFree, ctx, cursor_term))
  | CursorE(_, Case(InconsistentBranches(rule_types, _), _, _)) =>
    Some(
      CursorInfo_common.mk(
        SynInconsistentBranches(rule_types, steps),
        ctx,
        cursor_term,
      ),
    )
  | CursorE(
      _,
      ApLivelit(
        _,
        InHole(TypeInconsistent(Some(IllTypedExpansion)), _),
        _,
        name,
        model,
        splice_info,
      ),
    ) =>
    let* declared_ty = Statics_Exp.declared_livelit_expansion_type(ctx, name);
    let* actual_ty =
      Statics_Exp.actual_livelit_expansion_type(
        ctx,
        name,
        model,
        splice_info,
      );
    Some(
      CursorInfo_common.mk(
        LivelitIllTypedExpansion(declared_ty, actual_ty),
        ctx,
        cursor_term,
      ),
    );
  | CursorE(
      _,
      ApLivelit(
        _,
        InHole(TypeInconsistent(Some(DecodingError)), _),
        _,
        _,
        _,
        _,
      ),
    ) =>
    Some(CursorInfo_common.mk(SynLivelitDecodingError, ctx, cursor_term))
  | CursorE(_, e) =>
    switch (Statics_Exp.syn_operand(ctx, e)) {
    | None => None
    | Some(ty) =>
      Some(CursorInfo_common.mk(Synthesized(ty, ""), ctx, cursor_term))
    }
  | ParenthesizedZ(zbody) => syn_cursor_info(~steps=steps @ [0], ctx, zbody)
  | LamZP(_, zp, body) =>
    let* (ty, _) = Statics_Pat.syn(ctx, ZPat.erase(zp));
    let+ defferrable =
      CursorInfo_Pat.ana_cursor_info(~steps=steps @ [0], ctx, zp, ty);
    switch (defferrable) {
    | CursorNotOnDeferredVarPat(ci) => ci
    | CursorOnDeferredVarPat(deferred_ci, x) =>
      let uses = UsageAnalysis.find_uses(~steps=steps @ [1], x, body);
      uses |> deferred_ci;
    };
  | LamZE(_, p, zbody) =>
    let* (_, body_ctx) = Statics_Pat.syn_opseq(ctx, p);
    syn_cursor_info(~steps=steps @ [1], body_ctx, zbody);
  | InjZ(_, _, zbody) => syn_cursor_info(~steps=steps @ [0], ctx, zbody)
  | CaseZE(_, zscrut, rules) =>
    let ty_join =
      switch (Statics_Exp.joined_pattern_type(ctx, rules)) {
      | Some(ty) => ty
      | _ => HTyp.Hole
      };
    /* Note that strictly speaking this should just be syn_cursor_info;
     * This provides a bit of potentially useful type information to
     * the user in the case where some of pattern branches are already
     * populated with patterns having a consistent type. */
    ana_cursor_info(~steps=steps @ [0], ctx, zscrut, ty_join);
  | CaseZR(_, scrut, (prefix, zrule, suffix)) =>
    switch (Statics_Exp.syn(ctx, scrut)) {
    | None => None
    | Some(pat_ty) =>
      /* lub of all of the branches except the one with the cursor */
      let lub_opt: option(CursorInfo.join_of_branches) =
        switch (prefix @ suffix) {
        | [] => Some(NoBranches)
        | other_branches =>
          let clause_types =
            List.fold_left(
              (types_opt, r) =>
                switch (types_opt) {
                | None => None
                | Some(types) =>
                  switch (Statics_Exp.syn_rule(ctx, r, pat_ty)) {
                  | None => None
                  | Some(r_ty) => Some([r_ty, ...types])
                  }
                },
              Some([]),
              other_branches,
            );
          switch (clause_types) {
          | None => None
          | Some(types) =>
            switch (HTyp.join_all(LUB, types)) {
            | None => Some(InconsistentBranchTys(List.rev(types), steps))
            | Some(lub) => Some(JoinTy(lub))
            }
          };
        };
      switch (lub_opt) {
      | None => None
      | Some(lub) =>
        syn_cursor_info_rule(
          ~steps=steps @ [1 + List.length(prefix)],
          ctx,
          zrule,
          pat_ty,
          lub,
          List.length(prefix),
        )
      };
    }
  | SubscriptZE1(_, ztarget, _, _) =>
    ana_cursor_info(~steps=steps @ [0], ctx, ztarget, String)
  | SubscriptZE2(_, _, zstart_, _) =>
    ana_cursor_info(~steps=steps @ [1], ctx, zstart_, Int)
  | SubscriptZE3(_, _, _, zend_) =>
    ana_cursor_info(~steps=steps @ [2], ctx, zend_, Int)
  | ApLivelitZ(_, _, _, _, _, zsi) =>
    let (n, (ty, zblock)) = ZIntMap.prj_z_kv(zsi.zsplice_map);
    ana_cursor_info(~steps=steps @ [n], ctx, zblock, ty);
  };
}
and ana_cursor_info =
    (~steps=[], ctx: Contexts.t, ze: ZExp.t, ty: HTyp.t)
    : option(CursorInfo.t) =>
  ana_cursor_info_zblock(~steps, ctx, ze, ty)
and ana_cursor_info_zblock =
    (
      ~steps: CursorPath.steps,
      ctx: Contexts.t,
      (prefix, zline, suffix): ZExp.zblock,
      ty: HTyp.t,
    )
    : option(CursorInfo.t) =>
  switch (Statics_Exp.syn_lines(ctx, prefix)) {
  | None => None
  | Some(ctx) =>
    switch (suffix) {
    | [] =>
      switch (zline) {
      | CursorL(_)
      | LetLineZP(_)
      | LetLineZE(_)
      | LivelitDefLineZExpansionType(_)
      | LivelitDefLineZCaptures(_)
      | LivelitDefLineZModelType(_)
      | LivelitDefLineZActionType(_)
      | LivelitDefLineZInit(_)
      | LivelitDefLineZUpdate(_)
      | LivelitDefLineZView(_)
      | LivelitDefLineZShape(_)
      | LivelitDefLineZExpand(_) => None
      | ExpLineZ(zopseq) =>
        ana_cursor_info_zopseq(
          ~steps=steps @ [List.length(prefix)],
          ctx,
          zopseq,
          ty,
        )
      }
    | [_, ..._] =>
      switch (
        syn_cursor_info_line(
          ~steps=steps @ [List.length(prefix)],
          ctx,
          zline,
        )
      ) {
      | None => None
      | Some(CursorNotOnDeferredVarPat(ci)) => Some(ci)
      | Some(CursorOnDeferredVarPat(deferred_ci, x)) =>
        let uses =
          UsageAnalysis.find_uses_block(
            ~offset=List.length(prefix) + 1,
            ~steps,
            x,
            suffix,
          );
        Some(uses |> deferred_ci);
      }
    }
  }
and ana_cursor_info_zopseq =
    (
      ~steps: CursorPath.steps,
      ctx: Contexts.t,
      ZOpSeq(skel, zseq) as zopseq: ZExp.zopseq,
      ty: HTyp.t,
    )
    : option(CursorInfo.t) => {
  let cursor_term = extract_from_zexp_zseq(zseq);
  switch (zseq) {
  | ZOperator((_, Comma), _) =>
    // cursor on tuple comma
    let opseq = ZExp.erase_zopseq(zopseq);
    let err = UHExp.get_err_status_opseq(opseq);
    switch (err) {
    | NotInHole =>
      Some(CursorInfo_common.mk(Analyzed(ty), ctx, cursor_term))
    | InHole(WrongLength, _) =>
      let expected_length = ty |> HTyp.get_prod_elements |> List.length;
      let got_length = skel |> UHExp.get_tuple_elements |> List.length;
      Some(
        CursorInfo_common.mk(
          AnaWrongLength(expected_length, got_length, ty),
          ctx,
          cursor_term,
        ),
      );
    | InHole(TypeInconsistent(_), _) =>
      let opseq' = UHExp.set_err_status_opseq(NotInHole, opseq);
      Statics_Exp.syn_opseq(ctx, opseq')
      |> Option.map(ty' =>
           CursorInfo_common.mk(
             AnaTypeInconsistent(ty, ty', ""),
             ctx,
             cursor_term,
           )
         );
    };
  | _ =>
    // cursor in tuple element
    switch (Statics_Exp.tuple_zip(skel, ty)) {
    | None =>
      // wrong length, switch to syn
      let zopseq_not_in_hole =
        zopseq |> ZExp.set_err_status_zopseq(NotInHole);
      syn_cursor_info_zopseq(~steps, ctx, zopseq_not_in_hole);
    | Some(skel_tys) =>
      let (cursor_skel, cursor_skel_ty) =
        skel_tys
        |> List.find(((skel, _)) => ZOpSeq.skel_contains_cursor(skel, zseq));
      ana_cursor_info_skel(~steps, ctx, cursor_skel, zseq, cursor_skel_ty);
    }
  };
}
and ana_cursor_info_skel =
    // steps of whole opseq
    (
      ~steps: CursorPath.steps,
      ctx: Contexts.t,
      skel: UHExp.skel,
      zseq: ZExp.zseq,
      ty: HTyp.t,
    )
    : option(CursorInfo.t) => {
  let cursor_term = extract_from_zexp_zseq(zseq);
  let syn_go = skel => syn_cursor_info_skel(~steps, ctx, skel, zseq);
  let ana_go = (skel, ty) =>
    ana_cursor_info_skel(~steps, ctx, skel, zseq, ty);
  let seq = zseq |> ZExp.erase_zseq;
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
      let opseq = ZExp.erase_zopseq(ZOpSeq.ZOpSeq(skel, zseq));
      let err = UHExp.get_err_status_opseq(opseq);
      switch (err) {
      | NotInHole =>
        Statics_Exp.ana_skel(ctx, skel, seq, ty)
        |> Option.map(_ =>
             CursorInfo_common.mk(Analyzed(ty), ctx, cursor_term)
           )
      | InHole(WrongLength, _) =>
        failwith(__LOC__ ++ ": n-tuples handled at opseq level")
      | InHole(TypeInconsistent(_), _) =>
        let opseq' = UHExp.set_err_status_opseq(NotInHole, opseq);
        Statics_Exp.syn_opseq(ctx, opseq')
        |> Option.map(ty' =>
             CursorInfo_common.mk(
               AnaTypeInconsistent(ty, ty', ""),
               ctx,
               cursor_term,
             )
           );
      };
    };
  } else {
    // recurse toward cursor
    switch (skel) {
    | Placeholder(_) => None
    | BinOp(InHole(_), _, _, _) => syn_go(skel)
    | BinOp(_, Comma, _, _) =>
      failwith(
        "Exp.ana_cursor_info_skel: expected commas too be handled at opseq level",
      )
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (HTyp.matched_list(ty)) {
      | None => None
      | Some(ty_elt) =>
        switch (ana_go(skel1, ty_elt)) {
        | Some(_) as result => result
        | None =>
          let ty_list = HTyp.List(ty_elt);
          ana_go(skel2, ty_list);
        }
      }
    | BinOp(
        _,
        Plus | Minus | Times | Divide | FPlus | FMinus | FTimes | FDivide |
        LessThan |
        GreaterThan |
        Equals |
        FLessThan |
        FGreaterThan |
        FEquals |
        And |
        Or |
        Caret |
        Space,
        _,
        _,
      ) =>
      syn_go(skel)
    };
  };
}
and ana_cursor_info_zoperand =
    (
      ~steps: CursorPath.steps,
      ctx: Contexts.t,
      zoperand: ZExp.zoperand,
      ty: HTyp.t,
    )
    : option(CursorInfo.t) => {
  let cursor_term = extract_from_zexp_operand(zoperand);
  switch (zoperand) {
  | CursorE(_, e) =>
    switch (e) {
    /* in hole */
    | Var(_, InVarHole(Keyword(k), _), _) =>
      Some(CursorInfo_common.mk(AnaKeyword(ty, k), ctx, cursor_term))
    | Var(_, InVarHole(Free, _), _) =>
      Some(CursorInfo_common.mk(AnaFree(ty), ctx, cursor_term))
    | InvalidText(_) =>
      Some(CursorInfo_common.mk(AnaInvalid(ty), ctx, cursor_term))
    | Var(InHole(TypeInconsistent(_), _), _, _)
    | IntLit(InHole(TypeInconsistent(_), _), _)
    | FloatLit(InHole(TypeInconsistent(_), _), _)
    | BoolLit(InHole(TypeInconsistent(_), _), _)
    | StringLit(InHole(TypeInconsistent(_), _), _)
    | ListNil(InHole(TypeInconsistent(_), _))
    | Lam(InHole(TypeInconsistent(_), _), _, _)
    | Inj(InHole(TypeInconsistent(_), _), _, _)
    | Case(StandardErrStatus(InHole(TypeInconsistent(_), _)), _, _)
    | Subscript(InHole(TypeInconsistent(_), _), _, _, _)
    | ApLivelit(_, InHole(TypeInconsistent(None), _), _, _, _, _) =>
      let operand' =
        zoperand
        |> ZExp.erase_zoperand
        |> UHExp.set_err_status_operand(NotInHole);
      switch (Statics_Exp.syn_operand(ctx, operand')) {
      | None => None
      | Some(ty') =>
        switch (zoperand) {
        | CursorE(OnText(j), StringLit(_, s)) =>
          switch (CursorInfo_common.is_invalid_escape_sequence(j, s)) {
          | None =>
            Some(
              CursorInfo_common.mk(
                AnaTypeInconsistent(ty, ty', ""),
                ctx,
                cursor_term,
              ),
            )
          | Some(s) =>
            Some(
              CursorInfo_common.mk(
                AnaTypeInconsistent(ty, ty', s),
                ctx,
                cursor_term,
              ),
            )
          }
        | _ =>
          Some(
            CursorInfo_common.mk(
              AnaTypeInconsistent(ty, ty', ""),
              ctx,
              cursor_term,
            ),
          )
        }
      };
    | Var(InHole(WrongLength, _), _, _)
    | IntLit(InHole(WrongLength, _), _)
    | FloatLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | StringLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _))
    | Lam(InHole(WrongLength, _), _, _)
    | Inj(InHole(WrongLength, _), _, _)
    | Case(
        StandardErrStatus(InHole(WrongLength, _)) | InconsistentBranches(_),
        _,
        _,
      )
    | Subscript(InHole(WrongLength, _), _, _, _)
    | ApLivelit(_, InHole(WrongLength, _), _, _, _, _) => None
    /* not in hole */
    | EmptyHole(_)
    | Var(NotInHole, NotInVarHole, _)
    | IntLit(NotInHole, _)
    | FloatLit(NotInHole, _)
    | BoolLit(NotInHole, _)
    | StringLit(NotInHole, _)
    | Subscript(NotInHole, _, _, _)
    | ApLivelit(_, NotInHole, _, _, _, _) =>
      switch (Statics_Exp.syn_operand(ctx, e)) {
      | None => None
      | Some(ty') =>
        switch (zoperand) {
        | CursorE(OnText(j), StringLit(_, s)) =>
          switch (CursorInfo_common.is_invalid_escape_sequence(j, s)) {
          | None =>
            Some(
              CursorInfo_common.mk(
                AnaSubsumed(ty, ty', ""),
                ctx,
                cursor_term,
              ),
            )
          | Some(s) =>
            Some(
              CursorInfo_common.mk(
                AnaSubsumed(ty, ty', s),
                ctx,
                cursor_term,
              ),
            )
          }
        | _ =>
          Some(
            CursorInfo_common.mk(AnaSubsumed(ty, ty', ""), ctx, cursor_term),
          )
        }
      }
    | ApLivelit(
        _,
        InHole(TypeInconsistent(Some(InsufficientParams)), _),
        _,
        _,
        _,
        _,
      ) =>
      switch (Statics_Exp.syn_operand(ctx, e)) {
      | None => None
      | Some(ty') =>
        Some(
          CursorInfo_common.mk(
            AnaInsufficientLivelitArgs(ty, ty'),
            ctx,
            cursor_term,
          ),
        )
      }

    | ApLivelit(
        _,
        InHole(TypeInconsistent(Some(IllTypedExpansion)), _),
        _,
        name,
        model,
        splice_info,
      ) =>
      let* declared_ty =
        Statics_Exp.declared_livelit_expansion_type(ctx, name);
      let* actual_ty =
        Statics_Exp.actual_livelit_expansion_type(
          ctx,
          name,
          model,
          splice_info,
        );
      Some(
        CursorInfo_common.mk(
          LivelitIllTypedExpansion(declared_ty, actual_ty),
          ctx,
          cursor_term,
        ),
      );
    | ApLivelit(
        _,
        InHole(TypeInconsistent(Some(DecodingError)), _),
        _,
        _,
        _,
        _,
      ) =>
      Some(
        CursorInfo_common.mk(AnaLivelitDecodingError(ty), ctx, cursor_term),
      )
    | FreeLivelit(_, _) =>
      Some(CursorInfo_common.mk(AnaFreeLivelit(ty), ctx, cursor_term))
    | ListNil(NotInHole)
    | Inj(NotInHole, _, _)
    | Case(StandardErrStatus(NotInHole), _, _) =>
      Some(CursorInfo_common.mk(Analyzed(ty), ctx, cursor_term))
    | Parenthesized(body) =>
      Statics_Exp.ana(ctx, body, ty)
      |> Option.map(_ =>
           CursorInfo_common.mk(Analyzed(ty), ctx, cursor_term)
         )

    | Lam(NotInHole, p, body) =>
      let* (ty_p, body_ctx) = Statics_Pat.syn(ctx, p);
      let+ ty_body = Statics_Exp.syn(body_ctx, body);
      CursorInfo_common.mk(
        AnaAnnotatedLambda(ty, Arrow(ty_p, ty_body)),
        ctx,
        cursor_term,
      );
    /* zipper cases */
    }
  | ParenthesizedZ(zbody) =>
    ana_cursor_info(~steps=steps @ [0], ctx, zbody, ty) /* zipper in hole */
  | LamZP(InHole(WrongLength, _), _, _)
  | LamZE(InHole(WrongLength, _), _, _)
  | InjZ(InHole(WrongLength, _), _, _)
  | CaseZE(
      StandardErrStatus(InHole(WrongLength, _)) | InconsistentBranches(_, _),
      _,
      _,
    )
  | CaseZR(
      StandardErrStatus(InHole(WrongLength, _)) | InconsistentBranches(_, _),
      _,
      _,
    )
  | SubscriptZE1(InHole(WrongLength, _), _, _, _)
  | SubscriptZE2(InHole(WrongLength, _), _, _, _)
  | SubscriptZE3(InHole(WrongLength, _), _, _, _)
  | ApLivelitZ(_, InHole(WrongLength, _), _, _, _, _) => None
  | LamZP(InHole(TypeInconsistent(_), _), _, _)
  | LamZE(InHole(TypeInconsistent(_), _), _, _)
  | InjZ(InHole(TypeInconsistent(_), _), _, _)
  | CaseZE(StandardErrStatus(InHole(TypeInconsistent(_), _)), _, _)
  | CaseZR(StandardErrStatus(InHole(TypeInconsistent(_), _)), _, _)
  | SubscriptZE1(InHole(TypeInconsistent(_), _), _, _, _)
  | SubscriptZE2(InHole(TypeInconsistent(_), _), _, _, _)
  | SubscriptZE3(InHole(TypeInconsistent(_), _), _, _, _)
  | ApLivelitZ(_) => syn_cursor_info_zoperand(~steps, ctx, zoperand)
  /* zipper not in hole */
  | LamZP(NotInHole, zp, body) =>
    let* (ty_p_given, _) = HTyp.matched_arrow(ty);
    let+ defferrable =
      CursorInfo_Pat.ana_cursor_info(
        ~steps=steps @ [0],
        ctx,
        zp,
        ty_p_given,
      );
    switch (defferrable) {
    | CursorNotOnDeferredVarPat(ci) => ci
    | CursorOnDeferredVarPat(deferred_ci, x) =>
      let uses = UsageAnalysis.find_uses(~steps=steps @ [1], x, body);
      uses |> deferred_ci;
    };

  | LamZE(NotInHole, p, zbody) =>
    let* (ty_p_given, ty_body_given) = HTyp.matched_arrow(ty);
    switch (Statics_Pat.ana(ctx, p, ty_p_given)) {
    | None => None
    | Some(body_ctx) =>
      ana_cursor_info(~steps=steps @ [1], body_ctx, zbody, ty_body_given)
    };
  | InjZ(NotInHole, position, zbody) =>
    switch (HTyp.matched_sum(ty)) {
    | None => None
    | Some((ty1, ty2)) =>
      ana_cursor_info(
        ~steps=steps @ [0],
        ctx,
        zbody,
        InjSide.pick(position, ty1, ty2),
      )
    }
  | CaseZE(StandardErrStatus(NotInHole), zscrut, _) =>
    syn_cursor_info(~steps=steps @ [0], ctx, zscrut)
  | CaseZR(StandardErrStatus(NotInHole), scrut, (prefix, zrule, _)) =>
    switch (Statics_Exp.syn(ctx, scrut)) {
    | None => None
    | Some(ty1) =>
      ana_cursor_info_rule(
        ~steps=steps @ [1 + List.length(prefix)],
        ctx,
        zrule,
        ty1,
        ty,
      )
    }
  | SubscriptZE1(NotInHole, ztarget, _, _) =>
    ana_cursor_info(~steps=steps @ [0], ctx, ztarget, String)
  | SubscriptZE2(NotInHole, _, zstart_, _) =>
    ana_cursor_info(~steps=steps @ [1], ctx, zstart_, Int)
  | SubscriptZE3(NotInHole, _, _, zend_) =>
    ana_cursor_info(~steps=steps @ [2], ctx, zend_, Int)
  };
}
and syn_cursor_info_rule =
    (
      ~steps: CursorPath.steps,
      ctx: Contexts.t,
      zrule: ZExp.zrule,
      pat_ty: HTyp.t,
      lub: CursorInfo.join_of_branches,
      rule_index: int,
    )
    : option(CursorInfo.t) =>
  switch (zrule) {
  | CursorR(_) =>
    Some(CursorInfo_common.mk(OnRule, ctx, extract_from_zrule(zrule)))
  | RuleZP(zp, clause) =>
    switch (
      CursorInfo_Pat.ana_cursor_info(~steps=steps @ [0], ctx, zp, pat_ty)
    ) {
    | None => None
    | Some(CursorNotOnDeferredVarPat(ci)) => Some(ci)
    | Some(CursorOnDeferredVarPat(deferred_ci, x)) =>
      let uses = UsageAnalysis.find_uses(~steps=steps @ [1], x, clause);
      Some(deferred_ci(uses));
    }

  | RuleZE(p, zclause) =>
    switch (Statics_Pat.ana(ctx, p, pat_ty)) {
    | None => None
    | Some(ctx) =>
      let cursor_info = syn_cursor_info(~steps=steps @ [1], ctx, zclause);
      /* Check if the cursor is on the outermost form of the clause */
      let is_outer = ZExp.is_outer(zclause);
      switch (is_outer, cursor_info) {
      | (_, None) => None
      | (false, _) => cursor_info
      | (true, Some({typed, ctx, uses, _})) =>
        let typed = CursorInfo.SynBranchClause(lub, typed, rule_index);
        let cursor_term = extract_from_zrule(zrule);
        Some({cursor_term, typed, ctx, uses});
      };
    }
  }
and ana_cursor_info_rule =
    (
      ~steps: CursorPath.steps,
      ctx: Contexts.t,
      zrule: ZExp.zrule,
      pat_ty: HTyp.t,
      clause_ty: HTyp.t,
    )
    : option(CursorInfo.t) =>
  switch (zrule) {
  | CursorR(_) =>
    Some(CursorInfo_common.mk(OnRule, ctx, extract_from_zrule(zrule)))
  | RuleZP(zp, clause) =>
    switch (
      CursorInfo_Pat.ana_cursor_info(~steps=steps @ [0], ctx, zp, pat_ty)
    ) {
    | None => None
    | Some(CursorNotOnDeferredVarPat(ci)) => Some(ci)
    | Some(CursorOnDeferredVarPat(deferred_ci, x)) =>
      let uses = UsageAnalysis.find_uses(~steps=steps @ [1], x, clause);
      Some(deferred_ci(uses));
    }
  | RuleZE(p, zclause) =>
    switch (Statics_Pat.ana(ctx, p, pat_ty)) {
    | None => None
    | Some(ctx) =>
      ana_cursor_info(~steps=steps @ [1], ctx, zclause, clause_ty)
    }
  };
