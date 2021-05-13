type cursor_term = CursorInfo.cursor_term;
type zoperand = CursorInfo_common.zoperand;

let rec extract_cursor_term = (exp: ZExp.t): cursor_term => {
  extract_from_zline(ZList.prj_z(exp));
}
and extract_from_zline = (zline: ZExp.zline): cursor_term => {
  switch (zline) {
  | CursorL(cursor_pos, uex_line) => Line(cursor_pos, uex_line)
  | ExpLineZ(zopseq) => extract_from_zexp_opseq(zopseq)
  | LetLineZP(zpat, _, _) => CursorInfo_Pat.extract_cursor_term(zpat)
  | LetLineZA(_, ztyp, _) => CursorInfo_Typ.extract_cursor_term(ztyp)
  | LetLineZE(_, _, zexp) => extract_cursor_term(zexp)
  | StructLineZP(zpat, _, _) => CursorInfo_Pat.extract_cursor_term(zpat)
  | StructLineZE(_, _, ze) => extract_cursor_term(ze)
  };
}
and extract_from_zexp_operand = (zexp_operand: ZExp.zoperand): cursor_term => {
  switch (zexp_operand) {
  | CursorE(cursor_pos, operand) => Exp(cursor_pos, operand)
  | ParenthesizedZ(zexp) => extract_cursor_term(zexp)
  | LamZP(_, zpat, _, _) => CursorInfo_Pat.extract_cursor_term(zpat)
  | LamZA(_, _, ztyp, _) => CursorInfo_Typ.extract_cursor_term(ztyp)
  | LamZE(_, _, _, zexp)
  | InjZ(_, _, zexp)
  | CaseZE(_, zexp, _) => extract_cursor_term(zexp)
  | CaseZR(_, _, zrules) => extract_from_zrules(zrules)
  | ApPaletteZ(_, _, _, _) => failwith("ApPalette is not implemented")
  | PrjZE(_, zoperand, _) => extract_from_zexp_operand(zoperand)
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
  | LetLineZP(zpat, _, _) => CursorInfo_Pat.get_zoperand_from_zpat(zpat)
  | LetLineZA(_, ztyp, _) => CursorInfo_Typ.get_zoperand_from_ztyp(ztyp)
  | LetLineZE(_, _, zexp) => get_zoperand_from_zexp(zexp)
  | StructLineZP(zpat, _, _) => CursorInfo_Pat.get_zoperand_from_zpat(zpat)
  | StructLineZE(_, _, ze) => get_zoperand_from_zexp(ze)
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
    (zoperand: ZExp.zoperand): option(zoperand) => {
  switch (zoperand) {
  | CursorE(_, _) => Some(ZExp(zoperand))
  | ParenthesizedZ(zexp) => get_zoperand_from_zexp(zexp)
  | LamZP(_, zpat, _, _) => CursorInfo_Pat.get_zoperand_from_zpat(zpat)
  | LamZA(_, _, ztyp, _) => CursorInfo_Typ.get_zoperand_from_ztyp(ztyp)
  | LamZE(_, _, _, zexp)
  | InjZ(_, _, zexp)
  | CaseZE(_, zexp, _) => get_zoperand_from_zexp(zexp)
  | CaseZR(_, _, zrules) => get_zoperand_from_zrules(zrules)
  | ApPaletteZ(_, _, _, _) => failwith("not implemented")
  | PrjZE(_, zoperand_, _) => get_zoperand_from_zexp_operand(zoperand_)
  };
}
and get_zoperand_from_zrules = (zrules: ZExp.zrules): option(zoperand) => {
  get_zoperand_from_zrule(ZList.prj_z(zrules));
}
and get_zoperand_from_zrule = (zrule: ZExp.zrule): option(zoperand) => {
  switch (zrule) {
  | CursorR(_, _) => None
  | RuleZP(zpat, _) => CursorInfo_Pat.get_zoperand_from_zpat(zpat)
  | RuleZE(_, zexp) => get_zoperand_from_zexp(zexp)
  };
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
  | LetLineZP(_, _, _)
  | LetLineZA(_, _, _) => outer_zrules
  | LetLineZE(_, _, zexp) => get_outer_zrules_from_zexp(zexp, outer_zrules)
  | StructLineZP(_, _, _) => outer_zrules // TODO (hejohns): ?
  | StructLineZE(_, _, ze) => get_outer_zrules_from_zexp(ze, outer_zrules)
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
  | LamZP(_, _, _, _)
  | LamZA(_, _, _, _) => outer_zrules
  | LamZE(_, _, _, zexp)
  | InjZ(_, _, zexp)
  | CaseZE(_, zexp, _) => get_outer_zrules_from_zexp(zexp, outer_zrules)
  | CaseZR(_, _, zrules) => get_outer_zrules_from_zrules(zrules)
  | ApPaletteZ(_, _, _, _) => failwith("not implemented")
  | PrjZE(_, zoperand_, _) =>
    get_outer_zrules_from_zexp_operand(zoperand_, outer_zrules)
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
  | InconsistentBranchesErr(list(HTyp.t))
  | LabelErr(LabelErrStatus.HoleReason.t);

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
      | LetLineZP(_, _, _)
      | LetLineZA(_, _, _)
      | LetLineZE(_, _, _) => true
      | StructLineZP(_, _, _)
      | StructLineZE(_, _, _) => failwith("TODO (hejohns): ?")
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
      | LetLineZP(_, _, _)
      | LetLineZA(_, _, _)
      | LetLineZE(_, _, _) => false
      | StructLineZP(_, _, _)
      | StructLineZE(_, _, _) => failwith("TODO (hejohns): ?")
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
    : option(CursorInfo.t) =>
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
  | LetLineZP(zp, ann, def) =>
    let pat_ci = CursorInfo_Pat.ana_cursor_info(~steps=steps @ [0], ctx, zp);
    switch (ann) {
    | None =>
      switch (Statics_Exp.syn(ctx, def)) {
      | None => None
      | Some(ty1) => pat_ci(ty1)
      }
    | Some(ann) =>
      let ty1 = ann |> UHTyp.expand;
      switch (pat_ci(ty1)) {
      | None => None
      | Some(CursorNotOnDeferredVarPat(_)) as deferrable => deferrable
      | Some(CursorOnDeferredVarPat(deferred, x)) as deferrable =>
        switch (HTyp.matched_arrow(ty1)) {
        | None => deferrable
        | Some(_) =>
          let rec_uses = UsageAnalysis.find_uses(~steps=steps @ [2], x, def);
          Some(
            CursorOnDeferredVarPat(uses => rec_uses @ uses |> deferred, x),
          );
        }
      };
    };
  | LetLineZA(_, zann, _) =>
    CursorInfo_Typ.cursor_info(~steps=steps @ [1], ctx, zann)
    |> Option.map(ci => CursorInfo_common.CursorNotOnDeferredVarPat(ci))
  | LetLineZE(p, ann, zdef) =>
    switch (ann) {
    | None =>
      syn_cursor_info(~steps=steps @ [2], ctx, zdef)
      |> Option.map(ci => CursorInfo_common.CursorNotOnDeferredVarPat(ci))
    | Some(ann) =>
      let ty = UHTyp.expand(ann);
      let (ctx_def, _) =
        Statics_Exp.ctx_for_let(ctx, p, ty, zdef |> ZExp.erase);
      ana_cursor_info(~steps=steps @ [2], ctx_def, zdef, ty)
      |> Option.map(ci => CursorInfo_common.CursorNotOnDeferredVarPat(ci));
    }
  | StructLineZP(zp, _, def) =>
    // TODO (hejohns): ?
    let pat_ci = CursorInfo_Pat.ana_cursor_info(~steps=steps @ [0], ctx, zp);
    switch (Statics_Exp.syn(ctx, def)) {
    | None => None
    | Some(ty1) => pat_ci(ty1)
    };
  | StructLineZE(_, _, zdef) =>
    syn_cursor_info(~steps=steps @ [1], ctx, zdef)
    |> Option.map(ci => CursorInfo_common.CursorNotOnDeferredVarPat(ci))
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
           Synthesized(ty),
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
             Synthesized(ty),
             ctx,
             extract_from_zexp_zseq(zseq),
           )
         )
    };
  } else {
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
    | BinOp(_, Space, Placeholder(n) as skel1, skel2) =>
      if (ZOpSeq.skel_contains_cursor(skel1, zseq)) {
        let zoperand =
          switch (zseq) {
          | ZOperator(_) => assert(false)
          | ZOperand(zoperand, _) => zoperand
          };
        let mk = typed =>
          CursorInfo_common.mk(typed, ctx, extract_from_zexp_zseq(zseq));
        switch (cursor_on_outer_expr(zoperand)) {
        | None => syn_cursor_info_zoperand(~steps=steps @ [n], ctx, zoperand)
        | Some(StandardErr(InHole(WrongLength, _))) => None
        | Some(StandardErr(InHole(TypeInconsistent, _))) =>
          let operand_nih =
            zoperand
            |> ZExp.erase_zoperand
            |> UHExp.set_err_status_operand(NotInHole);
          Statics_Exp.syn_operand(ctx, operand_nih)
          |> Option.map(ty => mk(SynErrorArrow(Arrow(Hole, Hole), ty)));
        | Some(VarErr(Free)) => Some(mk(SynFreeArrow(Arrow(Hole, Hole))))
        | Some(VarErr(Keyword(k))) =>
          Some(mk(SynKeywordArrow(Arrow(Hole, Hole), k)))
        | Some(InconsistentBranchesErr(rule_types)) =>
          Some(mk(SynInconsistentBranchesArrow(rule_types, steps @ [n])))
        | Some(LabelErr(Standalone))
        | Some(LabelErr(Duplicate))
        | Some(LabelErr(Empty)) =>
          failwith(__LOC__ ++ "Unimplemented Label Err Status")
        | Some(StandardErr(NotInHole)) =>
          let operand_nih =
            zoperand
            |> ZExp.erase_zoperand
            |> UHExp.set_err_status_operand(NotInHole);
          switch (operand_nih) {
          | InvalidText(_) => Some(mk(SynInvalidArrow(Arrow(Hole, Hole))))
          | Label(err, l) => Some(mk(SynLabel(err, l)))
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
          | None =>
            switch (seq |> Seq.nth_operand(n)) {
            | Label(_, _) => syn_cursor_info_skel(~steps, ctx, skel2, zseq)
            | _ => None
            }
          | Some((ty1, _)) =>
            ana_cursor_info_skel(~steps, ctx, skel2, zseq, ty1)
          }
        };
      }
    | BinOp(_, Space, skel1, skel2) =>
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
  | CursorE(_, Label(lerr, l)) =>
    Some(CursorInfo_common.mk(SynLabel(lerr, l), ctx, cursor_term))
  | CursorE(_, e) =>
    switch (Statics_Exp.syn_operand(ctx, e)) {
    | None => None
    | Some(ty) =>
      Some(CursorInfo_common.mk(Synthesized(ty), ctx, cursor_term))
    }
  | ParenthesizedZ(zbody) => syn_cursor_info(~steps=steps @ [0], ctx, zbody)
  | LamZP(_, zp, ann, body) =>
    let ty1 =
      switch (ann) {
      | None => HTyp.Hole
      | Some(uty1) => UHTyp.expand(uty1)
      };
    switch (CursorInfo_Pat.ana_cursor_info(~steps=steps @ [0], ctx, zp, ty1)) {
    | None => None
    | Some(CursorNotOnDeferredVarPat(ci)) => Some(ci)
    | Some(CursorOnDeferredVarPat(deferred_ci, x)) =>
      let uses = UsageAnalysis.find_uses(~steps=steps @ [2], x, body);
      Some(uses |> deferred_ci);
    };
  | LamZA(_, _, zann, _) =>
    CursorInfo_Typ.cursor_info(~steps=steps @ [1], ctx, zann)
  | LamZE(_, p, ann, zbody) =>
    let ty1 =
      switch (ann) {
      | Some(uty1) => UHTyp.expand(uty1)
      | None => Hole
      };
    switch (Statics_Pat.ana(ctx, p, ty1)) {
    | None => None
    | Some(ctx1) => syn_cursor_info(~steps=steps @ [2], ctx1, zbody)
    };
  | InjZ(_, _, zbody) => syn_cursor_info(~steps=steps @ [0], ctx, zbody)
  | CaseZE(_, zscrut, _) => syn_cursor_info(~steps=steps @ [0], ctx, zscrut)
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
  | ApPaletteZ(_, _, _, zpsi) =>
    let (ty, ze) = ZIntMap.prj_z_v(zpsi.zsplice_map);
    ana_cursor_info(~steps, ctx, ze, ty);
  | PrjZE(_, zoperand_, _) =>
    // ECD TODO: check if steps actually needs the appended 0 and check if needs to be analyzed to labeled type
    syn_cursor_info_zoperand(~steps=steps @ [0], ctx, zoperand_)
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
      | LetLineZA(_)
      | LetLineZE(_) => None
      | ExpLineZ(zopseq) =>
        ana_cursor_info_zopseq(
          ~steps=steps @ [List.length(prefix)],
          ctx,
          zopseq,
          ty,
        )
      | StructLineZP(_) => failwith("TODO (hejohns): 672")
      | StructLineZE(_) => failwith("TODO (hejohns): 673")
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
    | InHole(TypeInconsistent, _) =>
      let opseq' = UHExp.set_err_status_opseq(NotInHole, opseq);
      Statics_Exp.syn_opseq(ctx, opseq')
      |> Option.map(ty' =>
           CursorInfo_common.mk(
             AnaTypeInconsistent(ty, ty'),
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
      | InHole(TypeInconsistent, _) =>
        let opseq' = UHExp.set_err_status_opseq(NotInHole, opseq);
        Statics_Exp.syn_opseq(ctx, opseq')
        |> Option.map(ty' =>
             CursorInfo_common.mk(
               AnaTypeInconsistent(ty, ty'),
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
    | Label(_, _) =>
      Some(CursorInfo_common.mk(AnaLabel(ty), ctx, cursor_term))
    | Var(InHole(TypeInconsistent, _), _, _)
    | IntLit(InHole(TypeInconsistent, _), _)
    | FloatLit(InHole(TypeInconsistent, _), _)
    | BoolLit(InHole(TypeInconsistent, _), _)
    | ListNil(InHole(TypeInconsistent, _))
    | Lam(InHole(TypeInconsistent, _), _, _, _)
    | Inj(InHole(TypeInconsistent, _), _, _)
    | Case(StandardErrStatus(InHole(TypeInconsistent, _)), _, _)
    | ApPalette(InHole(TypeInconsistent, _), _, _, _)
    | Prj(InHole(TypeInconsistent, _), _, _) =>
      let operand' =
        zoperand
        |> ZExp.erase_zoperand
        |> UHExp.set_err_status_operand(NotInHole);
      switch (Statics_Exp.syn_operand(ctx, operand')) {
      | None => None
      | Some(ty') =>
        Some(
          CursorInfo_common.mk(
            AnaTypeInconsistent(ty, ty'),
            ctx,
            cursor_term,
          ),
        )
      };
    | Var(InHole(WrongLength, _), _, _)
    | IntLit(InHole(WrongLength, _), _)
    | FloatLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _))
    | Lam(InHole(WrongLength, _), _, _, _)
    | Inj(InHole(WrongLength, _), _, _)
    | Case(
        StandardErrStatus(InHole(WrongLength, _)) | InconsistentBranches(_),
        _,
        _,
      )
    | ApPalette(InHole(WrongLength, _), _, _, _)
    | Prj(InHole(WrongLength, _), _, _) => None
    /* not in hole */
    | EmptyHole(_)
    | Var(NotInHole, NotInVarHole, _)
    | IntLit(NotInHole, _)
    | FloatLit(NotInHole, _)
    | BoolLit(NotInHole, _)
    | ApPalette(NotInHole, _, _, _)
    | Prj(NotInHole, _, _) =>
      switch (Statics_Exp.syn_operand(ctx, e)) {
      | None => None
      | Some(ty') =>
        Some(CursorInfo_common.mk(AnaSubsumed(ty, ty'), ctx, cursor_term))
      }
    | ListNil(NotInHole)
    | Inj(NotInHole, _, _)
    | Case(StandardErrStatus(NotInHole), _, _) =>
      Some(CursorInfo_common.mk(Analyzed(ty), ctx, cursor_term))
    | Parenthesized(body) =>
      Statics_Exp.ana(ctx, body, ty)
      |> Option.map(_ =>
           CursorInfo_common.mk(Analyzed(ty), ctx, cursor_term)
         )
    | Lam(NotInHole, _, ann, _) =>
      switch (HTyp.matched_arrow(ty)) {
      | None => None
      | Some((ty1, ty2)) =>
        switch (ann) {
        | None => Some(CursorInfo_common.mk(Analyzed(ty), ctx, cursor_term))
        | Some(ann) =>
          let ann_ty = ann |> UHTyp.expand;
          HTyp.consistent(ann_ty, ty1)
            ? Some(
                CursorInfo_common.mk(
                  AnaAnnotatedLambda(ty, Arrow(ann_ty, ty2)),
                  ctx,
                  cursor_term,
                ),
              )
            : None;
        }
      }
    } /* zipper cases */
  | ParenthesizedZ(zbody) =>
    ana_cursor_info(~steps=steps @ [0], ctx, zbody, ty) /* zipper in hole */
  | LamZP(InHole(WrongLength, _), _, _, _)
  | LamZA(InHole(WrongLength, _), _, _, _)
  | LamZE(InHole(WrongLength, _), _, _, _)
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
  | ApPaletteZ(InHole(WrongLength, _), _, _, _)
  | PrjZE(InHole(WrongLength, _), _, _) => None
  | LamZP(InHole(TypeInconsistent, _), _, _, _)
  | LamZA(InHole(TypeInconsistent, _), _, _, _)
  | LamZE(InHole(TypeInconsistent, _), _, _, _)
  | InjZ(InHole(TypeInconsistent, _), _, _)
  | CaseZE(StandardErrStatus(InHole(TypeInconsistent, _)), _, _)
  | CaseZR(StandardErrStatus(InHole(TypeInconsistent, _)), _, _)
  | ApPaletteZ(InHole(TypeInconsistent, _), _, _, _)
  | PrjZE(InHole(TypeInconsistent, _), _, _) =>
    syn_cursor_info_zoperand(~steps, ctx, zoperand) /* zipper not in hole */
  | LamZP(NotInHole, zp, ann, body) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => None
    | Some((ty1_given, _)) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => ty1_given
        };
      switch (
        CursorInfo_Pat.ana_cursor_info(~steps=steps @ [0], ctx, zp, ty1)
      ) {
      | None => None
      | Some(CursorNotOnDeferredVarPat(ci)) => Some(ci)
      | Some(CursorOnDeferredVarPat(deferred_ci, x)) =>
        let uses = UsageAnalysis.find_uses(~steps=steps @ [2], x, body);
        Some(uses |> deferred_ci);
      };
    }
  | LamZA(NotInHole, _, zann, _) =>
    CursorInfo_Typ.cursor_info(~steps=steps @ [1], ctx, zann)
  | LamZE(NotInHole, p, ann, zbody) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => None
    | Some((ty1_given, ty2)) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => ty1_given
        };
      switch (Statics_Pat.ana(ctx, p, ty1)) {
      | None => None
      | Some(ctx) => ana_cursor_info(~steps=steps @ [2], ctx, zbody, ty2)
      };
    }
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
  | ApPaletteZ(NotInHole, _, _, _) =>
    syn_cursor_info_zoperand(~steps, ctx, zoperand)
  | PrjZE(NotInHole, zoperand_, _) =>
    syn_cursor_info_zoperand(~steps=steps @ [0], ctx, zoperand_)
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
