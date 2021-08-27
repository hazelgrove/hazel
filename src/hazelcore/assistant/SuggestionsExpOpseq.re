open SuggestionsExp;

let mk_replace_operator_suggestion =
    (
      seq_ty: HTyp.t,
      zopseq: ZExp.zopseq,
      ctx: Contexts.t,
      new_operator: Operators_Exp.t,
    )
    : suggestion => {
  /* TODO: only support binary operators */
  /* TODO: retain cursor position */
  let fix_holes_local = (ctx: Contexts.t, exp: UHExp.t): UHExp.t =>
    exp
    |> Statics_Exp.syn_fix_holes(ctx, MetaVarGen.init)
    |> (((x, _, _)) => x);
  let OpSeq(_, seq) = ZExp.erase_zopseq(zopseq);
  let new_seq =
    switch (seq) {
    | S(operand1, A(_operator, S(operand2, E))) =>
      Seq.S(operand1, A(new_operator, S(operand2, E)))
    | _ => failwith("mk_replace_operator_suggestion impossible case")
    };
  let new_opseq = new_seq |> UHExp.mk_OpSeq;
  let new_zopseq = ZExp.place_before_opseq(new_opseq);
  Suggestion.mk(
    ~category=ReplaceOperator,
    ~result_text=Operators_Exp.to_string(new_operator),
    ~action=ReplaceOpSeq(new_zopseq),
    ~res_ty=seq_ty,
    ~result=UHExp.Block.wrap'(new_opseq) |> fix_holes_local(ctx),
  );
};

let actual_ty_operand = (~ctx, operand) =>
  switch (
    Statics_Exp.syn_operand(
      ctx,
      UHExp.set_err_status_operand(NotInHole, operand),
    )
  ) {
  | None => HTyp.Hole
  | Some(ty) => ty
  };

let replace_operator_suggestions =
    (ctx: Contexts.t, zopseq: ZExp.zopseq, ty: option(HTyp.t)) => {
  /* TODO: only supports binary operators */
  let OpSeq(_, seq) = ZExp.erase_zopseq(zopseq);
  let ty =
    switch (ty) {
    | None => HTyp.Hole
    | Some(ty) => ty
    };
  switch (seq) {
  | S(operand1, A(_operator, S(operand2, E))) =>
    let in1_ty = actual_ty_operand(~ctx, operand1);
    let in2_ty = actual_ty_operand(~ctx, operand2);
    UHExp.operators_of_ty(in1_ty, in2_ty, ty)
    |> List.map(mk_replace_operator_suggestion(ty, zopseq, ctx));
  | _ => []
  };
};

let operator_suggestions =
    ({enclosing_zopseq, ctx, _}: CursorInfo.t): list(suggestion) =>
  switch (enclosing_zopseq) {
  | ExpSeq(zopseq, ty) => replace_operator_suggestions(ctx, zopseq, ty)
  | _ => []
  };

let mk = operator_suggestions;
