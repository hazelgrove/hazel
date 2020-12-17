let operator_of_shape =
    (os: Action_common.operator_shape): option(UHTyp.operator) =>
  switch (os) {
  | SArrow => Some(Arrow)
  | SComma => Some(Prod)
  | SVBar => Some(Sum)
  | SMinus
  | SPlus
  | STimes
  | SDivide
  | SAnd
  | SOr
  | SLessThan
  | SGreaterThan
  | SEquals
  | SSpace
  | SCons => None
  };

let shape_of_operator = (op: UHTyp.operator): Action_common.operator_shape =>
  switch (op) {
  | Arrow => SArrow
  | Prod => SComma
  | Sum => SVBar
  };

type syn_success = (ZTyp.t, Kind.t, MetaVarGen.t);
type ana_success = (ZTyp.t, MetaVarGen.t);

let mk_syn_result =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, zty: ZTyp.t)
    : ActionOutcome.t(syn_success) => {
  let hty = UHTyp.expand(Contexts.tyvars(ctx), zty |> ZTyp.erase);
  switch (Statics_Typ.syn(ctx, hty)) {
  | None => Failed
  | Some(kind) => Succeeded((zty, kind, u_gen))
  };
};
let mk_ana_result =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, zty: ZTyp.t, k: Kind.t)
    : ActionOutcome.t(ana_success) => {
  let hty = UHTyp.expand(Contexts.tyvars(ctx), zty |> ZTyp.erase);
  if (Statics_Typ.ana(ctx, hty, k)) {
    Succeeded((zty, u_gen));
  } else {
    Failed;
  };
};

let construct_operator =
    (
      operator: UHTyp.operator,
      zoperand: ZTyp.zoperand,
      (prefix, suffix): ZTyp.operand_surround,
    )
    : ZTyp.zopseq => {
  let operand = zoperand |> ZTyp.erase_zoperand;
  let (zoperand, surround) =
    if (ZTyp.is_before_zoperand(zoperand)) {
      let zoperand = UHTyp.Hole |> ZTyp.place_before_operand;
      let new_suffix = Seq.A(operator, S(operand, suffix));
      (zoperand, (prefix, new_suffix));
    } else {
      let zoperand = UHTyp.Hole |> ZTyp.place_before_operand;
      let new_prefix = Seq.A(operator, S(operand, prefix));
      (zoperand, (new_prefix, suffix));
    };
  ZTyp.mk_ZOpSeq(ZOperand(zoperand, surround));
};

let rec syn_move =
        (a: Action_common.t, (zty: ZTyp.t, k: Kind.t, u_gen: MetaVarGen.t))
        : ActionOutcome.t(syn_success) =>
  switch (a) {
  | MoveTo(path) =>
    switch (CursorPath_Typ.follow(path, zty |> ZTyp.erase)) {
    | None => Failed
    | Some(zty) => Succeeded((zty, k, u_gen))
    }
  | MoveToPrevHole =>
    switch (
      CursorPath_common.(prev_hole_steps(CursorPath_Typ.holes_z(zty, [])))
    ) {
    | None => Failed
    | Some(steps) =>
      switch (CursorPath_Typ.of_steps(steps, zty |> ZTyp.erase)) {
      | None => Failed
      | Some(path) => syn_move(MoveTo(path), (zty, k, u_gen))
      }
    }
  | MoveToNextHole =>
    switch (
      CursorPath_common.(next_hole_steps(CursorPath_Typ.holes_z(zty, [])))
    ) {
    | None => Failed
    | Some(steps) =>
      switch (CursorPath_Typ.of_steps(steps, zty |> ZTyp.erase)) {
      | None => Failed
      | Some(path) => syn_move(MoveTo(path), (zty, k, u_gen))
      }
    }
  | MoveLeft =>
    zty
    |> ZTyp.move_cursor_left
    |> OptUtil.map_default(~default=ActionOutcome.CursorEscaped(Before), z =>
         Succeeded((z, k, u_gen))
       )
  | MoveRight =>
    zty
    |> ZTyp.move_cursor_right
    |> OptUtil.map_default(~default=ActionOutcome.CursorEscaped(After), z =>
         Succeeded((z, k, u_gen))
       )
  | Construct(_)
  | Delete
  | Backspace
  | UpdateApPalette(_)
  | SwapLeft
  | SwapRight
  | SwapUp
  | SwapDown
  | Init =>
    failwith(
      __LOC__
      ++ ": expected movement action, got "
      ++ Sexplib.Sexp.to_string(Action_common.sexp_of_t(a)),
    )
  };

let rec ana_move =
        (a: Action_common.t, (zty: ZTyp.t, u_gen: MetaVarGen.t), k: Kind.t)
        : ActionOutcome.t(ana_success) =>
  switch (a) {
  | MoveTo(path) =>
    switch (CursorPath_Typ.follow(path, zty |> ZTyp.erase)) {
    | None => Failed
    | Some(zty) => Succeeded((zty, u_gen))
    }
  | MoveToPrevHole =>
    switch (
      CursorPath_common.(prev_hole_steps(CursorPath_Typ.holes_z(zty, [])))
    ) {
    | None => Failed
    | Some(steps) =>
      switch (CursorPath_Typ.of_steps(steps, zty |> ZTyp.erase)) {
      | None => Failed
      | Some(path) => ana_move(MoveTo(path), (zty, u_gen), k)
      }
    }
  | MoveToNextHole =>
    switch (
      CursorPath_common.(next_hole_steps(CursorPath_Typ.holes_z(zty, [])))
    ) {
    | None => Failed
    | Some(steps) =>
      switch (CursorPath_Typ.of_steps(steps, zty |> ZTyp.erase)) {
      | None => Failed
      | Some(path) => ana_move(MoveTo(path), (zty, u_gen), k)
      }
    }
  | MoveLeft =>
    zty
    |> ZTyp.move_cursor_left
    |> OptUtil.map_default(~default=ActionOutcome.CursorEscaped(Before), z =>
         Succeeded((z, u_gen))
       )
  | MoveRight =>
    zty
    |> ZTyp.move_cursor_right
    |> OptUtil.map_default(~default=ActionOutcome.CursorEscaped(After), z =>
         Succeeded((z, u_gen))
       )
  | Construct(_)
  | Delete
  | Backspace
  | UpdateApPalette(_)
  | SwapLeft
  | SwapRight
  | SwapUp
  | SwapDown
  | Init =>
    failwith(
      __LOC__
      ++ ": expected movement action, got "
      ++ Sexplib.Sexp.to_string(Action_common.sexp_of_t(a)),
    )
  };

let mk_syn_text =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, caret_index: int, text: string)
    : ActionOutcome.t(syn_success) => {
  let text_cursor = CursorPosition.OnText(caret_index);
  switch (TyTextShape.of_text(text)) {
  | None =>
    if (text |> StringUtil.is_empty) {
      Succeeded((
        ZOpSeq.wrap(ZTyp.CursorT(OnDelim(0, Before), Hole)),
        Kind.KHole,
        u_gen,
      ));
    } else {
      Failed;
    }
  | Some(Bool) =>
    Succeeded((
      ZOpSeq.wrap(ZTyp.CursorT(text_cursor, Bool)),
      Kind.Type,
      u_gen,
    ))
  | Some(Int) =>
    Succeeded((
      ZOpSeq.wrap(ZTyp.CursorT(text_cursor, Int)),
      Kind.Type,
      u_gen,
    ))
  | Some(Float) =>
    Succeeded((
      ZOpSeq.wrap(ZTyp.CursorT(text_cursor, Float)),
      Kind.Type,
      u_gen,
    ))
  | Some(ExpandingKeyword(k)) =>
    let (u, u_gen) = u_gen |> MetaVarGen.next;
    Succeeded((
      ZOpSeq.wrap(
        ZTyp.CursorT(
          text_cursor,
          TyVar(InVarHole(Keyword(k), u), k |> ExpandingKeyword.to_string),
        ),
      ),
      Kind.KHole,
      u_gen,
    ));
  | Some(TyVar(x)) =>
    if (TyVarCtx.contains(Contexts.tyvars(ctx), x)) {
      let _ = TyVarCtx.print(ctx.tyvars);
      let idx = TyVarCtx.index_of_exn(Contexts.tyvars(ctx), x);
      let (_, k) = TyVarCtx.tyvar_with_idx(Contexts.tyvars(ctx), idx);
      Succeeded((
        ZOpSeq.wrap(ZTyp.CursorT(text_cursor, TyVar(NotInVarHole, x))),
        k,
        u_gen,
      ));
    } else {
      let (u, u_gen) = u_gen |> MetaVarGen.next;
      Succeeded((
        ZOpSeq.wrap(
          ZTyp.CursorT(text_cursor, TyVar(InVarHole(Free, u), x)),
        ),
        Kind.KHole,
        u_gen,
      ));
    }
  };
};

let mk_ana_text =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      caret_index: int,
      text: string,
      k: Kind.t,
    )
    : ActionOutcome.t(ana_success) => {
  let text_cursor = CursorPosition.OnText(caret_index);
  switch (TyTextShape.of_text(text)) {
  | None =>
    if (text |> StringUtil.is_empty) {
      Succeeded((
        ZOpSeq.wrap(ZTyp.CursorT(OnDelim(0, Before), Hole)),
        u_gen,
      ));
    } else {
      Failed;
    }
  | Some(ExpandingKeyword(k)) =>
    let (u, u_gen) = u_gen |> MetaVarGen.next;
    Succeeded((
      ZOpSeq.wrap(
        ZTyp.CursorT(
          text_cursor,
          TyVar(InVarHole(Keyword(k), u), k |> ExpandingKeyword.to_string),
        ),
      ),
      u_gen,
    ));
  | Some(Int | Float | Bool | TyVar(_)) =>
    switch (mk_syn_text(ctx, u_gen, caret_index, text)) {
    | (Failed | CursorEscaped(_)) as err => err
    | Succeeded((zty, k', u_gen)) =>
      if (KindUtil.consistent(ctx, k, k')) {
        Succeeded((zty, u_gen));
      } else {
        Failed;
      }
    }
  };
};

let syn_insert_text = Action_common.syn_insert_text_(~mk_syn_text);
let ana_insert_text = Action_common.ana_insert_text_(~mk_ana_text);
let syn_backspace_text = Action_common.syn_backspace_text_(~mk_syn_text);
let ana_backspace_text = Action_common.ana_backspace_text_(~mk_ana_text);
let syn_delete_text = Action_common.syn_delete_text_(~mk_syn_text);
let ana_delete_text = Action_common.ana_delete_text_(~mk_ana_text);

let text_operand =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, shape: TyTextShape.t)
    : (UHTyp.operand, MetaVarGen.t) =>
  switch (shape) {
  | Int => (Int, u_gen)
  | Bool => (Bool, u_gen)
  | Float => (Float, u_gen)
  | ExpandingKeyword(k) =>
    let (u, u_gen) = u_gen |> MetaVarGen.next;
    (
      TyVar(InVarHole(Keyword(k), u), k |> ExpandingKeyword.to_string),
      u_gen,
    );
  | TyVar(id) =>
    if (TyVarCtx.contains(Contexts.tyvars(ctx), id)) {
      (TyVar(NotInVarHole, id), u_gen);
    } else {
      let (u, u_gen) = u_gen |> MetaVarGen.next;
      (TyVar(InVarHole(Free, u), id), u_gen);
    }
  };

let syn_split_text =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      caret_index: int,
      sop: Action_common.operator_shape,
      text: string,
    )
    : ActionOutcome.t(syn_success) => {
  let (l, r) = text |> StringUtil.split_string(caret_index);
  switch (
    TyTextShape.of_text(l),
    operator_of_shape(sop),
    TyTextShape.of_text(r),
  ) {
  | (None, _, _)
  | (_, None, _)
  | (_, _, None) => Failed
  | (Some(lshape), Some(op), Some(rshape)) =>
    let (loperand, u_gen) = text_operand(ctx, u_gen, lshape);
    let (roperand, u_gen) = text_operand(ctx, u_gen, rshape);
    let new_zty = {
      let zoperand = roperand |> ZTyp.place_before_operand;
      ZTyp.mk_ZOpSeq(ZOperand(zoperand, (A(op, S(loperand, E)), E)));
    };
    switch (
      Statics_Typ.syn(
        ctx,
        UHTyp.expand(Contexts.tyvars(ctx), ZTyp.erase(new_zty)),
      )
    ) {
    | None => Failed
    | Some(kind) => Succeeded((new_zty, kind, u_gen))
    };
  };
};

let ana_split_text =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      caret_index: int,
      sop: Action_common.operator_shape,
      text: string,
    )
    : ActionOutcome.t(ana_success) => {
  let (l, r) = text |> StringUtil.split_string(caret_index);
  switch (
    TyTextShape.of_text(l),
    operator_of_shape(sop),
    TyTextShape.of_text(r),
  ) {
  | (None, _, _)
  | (_, None, _)
  | (_, _, None) => Failed
  | (Some(lshape), Some(op), Some(rshape)) =>
    let (loperand, u_gen) = text_operand(ctx, u_gen, lshape);
    let (roperand, u_gen) = text_operand(ctx, u_gen, rshape);
    let new_zty = {
      let zoperand = roperand |> ZTyp.place_before_operand;
      ZTyp.mk_ZOpSeq(ZOperand(zoperand, (A(op, S(loperand, E)), E)));
    };
    Succeeded((new_zty, u_gen));
  };
};

let rec syn_perform =
        (
          ctx: Contexts.t,
          a: Action_common.t,
          (zty: ZTyp.t, k: Kind.t, u_gen: MetaVarGen.t),
        )
        : ActionOutcome.t(syn_success) =>
  syn_perform_opseq(ctx, a, (zty, k, u_gen))
and syn_perform_opseq =
    (
      ctx: Contexts.t,
      a: Action_common.t,
      (
        ZOpSeq(skel, zseq) as zopseq: ZTyp.zopseq,
        k: Kind.t,
        u_gen: MetaVarGen.t,
      ),
    )
    : ActionOutcome.t(syn_success) =>
  switch (a, zseq) {
  /* Invalid actions at type level */
  | (
      UpdateApPalette(_) |
      Construct(
        SAsc | SLet | SLine | SLam | SListNil | SInj(_) | SCase | SApPalette(_),
      ) |
      SwapUp |
      SwapDown,
      _,
    ) =>
    Failed
  /* Invalid cursor positions */
  | (_, ZOperator((OnText(_) | OnDelim(_), _), _)) => Failed

  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    syn_move(a, (zopseq, k, u_gen))
  | (Delete, ZOperator((OnOp(After as side), _), _))
  | (Backspace, ZOperator((OnOp(Before as side), _), _)) =>
    syn_perform_opseq(ctx, Action_common.escape(side), (zopseq, k, u_gen))
  /* Delete before operator == Backspace after operator */
  | (Delete, ZOperator((OnOp(Before), op), surround)) =>
    syn_perform_opseq(
      ctx,
      Backspace,
      (ZOpSeq(skel, ZOperator((OnOp(After), op), surround)), k, u_gen),
    )
  /* ... + [k-2] + [k-1] +<| [k] + ...   ==>   ... + [k-2] + [k-1]| + ...
   * (for now until we have proper type constructors) */
  | (Backspace, ZOperator((OnOp(After), _), (prefix, suffix))) =>
    let S(prefix_hd, new_prefix) = prefix;
    let zoperand = prefix_hd |> ZTyp.place_after_operand;
    let S(_, new_suffix) = suffix;
    mk_syn_result(
      ctx,
      u_gen,
      ZTyp.mk_ZOpSeq(ZOperand(zoperand, (new_prefix, new_suffix))),
    );

  /* Construction */
  /* constructio on operators becomes movement... */
  | (Construct(SOp(SSpace)), ZOperator((OnOp(After), _), _)) =>
    syn_perform_opseq(ctx, MoveRight, (zopseq, k, u_gen))
  /* ... or construction after movement */
  | (Construct(_) as a, ZOperator((OnOp(side), _), _)) =>
    switch (
      syn_perform_opseq(ctx, Action_common.escape(side), (zopseq, k, u_gen))
    ) {
    | Failed
    | CursorEscaped(_) => Failed
    | Succeeded((zty, k, u_gen)) =>
      syn_perform_opseq(ctx, a, (zty, k, u_gen))
    }
  /* Space becomes movement until we have proper type constructors */
  | (Construct(SOp(SSpace)), ZOperand(zoperand, _))
      when ZTyp.is_after_zoperand(zoperand) =>
    syn_perform_opseq(ctx, MoveRight, (zopseq, k, u_gen))

  /* SwapLeft and SwapRight actions */
  | (SwapLeft, ZOperator(_))
  | (SwapRight, ZOperator(_)) => Failed
  | (SwapLeft, ZOperand(CursorT(_), (E, _))) => Failed
  | (
      SwapLeft,
      ZOperand(
        CursorT(_) as zoperand,
        (A(operator, S(operand, new_prefix)), suffix),
      ),
    ) =>
    let new_suffix = Seq.A(operator, S(operand, suffix));
    let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, new_suffix));
    mk_syn_result(ctx, u_gen, ZTyp.mk_ZOpSeq(new_zseq));
  | (SwapRight, ZOperand(CursorT(_), (_, E))) => Failed
  | (
      SwapRight,
      ZOperand(
        CursorT(_) as zoperand,
        (prefix, A(operator, S(operand, new_suffix))),
      ),
    ) =>
    let new_prefix = Seq.A(operator, S(operand, prefix));
    let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, new_suffix));
    mk_syn_result(ctx, u_gen, ZTyp.mk_ZOpSeq(new_zseq));
  | (Init, _) => failwith("Init action should not be performed.")
  /* Zipper */
  | (_, ZOperand(zoperand, (E, E))) =>
    let _ = "Typ perform_operand";
    let _ = TyVarCtx.print(ctx.tyvars);
    syn_perform_operand(ctx, a, (zoperand, k, u_gen));
  | (_, ZOperand(zoperand, (prefix, suffix))) =>
    let uhty = ZTyp.erase(ZOpSeq.wrap(zoperand));
    let hty = UHTyp.expand(Contexts.tyvars(ctx), uhty);
    switch (Statics_Typ.syn(ctx, hty)) {
    | None => Failed
    | Some(kind) =>
      switch (syn_perform_operand(ctx, a, (zoperand, kind, u_gen))) {
      | Failed => Failed
      | CursorEscaped(side) =>
        syn_perform_opseq(
          ctx,
          Action_common.escape(side),
          (zopseq, k, u_gen),
        )
      | Succeeded((ZOpSeq(_, zseq), _, u_gen)) =>
        let _ = "inside succeeded";
        switch (zseq) {
        | ZOperand(zoperand, (inner_prefix, inner_suffix)) =>
          let new_prefix = Seq.affix_affix(inner_prefix, prefix);
          let new_suffix = Seq.affix_affix(inner_suffix, suffix);
          mk_syn_result(
            ctx,
            u_gen,
            ZTyp.mk_ZOpSeq(ZOperand(zoperand, (new_prefix, new_suffix))),
          );
        | ZOperator(zoperator, (inner_prefix, inner_suffix)) =>
          let new_prefix = Seq.seq_affix(inner_prefix, prefix);
          let new_suffix = Seq.seq_affix(inner_suffix, suffix);
          mk_syn_result(
            ctx,
            u_gen,
            ZTyp.mk_ZOpSeq(ZOperator(zoperator, (new_prefix, new_suffix))),
          );
        };
      }
    };
  }
and syn_perform_operand =
    (
      ctx: Contexts.t,
      a: Action_common.t,
      (zoperand: ZTyp.zoperand, k: Kind.t, u_gen: MetaVarGen.t),
    )
    : ActionOutcome.t(syn_success) => {
  switch (a, zoperand) {
  /* Invalid actions at type level */
  | (
      UpdateApPalette(_) |
      Construct(
        SAsc | SLet | SLine | SLam | SListNil | SInj(_) | SCase | SApPalette(_) |
        SDefine,
      ) |
      SwapUp |
      SwapDown,
      _,
    ) =>
    Failed
  /* Invalid cursor positions */
  | (
      _,
      CursorT(OnText(_) | OnOp(_), Hole | Unit | Parenthesized(_) | List(_)),
    ) =>
    Failed
  | (_, CursorT(OnDelim(_) | OnOp(_), TyVar(_) | Int | Float | Bool)) =>
    Failed
  | (_, CursorT(OnDelim(_) as cursor, operand))
      when !ZTyp.is_valid_cursor_operand(cursor, operand) =>
    Failed
  /* Movement handled at top level */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    syn_move(a, (ZOpSeq.wrap(zoperand), k, u_gen))

  /* Backspace and Delete */
  /* ( _ <|)   ==>   ( _| ) */
  | (Backspace, CursorT(OnDelim(_, Before), _)) =>
    zoperand |> ZTyp.is_before_zoperand
      ? CursorEscaped(Before)
      : syn_perform_operand(ctx, MoveLeft, (zoperand, k, u_gen))

  /* (|> _ )   ==>   ( |_ ) */
  | (Delete, CursorT(OnDelim(_, After), _)) =>
    zoperand |> ZTyp.is_after_zoperand
      ? CursorEscaped(After)
      : syn_perform_operand(ctx, MoveRight, (zoperand, k, u_gen))

  /* Delete before delimiter == Backspace after delimiter */
  | (Delete, CursorT(OnDelim(x, Before), operand)) =>
    syn_perform_operand(
      ctx,
      Backspace,
      (CursorT(OnDelim(x, After), operand), k, u_gen),
    )

  | (Backspace, CursorT(OnDelim(_, After), Hole | Unit)) =>
    mk_syn_result(ctx, u_gen, ZOpSeq.wrap(ZTyp.place_before_operand(Hole)))
  | (
      Backspace,
      CursorT(OnText(caret_index), (Int | Bool | Float) as operand),
    ) =>
    syn_backspace_text(ctx, u_gen, caret_index, operand |> UHTyp.to_string)
  | (Delete, CursorT(OnText(caret_index), (Int | Bool | Float) as operand)) =>
    syn_delete_text(ctx, u_gen, caret_index, operand |> UHTyp.to_string)
  /* TyVar-related Backspace & Delete */
  | (Delete, CursorT(OnText(caret_index), TyVar(_, text))) =>
    syn_delete_text(ctx, u_gen, caret_index, text)
  | (Backspace, CursorT(OnText(caret_index), TyVar(_, text))) =>
    syn_backspace_text(ctx, u_gen, caret_index, text)

  /* (<| _ )  ==>  |_ */
  /* ( _ )<|  ==>  _| */
  | (
      Backspace,
      CursorT(OnDelim(x, After), Parenthesized(body) | List(body)),
    ) =>
    let place_cursor = x == 0 ? ZTyp.place_before : ZTyp.place_after;
    mk_syn_result(ctx, u_gen, place_cursor(body));

  /* Construction */
  | (Construct(SOp(SSpace)), CursorT(OnDelim(_, After), _)) =>
    syn_perform_operand(ctx, MoveRight, (zoperand, k, u_gen))
  | (Construct(_) as a, CursorT(OnDelim(_, side), _))
      when
        !ZTyp.is_before_zoperand(zoperand)
        && !ZTyp.is_after_zoperand(zoperand) =>
    switch (
      syn_perform_operand(
        ctx,
        Action_common.escape(side),
        (zoperand, k, u_gen),
      )
    ) {
    | (Failed | CursorEscaped(_)) as err => err
    | Succeeded((zty, k, u_gen)) => syn_perform(ctx, a, (zty, k, u_gen))
    }
  | (Construct(SChar(s)), CursorT(_, Hole)) =>
    syn_insert_text(ctx, u_gen, (0, s), "")
  | (
      Construct(SChar(s)),
      CursorT(OnText(j), (Int | Bool | Float) as operand),
    ) =>
    syn_insert_text(ctx, u_gen, (j, s), operand |> UHTyp.to_string)
  | (Construct(SChar(s)), CursorT(OnText(j), TyVar(_, x))) =>
    syn_insert_text(ctx, u_gen, (j, s), x)
  | (Construct(SChar(_)), CursorT(_)) => Failed

  | (Construct(SList), CursorT(_)) =>
    mk_syn_result(
      ctx,
      u_gen,
      ZOpSeq.wrap(ZTyp.ListZ(ZOpSeq.wrap(zoperand))),
    )
  | (Construct(SParenthesized), CursorT(_)) =>
    mk_syn_result(
      ctx,
      u_gen,
      ZOpSeq.wrap(ZTyp.ParenthesizedZ(ZOpSeq.wrap(zoperand))),
    )
  /* split */
  | (
      Construct(SOp(os)),
      CursorT(OnText(j), (Int | Bool | Float) as operand),
    )
      when
        !ZTyp.is_before_zoperand(zoperand)
        && !ZTyp.is_after_zoperand(zoperand) =>
    syn_split_text(ctx, u_gen, j, os, operand |> UHTyp.to_string)
  | (Construct(SOp(os)), CursorT(OnText(j), TyVar(_, id)))
      when
        !ZTyp.is_before_zoperand(zoperand)
        && !ZTyp.is_after_zoperand(zoperand) =>
    syn_split_text(ctx, u_gen, j, os, id)
  | (Construct(SOp(os)), CursorT(_)) =>
    switch (operator_of_shape(os)) {
    | None => Failed
    | Some(op) =>
      mk_syn_result(ctx, u_gen, construct_operator(op, zoperand, (E, E)))
    }

  /* Invalid SwapLeft and SwapRight acitons */
  | (SwapLeft | SwapRight, CursorT(_)) => Failed
  | (Init, _) => failwith("Init action should not be performed.")
  /* Zipper Cases */
  | (_, ParenthesizedZ(zbody)) =>
    switch (syn_perform(ctx, a, (zbody, k, u_gen))) {
    | Failed => Failed
    | CursorEscaped(side) =>
      syn_perform_operand(
        ctx,
        Action_common.escape(side),
        (zoperand, k, u_gen),
      )
    | Succeeded((zbody, k, u_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.ParenthesizedZ(zbody)), k, u_gen))
    }
  | (_, ListZ(zbody)) =>
    switch (syn_perform(ctx, a, (zbody, k, u_gen))) {
    | Failed => Failed
    | CursorEscaped(side) =>
      syn_perform_operand(
        ctx,
        Action_common.escape(side),
        (zoperand, k, u_gen),
      )
    | Succeeded((zbody, k, u_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.ListZ(zbody)), k, u_gen))
    }
  };
}
and ana_perform =
    (
      ctx: Contexts.t,
      a: Action_common.t,
      (zty, u_gen): (ZTyp.t, MetaVarGen.t),
      k: Kind.t,
    )
    : ActionOutcome.t(ana_success) =>
  ana_perform_opseq(ctx, a, (zty, u_gen), k)
and ana_perform_opseq =
    (
      ctx: Contexts.t,
      a: Action_common.t,
      (ZOpSeq(skel, zseq) as zopseq: ZTyp.zopseq, u_gen: MetaVarGen.t),
      k: Kind.t,
    )
    : ActionOutcome.t(ana_success) =>
  switch (a, zseq) {
  /* Invalid actions at type level */
  | (
      UpdateApPalette(_) |
      Construct(
        SAsc | SLet | SLine | SLam | SListNil | SInj(_) | SCase | SApPalette(_),
      ) |
      SwapUp |
      SwapDown,
      _,
    ) =>
    Failed
  /*Invalid cursor positions */
  | (_, ZOperator((OnText(_) | OnDelim(_), _), _)) => Failed

  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    ana_move(a, (zopseq, u_gen), k)
  | (Delete, ZOperator((OnOp(After as side), _), _))
  | (Backspace, ZOperator((OnOp(Before as side), _), _)) =>
    ana_perform_opseq(ctx, Action_common.escape(side), (zopseq, u_gen), k)

  /* Delete before operator == Backspace after operator */
  | (Delete, ZOperator((OnOp(Before), op), surround)) =>
    ana_perform_opseq(
      ctx,
      Backspace,
      (ZOpSeq(skel, ZOperator((OnOp(After), op), surround)), u_gen),
      k,
    )
  /* ... + [k-2] + [k-1] +<| [k] + ...   ==>   ... + [k-2] + [k-1]| + ...
   * (for now until we have proper type constructors) */
  | (Backspace, ZOperator((OnOp(After), _), (prefix, suffix))) =>
    let S(prefix_hd, new_prefix) = prefix;
    let zoperand = prefix_hd |> ZTyp.place_after_operand;
    let S(_, new_suffix) = suffix;
    mk_ana_result(
      ctx,
      u_gen,
      ZTyp.mk_ZOpSeq(ZOperand(zoperand, (new_prefix, new_suffix))),
      k,
    );

  /* Construction */
  /* constructio on operators becomes movement... */
  | (Construct(SOp(SSpace)), ZOperator((OnOp(After), _), _)) =>
    ana_perform_opseq(ctx, MoveRight, (zopseq, u_gen), k)
  /* ... or construction after movement */
  | (Construct(_) as a, ZOperator((OnOp(side), _), _)) =>
    switch (
      ana_perform_opseq(ctx, Action_common.escape(side), (zopseq, u_gen), k)
    ) {
    | Failed
    | CursorEscaped(_) => Failed
    | Succeeded((zty, u_gen)) => ana_perform_opseq(ctx, a, (zty, u_gen), k)
    }
  /* Space becomes movement until we have proper type constructors */
  | (Construct(SOp(SSpace)), ZOperand(zoperand, _))
      when ZTyp.is_after_zoperand(zoperand) =>
    ana_perform_opseq(ctx, MoveRight, (zopseq, u_gen), k)
  /*| (Construct(SOp(os)), ZOperand(CursorT(_) as zoperand, surround)) =>
    switch (operator_of_shape(os)) {
    | None => Failed
    | Some(op) =>
      mk_ana_result(
        ctx,
        u_gen,
        construct_operator(op, zoperand, surround),
        k,
      )
    }*/
  /* SwapLeft and SwapRight actions */
  | (SwapLeft, ZOperator(_))
  | (SwapRight, ZOperator(_)) => Failed
  | (SwapLeft, ZOperand(CursorT(_), (E, _))) => Failed
  | (
      SwapLeft,
      ZOperand(
        CursorT(_) as zoperand,
        (A(operator, S(operand, new_prefix)), suffix),
      ),
    ) =>
    let new_suffix = Seq.A(operator, S(operand, suffix));
    let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, new_suffix));
    mk_ana_result(ctx, u_gen, ZTyp.mk_ZOpSeq(new_zseq), k);
  | (SwapRight, ZOperand(CursorT(_), (_, E))) => Failed
  | (
      SwapRight,
      ZOperand(
        CursorT(_) as zoperand,
        (prefix, A(operator, S(operand, new_suffix))),
      ),
    ) =>
    let new_prefix = Seq.A(operator, S(operand, prefix));
    let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, new_suffix));
    mk_ana_result(ctx, u_gen, ZTyp.mk_ZOpSeq(new_zseq), k);
  | (Init, _) => failwith("Init action should not be performed.")
  /* Zipper */
  | (_, ZOperand(zoperand, (E, E))) =>
    ana_perform_operand(ctx, a, (zoperand, u_gen), k)
  | (_, ZOperand(zoperand, (prefix, suffix))) =>
    let uhty = ZTyp.erase(ZOpSeq.wrap(zoperand));
    let hty = UHTyp.expand(Contexts.tyvars(ctx), uhty);
    switch (Statics_Typ.syn(ctx, hty)) {
    | None => Failed
    | Some(kind) =>
      switch (syn_perform_operand(ctx, a, (zoperand, kind, u_gen))) {
      | Failed => Failed
      | CursorEscaped(side) =>
        ana_perform_opseq(
          ctx,
          Action_common.escape(side),
          (zopseq, u_gen),
          k,
        )
      | Succeeded((ZOpSeq(_, zseq), _, u_gen)) =>
        switch (zseq) {
        | ZOperand(zoperand, (inner_prefix, inner_suffix)) =>
          let new_prefix = Seq.affix_affix(inner_prefix, prefix);
          let new_suffix = Seq.affix_affix(inner_suffix, suffix);
          mk_ana_result(
            ctx,
            u_gen,
            ZTyp.mk_ZOpSeq(ZOperand(zoperand, (new_prefix, new_suffix))),
            k,
          );
        | ZOperator(zoperator, (inner_prefix, inner_suffix)) =>
          let new_prefix = Seq.seq_affix(inner_prefix, prefix);
          let new_suffix = Seq.seq_affix(inner_suffix, suffix);
          mk_ana_result(
            ctx,
            u_gen,
            ZTyp.mk_ZOpSeq(ZOperator(zoperator, (new_prefix, new_suffix))),
            k,
          );
        }
      }
    };
  }
and ana_perform_operand =
    (
      ctx: Contexts.t,
      a: Action_common.t,
      (zoperand, u_gen): (ZTyp.zoperand, MetaVarGen.t),
      k: Kind.t,
    )
    : ActionOutcome.t(ana_success) =>
  switch (a, zoperand) {
  /* Invalid actions at type level */
  | (
      UpdateApPalette(_) |
      Construct(
        SAsc | SLet | SLine | SLam | SListNil | SInj(_) | SCase | SApPalette(_) |
        SDefine,
      ) |
      SwapUp |
      SwapDown,
      _,
    ) =>
    Failed
  /* Invalid cursor positions */
  | (
      _,
      CursorT(OnText(_) | OnOp(_), Hole | Unit | Parenthesized(_) | List(_)),
    ) =>
    Failed
  | (_, CursorT(OnDelim(_) | OnOp(_), TyVar(_) | Int | Float | Bool)) =>
    Failed
  | (_, CursorT(OnDelim(_) as cursor, operand))
      when !ZTyp.is_valid_cursor_operand(cursor, operand) =>
    Failed
  /* Movement handled at top level */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    ana_move(a, (ZOpSeq.wrap(zoperand), u_gen), k)

  /* Backspace and Delete */
  /* ( _ <|)   ==>   ( _| ) */
  | (Backspace, CursorT(OnDelim(_, Before), _)) =>
    zoperand |> ZTyp.is_before_zoperand
      ? CursorEscaped(Before)
      : ana_perform_operand(ctx, MoveLeft, (zoperand, u_gen), k)

  /* (|> _ )   ==>   ( |_ ) */
  | (Delete, CursorT(OnDelim(_, After), _)) =>
    zoperand |> ZTyp.is_after_zoperand
      ? CursorEscaped(After)
      : ana_perform_operand(ctx, MoveRight, (zoperand, u_gen), k)

  /* Delete before delimiter == Backspace after delimiter */
  | (Delete, CursorT(OnDelim(x, Before), operand)) =>
    ana_perform_operand(
      ctx,
      Backspace,
      (CursorT(OnDelim(x, After), operand), u_gen),
      k,
    )

  | (Backspace, CursorT(OnDelim(_, After), Hole | Unit)) =>
    mk_ana_result(
      ctx,
      u_gen,
      ZOpSeq.wrap(ZTyp.place_before_operand(Hole)),
      k,
    )
  | (
      Backspace,
      CursorT(OnText(caret_index), (Int | Bool | Float) as operand),
    ) =>
    ana_backspace_text(ctx, u_gen, caret_index, operand |> UHTyp.to_string, k)
  | (Delete, CursorT(OnText(caret_index), (Int | Bool | Float) as operand)) =>
    ana_delete_text(ctx, u_gen, caret_index, operand |> UHTyp.to_string, k)
  /* TyVar-related Backspace & Delete */
  | (Delete, CursorT(OnText(caret_index), TyVar(_, text))) =>
    ana_delete_text(ctx, u_gen, caret_index, text, k)
  | (Backspace, CursorT(OnText(caret_index), TyVar(_, text))) =>
    ana_backspace_text(ctx, u_gen, caret_index, text, k)

  /* (<| _ )  ==>  |_ */
  /* ( _ )<|  ==>  _| */
  | (
      Backspace,
      CursorT(OnDelim(x, After), Parenthesized(body) | List(body)),
    ) =>
    let place_cursor = x == 0 ? ZTyp.place_before : ZTyp.place_after;
    mk_ana_result(ctx, u_gen, place_cursor(body), k);

  /* Construction */
  | (Construct(SOp(SSpace)), CursorT(OnDelim(_, After), _)) =>
    ana_perform_operand(ctx, MoveRight, (zoperand, u_gen), k)
  | (Construct(_) as a, CursorT(OnDelim(_, side), _))
      when
        !ZTyp.is_before_zoperand(zoperand)
        && !ZTyp.is_after_zoperand(zoperand) =>
    switch (
      ana_perform_operand(
        ctx,
        Action_common.escape(side),
        (zoperand, u_gen),
        k,
      )
    ) {
    | (Failed | CursorEscaped(_)) as err => err
    | Succeeded((zty, u_gen)) => ana_perform(ctx, a, (zty, u_gen), k)
    }
  | (Construct(SChar(s)), CursorT(_, Hole)) =>
    ana_insert_text(ctx, u_gen, (0, s), "", k)
  | (
      Construct(SChar(s)),
      CursorT(OnText(j), (Int | Bool | Float) as operand),
    ) =>
    ana_insert_text(ctx, u_gen, (j, s), operand |> UHTyp.to_string, k)
  | (Construct(SChar(s)), CursorT(OnText(j), TyVar(_, x))) =>
    ana_insert_text(ctx, u_gen, (j, s), x, k)
  | (Construct(SChar(_)), CursorT(_)) => Failed

  | (Construct(SList), CursorT(_)) =>
    mk_ana_result(
      ctx,
      u_gen,
      ZOpSeq.wrap(ZTyp.ListZ(ZOpSeq.wrap(zoperand))),
      k,
    )
  | (Construct(SParenthesized), CursorT(_)) =>
    mk_ana_result(
      ctx,
      u_gen,
      ZOpSeq.wrap(ZTyp.ParenthesizedZ(ZOpSeq.wrap(zoperand))),
      k,
    )
  /* split */
  | (
      Construct(SOp(os)),
      CursorT(OnText(j), (Int | Bool | Float) as operand),
    )
      when
        !ZTyp.is_before_zoperand(zoperand)
        && !ZTyp.is_after_zoperand(zoperand) =>
    ana_split_text(ctx, u_gen, j, os, operand |> UHTyp.to_string)
  | (Construct(SOp(os)), CursorT(OnText(j), TyVar(_, id)))
      when
        !ZTyp.is_before_zoperand(zoperand)
        && !ZTyp.is_after_zoperand(zoperand) =>
    ana_split_text(ctx, u_gen, j, os, id)
  | (Construct(SOp(os)), CursorT(_)) =>
    switch (operator_of_shape(os)) {
    | None => Failed
    | Some(op) =>
      mk_ana_result(ctx, u_gen, construct_operator(op, zoperand, (E, E)), k)
    }

  /* Invalid SwapLeft and SwapRight acitons */
  | (SwapLeft | SwapRight, CursorT(_)) => Failed
  | (Init, _) => failwith("Init action should not be performed.")
  /* Zipper Cases */
  | (_, ParenthesizedZ(zbody)) =>
    switch (ana_perform(ctx, a, (zbody, u_gen), k)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      ana_perform_operand(
        ctx,
        Action_common.escape(side),
        (zoperand, u_gen),
        k,
      )
    | Succeeded((zbody, u_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.ParenthesizedZ(zbody)), u_gen))
    }
  | (_, ListZ(zbody)) =>
    switch (ana_perform(ctx, a, (zbody, u_gen), k)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      ana_perform_operand(
        ctx,
        Action_common.escape(side),
        (zoperand, u_gen),
        k,
      )
    | Succeeded((zbody, u_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.ListZ(zbody)), u_gen))
    }
  };
