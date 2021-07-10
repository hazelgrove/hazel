module Outcome = {
  open Sexplib.Std;

  [@deriving sexp]
  type t_('success) =
    | Succeeded('success)
    | CursorEscaped(Side.t);

  [@deriving sexp]
  type t('success) = option(t_('success));

  let succeeded = x => Some(Succeeded(x));

  let cursor_escaped = side => Some(CursorEscaped(side));

  include OptUtil;

  let of_action_outcome = (a: ActionOutcome.t('a)) =>
    switch (a) {
    | Succeeded(x) => succeeded(x)
    | CursorEscaped(side) => cursor_escaped(side)
    | Failed => None
    };
};

let operator_of_shape = (os: Action.operator_shape): option(UHTyp.operator) =>
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

let shape_of_operator = (op: UHTyp.operator): Action.operator_shape =>
  switch (op) {
  | Arrow => SArrow
  | Prod => SComma
  | Sum => SVBar
  };

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
      TyVar(
        InVarHole(Keyword(k), u),
        k |> ExpandingKeyword.to_string |> TyId.of_string,
      ),
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

let construct_operator =
    (
      u_gen: MetaVarGen.t,
      operator: UHTyp.operator,
      zoperand: ZTyp.zoperand,
      (prefix, suffix): ZTyp.operand_surround,
    )
    : (MetaVarGen.t, ZTyp.zopseq) => {
  let operand = zoperand |> ZTyp.erase_zoperand;
  let (u, u_gen) = MetaVarGen.next(u_gen);
  let (zoperand, surround) =
    if (ZTyp.is_before_zoperand(zoperand)) {
      let zoperand = UHTyp.Hole(u) |> ZTyp.place_before_operand;
      let new_suffix = Seq.A(operator, S(operand, suffix));
      (zoperand, (prefix, new_suffix));
    } else {
      let zoperand = UHTyp.Hole(u) |> ZTyp.place_before_operand;
      let new_prefix = Seq.A(operator, S(operand, prefix));
      (zoperand, (new_prefix, suffix));
    };
  (u_gen, ZTyp.mk_ZOpSeq(ZOperand(zoperand, surround)));
};

open Outcome;
module Syn_success = {
  module Poly = {
    [@deriving sexp]
    type t('z) = {
      zty: 'z,
      kind: Kind.t,
      u_gen: MetaVarGen.t,
    };
  };

  [@deriving sexp]
  type t = Poly.t(ZTyp.t);

  let mk_result =
      (ctx: Contexts.t, u_gen: MetaVarGen.t, zty: ZTyp.t): Outcome.t(t) => {
    open Outcome.Syntax;
    let+ kind = Elaborator_Typ.syn_kind(ctx, zty |> ZTyp.erase);
    Succeeded({Poly.zty, kind, u_gen});
  };
};
open Syn_success.Poly;

let mk_syn_text =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, caret_index: int, text: string)
    : ActionOutcome.t(Syn_success.t) => {
  let text_cursor = CursorPosition.OnText(caret_index);
  switch (TyTextShape.of_tyid(TyId.of_string(text))) {
  | None =>
    if (text |> StringUtil.is_empty) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      Succeeded({
        zty: ZOpSeq.wrap(ZTyp.CursorT(OnDelim(0, Before), Hole(u))),
        kind: Kind.KHole,
        u_gen,
      });
    } else {
      Failed;
    }
  | Some(Bool) =>
    Succeeded({
      zty: ZOpSeq.wrap(ZTyp.CursorT(text_cursor, Bool)),
      kind: Kind.Type,
      u_gen,
    })
  | Some(Int) =>
    Succeeded({
      zty: ZOpSeq.wrap(ZTyp.CursorT(text_cursor, Int)),
      kind: Kind.Type,
      u_gen,
    })
  | Some(Float) =>
    Succeeded({
      zty: ZOpSeq.wrap(ZTyp.CursorT(text_cursor, Float)),
      kind: Kind.Type,
      u_gen,
    })
  | Some(ExpandingKeyword(k)) =>
    let (u, u_gen) = u_gen |> MetaVarGen.next;
    Succeeded({
      zty:
        ZOpSeq.wrap(
          ZTyp.CursorT(
            text_cursor,
            TyVar(
              InVarHole(Keyword(k), u),
              k |> ExpandingKeyword.to_string |> TyId.of_string,
            ),
          ),
        ),
      kind: Kind.KHole,
      u_gen,
    });
  | Some(TyVar(x)) =>
    if (TyVarCtx.contains(Contexts.tyvars(ctx), x)) {
      let idx = TyVarCtx.index_of_exn(Contexts.tyvars(ctx), x);
      let (_, k) = TyVarCtx.tyvar_with_idx(Contexts.tyvars(ctx), idx);
      Succeeded({
        zty: ZOpSeq.wrap(ZTyp.CursorT(text_cursor, TyVar(NotInVarHole, x))),
        kind: k,
        u_gen,
      });
    } else {
      let (u, u_gen) = u_gen |> MetaVarGen.next;
      Succeeded({
        zty:
          ZOpSeq.wrap(
            ZTyp.CursorT(text_cursor, TyVar(InVarHole(Free, u), x)),
          ),
        kind: Kind.KHole,
        u_gen,
      });
    }
  };
};

open Outcome;
open Outcome.Syntax;

let rec syn_move =
        (a: Action.t, {zty, kind: _, u_gen: _} as syn_r: Syn_success.t)
        : Outcome.t(Syn_success.t) =>
  switch (a) {
  | MoveTo(path) =>
    let* zty = CursorPath_Typ.follow(path, zty |> ZTyp.erase);
    succeeded({...syn_r, zty});
  | MoveToPrevHole =>
    let* steps =
      CursorPath_common.(prev_hole_steps(CursorPath_Typ.holes_z(zty, [])));
    let* path = CursorPath_Typ.of_steps(steps, zty |> ZTyp.erase);
    syn_move(MoveTo(path), syn_r);

  | MoveToNextHole =>
    let* steps =
      CursorPath_common.(next_hole_steps(CursorPath_Typ.holes_z(zty, [])));
    let* path = CursorPath_Typ.of_steps(steps, zty |> ZTyp.erase);
    syn_move(MoveTo(path), syn_r);
  | MoveLeft =>
    switch (ZTyp.move_cursor_left(zty)) {
    | None => cursor_escaped(Before)
    | Some(z) => succeeded({...syn_r, zty: z})
    }
  | MoveRight =>
    switch (ZTyp.move_cursor_right(zty)) {
    | None => cursor_escaped(After)
    | Some(z) => succeeded({...syn_r, zty: z})
    }
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
      ++ Sexplib.Sexp.to_string(Action.sexp_of_t(a)),
    )
  };

let syn_insert_text = Action_common.syn_insert_text_(~mk_syn_text);
let syn_backspace_text = Action_common.syn_backspace_text_(~mk_syn_text);
let syn_delete_text = Action_common.syn_delete_text_(~mk_syn_text);

let syn_split_text =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      caret_index: int,
      sop: Action.operator_shape,
      text: string,
    )
    : Outcome.t(Syn_success.t) => {
  let (l, r) = text |> StringUtil.split_string(caret_index);
  switch (
    TyTextShape.of_tyid(TyId.of_string(l)),
    operator_of_shape(sop),
    TyTextShape.of_tyid(TyId.of_string(r)),
  ) {
  | (None, _, _)
  | (_, None, _)
  | (_, _, None) => None
  | (Some(lshape), Some(op), Some(rshape)) =>
    let (loperand, u_gen) = text_operand(ctx, u_gen, lshape);
    let (roperand, u_gen) = text_operand(ctx, u_gen, rshape);
    let new_zty = {
      let zoperand = roperand |> ZTyp.place_before_operand;
      ZTyp.mk_ZOpSeq(ZOperand(zoperand, (A(op, S(loperand, E)), E)));
    };
    let* kind = Elaborator_Typ.syn_kind(ctx, ZTyp.erase(new_zty));
    succeeded({zty: new_zty, kind, u_gen});
  };
};

let rec syn_perform =
        (ctx: Contexts.t, a: Action.t, syn_r: Syn_success.t)
        : Outcome.t(Syn_success.t) =>
  syn_perform_opseq(ctx, a, syn_r)
and syn_perform_opseq =
    (
      ctx: Contexts.t,
      a: Action.t,
      {zty: ZOpSeq(skel, zseq) as zopseq, kind: _, u_gen} as syn_r: Syn_success.t,
    )
    : Outcome.t(Syn_success.t) =>
  switch (a, zseq) {
  /* Invalid actions at the type level */
  | (
      UpdateApPalette(_) |
      Construct(
        SAnn | SLet | SLine | SLam | SListNil | SInj(_) | SCase | SApPalette(_),
      ) |
      SwapUp |
      SwapDown,
      _,
    )
  /* Invalid cursor positions */
  | (_, ZOperator((OnText(_) | OnDelim(_), _), _)) => None

  /* Movement handled at top level */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    syn_move(a, syn_r)

  /* Deletion */

  | (Delete, ZOperator((OnOp(After as side), _), _))
  | (Backspace, ZOperator((OnOp(Before as side), _), _)) =>
    syn_perform_opseq(ctx, Action_common.escape(side), syn_r)

  /* Delete before operator == Backspace after operator */
  | (Delete, ZOperator((OnOp(Before), op), surround)) =>
    syn_perform_opseq(
      ctx,
      Backspace,
      {
        ...syn_r,
        zty: ZOpSeq(skel, ZOperator((OnOp(After), op), surround)),
      },
    )
  /* ... + [k-2] + [k-1] +<| [k] + ...   ==>   ... + [k-2] + [k-1]| + ...
   * (for now until we have proper type constructors) */
  | (Backspace, ZOperator((OnOp(After), _), (prefix, suffix))) =>
    let S(prefix_hd, new_prefix) = prefix;
    let zoperand = prefix_hd |> ZTyp.place_after_operand;
    let S(_, new_suffix) = suffix;
    Syn_success.mk_result(
      ctx,
      u_gen,
      ZTyp.mk_ZOpSeq(ZOperand(zoperand, (new_prefix, new_suffix))),
    );

  /* Construction */
  /* construction on operators becomes movement... */
  | (Construct(SOp(SSpace)), ZOperator((OnOp(After), _), _)) =>
    syn_perform_opseq(ctx, MoveRight, syn_r)
  /* ...or construction after movement */
  | (Construct(_) as a, ZOperator((OnOp(side), _), _)) =>
    let* outcome' =
      syn_perform_opseq(ctx, Action_common.escape(side), syn_r);
    switch (outcome') {
    | CursorEscaped(_) => None
    | Succeeded(syn_r') => syn_perform(ctx, a, syn_r')
    };

  /* Space becomes movement until we have proper type constructors */
  | (Construct(SOp(SSpace)), ZOperand(zoperand, _))
      when ZTyp.is_after_zoperand(zoperand) =>
    syn_perform_opseq(ctx, MoveRight, syn_r)

  | (Construct(SOp(os)), ZOperand(CursorT(_) as zoperand, surround)) =>
    open Outcome.Syntax;
    let* op = operator_of_shape(os);
    let (u_gen, zty) = construct_operator(u_gen, op, zoperand, surround);
    Syn_success.mk_result(ctx, u_gen, zty);

  /* SwapLeft and SwapRight is handled at block level */

  | (SwapLeft, ZOperator(_))
  | (SwapRight, ZOperator(_)) => None

  | (SwapLeft, ZOperand(CursorT(_), (E, _))) => None
  | (
      SwapLeft,
      ZOperand(
        CursorT(_) as zoperand,
        (A(operator, S(operand, new_prefix)), suffix),
      ),
    ) =>
    let new_suffix = Seq.A(operator, S(operand, suffix));
    let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, new_suffix));
    Syn_success.mk_result(ctx, u_gen, ZTyp.mk_ZOpSeq(new_zseq));
  | (SwapRight, ZOperand(CursorT(_), (_, E))) => None
  | (
      SwapRight,
      ZOperand(
        CursorT(_) as zoperand,
        (prefix, A(operator, S(operand, new_suffix))),
      ),
    ) =>
    let new_prefix = Seq.A(operator, S(operand, prefix));
    let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, new_suffix));
    Syn_success.mk_result(ctx, u_gen, ZTyp.mk_ZOpSeq(new_zseq));

  /* Zipper */
  | (_, ZOperand(zoperand, (prefix, suffix))) =>
    let uhty = ZTyp.erase(ZOpSeq.wrap(zoperand));

    let* kind = Elaborator_Typ.syn_kind(ctx, uhty);
    let* outcome = syn_perform_operand(ctx, a, {zty: zoperand, kind, u_gen});
    switch (outcome) {
    | CursorEscaped(side) =>
      syn_perform_opseq(
        ctx,
        Action_common.escape(side),
        {zty: zopseq, kind, u_gen},
      )
    | Succeeded({zty: ZOpSeq(_, zseq), kind: _, u_gen}) =>
      switch (zseq) {
      | ZOperand(zoperand, (inner_prefix, inner_suffix)) =>
        let new_prefix = Seq.affix_affix(inner_prefix, prefix);
        let new_suffix = Seq.affix_affix(inner_suffix, suffix);
        Syn_success.mk_result(
          ctx,
          u_gen,
          ZTyp.mk_ZOpSeq(ZOperand(zoperand, (new_prefix, new_suffix))),
        );
      | ZOperator(zoperator, (inner_prefix, inner_suffix)) =>
        let new_prefix = Seq.seq_affix(inner_prefix, prefix);
        let new_suffix = Seq.seq_affix(inner_suffix, suffix);
        Syn_success.mk_result(
          ctx,
          u_gen,
          ZTyp.mk_ZOpSeq(ZOperator(zoperator, (new_prefix, new_suffix))),
        );
      }
    };
  | (Init, _) => failwith("Init action should not be performed.")
  }
and syn_perform_operand =
    (ctx: Contexts.t, a: Action.t, {zty: zoperand, kind, u_gen})
    : Outcome.t(Syn_success.t) =>
  switch (a, zoperand) {
  /* Invalid actions at the type level */
  | (
      UpdateApPalette(_) |
      Construct(
        SAnn | SLet | STyAlias | SLine | SLam | SListNil | SInj(_) | SCase |
        SApPalette(_) |
        SCommentLine,
      ) |
      SwapUp |
      SwapDown,
      _,
    ) =>
    None

  /* Movement handled at top level */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    syn_move(a, {zty: ZOpSeq.wrap(zoperand), kind, u_gen})

  /* Backspace and Delete */

  | (Backspace, CursorT(OnText(caret_index), Int)) =>
    syn_backspace_text(ctx, u_gen, caret_index, "Int")
    |> Outcome.of_action_outcome
  | (Backspace, CursorT(OnText(caret_index), Bool)) =>
    syn_backspace_text(ctx, u_gen, caret_index, "Bool")
    |> Outcome.of_action_outcome
  | (Backspace, CursorT(OnText(caret_index), Float)) =>
    syn_backspace_text(ctx, u_gen, caret_index, "Float")
    |> Outcome.of_action_outcome

  | (Delete, CursorT(OnText(caret_index), Int)) =>
    syn_delete_text(ctx, u_gen, caret_index, "Int")
    |> Outcome.of_action_outcome
  | (Delete, CursorT(OnText(caret_index), Bool)) =>
    syn_delete_text(ctx, u_gen, caret_index, "Bool")
    |> Outcome.of_action_outcome
  | (Delete, CursorT(OnText(caret_index), Float)) =>
    syn_delete_text(ctx, u_gen, caret_index, "Float")
    |> Outcome.of_action_outcome

  /* ( _ <|)   ==>   ( _| ) */
  | (Backspace, CursorT(OnDelim(_, Before), _)) =>
    zoperand |> ZTyp.is_before_zoperand
      ? cursor_escaped(Before)
      : syn_perform_operand(ctx, MoveLeft, {zty: zoperand, kind, u_gen})
  /* (|> _ )   ==>   ( |_ ) */
  | (Delete, CursorT(OnDelim(_, After), _)) =>
    zoperand |> ZTyp.is_after_zoperand
      ? cursor_escaped(After)
      : syn_perform_operand(ctx, MoveRight, {zty: zoperand, kind, u_gen})

  /* Delete before delimiter == Backspace after delimiter */
  | (Delete, CursorT(OnDelim(k, Before), operand)) =>
    syn_perform_operand(
      ctx,
      Backspace,
      {zty: CursorT(OnDelim(k, After), operand), kind, u_gen},
    )

  | (Backspace, CursorT(OnDelim(_, After), Hole(u))) =>
    Syn_success.mk_result(
      ctx,
      u_gen,
      ZOpSeq.wrap(ZTyp.place_before_operand(Hole(u))),
    )
  | (Backspace, CursorT(OnDelim(_, After), Unit)) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    Syn_success.mk_result(
      ctx,
      u_gen,
      ZOpSeq.wrap(ZTyp.place_before_operand(Hole(u))),
    );

  | (
      Backspace,
      CursorT(OnDelim(_, After), Int | Float | Bool | TyVar(_, _)),
    ) =>
    failwith("Impossible: Int|Float|Bool|TyVar are treated as text")
  /* TyVar-related Backspace & Delete */
  | (Delete, CursorT(OnText(caret_index), TyVar(_, text))) =>
    syn_delete_text(ctx, u_gen, caret_index, text |> TyId.to_string)
    |> Outcome.of_action_outcome
  | (Backspace, CursorT(OnText(caret_index), TyVar(_, text))) =>
    syn_backspace_text(ctx, u_gen, caret_index, text |> TyId.to_string)
    |> Outcome.of_action_outcome

  /* ( _ )<|  ==>  _| */
  /* (<| _ )  ==>  |_ */
  | (
      Backspace,
      CursorT(OnDelim(k, After), Parenthesized(body) | List(body)),
    ) =>
    let place_cursor = k == 0 ? ZTyp.place_before : ZTyp.place_after;
    Syn_success.mk_result(ctx, u_gen, body |> place_cursor);

  /* Construction */

  | (Construct(SOp(SSpace)), CursorT(OnDelim(_, After), _)) =>
    syn_perform_operand(ctx, MoveRight, {zty: zoperand, kind, u_gen})
  | (Construct(_) as a, CursorT(OnDelim(_, side), _))
      when
        !ZTyp.is_before_zoperand(zoperand)
        && !ZTyp.is_after_zoperand(zoperand) =>
    switch (
      syn_perform_operand(
        ctx,
        Action_common.escape(side),
        {zty: zoperand, kind, u_gen},
      )
    ) {
    | (None | Some(CursorEscaped(_))) as err => err
    | Some(Succeeded(syn_r)) => syn_perform(ctx, a, syn_r)
    }

  | (Construct(SChar(s)), CursorT(_, Hole(_))) =>
    syn_insert_text(ctx, u_gen, (0, s), "") |> Outcome.of_action_outcome
  | (Construct(SChar(s)), CursorT(OnText(j), Int)) =>
    syn_insert_text(ctx, u_gen, (j, s), "Int") |> Outcome.of_action_outcome
  | (Construct(SChar(s)), CursorT(OnText(j), Bool)) =>
    syn_insert_text(ctx, u_gen, (j, s), "Bool") |> Outcome.of_action_outcome
  | (Construct(SChar(s)), CursorT(OnText(j), Float)) =>
    syn_insert_text(ctx, u_gen, (j, s), "Float") |> Outcome.of_action_outcome

  | (Construct(SChar(s)), CursorT(OnText(j), TyVar(_, x))) =>
    syn_insert_text(ctx, u_gen, (j, s), x |> TyId.to_string)
    |> Outcome.of_action_outcome
  | (Construct(SChar(_)), CursorT(_)) => None

  | (Construct(SList), CursorT(_)) =>
    Syn_success.mk_result(
      ctx,
      u_gen,
      ZOpSeq.wrap(ZTyp.ListZ(ZOpSeq.wrap(zoperand))),
    )

  | (Construct(SParenthesized), CursorT(_)) =>
    Syn_success.mk_result(
      ctx,
      u_gen,
      ZOpSeq.wrap(ZTyp.ParenthesizedZ(ZOpSeq.wrap(zoperand))),
    )

  /* split */
  | (Construct(SOp(os)), CursorT(OnText(j), Int))
      when
        !ZTyp.is_before_zoperand(zoperand)
        && !ZTyp.is_after_zoperand(zoperand) =>
    syn_split_text(ctx, u_gen, j, os, "Int")
  | (Construct(SOp(os)), CursorT(OnText(j), Bool))
      when
        !ZTyp.is_before_zoperand(zoperand)
        && !ZTyp.is_after_zoperand(zoperand) =>
    syn_split_text(ctx, u_gen, j, os, "Bool")
  | (Construct(SOp(os)), CursorT(OnText(j), Float))
      when
        !ZTyp.is_before_zoperand(zoperand)
        && !ZTyp.is_after_zoperand(zoperand) =>
    syn_split_text(ctx, u_gen, j, os, "Float")

  | (Construct(SOp(os)), CursorT(OnText(j), TyVar(_, id)))
      when
        !ZTyp.is_before_zoperand(zoperand)
        && !ZTyp.is_after_zoperand(zoperand) =>
    syn_split_text(ctx, u_gen, j, os, id |> TyId.to_string)

  | (Construct(SOp(os)), CursorT(_)) =>
    open Outcome.Syntax;
    let* op = operator_of_shape(os);
    let (u_gen, zty) = construct_operator(u_gen, op, zoperand, (E, E));
    Syn_success.mk_result(ctx, u_gen, zty);

  /* Invalid SwapLeft and SwapRight actions */
  | (SwapLeft | SwapRight, CursorT(_)) => None

  /* Zipper Cases */
  | (_, ParenthesizedZ(zbody)) =>
    open Outcome.Syntax;
    let* outcome = syn_perform(ctx, a, {zty: zbody, kind, u_gen});
    switch (outcome) {
    | CursorEscaped(side) =>
      syn_perform_operand(
        ctx,
        Action_common.escape(side),
        {zty: zoperand, kind, u_gen},
      )
    | Succeeded({zty: zbody, u_gen, kind: _}) =>
      Syn_success.mk_result(
        ctx,
        u_gen,
        ZOpSeq.wrap(ZTyp.ParenthesizedZ(zbody)),
      )
    };
  | (_, ListZ(zbody)) =>
    open Outcome.Syntax;
    let* outcome = syn_perform(ctx, a, {zty: zbody, kind, u_gen});
    switch (outcome) {
    | CursorEscaped(side) =>
      syn_perform_operand(
        ctx,
        Action_common.escape(side),
        {zty: zoperand, kind, u_gen},
      )
    | Succeeded({zty: zbody, kind: _, u_gen}) =>
      Syn_success.mk_result(ctx, u_gen, ZOpSeq.wrap(ZTyp.ListZ(zbody)))
    };

  /* Invalid cursor positions */
  | (_, CursorT(OnText(_) | OnOp(_), _)) => None
  | (_, CursorT(cursor, operand))
      when !ZTyp.is_valid_cursor_operand(cursor, operand) =>
    None

  | (Init, _) => failwith("Init action should not be performed.")
  };

open Outcome;
open Outcome.Syntax;
module Ana_success = {
  module Poly = {
    [@deriving sexp]
    type t('z) = {
      zty: 'z,
      u_gen: MetaVarGen.t,
    };

    let of_syn = ({Syn_success.Poly.zty, kind: _, u_gen}) => {zty, u_gen};
  };

  [@deriving sexp]
  type t = Poly.t(ZTyp.t);

  let mk_result =
      (_ctx: Contexts.t, u_gen: MetaVarGen.t, zty: ZTyp.t, _k: Kind.t)
      : Outcome.t(t) => {
    // TODO: Add an error status: Don't fail -- put an error status on it when it can fail
    succeeded({
      Poly.zty,
      u_gen,
    });
  };
};
open Ana_success.Poly;

let rec ana_move =
        (a: Action.t, {zty, u_gen: _} as ana_r: Ana_success.t, kind: Kind.t)
        : Outcome.t(Ana_success.t) =>
  switch (a) {
  | MoveTo(path) =>
    let* zty = CursorPath_Typ.follow(path, zty |> ZTyp.erase);
    succeeded({...ana_r, zty});
  | MoveToPrevHole =>
    let* steps =
      CursorPath_common.(prev_hole_steps(CursorPath_Typ.holes_z(zty, [])));
    let* path = CursorPath_Typ.of_steps(steps, zty |> ZTyp.erase);
    ana_move(MoveTo(path), ana_r, kind);

  | MoveToNextHole =>
    let* steps =
      CursorPath_common.(next_hole_steps(CursorPath_Typ.holes_z(zty, [])));
    let* path = CursorPath_Typ.of_steps(steps, zty |> ZTyp.erase);
    ana_move(MoveTo(path), ana_r, kind);
  | MoveLeft =>
    switch (ZTyp.move_cursor_left(zty)) {
    | None => cursor_escaped(Before)
    | Some(z) => succeeded({...ana_r, zty: z})
    }
  | MoveRight =>
    switch (ZTyp.move_cursor_right(zty)) {
    | None => cursor_escaped(After)
    | Some(z) => succeeded({...ana_r, zty: z})
    }
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
      ++ Sexplib.Sexp.to_string(Action.sexp_of_t(a)),
    )
  };

let rec ana_perform = (ctx, a, s, k) => ana_perform_opseq(ctx, a, s, k)
and ana_perform_opseq =
    (
      ctx: Contexts.t,
      a: Action.t,
      {zty: ZOpSeq(_, zseq) as zopseq, u_gen}: Ana_success.t,
      k: Kind.t,
    )
    : Outcome.t(Ana_success.t) => {
  switch (a, zseq) {
  | (_, ZOperand(zoperand, (E, E))) =>
    ana_perform_operand(ctx, a, {zty: zoperand, u_gen}, k)
  | (_, ZOperand(zoperand, (prefix, suffix))) =>
    open Outcome.Syntax;
    let uhty = ZTyp.erase(ZOpSeq.wrap(zoperand));
    let* kind = Elaborator_Typ.syn_kind(ctx, uhty);
    let* outcome =
      syn_perform_operand(
        ctx,
        a,
        {Syn_success.Poly.zty: zoperand, kind, u_gen},
      );
    switch (outcome) {
    | CursorEscaped(side) =>
      ana_perform_opseq(
        ctx,
        Action_common.escape(side),
        {zty: zopseq, u_gen},
        kind,
      )
    | Succeeded({Syn_success.Poly.zty: ZOpSeq(_, zseq), u_gen, kind: _}) =>
      switch (zseq) {
      | ZOperand(zoperand, (inner_prefix, inner_suffix)) =>
        let new_prefix = Seq.affix_affix(inner_prefix, prefix);
        let new_suffix = Seq.affix_affix(inner_suffix, suffix);
        Ana_success.mk_result(
          ctx,
          u_gen,
          ZTyp.mk_ZOpSeq(ZOperand(zoperand, (new_prefix, new_suffix))),
          k,
        );
      | ZOperator(zoperator, (inner_prefix, inner_suffix)) =>
        let new_prefix = Seq.seq_affix(inner_prefix, prefix);
        let new_suffix = Seq.seq_affix(inner_suffix, suffix);
        Ana_success.mk_result(
          ctx,
          u_gen,
          ZTyp.mk_ZOpSeq(ZOperator(zoperator, (new_prefix, new_suffix))),
          k,
        );
      }
    };
  | (_, _) =>
    open Outcome.Syntax;
    // Subsumption

    let* outcome =
      syn_perform_opseq(
        ctx,
        a,
        {Syn_success.Poly.zty: zopseq, u_gen, kind: k},
      );
    switch (outcome) {
    | CursorEscaped(side) => cursor_escaped(side)
    | Succeeded(syn_r) =>
      Ana_success.mk_result(ctx, syn_r.u_gen, syn_r.zty, k)
    };
  };
}
and ana_perform_operand =
    (
      ctx: Contexts.t,
      a: Action.t,
      {zty: zoperand, u_gen}: Ana_success.Poly.t(ZTyp.zoperand),
      kind: Kind.t,
    )
    : Outcome.t(Ana_success.t) => {
  open Outcome.Syntax;

  let* outcome =
    syn_perform_operand(
      ctx,
      a,
      {Syn_success.Poly.zty: zoperand, kind, u_gen},
    );
  switch (outcome) {
  | CursorEscaped(side) => cursor_escaped(side)
  | Succeeded(syn_r) => Ana_success.Poly.of_syn(syn_r) |> succeeded
  };
};
