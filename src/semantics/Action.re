let _TEST_PERFORM = false;
open SemanticsCommon;
open GeneralUtil;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp)]
type op_shape =
  | SMinus
  | SPlus
  | STimes
  | SLessThan
  | SSpace
  | SComma
  | SArrow
  | SVBar
  | SCons
  | SAnd
  | SOr;

let ty_op_of = (os: op_shape): option(UHTyp.op) =>
  switch (os) {
  | SArrow => Some(Arrow)
  | SComma => Some(Prod)
  | SVBar => Some(Sum)
  | SMinus
  | SPlus
  | STimes
  | SAnd
  | SOr
  | SLessThan
  | SSpace
  | SCons => None
  };

let op_shape_of_ty_op = (op: UHTyp.op): op_shape =>
  switch (op) {
  | Arrow => SArrow
  | Prod => SComma
  | Sum => SVBar
  };

let pat_op_of = (os: op_shape): option(UHPat.op) =>
  switch (os) {
  | SComma => Some(Comma)
  | SSpace => Some(Space)
  | SCons => Some(Cons)
  | SAnd
  | SOr
  | SMinus
  | SPlus
  | STimes
  | SLessThan
  | SArrow
  | SVBar => None
  };

let op_shape_of_pat_op = (op: UHPat.op): op_shape =>
  switch (op) {
  | Comma => SComma
  | Space => SSpace
  | Cons => SCons
  };

let exp_op_of = (os: op_shape): option(UHExp.op) =>
  switch (os) {
  | SPlus => Some(Plus)
  | SMinus => Some(Minus)
  | STimes => Some(Times)
  | SLessThan => Some(LessThan)
  | SSpace => Some(Space)
  | SComma => Some(Comma)
  | SCons => Some(Cons)
  | SAnd => Some(And)
  | SOr => Some(Or)
  | SArrow
  | SVBar => None
  };

let op_shape_of_exp_op = (op: UHExp.op): op_shape =>
  switch (op) {
  | Minus => SMinus
  | Plus => SPlus
  | Times => STimes
  | LessThan => SLessThan
  | Space => SSpace
  | Comma => SComma
  | Cons => SCons
  | And => SAnd
  | Or => SOr
  };

[@deriving (show({with_path: false}), sexp)]
type shape =
  | SParenthesized
  /* type shapes */
  | SNum
  | SBool
  | SList
  /* expression shapes */
  | SAsc
  | SVar(Var.t, cursor_position)
  | SLam
  | SNumLit(int, cursor_position)
  | SListNil
  | SInj(inj_side)
  | SLet
  | SLine
  | SCase
  | SOp(op_shape)
  | SLivelitName(LivelitName.t)
  /* pattern-only shapes */
  | SWild;

[@deriving (show({with_path: false}), sexp)]
type t =
  | MoveTo(Path.t)
  | MoveToBefore(Path.steps)
  | MoveLeft
  | MoveRight
  | MoveToNextHole
  | MoveToPrevHole
  | LivelitAction(SerializedAction.t)
  | Delete
  | Backspace
  | Construct(shape)
  | ShiftLeft
  | ShiftRight
  | ShiftUp
  | ShiftDown;

type result('success) =
  | Succeeded('success)
  | CursorEscaped(side)
  | CantShift
  | Failed;

let make_ty_OpSeqZ = (zty0: ZTyp.t, surround: ZTyp.opseq_surround): ZTyp.t => {
  let uty0 = ZTyp.erase(zty0);
  let seq = OperatorSeq.opseq_of_exp_and_surround(uty0, surround);
  let skel = Associator.associate_ty(seq);
  OpSeqZ(skel, zty0, surround);
};

let rec perform_ty = (a: t, zty: ZTyp.t): result(ZTyp.t) =>
  switch (a, zty) {
  | (
      _,
      CursorT(OnText(_), _) |
      CursorT(Staging(_), Hole | Unit | Num | Bool | OpSeq(_, _)),
    ) =>
    // invalid cursor position
    Failed
  | (_, CursorT(cursor, uty)) when !ZTyp.is_valid_cursor(cursor, uty) =>
    Failed
  /* Staging */
  | (ShiftUp | ShiftDown, _) =>
    // only can shift up and down in blocks
    Failed
  | (ShiftLeft | ShiftRight, CursorT(OnDelim(_, _), _)) => Failed
  | (
      ShiftLeft | ShiftRight,
      CursorT(Staging(k), (Parenthesized(body) | List(body)) as staged) |
      OpSeqZ(
        _,
        CursorT(Staging(k), (Parenthesized(body) | List(body)) as staged),
        _,
      ),
    ) =>
    let shift_optm =
      switch (k, a) {
      | (0, ShiftLeft) => OpSeqUtil.Typ.shift_optm_from_prefix
      | (0, ShiftRight) => OpSeqUtil.Typ.shift_optm_to_prefix
      | (1, ShiftLeft) => OpSeqUtil.Typ.shift_optm_to_suffix
      | (_one, _shift_right) => OpSeqUtil.Typ.shift_optm_from_suffix
      };
    let surround =
      switch (zty) {
      | OpSeqZ(_, _, surround) => Some(surround)
      | _cursor_t => None
      };
    switch (body |> shift_optm(~surround)) {
    | None => CantShift
    | Some((new_body, new_surround)) =>
      let new_ztm =
        ZTyp.CursorT(
          Staging(k),
          switch (staged) {
          | List(_) => List(new_body)
          | _parenthesized => Parenthesized(new_body)
          },
        );
      let new_zty =
        switch (new_surround) {
        | None => new_ztm
        | Some(surround) => OpSeqUtil.Typ.mk_OpSeqZ(new_ztm, surround)
        };
      Succeeded(new_zty);
    };
  /* Movement */
  | (MoveTo(path), _) =>
    switch (Path.follow_ty(path, ZTyp.erase(zty))) {
    | None => Failed
    | Some(zty) => Succeeded(zty)
    }
  | (MoveToBefore(steps), _) =>
    switch (Path.follow_ty_and_place_before(steps, ZTyp.erase(zty))) {
    | None => Failed
    | Some(zty) => Succeeded(zty)
    }
  | (MoveToPrevHole, _) =>
    switch (Path.prev_hole_path(Path.holes_zty(zty, []))) {
    | None => Failed
    | Some(path) => perform_ty(MoveTo(path), zty)
    }
  | (MoveToNextHole, _) =>
    switch (Path.next_hole_path(Path.holes_zty(zty, []))) {
    | None => Failed
    | Some(path) =>
      /* [debug] let path = Helper.log_path path in */
      perform_ty(MoveTo(path), zty)
    }
  | (MoveLeft, _) =>
    ZTyp.move_cursor_left(zty)
    |> Opt.map_default(~default=CursorEscaped(Before), zty => Succeeded(zty))
  | (MoveRight, _) =>
    ZTyp.move_cursor_right(zty)
    |> Opt.map_default(~default=CursorEscaped(After), zty => Succeeded(zty))
  /* Backspace and Delete */
  | (Backspace, _) when ZTyp.is_before(zty) => CursorEscaped(Before)
  | (Delete, _) when ZTyp.is_after(zty) => CursorEscaped(After)
  | (Backspace, CursorT(_, Hole as uty)) =>
    ZTyp.is_after(zty) ? Succeeded(ZTyp.place_before(uty)) : Failed
  | (Delete, CursorT(_, Hole as uty)) =>
    ZTyp.is_before(zty) ? Succeeded(ZTyp.place_after(uty)) : Failed
  | (Backspace | Delete, CursorT(_, Unit | Num | Bool)) =>
    Succeeded(ZTyp.place_before(Hole))
  /* ( _ <|)   ==>   ( _| ) */
  /* ... + [k-2] + [k-1] <|+ [k] + ...   ==>   ... + [k-2] + [k-1]| + [k] + ... */
  | (
      Backspace,
      CursorT(OnDelim(_, Before), Parenthesized(_) | List(_) | OpSeq(_, _)),
    ) =>
    perform_ty(MoveLeft, zty)
  /* (|> _ )   ==>   ( |_ ) */
  /* ... + [k-1] +|> [k] + [k+1] + ...   ==>   ... + [k-1] + |[k] + [k+1] + ... */
  | (
      Delete,
      CursorT(OnDelim(_, After), Parenthesized(_) | List(_) | OpSeq(_, _)),
    ) =>
    perform_ty(MoveRight, zty)
  /* Delete before delimiter == Backspace after delimiter */
  | (
      Delete,
      CursorT(
        OnDelim(k, Before),
        (Parenthesized(_) | List(_) | OpSeq(_, _)) as uty,
      ),
    ) =>
    perform_ty(Backspace, CursorT(OnDelim(k, After), uty))
  /* ( _ )<|  ==>  ( _ [)] */
  | (
      Backspace,
      CursorT(OnDelim(k, After), (Parenthesized(_) | List(_)) as uty),
    ) =>
    Succeeded(CursorT(Staging(k), uty))
  | (
      Backspace | Delete,
      CursorT(Staging(k), Parenthesized(body) | List(body)),
    ) =>
    Succeeded(
      body
      |> (
        switch (k) {
        | 0 => ZTyp.place_before
        | _one => ZTyp.place_after
        }
      ),
    )
  /* ... + [k-2] + [k-1] +<| [k] + ...   ==>   ... + [k-2] + [k-1]| + ... */
  | (Backspace, CursorT(OnDelim(k, After), OpSeq(_, seq))) =>
    switch (OperatorSeq.split(k, seq)) {
    | None => Failed /* invalid cursor position */
    | Some((_, surround)) =>
      switch (surround) {
      | EmptyPrefix(_) => Failed /* invalid cursor position */
      | EmptySuffix(prefix) =>
        switch (prefix) {
        | ExpPrefix(ty, _) => Succeeded(ZTyp.place_after(ty))
        | SeqPrefix(seq, _) =>
          let skel = Associator.associate_ty(seq);
          Succeeded(ZTyp.place_after(OpSeq(skel, seq)));
        }
      | BothNonEmpty(prefix, suffix) =>
        let (zty0: ZTyp.t, surround: ZTyp.opseq_surround) =
          switch (prefix) {
          | ExpPrefix(ty, _) => (ZTyp.place_after(ty), EmptyPrefix(suffix))
          | SeqPrefix(ExpOpExp(ty1, op, ty2), _) => (
              ZTyp.place_after(ty2),
              BothNonEmpty(ExpPrefix(ty1, op), suffix),
            )
          | SeqPrefix(SeqOpExp(seq, op, ty), _) => (
              ZTyp.place_after(ty),
              BothNonEmpty(SeqPrefix(seq, op), suffix),
            )
          };
        let skel =
          Associator.associate_ty(
            OperatorSeq.opseq_of_exp_and_surround(
              ZTyp.erase(zty0),
              surround,
            ),
          );
        Succeeded(OpSeqZ(skel, zty0, surround));
      }
    }
  /* Construction */
  | (Construct(SOp(SSpace)), CursorT(OnDelim(_, After), _)) =>
    perform_ty(MoveRight, zty)
  | (Construct(_) as a, CursorT(OnDelim(_, side), _))
      when !ZTyp.is_before(zty) && !ZTyp.is_after(zty) =>
    let move_then_perform = move_action =>
      switch (perform_ty(move_action, zty)) {
      | Failed
      | CantShift
      | CursorEscaped(_) => assert(false)
      | Succeeded(zty) => perform_ty(a, zty)
      };
    switch (side) {
    | Before => move_then_perform(MoveLeft)
    | After => move_then_perform(MoveRight)
    };
  | (
      Construct(SLine),
      CursorT(Staging(k), (Parenthesized(_) | List(_)) as uty),
    ) =>
    Succeeded(CursorT(OnDelim(k, k == 0 ? Before : After), uty))
  | (Construct(_), CursorT(Staging(_), _)) => Failed
  | (Construct(SParenthesized), CursorT(_, _)) =>
    Succeeded(ParenthesizedZ(zty))
  | (Construct(SNum), CursorT(_, Hole)) =>
    Succeeded(ZTyp.place_after(Num))
  | (Construct(SNum), CursorT(_, _)) => Failed
  | (Construct(SBool), CursorT(_, Hole)) =>
    Succeeded(ZTyp.place_after(Bool))
  | (Construct(SBool), CursorT(_, _)) => Failed
  | (Construct(SList), CursorT(_, _)) => Succeeded(ListZ(zty))
  | (Construct(SOp(os)), CursorT(_, _)) =>
    let uty = ZTyp.erase(zty);
    if (ZTyp.is_before(zty)) {
      switch (ty_op_of(os)) {
      | None => Failed
      | Some(op) =>
        let surround = OperatorSeq.EmptyPrefix(ExpSuffix(op, uty));
        let zty0 = ZTyp.place_before(Hole);
        Succeeded(make_ty_OpSeqZ(zty0, surround));
      };
    } else {
      switch (ty_op_of(os)) {
      | None => Failed
      | Some(op) =>
        let surround = OperatorSeq.EmptySuffix(ExpPrefix(uty, op));
        let zty0 = ZTyp.place_before(Hole);
        Succeeded(make_ty_OpSeqZ(zty0, surround));
      };
    };
  | (Construct(SOp(SSpace)), OpSeqZ(_, CursorT(OnDelim(_, After), _), _)) =>
    perform_ty(MoveRight, zty)
  | (Construct(SOp(os)), OpSeqZ(_, CursorT(_, _) as zty, surround)) =>
    let uty = ZTyp.erase(zty);
    if (ZTyp.is_before(zty)) {
      switch (ty_op_of(os)) {
      | None => Failed
      | Some(op) =>
        switch (surround) {
        | EmptyPrefix(suffix) =>
          /* |zty0 suffix -> |_ op uty0 suffix */
          let suffix' = OperatorSeq.suffix_prepend_exp(suffix, op, uty);
          let surround' = OperatorSeq.EmptyPrefix(suffix');
          let zty0' = ZTyp.place_before(Hole);
          Succeeded(make_ty_OpSeqZ(zty0', surround'));
        | EmptySuffix(prefix) =>
          /* prefix |zty0 -> prefix |_ op uty0 */
          let suffix' = OperatorSeq.ExpSuffix(op, uty);
          let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
          let zty0' = ZTyp.place_before(Hole);
          Succeeded(make_ty_OpSeqZ(zty0', surround'));
        | BothNonEmpty(prefix, suffix) =>
          /* prefix |zty0 suffix -> prefix |_ op uty0 suffix */
          let suffix' = OperatorSeq.suffix_prepend_exp(suffix, op, uty);
          let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
          let zty0' = ZTyp.place_before(Hole);
          Succeeded(make_ty_OpSeqZ(zty0', surround'));
        }
      };
    } else {
      switch (ty_op_of(os)) {
      | None => Failed
      | Some(op) =>
        switch (surround) {
        | EmptyPrefix(suffix) =>
          /* zty0| suffix -> uty0 op |_ suffix */
          let prefix' = OperatorSeq.ExpPrefix(uty, op);
          let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
          let zty0' = ZTyp.place_before(Hole);
          Succeeded(make_ty_OpSeqZ(zty0', surround'));
        | EmptySuffix(prefix) =>
          /* prefix zty0| -> prefix uty0 op |_ */
          let prefix' = OperatorSeq.prefix_append_exp(prefix, uty, op);
          let surround' = OperatorSeq.EmptySuffix(prefix');
          let zty0' = ZTyp.place_before(Hole);
          Succeeded(make_ty_OpSeqZ(zty0', surround'));
        | BothNonEmpty(prefix, suffix) =>
          /* prefix zty0| suffix -> prefix uty0 op |_ suffix */
          let prefix' = OperatorSeq.prefix_append_exp(prefix, uty, op);
          let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
          let zty0' = ZTyp.place_before(Hole);
          Succeeded(make_ty_OpSeqZ(zty0', surround'));
        }
      };
    };
  /* Zipper Cases */
  | (_, ParenthesizedZ(zty1)) =>
    switch (perform_ty(a, zty1)) {
    | Failed => Failed
    | CantShift => CantShift
    | CursorEscaped(Before) => perform_ty(MoveLeft, zty)
    | CursorEscaped(After) => perform_ty(MoveRight, zty)
    | Succeeded(zty1') => Succeeded(ParenthesizedZ(zty1'))
    }
  | (_, ListZ(zty1)) =>
    switch (perform_ty(a, zty1)) {
    | Failed => Failed
    | CantShift => CantShift
    | CursorEscaped(Before) => perform_ty(MoveLeft, zty)
    | CursorEscaped(After) => perform_ty(MoveRight, zty)
    | Succeeded(zty1) => Succeeded(ListZ(zty1))
    }
  | (_, OpSeqZ(skel, zty0, surround)) =>
    switch (perform_ty(a, zty0)) {
    | Failed => Failed
    | CantShift => CantShift
    | CursorEscaped(Before) => perform_ty(MoveLeft, zty)
    | CursorEscaped(After) => perform_ty(MoveRight, zty)
    | Succeeded(zty0) =>
      let (zty0, surround) = OpSeqUtil.Typ.resurround(zty0, surround);
      Succeeded(OpSeqZ(skel, zty0, surround));
    }
  /* Invalid actions at the type level */
  | (UpdateApPalette(_), _)
  | (Construct(SAsc), _)
  | (Construct(SLet), _)
  | (Construct(SLine), _)
  | (Construct(SVar(_, _)), _)
  | (Construct(SLam), _)
  | (Construct(SNumLit(_, _)), _)
  | (Construct(SListNil), _)
  | (Construct(SInj(_)), _)
  | (Construct(SCase), _)
  | (Construct(SApPalette(_)), _)
  | (Construct(SWild), _) => Failed
  };

let abs_perform_Backspace_Before_op =
    (
      combine_for_Backspace_Space: ('e, 'z) => 'z,
      z_typecheck_fix_holes: (Contexts.t, MetaVarGen.t, 'z) => 'm,
      make_and_typecheck_OpSeqZ:
        (Contexts.t, MetaVarGen.t, 'z, OperatorSeq.opseq_surround('e, 'op)) =>
        'm,
      is_EmptyHole: 'e => bool,
      is_Space: 'op => bool,
      _Space: 'op,
      place_after: 'e => 'z,
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      e0: 'e,
      ze0: 'z,
      surround: OperatorSeq.opseq_surround('e, 'op),
    )
    : option('m) =>
  switch (surround) {
  | EmptyPrefix(_) => None
  | EmptySuffix(prefix) =>
    switch (prefix) {
    | ExpPrefix(e1, op1) =>
      /* e1 op1 |ze0 */
      if (is_Space(op1)) {
        /* e1 |ze0 */
        let ze0' = combine_for_Backspace_Space(e1, ze0);
        Some(z_typecheck_fix_holes(ctx, u_gen, ze0'));
      } else {
        switch (is_EmptyHole(e1), is_EmptyHole(e0)) {
        | (true, true) =>
          /* _1 op1 |_0 --> _1| */
          let ze0' = place_after(e1);
          Some(z_typecheck_fix_holes(ctx, u_gen, ze0'));
        | (true, _) =>
          /* _1 op1 |e0 --> |e0 */
          Some(z_typecheck_fix_holes(ctx, u_gen, ze0))
        | (false, true) =>
          /* e1 op1 |_0 --> e1| */
          let ze0' = place_after(e1);
          Some(z_typecheck_fix_holes(ctx, u_gen, ze0'));
        | (false, false) =>
          /* e1 op1 |ze0 --> e1 |ze0 */
          let surround' = OperatorSeq.EmptySuffix(ExpPrefix(e1, _Space));
          Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround'));
        };
      }
    | SeqPrefix(seq1, op1) =>
      /* seq1 op1 |ze0 */
      is_Space(op1)
        /* seq1 |ze0 */
        ? {
          let (e1, prefix') = OperatorSeq.split_tail(seq1);
          let surround' = OperatorSeq.EmptySuffix(prefix');
          let ze0' = combine_for_Backspace_Space(e1, ze0);
          Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze0', surround'));
        }
        : {
          let (e1, prefix') = OperatorSeq.split_tail(seq1);
          if (is_EmptyHole(e0)) {
            /* prefix' e1 op1 |_0 --> prefix' e1| */
            let surround' = OperatorSeq.EmptySuffix(prefix');
            let ze0' = place_after(e1);
            Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze0', surround'));
          } else if (is_EmptyHole(e1)) {
            /* prefix' _1 op1 |e0 --> prefix' |e0 */
            let surround' = OperatorSeq.EmptySuffix(prefix');
            Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround'));
          } else {
            /* seq1 op1 |ze0 --> seq1 |ze0 */
            let prefix' = OperatorSeq.SeqPrefix(seq1, _Space);
            let surround' = OperatorSeq.EmptySuffix(prefix');
            Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround'));
          };
        }
    }
  | BothNonEmpty(prefix, suffix) =>
    switch (prefix) {
    | ExpPrefix(e1, op1) =>
      /* e1 op1 |ze0 ...suffix */
      is_Space(op1)
        /* e1 |ze0 ...suffix */
        ? {
          let ze0' = combine_for_Backspace_Space(e1, ze0);
          let surround' = OperatorSeq.EmptyPrefix(suffix);
          Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze0', surround'));
        }
        : (
          if (is_EmptyHole(e0)) {
            /* e1 op1 |_0 suffix --> e1| suffix */
            let surround' = OperatorSeq.EmptyPrefix(suffix);
            let ze0' = place_after(e1);
            Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze0', surround'));
          } else if (is_EmptyHole(e1)) {
            /* _1 op1 |e0 suffix --> |e0 suffix */
            let surround' = OperatorSeq.EmptyPrefix(suffix);
            Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround'));
          } else {
            /* e1 op1 |ze0 --> e1 |ze0 ...suffix */
            let surround' =
              OperatorSeq.BothNonEmpty(ExpPrefix(e1, _Space), suffix);
            Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround'));
          }
        )
    | SeqPrefix(seq1, op1) =>
      /* seq1 op1 |ze0 ...suffix */
      is_Space(op1)
        /* seq1 |ze0 ...suffix */
        ? {
          let (e1, prefix') = OperatorSeq.split_tail(seq1);
          let ze0' = combine_for_Backspace_Space(e1, ze0);
          let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
          Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze0', surround'));
        }
        : {
          let (e1, prefix') = OperatorSeq.split_tail(seq1);
          if (is_EmptyHole(e0)) {
            /* prefix' e1 op1 |_0 suffix --> prefix' e1| suffix */
            let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
            let ze0' = place_after(e1);
            Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze0', surround'));
          } else if (is_EmptyHole(e1)) {
            /* prefix' _1 op1 |e0 suffix --> prefix' |e0 suffix */
            let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
            Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround'));
          } else {
            /* seq1 op1 |ze0 suffix --> seq1 |ze0 suffix */
            let prefix' = OperatorSeq.SeqPrefix(seq1, _Space);
            let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
            Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround'));
          };
        }
    }
  };

let abs_perform_Delete_After_op =
    (
      combine_for_Delete_Space: ('z, 'e) => 'z,
      z_typecheck_fix_holes: (Contexts.t, MetaVarGen.t, 'z) => 'm,
      make_and_typecheck_OpSeqZ:
        (Contexts.t, MetaVarGen.t, 'z, OperatorSeq.opseq_surround('e, 'op)) =>
        'm,
      is_EmptyHole: 'e => bool,
      is_Space: 'op => bool,
      _Space: 'op,
      place_before: 'e => 'z,
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      e0: 'e,
      ze0: 'z,
      surround: OperatorSeq.opseq_surround('e, 'op),
    )
    : option('m) =>
  switch (surround) {
  | EmptySuffix(_) => None /* precluded by pattern begin match above */
  | EmptyPrefix(suffix) =>
    switch (suffix) {
    | ExpSuffix(op, e1) =>
      is_Space(op)
        ? {
          let ze0' = combine_for_Delete_Space(ze0, e1);
          Some(z_typecheck_fix_holes(ctx, u_gen, ze0'));
        }
        : (
          switch (is_EmptyHole(e0), is_EmptyHole(e1)) {
          | (true, true) =>
            /* _0| op _1 --> _0| */
            Some(z_typecheck_fix_holes(ctx, u_gen, ze0))
          | (true, false) =>
            /* _0| op e1 --> |e1 */
            let ze1 = place_before(e1);
            Some(z_typecheck_fix_holes(ctx, u_gen, ze1));
          | (false, true) =>
            /* e0| op _ --> e0| */
            Some(z_typecheck_fix_holes(ctx, u_gen, ze0))
          | (false, false) =>
            /* e0| op e1 --> e0| e1 */
            let surround' = OperatorSeq.EmptyPrefix(ExpSuffix(_Space, e1));
            Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround'));
          }
        )
    | SeqSuffix(op, seq) =>
      is_Space(op)
        ? {
          let (e, suffix') = OperatorSeq.split0(seq);
          let surround' = OperatorSeq.EmptyPrefix(suffix');
          let ze0' = combine_for_Delete_Space(ze0, e);
          Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze0', surround'));
        }
        : {
          let (e1, suffix') = OperatorSeq.split0(seq);
          if (is_EmptyHole(e1)) {
            /* e0| op _ suffix' --> e0| suffix' */
            let surround' = OperatorSeq.EmptyPrefix(suffix');
            Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround'));
          } else if (is_EmptyHole(e0)) {
            /* _0| op e1 suffix' --> |e1 suffix' */
            let surround' = OperatorSeq.EmptyPrefix(suffix');
            let ze1 = place_before(e1);
            Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze1, surround'));
          } else {
            /* e0| op seq --> e0| seq */
            let suffix' = OperatorSeq.SeqSuffix(_Space, seq);
            let surround' = OperatorSeq.EmptyPrefix(suffix');
            Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround'));
          };
        }
    }
  | BothNonEmpty(prefix, suffix) =>
    switch (suffix) {
    | ExpSuffix(op, e1) =>
      is_Space(op)
        ? {
          let ze0' = combine_for_Delete_Space(ze0, e1);
          let surround' = OperatorSeq.EmptySuffix(prefix);
          Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze0', surround'));
        }
        : (
          if (is_EmptyHole(e1)) {
            /* prefix e0| op _ --> prefix e0| */
            let surround' = OperatorSeq.EmptySuffix(prefix);
            Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround'));
          } else if (is_EmptyHole(e0)) {
            /* prefix _0| op e1 --> prefix |e1 */
            let surround' = OperatorSeq.EmptySuffix(prefix);
            let ze1 = place_before(e1);
            Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze1, surround'));
          } else {
            /* prefix e0| op e1 --> e0| e1 */
            let surround' =
              OperatorSeq.BothNonEmpty(
                prefix,
                OperatorSeq.ExpSuffix(_Space, e1),
              );
            Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround'));
          }
        )
    | SeqSuffix(op, seq) =>
      is_Space(op)
        ? {
          let (e, suffix') = OperatorSeq.split0(seq);
          let ze0' = combine_for_Delete_Space(ze0, e);
          let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
          Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze0', surround'));
        }
        : {
          let (e1, suffix') = OperatorSeq.split0(seq);
          if (is_EmptyHole(e1)) {
            /* prefix e0| op _ suffix' --> prefix e0| suffix' */
            let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
            Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround'));
          } else if (is_EmptyHole(e0)) {
            /* prefix _0| op e1 suffix' --> prefix |e1 suffix' */
            let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
            let ze1 = place_before(e1);
            Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze1, surround'));
          } else {
            /* prefix e| op seq --> e| seq */
            let suffix' = OperatorSeq.SeqSuffix(_Space, seq);
            let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
            Some(make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround'));
          };
        }
    }
  };

let abs_perform_Construct_SOp_After =
    (
      bidelimit: 'e => 'e,
      new_EmptyHole: MetaVarGen.t => ('e, MetaVarGen.t),
      make_and_typecheck_OpSeq,
      make_and_typecheck_OpSeqZ:
        (Contexts.t, MetaVarGen.t, 'z, OperatorSeq.opseq_surround('e, 'op)) =>
        'm,
      is_Space: 'op => bool,
      place_before: 'e => 'z,
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      e: 'e,
      op: 'op,
    )
    : 'm => {
  let e' = bidelimit(e);
  let (new_tm, u_gen) = new_EmptyHole(u_gen);
  if (is_Space(op)) {
    let prefix = OperatorSeq.ExpPrefix(e', op);
    let surround = OperatorSeq.EmptySuffix(prefix);
    make_and_typecheck_OpSeqZ(ctx, u_gen, place_before(new_tm), surround);
  } else {
    let new_seq = OperatorSeq.ExpOpExp(e', op, new_tm);
    make_and_typecheck_OpSeq(ctx, u_gen, OnDelim(1, After), new_seq);
  };
};

let abs_perform_Construct_SOp_Before =
    (
      bidelimit: 'e => 'e,
      new_EmptyHole: MetaVarGen.t => ('e, MetaVarGen.t),
      make_and_typecheck_OpSeq:
        (
          Contexts.t,
          MetaVarGen.t,
          cursor_position,
          OperatorSeq.opseq('e, 'op)
        ) =>
        'm,
      make_and_typecheck_OpSeqZ:
        (Contexts.t, MetaVarGen.t, 'z, OperatorSeq.opseq_surround('e, 'op)) =>
        'm,
      is_Space: 'op => bool,
      place_before: 'e => 'z,
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      e: 'e,
      op: 'op,
    )
    : 'm => {
  let e' = bidelimit(e);
  let (new_tm, u_gen) = new_EmptyHole(u_gen);
  if (is_Space(op)) {
    let suffix = OperatorSeq.ExpSuffix(op, e');
    let surround = OperatorSeq.EmptyPrefix(suffix);
    make_and_typecheck_OpSeqZ(ctx, u_gen, place_before(new_tm), surround);
  } else {
    let new_seq = OperatorSeq.ExpOpExp(new_tm, op, e');
    make_and_typecheck_OpSeq(ctx, u_gen, OnDelim(1, After), new_seq);
  };
};

let abs_perform_Construct_SOp_After_surround =
    (
      new_EmptyHole: MetaVarGen.t => ('e, MetaVarGen.t),
      make_and_typecheck_OpSeq:
        (
          Contexts.t,
          MetaVarGen.t,
          cursor_position,
          OperatorSeq.opseq('e, 'op)
        ) =>
        'm,
      make_and_typecheck_OpSeqZ:
        (Contexts.t, MetaVarGen.t, 'z, OperatorSeq.opseq_surround('e, 'op)) =>
        'm,
      is_Space: 'op => bool,
      _Space: 'op,
      place_before: 'e => 'z,
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      e: 'e,
      op: 'op,
      surround: OperatorSeq.opseq_surround('e, 'op),
    )
    : 'm =>
  switch (surround) {
  | EmptySuffix(prefix) =>
    let (new_tm, u_gen) = u_gen |> new_EmptyHole;
    if (is_Space(op)) {
      let prefix' = OperatorSeq.prefix_append_exp(prefix, e, op);
      let surround' = OperatorSeq.EmptySuffix(prefix');
      let ztm = place_before(new_tm);
      make_and_typecheck_OpSeqZ(ctx, u_gen, ztm, surround');
    } else {
      let new_seq =
        OperatorSeq.(
          SeqOpExp(opseq_of_prefix_and_exp(prefix, e), op, new_tm)
        );
      make_and_typecheck_OpSeq(
        ctx,
        u_gen,
        OnDelim(OperatorSeq.seq_length(new_seq) - 1, After),
        new_seq,
      );
    };
  | EmptyPrefix(suffix) =>
    switch (suffix) {
    | OperatorSeq.ExpSuffix(op', e') =>
      is_Space(op)
        /* e| op' e' --> e |_ op' e' */
        ? {
          let prefix' = OperatorSeq.ExpPrefix(e, op);
          let suffix' = OperatorSeq.ExpSuffix(op', e');
          let surround' = OperatorSeq.BothNonEmpty(prefix', suffix');
          let (new_tm, u_gen) = new_EmptyHole(u_gen);
          make_and_typecheck_OpSeqZ(
            ctx,
            u_gen,
            place_before(new_tm),
            surround',
          );
        }
        : is_Space(op')
            /* e| e' --> e op| e' */
            ? {
              let new_seq = OperatorSeq.ExpOpExp(e, op, e');
              make_and_typecheck_OpSeq(
                ctx,
                u_gen,
                OnDelim(1, After),
                new_seq,
              );
            }
            /* e| op' e' --> e op| _ op' e' */
            : {
              let (new_tm, u_gen) = u_gen |> new_EmptyHole;
              let new_seq =
                OperatorSeq.SeqOpExp(ExpOpExp(e, op, new_tm), op', e');
              make_and_typecheck_OpSeq(
                ctx,
                u_gen,
                OnDelim(1, After),
                new_seq,
              );
            }
    | SeqSuffix(op', seq') =>
      is_Space(op)
        /* e| op' seq' --> e |_ op' seq' */
        ? {
          let prefix' = OperatorSeq.ExpPrefix(e, op);
          let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
          let (new_tm, u_gen) = new_EmptyHole(u_gen);
          make_and_typecheck_OpSeqZ(
            ctx,
            u_gen,
            place_before(new_tm),
            surround',
          );
        }
        : is_Space(op')
            /* e| seq' --> e op| seq' */
            ? {
              let new_seq = OperatorSeq.exp_op_seq(e, op, seq');
              make_and_typecheck_OpSeq(
                ctx,
                u_gen,
                OnDelim(1, After),
                new_seq,
              );
            }
            /* e| op' seq' --> e op| _ op' seq' */
            : {
              let (new_tm, u_gen) = u_gen |> new_EmptyHole;
              let new_seq =
                OperatorSeq.exp_op_seq(
                  e,
                  op,
                  OperatorSeq.exp_op_seq(new_tm, op', seq'),
                );
              make_and_typecheck_OpSeq(
                ctx,
                u_gen,
                OnDelim(1, After),
                new_seq,
              );
            }
    }
  | BothNonEmpty(prefix, suffix) =>
    switch (suffix) {
    | ExpSuffix(op', e') =>
      is_Space(op)
        /* prefix e| op' e' --> prefix e |_ op' e' */
        ? {
          let prefix' = OperatorSeq.prefix_append_exp(prefix, e, op);
          let suffix' = OperatorSeq.ExpSuffix(op', e');
          let surround' = OperatorSeq.BothNonEmpty(prefix', suffix');
          let (new_tm, u_gen) = new_EmptyHole(u_gen);
          make_and_typecheck_OpSeqZ(
            ctx,
            u_gen,
            place_before(new_tm),
            surround',
          );
        }
        : is_Space(op')
            /* prefix e| e' --> prefix e op| e' */
            ? {
              let new_seq =
                OperatorSeq.(
                  SeqOpExp(opseq_of_prefix_and_exp(prefix, e), op, e')
                );
              let k = OperatorSeq.prefix_length(prefix) + 1;
              make_and_typecheck_OpSeq(
                ctx,
                u_gen,
                OnDelim(k, After),
                new_seq,
              );
            }
            /* prefix e| op' e' --> prefix e op| _ op' e' */
            : {
              let (new_tm, u_gen) = u_gen |> new_EmptyHole;
              let new_seq =
                OperatorSeq.(
                  SeqOpExp(
                    SeqOpExp(opseq_of_prefix_and_exp(prefix, e), op, new_tm),
                    op',
                    e',
                  )
                );
              let k = OperatorSeq.prefix_length(prefix) + 1;
              make_and_typecheck_OpSeq(
                ctx,
                u_gen,
                OnDelim(k, After),
                new_seq,
              );
            }
    | SeqSuffix(op', seq') =>
      is_Space(op)
        /* prefix e| op' seq' --> prefix e |_ op' seq' */
        ? {
          let prefix' = OperatorSeq.prefix_append_exp(prefix, e, op);
          let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
          let (new_tm, u_gen) = new_EmptyHole(u_gen);
          make_and_typecheck_OpSeqZ(
            ctx,
            u_gen,
            place_before(new_tm),
            surround',
          );
        }
        : is_Space(op')
            /* prefix e| seq' --> prefix e op| seq' */
            ? {
              let new_seq =
                OperatorSeq.(
                  seq_op_seq(opseq_of_prefix_and_exp(prefix, e), op, seq')
                );
              let k = OperatorSeq.prefix_length(prefix) + 1;
              make_and_typecheck_OpSeq(
                ctx,
                u_gen,
                OnDelim(k, After),
                new_seq,
              );
            }
            /* prefix e| op' seq' --> prefix e op| _ op' seq' */
            : {
              let (new_tm, u_gen) = u_gen |> new_EmptyHole;
              let new_seq =
                OperatorSeq.(
                  seq_op_seq(
                    SeqOpExp(opseq_of_prefix_and_exp(prefix, e), op, new_tm),
                    op',
                    seq',
                  )
                );
              let k = OperatorSeq.prefix_length(prefix) + 1;
              make_and_typecheck_OpSeq(
                ctx,
                u_gen,
                OnDelim(k, After),
                new_seq,
              );
            }
    }
  };

let abs_perform_Construct_SOp_Before_surround =
    (
      new_EmptyHole: MetaVarGen.t => ('e, MetaVarGen.t),
      make_and_typecheck_OpSeq:
        (
          Contexts.t,
          MetaVarGen.t,
          cursor_position,
          OperatorSeq.opseq('e, 'op)
        ) =>
        'm,
      make_and_typecheck_OpSeqZ:
        (Contexts.t, MetaVarGen.t, 'z, OperatorSeq.opseq_surround('e, 'op)) =>
        'm,
      is_Space: 'op => bool,
      _Space: 'op,
      place_before: 'e => 'z,
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      e0: 'e,
      op: 'op,
      surround: OperatorSeq.opseq_surround('e, 'op),
    )
    : 'm =>
  switch (surround) {
  | EmptyPrefix(suffix) =>
    /* |e0 ... --> |_ op e0 ... */
    let suffix' = OperatorSeq.suffix_prepend_exp(suffix, op, e0);
    let surround' = OperatorSeq.EmptyPrefix(suffix');
    let (new_tm, u_gen) = new_EmptyHole(u_gen);
    make_and_typecheck_OpSeqZ(ctx, u_gen, place_before(new_tm), surround');
  | EmptySuffix(ExpPrefix(e1, op') as prefix) =>
    is_Space(op')
      ? is_Space(op)
          /* e1 |e0 --> e1 |_ e0 */
          ? {
            let suffix' = OperatorSeq.ExpSuffix(_Space, e0);
            let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
            let (new_tm, u_gen) = new_EmptyHole(u_gen);
            make_and_typecheck_OpSeqZ(
              ctx,
              u_gen,
              place_before(new_tm),
              surround',
            );
          }
          /* e1 |e0 --> e1 op| e0 */
          : {
            let new_seq = OperatorSeq.ExpOpExp(e1, op, e0);
            make_and_typecheck_OpSeq(ctx, u_gen, OnDelim(1, After), new_seq);
          }
      /* e1 op' |e0 --> e1 op' _ op| e0 */
      : {
        let (new_tm, u_gen) = new_EmptyHole(u_gen);
        let new_seq =
          OperatorSeq.(SeqOpExp(ExpOpExp(e1, op', new_tm), op, e0));
        make_and_typecheck_OpSeq(ctx, u_gen, OnDelim(2, After), new_seq);
      }
  | EmptySuffix(SeqPrefix(seq1, op') as prefix) =>
    is_Space(op')
      ? is_Space(op)
          /* seq1 |e0 --> seq1 |_ e0 */
          ? {
            let suffix' = OperatorSeq.ExpSuffix(_Space, e0);
            let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
            let (new_tm, u_gen) = new_EmptyHole(u_gen);
            make_and_typecheck_OpSeqZ(
              ctx,
              u_gen,
              place_before(new_tm),
              surround',
            );
          }
          /* seq1 |e0 --> seq1 op| e0 */
          : {
            let new_seq = OperatorSeq.SeqOpExp(seq1, op, e0);
            let k = OperatorSeq.seq_length(seq1);
            make_and_typecheck_OpSeq(ctx, u_gen, OnDelim(k, After), new_seq);
          }
      /* seq1 op' |e0 --> seq1 op' _ op| e0 */
      : {
        let (new_tm, u_gen) = new_EmptyHole(u_gen);
        let new_seq =
          OperatorSeq.(SeqOpExp(SeqOpExp(seq1, op', new_tm), op, e0));
        let k = OperatorSeq.seq_length(seq1) + 1;
        make_and_typecheck_OpSeq(ctx, u_gen, OnDelim(k, After), new_seq);
      }
  | BothNonEmpty(ExpPrefix(e1, op') as prefix, suffix) =>
    is_Space(op')
      ? is_Space(op)
          /* e1 |e0 suffix --> e1 |_ e0 suffix */
          ? {
            let suffix' = OperatorSeq.suffix_prepend_exp(suffix, _Space, e0);
            let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
            let (new_tm, u_gen) = new_EmptyHole(u_gen);
            make_and_typecheck_OpSeqZ(
              ctx,
              u_gen,
              place_before(new_tm),
              surround',
            );
          }
          /* e1 |e0 suffix --> e1 op| e0 suffix */
          : {
            let new_seq =
              OperatorSeq.(
                opseq_of_seq_and_suffix(ExpOpExp(e1, op, e0), suffix)
              );
            make_and_typecheck_OpSeq(ctx, u_gen, OnDelim(1, After), new_seq);
          }
      /* e1 op' |e0 suffix --> e1 op' _ op| e0 suffix */
      : {
        let (new_tm, u_gen) = new_EmptyHole(u_gen);
        let new_seq =
          OperatorSeq.(
            opseq_of_seq_and_suffix(
              SeqOpExp(ExpOpExp(e1, op', new_tm), op, e0),
              suffix,
            )
          );
        make_and_typecheck_OpSeq(ctx, u_gen, OnDelim(2, After), new_seq);
      }
  | BothNonEmpty(SeqPrefix(seq1, op') as prefix, suffix) =>
    is_Space(op')
      ? is_Space(op)
          /* seq1 |e0 suffix --> seq1 |_ e0 suffix */
          ? {
            let suffix' = OperatorSeq.suffix_prepend_exp(suffix, _Space, e0);
            let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
            let (new_tm, u_gen) = new_EmptyHole(u_gen);
            make_and_typecheck_OpSeqZ(
              ctx,
              u_gen,
              place_before(new_tm),
              surround',
            );
          }
          /* seq1 |e0 suffix --> seq1 op| e0 suffix */
          : {
            let new_seq =
              OperatorSeq.(
                opseq_of_seq_and_suffix(SeqOpExp(seq1, op, e0), suffix)
              );
            let k = OperatorSeq.seq_length(seq1);
            make_and_typecheck_OpSeq(ctx, u_gen, OnDelim(k, After), new_seq);
          }
      /* seq1 op' |e0 suffix --> seq op' _ op| e0 suffix */
      : {
        let (new_tm, u_gen) = new_EmptyHole(u_gen);
        let new_seq =
          OperatorSeq.(
            opseq_of_seq_and_suffix(
              SeqOpExp(SeqOpExp(seq1, op', new_tm), op, e0),
              suffix,
            )
          );
        let k = OperatorSeq.seq_length(seq1) + 1;
        make_and_typecheck_OpSeq(ctx, u_gen, OnDelim(k, After), new_seq);
      }
  };

let make_and_syn_OpSeqZ_pat =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      zp0: ZPat.t,
      surround: ZPat.opseq_surround,
    )
    : (ZPat.t, HTyp.t, Contexts.t, MetaVarGen.t) => {
  /* figure out the current path so that we can follow it again
   * to reconstitute the Z-exp after calling into the UHExp hole
   * insertion logic (otherwise we'd have to do a version of that
   * logic specific to Z-exps) */
  let path0 = Path.of_OpSeqZ_pat(zp0, surround);
  let p0 = ZPat.erase(zp0);
  let seq = OperatorSeq.opseq_of_exp_and_surround(p0, surround);
  let skel = Associator.associate_pat(seq);
  let (skel, seq, ty, ctx, u_gen) =
    Statics.syn_fix_holes_pat_skel(ctx, u_gen, skel, seq);
  let p = UHPat.OpSeq(skel, seq);
  let zp = Path.follow_pat_or_fail(path0, p);
  (zp, ty, ctx, u_gen);
};

let make_and_ana_OpSeqZ_pat =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      zp0: ZPat.t,
      surround: ZPat.opseq_surround,
      ty: HTyp.t,
    )
    : (ZPat.t, Contexts.t, MetaVarGen.t) => {
  /* figure out the current path so that we can follow it again
   * to reconstitute the Z-exp after calling into the UHExp hole
   * insertion logic (otherwise we'd have to do a version of that
   * logic specific to Z-exps) */
  let path0 = Path.of_OpSeqZ_pat(zp0, surround);
  let p0 = ZPat.erase(zp0);
  let seq = OperatorSeq.opseq_of_exp_and_surround(p0, surround);
  let skel = Associator.associate_pat(seq);
  let (skel, seq, ctx, u_gen) =
    Statics.ana_fix_holes_pat_skel(ctx, u_gen, skel, seq, ty);
  let p = UHPat.OpSeq(skel, seq);
  let zp = Path.follow_pat_or_fail(path0, p);
  (zp, ctx, u_gen);
};

let make_and_syn_OpSeq_pat =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      cursor: cursor_position,
      seq: UHPat.opseq,
    ) => {
  let zp = ZPat.CursorP(cursor, OpSeqUtil.Pat.mk_OpSeq(seq));
  Statics.syn_fix_holes_zpat(ctx, u_gen, zp);
};

let make_and_ana_OpSeq_pat =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      cursor: cursor_position,
      seq: UHPat.opseq,
      ty,
    ) => {
  let zp = ZPat.CursorP(cursor, OpSeqUtil.Pat.mk_OpSeq(seq));
  Statics.ana_fix_holes_zpat(ctx, u_gen, zp, ty);
};

let check_valid = (x: Var.t, result: result('a)): result('a) =>
  if (Var.is_valid(x)) {
    result;
  } else {
    Failed;
  };
let rec syn_perform_pat =
        (ctx: Contexts.t, u_gen: MetaVarGen.t, a: t, zp: ZPat.t)
        : result((ZPat.t, HTyp.t, Contexts.t, MetaVarGen.t)) => {
  switch (a, zp) {
  | (
      _,
      CursorP(
        OnText(_),
        EmptyHole(_) | Wild(_) | ListNil(_) | Parenthesized(_) | OpSeq(_, _) |
        Inj(_, _, _),
      ) |
      CursorP(OnDelim(_, _), Var(_, _, _) | NumLit(_, _) | BoolLit(_, _)) |
      CursorP(
        Staging(_),
        EmptyHole(_) | Wild(_) | ListNil(_) | Var(_) | NumLit(_, _) |
        BoolLit(_, _) |
        OpSeq(_, _),
      ),
    ) =>
    Failed
  | (_, CursorP(cursor, p)) when !ZPat.is_valid_cursor(cursor, p) => Failed
  /* Staging */
  | (ShiftUp | ShiftDown, _) =>
    // can only shift up and down in blocks
    Failed
  | (ShiftLeft | ShiftRight, CursorP(OnText(_) | OnDelim(_, _), _)) =>
    Failed
  | (
      ShiftLeft | ShiftRight,
      CursorP(
        Staging(k),
        (Parenthesized(body) | Inj(_, _, body)) as staged,
      ) |
      OpSeqZ(
        _,
        CursorP(
          Staging(k),
          (Parenthesized(body) | Inj(_, _, body)) as staged,
        ),
        _,
      ),
    ) =>
    let shift_optm =
      switch (k, a) {
      | (0, ShiftLeft) => OpSeqUtil.Pat.shift_optm_from_prefix
      | (0, ShiftRight) => OpSeqUtil.Pat.shift_optm_to_prefix
      | (1, ShiftLeft) => OpSeqUtil.Pat.shift_optm_to_suffix
      | (_one, _shift_right) => OpSeqUtil.Pat.shift_optm_from_suffix
      };
    let surround =
      switch (zp) {
      | OpSeqZ(_, _, surround) => Some(surround)
      | _cursor_p => None
      };
    switch (body |> shift_optm(~surround)) {
    | None => CantShift
    | Some((new_body, new_surround)) =>
      let new_ztm =
        ZPat.CursorP(
          Staging(k),
          switch (staged) {
          | Inj(err_status, side, _) => Inj(err_status, side, new_body)
          | _parenthesized => Parenthesized(new_body)
          },
        );
      let new_zp =
        switch (new_surround) {
        | None => new_ztm
        | Some(surround) => OpSeqUtil.Pat.mk_OpSeqZ(new_ztm, surround)
        };
      Succeeded(Statics.syn_fix_holes_zpat(ctx, u_gen, new_zp));
    };
  /* Movement */
  /* NOTE: we don't need to handle movement actions here for the purposes of the UI,
   * since it's handled at the top (expression) level, but for the sake of API completeness
   * we include it */
  | (MoveTo(path), _) =>
    let p = ZPat.erase(zp);
    switch (Statics.syn_pat(ctx, p)) {
    | None => Failed
    | Some((ty, ctx)) =>
      switch (Path.follow_pat(path, p)) {
      | None => Failed
      | Some(zp) => Succeeded((zp, ty, ctx, u_gen))
      }
    };
  | (MoveToBefore(steps), _) =>
    let p = ZPat.erase(zp);
    switch (Statics.syn_pat(ctx, p)) {
    | None => Failed
    | Some((ty, ctx)) =>
      switch (Path.follow_pat_and_place_before(steps, p)) {
      | None => Failed
      | Some(zp) => Succeeded((zp, ty, ctx, u_gen))
      }
    };
  | (MoveToPrevHole, _) =>
    switch (Path.prev_hole_path(Path.holes_zpat(zp, []))) {
    | None => Failed
    | Some(path) => syn_perform_pat(ctx, u_gen, MoveTo(path), zp)
    }
  | (MoveToNextHole, _) =>
    switch (Path.next_hole_path(Path.holes_zpat(zp, []))) {
    | None => Failed
    | Some(path) => syn_perform_pat(ctx, u_gen, MoveTo(path), zp)
    }
  | (MoveLeft, _) =>
    let p = ZPat.erase(zp);
    switch (Statics.syn_pat(ctx, p), ZPat.move_cursor_left(zp)) {
    | (None, _) => Failed
    | (_, None) => CursorEscaped(Before)
    | (Some((ty, ctx)), Some(zp)) => Succeeded((zp, ty, ctx, u_gen))
    };
  | (MoveRight, _) =>
    let p = ZPat.erase(zp);
    switch (Statics.syn_pat(ctx, p), ZPat.move_cursor_right(zp)) {
    | (None, _) => Failed
    | (_, None) => CursorEscaped(After)
    | (Some((ty, ctx)), Some(zp)) => Succeeded((zp, ty, ctx, u_gen))
    };
  /* Backspace and Delete */
  | (Backspace, _) when ZPat.is_before(zp) => CursorEscaped(Before)
  | (Delete, _) when ZPat.is_after(zp) => CursorEscaped(After)
  | (Backspace, CursorP(_, EmptyHole(_) as p)) =>
    ZPat.is_after(zp)
      ? Succeeded((ZPat.place_before(p), Hole, ctx, u_gen))
      : CursorEscaped(Before)
  | (Delete, CursorP(_, EmptyHole(_) as p)) =>
    ZPat.is_before(zp)
      ? Succeeded((ZPat.place_after(p), Hole, ctx, u_gen))
      : CursorEscaped(After)
  | (
      Backspace | Delete,
      CursorP(
        OnText(_) | OnDelim(_, _),
        Var(_, _, _) | Wild(_) | NumLit(_, _) | BoolLit(_, _) | ListNil(_),
      ),
    ) =>
    let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
    Succeeded((zp, Hole, ctx, u_gen));
  /* ( _ <|)   ==>   ( _| ) */
  /* ... + [k-1] <|+ [k] + ...   ==>   ... + [k-1]| + [k] + ... */
  | (
      Backspace,
      CursorP(
        OnDelim(_, Before),
        Parenthesized(_) | Inj(_, _, _) | OpSeq(_, _),
      ),
    ) =>
    syn_perform_pat(ctx, u_gen, MoveLeft, zp)
  /* (|> _ )   ==>   ( |_ ) */
  /* ... + [k-1] +|> [k] + ...   ==>   ... + [k-1] + |[k] + ... */
  | (
      Delete,
      CursorP(
        OnDelim(_, After),
        Parenthesized(_) | Inj(_, _, _) | OpSeq(_, _),
      ),
    ) =>
    syn_perform_pat(ctx, u_gen, MoveRight, zp)
  /* Delete before delimiter == Backspace after delimiter */
  | (
      Delete,
      CursorP(
        OnDelim(k, Before),
        (Parenthesized(_) | Inj(_, _, _) | OpSeq(_, _)) as p,
      ),
    ) =>
    syn_perform_pat(ctx, u_gen, Backspace, CursorP(OnDelim(k, After), p))
  /* ( _ )<|  ==>  ( _ [)] */
  | (
      Backspace,
      CursorP(OnDelim(k, After), (Parenthesized(_) | Inj(_, _, _)) as p),
    ) =>
    Succeeded(
      Statics.syn_fix_holes_zpat(ctx, u_gen, CursorP(Staging(k), p)),
    )
  | (
      Backspace | Delete,
      CursorP(Staging(k), Parenthesized(body) | Inj(_, _, body)),
    ) =>
    let result =
      body
      |> (
        switch (k) {
        | 0 => ZPat.place_before
        | _one => ZPat.place_after
        }
      );
    Succeeded(Statics.syn_fix_holes_zpat(ctx, u_gen, result));
  /* ... + [k-1] +<| [k] + ... */
  | (Backspace, CursorP(OnDelim(k, After), OpSeq(_, seq))) =>
    /* validity check at top of switch statement ensures
     * that op between [k-1] and [k] is not Space */
    switch (OperatorSeq.split(k - 1, seq), OperatorSeq.split(k, seq)) {
    /* invalid cursor position */
    | (None, _)
    | (_, None) => Failed
    /* ... + [k-1] +<| _ + ... */
    | (_, Some((EmptyHole(_), surround))) =>
      switch (surround) {
      /* invalid */
      | EmptyPrefix(_) => Failed
      /* ... + [k-1] +<| _   ==>   ... + [k-1]| */
      | EmptySuffix(prefix) =>
        let zp: ZPat.t =
          switch (prefix) {
          | ExpPrefix(p, _) => ZPat.place_after(p)
          | SeqPrefix(seq, _) =>
            let skel = Associator.associate_pat(seq);
            ZPat.place_after(OpSeq(skel, seq));
          };
        Succeeded(Statics.syn_fix_holes_zpat(ctx, u_gen, zp));
      /* ... + [k-1] +<| _ + ...   ==>   ... + [k-1]| + ... */
      | BothNonEmpty(prefix, suffix) =>
        let (zp0: ZPat.t, surround: ZPat.opseq_surround) =
          switch (prefix) {
          | ExpPrefix(p, _) => (ZPat.place_after(p), EmptyPrefix(suffix))
          | SeqPrefix(ExpOpExp(p1, op, p2), _) => (
              ZPat.place_after(p2),
              BothNonEmpty(ExpPrefix(p1, op), suffix),
            )
          | SeqPrefix(SeqOpExp(seq, op, p), _) => (
              ZPat.place_after(p),
              BothNonEmpty(SeqPrefix(seq, op), suffix),
            )
          };
        let skel =
          Associator.associate_pat(
            OperatorSeq.opseq_of_exp_and_surround(ZPat.erase(zp0), surround),
          );
        let zp = ZPat.OpSeqZ(skel, zp0, surround);
        Succeeded(Statics.syn_fix_holes_zpat(ctx, u_gen, zp));
      }
    /* ... + _ +<| [k] + ... */
    | (Some((EmptyHole(_), surround)), _) =>
      switch (surround) {
      /* invalid */
      | EmptySuffix(_) => Failed
      /* _ +<| [k] + ...   ==>   |[k] + ... */
      | EmptyPrefix(suffix) =>
        let zp: ZPat.t =
          switch (suffix) {
          | ExpSuffix(_, p) => ZPat.place_before(p)
          | SeqSuffix(_, seq) =>
            let skel = Associator.associate_pat(seq);
            ZPat.place_before(OpSeq(skel, seq));
          };
        Succeeded(Statics.syn_fix_holes_zpat(ctx, u_gen, zp));
      /* ... + [k-2] + _ +<| [k] + ...   ==>   ... + [k-2] +| [k] + ... */
      | BothNonEmpty(prefix, suffix) =>
        let seq =
          switch (suffix) {
          | ExpSuffix(_, p) => OperatorSeq.opseq_of_prefix_and_exp(prefix, p)
          | SeqSuffix(_, seq) =>
            OperatorSeq.opseq_of_prefix_and_seq(prefix, seq)
          };
        let skel = Associator.associate_pat(seq);
        let zp = ZPat.CursorP(OnDelim(k - 1, After), OpSeq(skel, seq));
        Succeeded(Statics.syn_fix_holes_zpat(ctx, u_gen, zp));
      }
    /* ... + [k-1] +<| [k] + ...   ==>   ... + [k-1]| [k] + ... */
    | (Some((p0, surround)), _) =>
      switch (OperatorSeq.replace_following_op(surround, UHPat.Space)) {
      | None => Failed /* invalid */
      | Some(surround) =>
        Succeeded(
          make_and_syn_OpSeqZ_pat(
            ctx,
            u_gen,
            ZPat.place_after(p0),
            surround,
          ),
        )
      }
    }
  /* ... + [k-1]  <|_ + [k+1] + ...  ==>   ... + [k-1]| + [k+1] + ... */
  | (
      Backspace,
      OpSeqZ(
        _,
        CursorP(_, EmptyHole(_)) as zp0,
        (
          EmptySuffix(ExpPrefix(_, Space) | SeqPrefix(_, Space)) |
          BothNonEmpty(ExpPrefix(_, Space) | SeqPrefix(_, Space), _)
        ) as surround,
      ),
    )
      when ZPat.is_before(zp0) =>
    switch (surround) {
    | EmptyPrefix(_) => CursorEscaped(Before) /* should never happen */
    | EmptySuffix(prefix) =>
      let p: UHPat.t =
        switch (prefix) {
        | ExpPrefix(p1, _space) => p1
        | SeqPrefix(seq, _space) =>
          let skel = Associator.associate_pat(seq);
          OpSeq(skel, seq);
        };
      Succeeded(
        Statics.syn_fix_holes_zpat(ctx, u_gen, ZPat.place_after(p)),
      );
    | BothNonEmpty(prefix, suffix) =>
      switch (prefix) {
      | ExpPrefix(p1, _space) =>
        let zp1 = ZPat.place_after(p1);
        let surround = OperatorSeq.EmptyPrefix(suffix);
        let skel =
          Associator.associate_pat(
            OperatorSeq.opseq_of_exp_and_surround(p1, surround),
          );
        let zp = ZPat.OpSeqZ(skel, zp1, surround);
        Succeeded(Statics.syn_fix_holes_zpat(ctx, u_gen, zp));
      | SeqPrefix(seq, _space) =>
        let (prefix: ZPat.opseq_prefix, p0) =
          switch (seq) {
          | ExpOpExp(p1, op, p2) => (ExpPrefix(p1, op), p2)
          | SeqOpExp(seq, op, p1) => (SeqPrefix(seq, op), p1)
          };
        let zp0 = ZPat.place_after(p0);
        let surround = OperatorSeq.BothNonEmpty(prefix, suffix);
        let skel =
          Associator.associate_pat(
            OperatorSeq.opseq_of_exp_and_surround(p0, surround),
          );
        let zp = ZPat.OpSeqZ(skel, zp0, surround);
        Succeeded(Statics.syn_fix_holes_zpat(ctx, u_gen, zp));
      }
    }
  /* ... + [k-1] + _|>  [k+1] + ...  ==>   ... + [k-1] + |[k+1] + ... */
  | (
      Delete,
      OpSeqZ(
        _,
        CursorP(_, EmptyHole(_)) as zp0,
        (
          EmptyPrefix(ExpSuffix(Space, _) | SeqSuffix(Space, _)) |
          BothNonEmpty(_, ExpSuffix(Space, _) | SeqSuffix(Space, _))
        ) as surround,
      ),
    )
      when ZPat.is_after(zp0) =>
    switch (surround) {
    | EmptySuffix(_) => CursorEscaped(After) /* should never happen */
    | EmptyPrefix(suffix) =>
      let p: UHPat.t =
        switch (suffix) {
        | ExpSuffix(_space, p1) => p1
        | SeqSuffix(_space, seq) =>
          let skel = Associator.associate_pat(seq);
          OpSeq(skel, seq);
        };
      Succeeded(
        Statics.syn_fix_holes_zpat(ctx, u_gen, ZPat.place_before(p)),
      );
    | BothNonEmpty(prefix, suffix) =>
      switch (suffix) {
      | ExpSuffix(_space, p1) =>
        let zp1 = ZPat.place_before(p1);
        let surround = OperatorSeq.EmptyPrefix(suffix);
        let skel =
          Associator.associate_pat(
            OperatorSeq.opseq_of_exp_and_surround(p1, surround),
          );
        let zp = ZPat.OpSeqZ(skel, zp1, surround);
        Succeeded(Statics.syn_fix_holes_zpat(ctx, u_gen, zp));
      | SeqSuffix(_space, seq) =>
        let (p0, suffix: ZPat.opseq_suffix) =
          switch (seq) {
          | ExpOpExp(p1, op, p2) => (p1, ExpSuffix(op, p2))
          | SeqOpExp(seq, op, p1) => (p1, SeqSuffix(op, seq))
          };
        let zp0 = ZPat.place_before(p0);
        let surround = OperatorSeq.BothNonEmpty(prefix, suffix);
        let skel =
          Associator.associate_pat(
            OperatorSeq.opseq_of_exp_and_surround(p0, surround),
          );
        let zp = ZPat.OpSeqZ(skel, zp0, surround);
        Succeeded(Statics.syn_fix_holes_zpat(ctx, u_gen, zp));
      }
    }
  /* Construct */
  | (Construct(SOp(SSpace)), CursorP(OnDelim(_, After), _))
      when !ZPat.is_after(zp) =>
    syn_perform_pat(ctx, u_gen, MoveRight, zp)
  | (Construct(_) as a, CursorP(OnDelim(_, side), _))
      when !ZPat.is_before(zp) && !ZPat.is_after(zp) =>
    let move_then_perform = move_action =>
      switch (syn_perform_pat(ctx, u_gen, move_action, zp)) {
      | Failed
      | CantShift
      | CursorEscaped(_) => assert(false)
      | Succeeded((zp, _, _, u_gen)) => syn_perform_pat(ctx, u_gen, a, zp)
      };
    switch (side) {
    | Before => move_then_perform(MoveLeft)
    | After => move_then_perform(MoveRight)
    };
  | (
      Construct(SLine),
      CursorP(Staging(k), (Parenthesized(_) | Inj(_, _, _)) as uty),
    ) =>
    Succeeded(
      Statics.syn_fix_holes_zpat(
        ctx,
        u_gen,
        CursorP(OnDelim(k, k == 0 ? Before : After), uty),
      ),
    )
  | (Construct(_), CursorP(Staging(_), _)) => Failed
  | (Construct(SParenthesized), CursorP(_, _)) =>
    switch (Statics.syn_pat(ctx, ZPat.erase(zp))) {
    | None => Failed
    | Some((ty, ctx)) => Succeeded((ParenthesizedZ(zp), ty, ctx, u_gen))
    }
  | (Construct(SVar(x, cursor)), CursorP(_, EmptyHole(_)))
  | (Construct(SVar(x, cursor)), CursorP(_, Wild(_)))
  | (Construct(SVar(x, cursor)), CursorP(_, Var(_, _, _)))
  | (Construct(SVar(x, cursor)), CursorP(_, NumLit(_, _)))
  | (Construct(SVar(x, cursor)), CursorP(_, BoolLit(_, _))) =>
    if (Var.is_true(x)) {
      Succeeded((
        CursorP(cursor, BoolLit(NotInHole, true)),
        Bool,
        ctx,
        u_gen,
      ));
    } else if (Var.is_false(x)) {
      Succeeded((
        CursorP(cursor, BoolLit(NotInHole, false)),
        Bool,
        ctx,
        u_gen,
      ));
    } else if (Var.is_let(x)) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      Succeeded((
        CursorP(cursor, Var(NotInHole, InVHole(Keyword(Let), u), x)),
        Hole,
        ctx,
        u_gen,
      ));
    } else if (Var.is_case(x)) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      Succeeded((
        CursorP(cursor, Var(NotInHole, InVHole(Keyword(Case), u), x)),
        Hole,
        ctx,
        u_gen,
      ));
    } else {
      check_valid(
        x,
        {
          let ctx = Contexts.extend_gamma(ctx, (x, Hole));
          Succeeded((
            ZPat.CursorP(cursor, Var(NotInHole, NotInVHole, x)),
            HTyp.Hole,
            ctx,
            u_gen,
          ));
        },
      );
    }
  | (Construct(SVar(_, _)), CursorP(_, _)) => Failed
  | (Construct(SWild), CursorP(_, EmptyHole(_)))
  | (Construct(SWild), CursorP(_, Wild(_)))
  | (Construct(SWild), CursorP(_, Var(_, _, _)))
  | (Construct(SWild), CursorP(_, NumLit(_, _)))
  | (Construct(SWild), CursorP(_, BoolLit(_, _))) =>
    Succeeded((ZPat.place_after(Wild(NotInHole)), Hole, ctx, u_gen))
  | (Construct(SWild), CursorP(_, _)) => Failed
  | (Construct(SNumLit(n, cursor)), CursorP(_, EmptyHole(_)))
  | (Construct(SNumLit(n, cursor)), CursorP(_, Wild(_)))
  | (Construct(SNumLit(n, cursor)), CursorP(_, Var(_, _, _)))
  | (Construct(SNumLit(n, cursor)), CursorP(_, NumLit(_, _)))
  | (Construct(SNumLit(n, cursor)), CursorP(_, BoolLit(_, _))) =>
    Succeeded((CursorP(cursor, NumLit(NotInHole, n)), Num, ctx, u_gen))
  | (Construct(SNumLit(_, _)), CursorP(_, _)) => Failed
  | (Construct(SInj(side)), CursorP(_, _)) =>
    switch (Statics.syn_pat(ctx, ZPat.erase(zp))) {
    | None => Failed
    | Some((ty1, ctx)) =>
      let zp = ZPat.InjZ(NotInHole, side, zp);
      let ty =
        switch (side) {
        | L => HTyp.Sum(ty1, Hole)
        | R => HTyp.Sum(Hole, ty1)
        };
      Succeeded((zp, ty, ctx, u_gen));
    }
  | (Construct(SListNil), CursorP(_, EmptyHole(_))) =>
    let zp = ZPat.place_after(ListNil(NotInHole));
    Succeeded((zp, List(Hole), ctx, u_gen));
  | (Construct(SListNil), CursorP(_, _)) => Failed
  | (
      Construct(SOp(SSpace)),
      OpSeqZ(_, CursorP(OnDelim(_, After), _) as zp0, _),
    )
      when !ZPat.is_after(zp0) =>
    syn_perform_pat(ctx, u_gen, MoveRight, zp)
  | (Construct(SOp(os)), OpSeqZ(_, zp0, surround)) when ZPat.is_after(zp0) =>
    switch (pat_op_of(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded(
        abs_perform_Construct_SOp_After_surround(
          UHPat.new_EmptyHole,
          make_and_syn_OpSeq_pat,
          make_and_syn_OpSeqZ_pat,
          UHPat.is_Space,
          UHPat.Space,
          ZPat.place_before,
          ctx,
          u_gen,
          ZPat.erase(zp0),
          op,
          surround,
        ),
      )
    }
  | (Construct(SOp(os)), OpSeqZ(_, zp0, surround)) when ZPat.is_before(zp0) =>
    switch (pat_op_of(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded(
        abs_perform_Construct_SOp_Before_surround(
          UHPat.new_EmptyHole,
          make_and_syn_OpSeq_pat,
          make_and_syn_OpSeqZ_pat,
          UHPat.is_Space,
          UHPat.Space,
          ZPat.place_before,
          ctx,
          u_gen,
          ZPat.erase(zp0),
          op,
          surround,
        ),
      )
    }
  | (Construct(SOp(os)), CursorP(_, _)) =>
    switch (pat_op_of(os)) {
    | None => Failed
    | Some(op) =>
      if (ZPat.is_before(zp)) {
        Succeeded(
          abs_perform_Construct_SOp_Before(
            UHPat.bidelimit,
            UHPat.new_EmptyHole,
            make_and_syn_OpSeq_pat,
            make_and_syn_OpSeqZ_pat,
            UHPat.is_Space,
            ZPat.place_before,
            ctx,
            u_gen,
            ZPat.erase(zp),
            op,
          ),
        );
      } else if (ZPat.is_after(zp)) {
        Succeeded(
          abs_perform_Construct_SOp_After(
            UHPat.bidelimit,
            UHPat.new_EmptyHole,
            make_and_syn_OpSeq_pat,
            make_and_syn_OpSeqZ_pat,
            UHPat.is_Space,
            ZPat.place_before,
            ctx,
            u_gen,
            ZPat.erase(zp),
            op,
          ),
        );
      } else {
        Failed;
      }
    }
  /* Zipper */
  | (_, ParenthesizedZ(zp1)) =>
    switch (syn_perform_pat(ctx, u_gen, a, zp1)) {
    | Failed => Failed
    | CantShift => CantShift
    | CursorEscaped(Before) => syn_perform_pat(ctx, u_gen, MoveLeft, zp)
    | CursorEscaped(After) => syn_perform_pat(ctx, u_gen, MoveRight, zp)
    | Succeeded((zp1, ty, ctx, u_gen)) =>
      Succeeded((ParenthesizedZ(zp1), ty, ctx, u_gen))
    }
  | (_, InjZ(_, side, zp1)) =>
    switch (syn_perform_pat(ctx, u_gen, a, zp1)) {
    | Failed => Failed
    | CantShift => CantShift
    | CursorEscaped(Before) => syn_perform_pat(ctx, u_gen, MoveLeft, zp)
    | CursorEscaped(After) => syn_perform_pat(ctx, u_gen, MoveRight, zp)
    | Succeeded((zp1, ty1, ctx, u_gen)) =>
      let zp = ZPat.InjZ(NotInHole, side, zp1);
      let ty =
        switch (side) {
        | L => HTyp.Sum(ty1, Hole)
        | R => HTyp.Sum(Hole, ty1)
        };
      Succeeded((zp, ty, ctx, u_gen));
    }
  | (_, OpSeqZ(_, zp0, surround)) =>
    let i = OperatorSeq.surround_prefix_length(surround);
    switch (ZPat.erase(zp)) {
    | OpSeq(skel, seq) =>
      switch (Statics.syn_skel_pat(ctx, skel, seq, Some(i))) {
      | Some((_ty, ctx, Some(mode))) =>
        switch (mode) {
        | Statics.AnalyzedAgainst(ty0) =>
          switch (ana_perform_pat(ctx, u_gen, a, zp0, ty0)) {
          | Failed => Failed
          | CantShift => CantShift
          | CursorEscaped(Before) =>
            syn_perform_pat(ctx, u_gen, MoveLeft, zp)
          | CursorEscaped(After) =>
            syn_perform_pat(ctx, u_gen, MoveRight, zp)
          | Succeeded((zp0, ctx, u_gen)) =>
            let zp0 = ZPat.bidelimit(zp0);
            let (zp0, surround) = OpSeqUtil.Pat.resurround(zp0, surround);
            Succeeded(make_and_syn_OpSeqZ_pat(ctx, u_gen, zp0, surround));
          }
        | Statics.Synthesized(_) =>
          switch (syn_perform_pat(ctx, u_gen, a, zp0)) {
          | Failed => Failed
          | CantShift => CantShift
          | CursorEscaped(Before) =>
            syn_perform_pat(ctx, u_gen, MoveLeft, zp)
          | CursorEscaped(After) =>
            syn_perform_pat(ctx, u_gen, MoveRight, zp)
          | Succeeded((zp0, _ty0, ctx, u_gen)) =>
            let zp0 = ZPat.bidelimit(zp0);
            let (zp0, surround) = OpSeqUtil.Pat.resurround(zp0, surround);
            Succeeded(make_and_syn_OpSeqZ_pat(ctx, u_gen, zp0, surround));
          }
        }
      | Some(_) => Failed /* should never happen */
      | None => Failed /* should never happen */
      }
    | _ => Failed /* should never happen */
    };
  | (UpdateApPalette(_), _)
  | (Construct(SApPalette(_)), _)
  | (Construct(SNum), _)
  | (Construct(SBool), _)
  | (Construct(SList), _)
  | (Construct(SAsc), _)
  | (Construct(SLet), _)
  | (Construct(SLine), _)
  | (Construct(SLam), _)
  | (Construct(SCase), _) => Failed
  };
}
and ana_perform_pat =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, a: t, zp: ZPat.t, ty: HTyp.t)
    : result((ZPat.t, Contexts.t, MetaVarGen.t)) =>
  switch (a, zp) {
  | (
      _,
      CursorP(
        OnText(_),
        EmptyHole(_) | Wild(_) | ListNil(_) | Parenthesized(_) | OpSeq(_, _) |
        Inj(_, _, _),
      ) |
      CursorP(OnDelim(_, _), Var(_, _, _) | NumLit(_, _) | BoolLit(_, _)) |
      CursorP(
        Staging(_),
        EmptyHole(_) | Wild(_) | ListNil(_) | Var(_) | NumLit(_, _) |
        BoolLit(_, _) |
        OpSeq(_, _),
      ),
    ) =>
    Failed
  | (_, CursorP(cursor, p)) when !ZPat.is_valid_cursor(cursor, p) => Failed
  /* switch to synthesis if in a hole */
  | (_, _) when ZPat.is_inconsistent(zp) =>
    let err = zp |> ZPat.erase |> UHPat.get_err_status_t;
    let zp_not_in_hole = ZPat.set_err_status_t(NotInHole, zp);
    let p = ZPat.erase(zp_not_in_hole);
    switch (Statics.syn_pat(ctx, p)) {
    | None => Failed
    | Some((_, _)) =>
      switch (syn_perform_pat(ctx, u_gen, a, zp_not_in_hole)) {
      | (Failed | CantShift | CursorEscaped(_)) as err => err
      | Succeeded((zp1, ty', ctx, u_gen)) =>
        if (HTyp.consistent(ty, ty')) {
          Succeeded((zp1, ctx, u_gen));
        } else {
          Succeeded((ZPat.set_err_status_t(err, zp1), ctx, u_gen));
        }
      }
    };
  /* Staging */
  | (ShiftUp | ShiftDown, _) =>
    // can only shift up and down in blocks
    Failed
  | (ShiftLeft | ShiftRight, CursorP(OnText(_) | OnDelim(_, _), _)) =>
    Failed
  | (
      ShiftLeft | ShiftRight,
      CursorP(
        Staging(k),
        (Parenthesized(body) | Inj(_, _, body)) as staged,
      ) |
      OpSeqZ(
        _,
        CursorP(
          Staging(k),
          (Parenthesized(body) | Inj(_, _, body)) as staged,
        ),
        _,
      ),
    ) =>
    let shift_optm =
      switch (k, a) {
      | (0, ShiftLeft) => OpSeqUtil.Pat.shift_optm_from_prefix
      | (0, ShiftRight) => OpSeqUtil.Pat.shift_optm_to_prefix
      | (1, ShiftLeft) => OpSeqUtil.Pat.shift_optm_to_suffix
      | (_one, _shift_right) => OpSeqUtil.Pat.shift_optm_from_suffix
      };
    let surround =
      switch (zp) {
      | OpSeqZ(_, _, surround) => Some(surround)
      | _cursor_p => None
      };
    switch (body |> shift_optm(~surround)) {
    | None => CantShift
    | Some((new_body, new_surround)) =>
      let new_ztm =
        ZPat.CursorP(
          Staging(k),
          switch (staged) {
          | Inj(err_status, side, _) => Inj(err_status, side, new_body)
          | _parenthesized => Parenthesized(new_body)
          },
        );
      let new_zp =
        switch (new_surround) {
        | None => new_ztm
        | Some(surround) => OpSeqUtil.Pat.mk_OpSeqZ(new_ztm, surround)
        };
      Succeeded(Statics.ana_fix_holes_zpat(ctx, u_gen, new_zp, ty));
    };
  /* Movement */
  /* NOTE: we don't need to handle movement actions here for the purposes of the UI,
   * since it's handled at the top (expression) level, but for the sake of API completeness
   * we include it */
  | (MoveTo(path), _) =>
    let p = ZPat.erase(zp);
    switch (Statics.ana_pat(ctx, p, ty), Path.follow_pat(path, p)) {
    | (None, _) => Failed
    | (_, None) => Failed
    | (Some(ctx), Some(zp)) => Succeeded((zp, ctx, u_gen))
    };
  | (MoveToBefore(steps), _) =>
    let p = ZPat.erase(zp);
    switch (
      Statics.ana_pat(ctx, p, ty),
      Path.follow_pat_and_place_before(steps, p),
    ) {
    | (None, _) => Failed
    | (_, None) => Failed
    | (Some(ctx), Some(zp)) => Succeeded((zp, ctx, u_gen))
    };
  | (MoveToPrevHole, _) =>
    switch (Path.prev_hole_path(Path.holes_zpat(zp, []))) {
    | None => Failed
    | Some(path) => ana_perform_pat(ctx, u_gen, MoveTo(path), zp, ty)
    }
  | (MoveToNextHole, _) =>
    switch (Path.next_hole_path(Path.holes_zpat(zp, []))) {
    | None => Failed
    | Some(path) => ana_perform_pat(ctx, u_gen, MoveTo(path), zp, ty)
    }
  | (MoveLeft, _) =>
    let p = ZPat.erase(zp);
    switch (Statics.ana_pat(ctx, p, ty), ZPat.move_cursor_left(zp)) {
    | (None, _) => Failed
    | (_, None) => CursorEscaped(Before)
    | (Some(ctx), Some(zp)) => Succeeded((zp, ctx, u_gen))
    };
  | (MoveRight, _) =>
    let p = ZPat.erase(zp);
    switch (Statics.ana_pat(ctx, p, ty), ZPat.move_cursor_right(zp)) {
    | (None, _) => Failed
    | (_, None) => CursorEscaped(After)
    | (Some(ctx), Some(zp)) => Succeeded((zp, ctx, u_gen))
    };
  /* Backspace and Delete */
  | (Backspace, _) when ZPat.is_before(zp) => CursorEscaped(Before)
  | (Delete, _) when ZPat.is_after(zp) => CursorEscaped(After)
  | (Backspace, CursorP(_, EmptyHole(_) as p)) =>
    ZPat.is_after(zp)
      ? Succeeded((ZPat.place_before(p), ctx, u_gen))
      : CursorEscaped(Before)
  | (Delete, CursorP(_, EmptyHole(_) as p)) =>
    ZPat.is_before(zp)
      ? Succeeded((ZPat.place_after(p), ctx, u_gen)) : CursorEscaped(After)
  | (
      Backspace | Delete,
      CursorP(
        OnText(_) | OnDelim(_, _),
        Var(_, _, _) | Wild(_) | NumLit(_, _) | BoolLit(_, _) | ListNil(_),
      ),
    ) =>
    let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
    Succeeded((zp, ctx, u_gen));
  /* ( _ <|)   ==>   ( _| ) */
  /* ... + [k-1] <|+ [k] + ...   ==>   ... + [k-1]| + [k] + ... */
  | (
      Backspace,
      CursorP(
        OnDelim(_, Before),
        Parenthesized(_) | Inj(_, _, _) | OpSeq(_, _),
      ),
    ) =>
    ana_perform_pat(ctx, u_gen, MoveLeft, zp, ty)
  /* (|> _ )   ==>   ( |_ ) */
  /* ... + [k-1] +|> [k] + ...   ==>   ... + [k-1] + |[k] + ... */
  | (
      Delete,
      CursorP(
        OnDelim(_, After),
        Parenthesized(_) | Inj(_, _, _) | OpSeq(_, _),
      ),
    ) =>
    ana_perform_pat(ctx, u_gen, MoveRight, zp, ty)
  /* Delete before delimiter == Backspace after delimiter */
  | (
      Delete,
      CursorP(
        OnDelim(k, Before),
        (Parenthesized(_) | Inj(_, _, _) | OpSeq(_, _)) as p,
      ),
    ) =>
    ana_perform_pat(
      ctx,
      u_gen,
      Backspace,
      CursorP(OnDelim(k, Before), p),
      ty,
    )
  /* ( _ )<|  ==>  ( _ [)] */
  | (
      Backspace,
      CursorP(OnDelim(k, After), (Parenthesized(_) | Inj(_, _, _)) as p),
    ) =>
    Succeeded(
      Statics.ana_fix_holes_zpat(ctx, u_gen, CursorP(Staging(k), p), ty),
    )
  | (
      Backspace | Delete,
      CursorP(Staging(k), Parenthesized(body) | Inj(_, _, body)),
    ) =>
    let result =
      body
      |> (
        switch (k) {
        | 0 => ZPat.place_before
        | _one => ZPat.place_after
        }
      );
    Succeeded(Statics.ana_fix_holes_zpat(ctx, u_gen, result, ty));
  /* ... + [k-1] +<| [k] + ... */
  | (Backspace, CursorP(OnDelim(k, After), OpSeq(_, seq))) =>
    /* validity check at top of switch statement ensures
     * that op between [k-1] and [k] is not Space */
    switch (OperatorSeq.split(k - 1, seq), OperatorSeq.split(k, seq)) {
    /* invalid cursor position */
    | (None, _)
    | (_, None) => Failed
    /* ... + [k-1] +<| _ + ... */
    | (_, Some((EmptyHole(_), surround))) =>
      switch (surround) {
      /* invalid */
      | EmptyPrefix(_) => Failed
      /* ... + [k-1] +<| _   ==>   ... + [k-1]| */
      | EmptySuffix(prefix) =>
        let zp: ZPat.t =
          switch (prefix) {
          | ExpPrefix(p, _) => ZPat.place_after(p)
          | SeqPrefix(seq, _) =>
            let skel = Associator.associate_pat(seq);
            ZPat.place_after(OpSeq(skel, seq));
          };
        Succeeded(Statics.ana_fix_holes_zpat(ctx, u_gen, zp, ty));
      /* ... + [k-1] +<| _ + ...   ==>   ... + [k-1]| + ... */
      | BothNonEmpty(prefix, suffix) =>
        let (zp0: ZPat.t, surround: ZPat.opseq_surround) =
          switch (prefix) {
          | ExpPrefix(p, _) => (ZPat.place_after(p), EmptyPrefix(suffix))
          | SeqPrefix(ExpOpExp(p1, op, p2), _) => (
              ZPat.place_after(p2),
              BothNonEmpty(ExpPrefix(p1, op), suffix),
            )
          | SeqPrefix(SeqOpExp(seq, op, p), _) => (
              ZPat.place_after(p),
              BothNonEmpty(SeqPrefix(seq, op), suffix),
            )
          };
        let skel =
          Associator.associate_pat(
            OperatorSeq.opseq_of_exp_and_surround(ZPat.erase(zp0), surround),
          );
        let zp = ZPat.OpSeqZ(skel, zp0, surround);
        Succeeded(Statics.ana_fix_holes_zpat(ctx, u_gen, zp, ty));
      }
    /* ... + _ +<| [k] + ... */
    | (Some((EmptyHole(_), surround)), _) =>
      switch (surround) {
      /* invalid */
      | EmptySuffix(_) => Failed
      /* _ +<| [k] + ...   ==>   |[k] + ... */
      | EmptyPrefix(suffix) =>
        let zp: ZPat.t =
          switch (suffix) {
          | ExpSuffix(_, p) => ZPat.place_before(p)
          | SeqSuffix(_, seq) =>
            let skel = Associator.associate_pat(seq);
            ZPat.place_before(OpSeq(skel, seq));
          };
        Succeeded(Statics.ana_fix_holes_zpat(ctx, u_gen, zp, ty));
      /* ... + [k-2] + _ +<| [k] + ...   ==>   ... + [k-2] +| [k] + ... */
      | BothNonEmpty(prefix, suffix) =>
        let seq =
          switch (suffix) {
          | ExpSuffix(_, p) => OperatorSeq.opseq_of_prefix_and_exp(prefix, p)
          | SeqSuffix(_, seq) =>
            OperatorSeq.opseq_of_prefix_and_seq(prefix, seq)
          };
        let skel = Associator.associate_pat(seq);
        let zp = ZPat.CursorP(OnDelim(k - 1, After), OpSeq(skel, seq));
        Succeeded(Statics.ana_fix_holes_zpat(ctx, u_gen, zp, ty));
      }
    /* ... + [k-1] +<| [k] + ...   ==>   ... + [k-1]| [k] + ... */
    | (Some((p0, surround)), _) =>
      switch (OperatorSeq.replace_following_op(surround, UHPat.Space)) {
      | None => Failed /* invalid */
      | Some(surround) =>
        Succeeded(
          make_and_ana_OpSeqZ_pat(
            ctx,
            u_gen,
            ZPat.place_after(p0),
            surround,
            ty,
          ),
        )
      }
    }
  /* ... + [k-1]  <|_ + [k+1] + ...  ==>   ... + [k-1]| + [k+1] + ... */
  | (
      Backspace,
      OpSeqZ(
        _,
        CursorP(_, EmptyHole(_)) as zp0,
        (
          EmptySuffix(ExpPrefix(_, Space) | SeqPrefix(_, Space)) |
          BothNonEmpty(ExpPrefix(_, Space) | SeqPrefix(_, Space), _)
        ) as surround,
      ),
    )
      when ZPat.is_before(zp0) =>
    switch (surround) {
    | EmptyPrefix(_) => CursorEscaped(Before) /* should never happen */
    | EmptySuffix(prefix) =>
      let p: UHPat.t =
        switch (prefix) {
        | ExpPrefix(p1, _space) => p1
        | SeqPrefix(seq, _space) =>
          let skel = Associator.associate_pat(seq);
          OpSeq(skel, seq);
        };
      Succeeded(
        Statics.ana_fix_holes_zpat(ctx, u_gen, ZPat.place_after(p), ty),
      );
    | BothNonEmpty(prefix, suffix) =>
      switch (prefix) {
      | ExpPrefix(p1, _space) =>
        let zp1 = ZPat.place_after(p1);
        let surround = OperatorSeq.EmptyPrefix(suffix);
        let skel =
          Associator.associate_pat(
            OperatorSeq.opseq_of_exp_and_surround(p1, surround),
          );
        let zp = ZPat.OpSeqZ(skel, zp1, surround);
        Succeeded(Statics.ana_fix_holes_zpat(ctx, u_gen, zp, ty));
      | SeqPrefix(seq, _space) =>
        let (prefix: ZPat.opseq_prefix, p0) =
          switch (seq) {
          | ExpOpExp(p1, op, p2) => (ExpPrefix(p1, op), p2)
          | SeqOpExp(seq, op, p1) => (SeqPrefix(seq, op), p1)
          };
        let zp0 = ZPat.place_after(p0);
        let surround = OperatorSeq.BothNonEmpty(prefix, suffix);
        let skel =
          Associator.associate_pat(
            OperatorSeq.opseq_of_exp_and_surround(p0, surround),
          );
        let zp = ZPat.OpSeqZ(skel, zp0, surround);
        Succeeded(Statics.ana_fix_holes_zpat(ctx, u_gen, zp, ty));
      }
    }
  /* ... + [k-1] + _|>  [k+1] + ...  ==>   ... + [k-1] + |[k+1] + ... */
  | (
      Delete,
      OpSeqZ(
        _,
        CursorP(_, EmptyHole(_)) as zp0,
        (
          EmptyPrefix(ExpSuffix(Space, _) | SeqSuffix(Space, _)) |
          BothNonEmpty(_, ExpSuffix(Space, _) | SeqSuffix(Space, _))
        ) as surround,
      ),
    )
      when ZPat.is_after(zp0) =>
    switch (surround) {
    | EmptySuffix(_) => CursorEscaped(After) /* should never happen */
    | EmptyPrefix(suffix) =>
      let p: UHPat.t =
        switch (suffix) {
        | ExpSuffix(_space, p1) => p1
        | SeqSuffix(_space, seq) =>
          let skel = Associator.associate_pat(seq);
          OpSeq(skel, seq);
        };
      Succeeded(
        Statics.ana_fix_holes_zpat(ctx, u_gen, ZPat.place_before(p), ty),
      );
    | BothNonEmpty(prefix, suffix) =>
      switch (suffix) {
      | ExpSuffix(_space, p1) =>
        let zp1 = ZPat.place_before(p1);
        let surround = OperatorSeq.EmptyPrefix(suffix);
        let skel =
          Associator.associate_pat(
            OperatorSeq.opseq_of_exp_and_surround(p1, surround),
          );
        let zp = ZPat.OpSeqZ(skel, zp1, surround);
        Succeeded(Statics.ana_fix_holes_zpat(ctx, u_gen, zp, ty));
      | SeqSuffix(_space, seq) =>
        let (p0, suffix: ZPat.opseq_suffix) =
          switch (seq) {
          | ExpOpExp(p1, op, p2) => (p1, ExpSuffix(op, p2))
          | SeqOpExp(seq, op, p1) => (p1, SeqSuffix(op, seq))
          };
        let zp0 = ZPat.place_before(p0);
        let surround = OperatorSeq.BothNonEmpty(prefix, suffix);
        let skel =
          Associator.associate_pat(
            OperatorSeq.opseq_of_exp_and_surround(p0, surround),
          );
        let zp = ZPat.OpSeqZ(skel, zp0, surround);
        Succeeded(Statics.ana_fix_holes_zpat(ctx, u_gen, zp, ty));
      }
    }
  /* Construct */
  | (Construct(SOp(SSpace)), CursorP(OnDelim(_, After), _))
      when !ZPat.is_after(zp) =>
    ana_perform_pat(ctx, u_gen, MoveRight, zp, ty)
  | (Construct(_) as a, CursorP(OnDelim(_, side), _))
      when !ZPat.is_before(zp) && !ZPat.is_after(zp) =>
    let move_then_perform = move_action =>
      switch (ana_perform_pat(ctx, u_gen, move_action, zp, ty)) {
      | Failed
      | CantShift
      | CursorEscaped(_) => assert(false)
      | Succeeded((zp, _, u_gen)) => ana_perform_pat(ctx, u_gen, a, zp, ty)
      };
    switch (side) {
    | Before => move_then_perform(MoveLeft)
    | After => move_then_perform(MoveRight)
    };
  | (
      Construct(SLine),
      CursorP(Staging(k), (Parenthesized(_) | Inj(_, _, _)) as p),
    ) =>
    Succeeded(
      Statics.ana_fix_holes_zpat(
        ctx,
        u_gen,
        CursorP(OnDelim(k, k == 0 ? Before : After), p),
        ty,
      ),
    )
  | (Construct(_), CursorP(Staging(_), _)) => Failed
  | (Construct(SParenthesized), CursorP(_, _)) =>
    switch (Statics.ana_pat(ctx, ZPat.erase(zp), ty)) {
    | None => Failed
    | Some(ctx) => Succeeded((ParenthesizedZ(zp), ctx, u_gen))
    }
  | (Construct(SVar("true", _)), _)
  | (Construct(SVar("false", _)), _) =>
    switch (syn_perform_pat(ctx, u_gen, a, zp)) {
    | (Failed | CantShift | CursorEscaped(_)) as err => err
    | Succeeded((zp, ty', ctx, u_gen)) =>
      if (HTyp.consistent(ty, ty')) {
        Succeeded((zp, ctx, u_gen));
      } else {
        let (zp, u_gen) = ZPat.make_t_inconsistent(u_gen, zp);
        Succeeded((zp, ctx, u_gen));
      }
    }
  | (Construct(SVar(x, cursor)), CursorP(_, EmptyHole(_)))
  | (Construct(SVar(x, cursor)), CursorP(_, Wild(_)))
  | (Construct(SVar(x, cursor)), CursorP(_, Var(_, _, _)))
  | (Construct(SVar(x, cursor)), CursorP(_, NumLit(_, _)))
  | (Construct(SVar(x, cursor)), CursorP(_, BoolLit(_, _))) =>
    if (Var.is_let(x)) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      Succeeded((
        CursorP(cursor, Var(NotInHole, InVHole(Keyword(Let), u), x)),
        ctx,
        u_gen,
      ));
    } else if (Var.is_case(x)) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      Succeeded((
        CursorP(cursor, Var(NotInHole, InVHole(Keyword(Case), u), x)),
        ctx,
        u_gen,
      ));
    } else {
      check_valid(
        x,
        {
          let ctx = Contexts.extend_gamma(ctx, (x, ty));
          Succeeded((
            ZPat.CursorP(cursor, Var(NotInHole, NotInVHole, x)),
            ctx,
            u_gen,
          ));
        },
      );
    }
  | (Construct(SVar(_, _)), CursorP(_, _)) => Failed
  | (Construct(SWild), CursorP(_, EmptyHole(_)))
  | (Construct(SWild), CursorP(_, Wild(_)))
  | (Construct(SWild), CursorP(_, Var(_, _, _)))
  | (Construct(SWild), CursorP(_, NumLit(_, _)))
  | (Construct(SWild), CursorP(_, BoolLit(_, _))) =>
    Succeeded((ZPat.place_after(Wild(NotInHole)), ctx, u_gen))
  | (Construct(SWild), CursorP(_, _)) => Failed
  | (Construct(SInj(side)), CursorP(_, _) as zp1) =>
    switch (HTyp.matched_sum(ty)) {
    | Some((tyL, tyR)) =>
      let ty1 = pick_side(side, tyL, tyR);
      let (zp1, ctx, u_gen) =
        Statics.ana_fix_holes_zpat(ctx, u_gen, zp1, ty1);
      let zp = ZPat.InjZ(NotInHole, side, zp1);
      Succeeded((zp, ctx, u_gen));
    | None =>
      let (zp1, _, ctx, u_gen) = Statics.syn_fix_holes_zpat(ctx, u_gen, zp1);
      let (u, u_gen) = MetaVarGen.next(u_gen);
      let zp = ZPat.InjZ(InHole(TypeInconsistent, u), side, zp1);
      Succeeded((zp, ctx, u_gen));
    }
  | (
      Construct(SOp(SSpace)),
      OpSeqZ(_, CursorP(OnDelim(_, After), _) as zp0, _),
    )
      when !ZPat.is_after(zp0) =>
    ana_perform_pat(ctx, u_gen, MoveRight, zp, ty)
  | (Construct(SOp(os)), OpSeqZ(_, zp0, surround)) when ZPat.is_after(zp0) =>
    switch (pat_op_of(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded(
        abs_perform_Construct_SOp_After_surround(
          UHPat.new_EmptyHole,
          (ctx, u_gen, cursor, seq) =>
            make_and_ana_OpSeq_pat(ctx, u_gen, cursor, seq, ty),
          (ctx, u_gen, zp, surround) =>
            make_and_ana_OpSeqZ_pat(ctx, u_gen, zp, surround, ty),
          UHPat.is_Space,
          UHPat.Space,
          ZPat.place_before,
          ctx,
          u_gen,
          ZPat.erase(zp0),
          op,
          surround,
        ),
      )
    }
  | (Construct(SOp(os)), OpSeqZ(_, zp0, surround)) when ZPat.is_before(zp0) =>
    switch (pat_op_of(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded(
        abs_perform_Construct_SOp_Before_surround(
          UHPat.new_EmptyHole,
          (ctx, u_gen, cursor, seq) =>
            make_and_ana_OpSeq_pat(ctx, u_gen, cursor, seq, ty),
          (ctx, u_gen, zp, surround) =>
            make_and_ana_OpSeqZ_pat(ctx, u_gen, zp, surround, ty),
          UHPat.is_Space,
          UHPat.Space,
          ZPat.place_before,
          ctx,
          u_gen,
          zp0 |> ZPat.erase,
          op,
          surround,
        ),
      )
    }
  | (Construct(SOp(os)), CursorP(_, _)) =>
    switch (pat_op_of(os)) {
    | None => Failed
    | Some(op) =>
      if (ZPat.is_before(zp)) {
        Succeeded(
          abs_perform_Construct_SOp_Before(
            UHPat.bidelimit,
            UHPat.new_EmptyHole,
            (ctx, u_gen, cursor, seq) =>
              make_and_ana_OpSeq_pat(ctx, u_gen, cursor, seq, ty),
            (ctx, u_gen, zp, surround) =>
              make_and_ana_OpSeqZ_pat(ctx, u_gen, zp, surround, ty),
            UHPat.is_Space,
            ZPat.place_before,
            ctx,
            u_gen,
            ZPat.erase(zp),
            op,
          ),
        );
      } else if (ZPat.is_after(zp)) {
        Succeeded(
          abs_perform_Construct_SOp_After(
            UHPat.bidelimit,
            UHPat.new_EmptyHole,
            (ctx, u_gen, cursor, seq) =>
              make_and_ana_OpSeq_pat(ctx, u_gen, cursor, seq, ty),
            (ctx, u_gen, zp, surround) =>
              make_and_ana_OpSeqZ_pat(ctx, u_gen, zp, surround, ty),
            UHPat.is_Space,
            ZPat.place_before,
            ctx,
            u_gen,
            ZPat.erase(zp),
            op,
          ),
        );
      } else {
        Failed;
      }
    }
  /* Zipper */
  | (_, ParenthesizedZ(zp1)) =>
    switch (ana_perform_pat(ctx, u_gen, a, zp1, ty)) {
    | Failed => Failed
    | CantShift => CantShift
    | CursorEscaped(Before) => ana_perform_pat(ctx, u_gen, MoveLeft, zp, ty)
    | CursorEscaped(After) => ana_perform_pat(ctx, u_gen, MoveRight, zp, ty)
    | Succeeded((zp1, ctx, u_gen)) =>
      Succeeded((ParenthesizedZ(zp1), ctx, u_gen))
    }
  | (_, InjZ(_, side, zp1)) =>
    switch (HTyp.matched_sum(ty)) {
    | None => Failed
    | Some((tyL, tyR)) =>
      let ty1 = pick_side(side, tyL, tyR);
      switch (ana_perform_pat(ctx, u_gen, a, zp1, ty1)) {
      | Failed => Failed
      | CantShift => CantShift
      | CursorEscaped(Before) =>
        ana_perform_pat(ctx, u_gen, MoveLeft, zp, ty)
      | CursorEscaped(After) =>
        ana_perform_pat(ctx, u_gen, MoveRight, zp, ty)
      | Succeeded((zp1, ctx, u_gen)) =>
        let zp = ZPat.InjZ(NotInHole, side, zp1);
        Succeeded((zp, ctx, u_gen));
      };
    }
  | (_, OpSeqZ(_, zp0, surround)) =>
    let i = OperatorSeq.surround_prefix_length(surround);
    switch (ZPat.erase(zp)) {
    | OpSeq(skel, seq) =>
      switch (Statics.ana_skel_pat(ctx, skel, seq, ty, Some(i))) {
      | Some((_, Some(mode))) =>
        switch (mode) {
        | Statics.AnalyzedAgainst(ty0) =>
          switch (ana_perform_pat(ctx, u_gen, a, zp0, ty0)) {
          | Failed => Failed
          | CantShift => CantShift
          | CursorEscaped(Before) =>
            ana_perform_pat(ctx, u_gen, MoveLeft, zp, ty)
          | CursorEscaped(After) =>
            ana_perform_pat(ctx, u_gen, MoveRight, zp, ty)
          | Succeeded((zp0, _, u_gen)) =>
            let zp0 = ZPat.bidelimit(zp0);
            Succeeded(
              make_and_ana_OpSeqZ_pat(ctx, u_gen, zp0, surround, ty),
            );
          }
        | Statics.Synthesized(_) =>
          switch (syn_perform_pat(ctx, u_gen, a, zp0)) {
          | Failed => Failed
          | CantShift => CantShift
          | CursorEscaped(Before) =>
            ana_perform_pat(ctx, u_gen, MoveLeft, zp, ty)
          | CursorEscaped(After) =>
            ana_perform_pat(ctx, u_gen, MoveRight, zp, ty)
          | Succeeded((zp0, _, _, u_gen)) =>
            let zp0 = ZPat.bidelimit(zp0);
            Succeeded(
              make_and_ana_OpSeqZ_pat(ctx, u_gen, zp0, surround, ty),
            );
          }
        }
      | Some(_) => Failed /* should never happen */
      | None => Failed /* should never happen */
      }
    | _ => Failed /* should never happen */
    };
  /* Subsumption */
  | (Construct(SNumLit(_, _)), _)
  | (Construct(SListNil), _) =>
    switch (syn_perform_pat(ctx, u_gen, a, zp)) {
    | (Failed | CantShift | CursorEscaped(_)) as err => err
    | Succeeded((zp, ty', ctx, u_gen)) =>
      if (HTyp.consistent(ty, ty')) {
        Succeeded((zp, ctx, u_gen));
      } else {
        let (zp, u_gen) = ZPat.make_t_inconsistent(u_gen, zp);
        Succeeded((zp, ctx, u_gen));
      }
    }
  /* Invalid actions at the pattern level */
  | (UpdateApPalette(_), _)
  | (Construct(SApPalette(_)), _)
  | (Construct(SNum), _)
  | (Construct(SBool), _)
  | (Construct(SList), _)
  | (Construct(SAsc), _)
  | (Construct(SLet), _)
  | (Construct(SLine), _)
  | (Construct(SLam), _)
  | (Construct(SCase), _) => Failed
  };

let make_and_syn_OpSeqZ =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ze0: ZExp.t,
      surround: ZExp.opseq_surround,
    )
    : (ZExp.t, HTyp.t, MetaVarGen.t) => {
  /* figure out the current path so that we can follow it again
   * to reconstitute the Z-exp after calling into the UHExp hole
   * insertion logic (otherwise we'd have to do a version of that
   * logic specific to Z-exps) */
  let path0 = Path.of_OpSeqZ(ze0, surround);
  let e0 = ZExp.erase(ze0);
  let seq = OperatorSeq.opseq_of_exp_and_surround(e0, surround);
  let skel = Associator.associate_exp(seq);
  let (skel, seq, ty, u_gen) =
    Statics.syn_fix_holes_exp_skel(ctx, u_gen, skel, seq);
  let e = UHExp.OpSeq(skel, seq);
  let ze = Path.follow_e_or_fail(path0, e);
  (ze, ty, u_gen);
};

let make_and_ana_OpSeqZ =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ze0: ZExp.t,
      surround: ZExp.opseq_surround,
      ty: HTyp.t,
    )
    : (ZExp.t, MetaVarGen.t) => {
  /* figure out the current path so that we can follow it again
   * to reconstitute the Z-exp after calling into the UHExp hole
   * insertion logic (otherwise we'd have to do a version of that
   * logic specific to Z-exps) */
  let path0 = Path.of_OpSeqZ(ze0, surround);
  let e0 = ZExp.erase(ze0);
  let seq = OperatorSeq.opseq_of_exp_and_surround(e0, surround);
  let skel = Associator.associate_exp(seq);
  switch (Statics.ana_fix_holes_exp_skel(ctx, u_gen, skel, seq, ty)) {
  | (Placeholder(_), _, _) =>
    raise(UHExp.SkelInconsistentWithOpSeq(skel, seq))
  | (BinOp(_, _, _, _) as skel, seq, u_gen) =>
    let e = UHExp.OpSeq(skel, seq);
    let ze = Path.follow_e_or_fail(path0, e);
    (ze, u_gen);
  };
};

let make_and_syn_OpSeq =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      cursor: cursor_position,
      seq: UHExp.opseq,
    ) => {
  let ze = ZExp.CursorE(cursor, OpSeqUtil.Exp.mk_OpSeq(seq));
  Statics.syn_fix_holes_zexp(ctx, u_gen, ze);
};

let make_and_ana_OpSeq =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      cursor: cursor_position,
      seq: UHExp.opseq,
      ty,
    ) => {
  let ze = ZExp.CursorE(cursor, OpSeqUtil.Exp.mk_OpSeq(seq));
  Statics.ana_fix_holes_zexp(ctx, u_gen, ze, ty);
};

let combine_for_Backspace_Space = (e1: UHExp.t, ze0: ZExp.t): ZExp.t =>
  switch (e1, ze0) {
  | (_, CursorE(_, EmptyHole(_))) =>
    /* e1 |_ --> e1| */
    ZExp.place_after_exp(e1)
  | _ => ze0
  };

let combine_for_Delete_Space = (ze0: ZExp.t, e: UHExp.t): ZExp.t =>
  switch (ze0, e) {
  | (CursorE(_, EmptyHole(_)), EmptyHole(_)) when ZExp.is_after_exp(ze0) =>
    /* _| _ --> _| */
    ze0
  | (CursorE(_, EmptyHole(_)), _) when ZExp.is_after_exp(ze0) =>
    /* _| e --> |e */
    ZExp.place_before_exp(e)
  | _ => ze0
  };

/**
 * Used to construct an expression from an opseq suffix that
 * follows a keyword when the user hits space after the keyword.
 * If the first operation is a space, then what follows the space
 * becomes the new expression. Otherwise, a new hole is generated,
 * prepended to the suffix, and the reuslting opseq becomes the
 * new expression.
 */
let keyword_suffix_to_exp =
    (
      suffix: OperatorSeq.opseq_suffix(UHExp.t, UHExp.op),
      u_gen: MetaVarGen.t,
    )
    : (UHExp.t, MetaVarGen.t) =>
  switch (suffix) {
  | ExpSuffix(Space, e) => (e, u_gen)
  | SeqSuffix(Space, seq) => (
      OpSeq(Associator.associate_exp(seq), seq),
      u_gen,
    )
  | ExpSuffix(_, _)
  | SeqSuffix(_, _) =>
    let (hole, u_gen) = UHExp.new_EmptyHole(u_gen);
    let opseq = OperatorSeq.opseq_of_exp_and_suffix(hole, suffix);
    let skel = Associator.associate_exp(opseq);
    (OpSeq(skel, opseq), u_gen);
  };

let keyword_action = (k: keyword): t =>
  switch (k) {
  | Let => Construct(SLet)
  | Case => Construct(SCase)
  };

type zexp_or_zblock = ZExp.zexp_or_zblock;

let set_err_status_zexp_or_zblock =
    (err: err_status, ze_zb: zexp_or_zblock): zexp_or_zblock =>
  switch (ze_zb) {
  | E(ze) => E(ZExp.set_err_status_t(err, ze))
  | B(zblock) => B(ZExp.set_err_status_block(err, zblock))
  };

let make_zexp_or_zblock_inconsistent =
    (u_gen: MetaVarGen.t, ze_zb: zexp_or_zblock)
    : (zexp_or_zblock, MetaVarGen.t) =>
  switch (ze_zb) {
  | E(ze) =>
    let (ze, u_gen) = ZExp.make_t_inconsistent(u_gen, ze);
    (E(ze), u_gen);
  | B(zblock) =>
    let (zblock, u_gen) = ZExp.make_block_inconsistent(u_gen, zblock);
    (B(zblock), u_gen);
  };

let rec syn_perform_block =
        (
          ~ci: CursorInfo.t,
          ctx: Contexts.t,
          a: t,
          (zblock, ty, u_gen): (ZExp.zblock, HTyp.t, MetaVarGen.t),
        )
        : result((ZExp.zblock, HTyp.t, MetaVarGen.t)) =>
  switch (a, zblock) {
  /* Staging */
  | (
      ShiftUp | ShiftDown,
      BlockZL(
        (prefix, CursorL(Staging(3), LetLine(p, ann, def)), suffix),
        e,
      ),
    ) =>
    let shift_line =
      switch (a) {
      | ShiftUp => UHExp.shift_line_to_suffix_block
      | _ => UHExp.shift_line_from_suffix_block(~is_node_terminal=false)
      };
    switch (def |> shift_line(~u_gen, Some(Block(suffix, e)))) {
    | None => CantShift
    | Some((_, None, _)) =>
      // should not happen since let line is not terminal
      assert(false)
    | Some((new_def, Some(Block(new_suffix, new_e)), u_gen)) =>
      let new_zblock =
        ZExp.BlockZL(
          (
            prefix,
            CursorL(Staging(3), LetLine(p, ann, new_def)),
            new_suffix,
          ),
          new_e,
        );
      Succeeded(Statics.syn_fix_holes_zblock(ctx, u_gen, new_zblock));
    };
  | (
      ShiftUp | ShiftDown | ShiftLeft | ShiftRight,
      BlockZL((_, CursorL(Staging(_), LetLine(_, _, _)), _), _),
    ) =>
    CantShift
  | (
      ShiftUp | ShiftDown | ShiftLeft | ShiftRight,
      BlockZL((_, CursorL(Staging(_), EmptyLine | ExpLine(_)), _), _),
    ) =>
    Failed
  | (
      ShiftUp | ShiftDown,
      BlockZL(
        (
          prefix,
          ExpLineZ(
            CursorE(
              Staging(0) as cursor,
              (
                Parenthesized(block) | Inj(_, _, block) |
                Case(_, block, _, _)
              ) as e_line,
            ),
          ),
          suffix,
        ),
        e,
      ),
    ) =>
    let shift_line =
      switch (a) {
      | ShiftUp => UHExp.shift_line_from_prefix
      | _ => UHExp.shift_line_to_prefix
      };
    switch (block |> shift_line(~u_gen, prefix)) {
    | None => CantShift
    | Some((new_prefix, new_block, u_gen)) =>
      let new_e_line =
        switch (e_line) {
        | Inj(err_status, side, _) => UHExp.Inj(err_status, side, new_block)
        | Case(err_status, _, rules, ann) =>
          Case(err_status, new_block, rules, ann)
        | _ => Parenthesized(new_block)
        };
      let new_zblock =
        ZExp.BlockZL(
          (new_prefix, ExpLineZ(CursorE(cursor, new_e_line)), suffix),
          e,
        );
      Succeeded(Statics.syn_fix_holes_zblock(ctx, u_gen, new_zblock));
    };
  | (
      ShiftUp | ShiftDown,
      BlockZL(
        (
          prefix,
          ExpLineZ(
            CursorE(
              Staging(1) as cursor,
              (Parenthesized(block) | Inj(_, _, block)) as e_line,
            ),
          ),
          suffix,
        ),
        e,
      ),
    ) =>
    let shift_line =
      switch (a) {
      | ShiftUp => UHExp.shift_line_to_suffix_block
      | _ => UHExp.shift_line_from_suffix_block(~is_node_terminal=true)
      };
    switch (block |> shift_line(~u_gen, Some(Block(suffix, e)))) {
    | None => CantShift
    | Some((new_block, None, u_gen)) =>
      let new_conclusion: UHExp.t =
        switch (e_line) {
        | Inj(err_status, side, _) => Inj(err_status, side, new_block)
        | _ => Parenthesized(new_block)
        };
      let new_zblock = ZExp.BlockZE(prefix, CursorE(cursor, new_conclusion));
      Succeeded(Statics.syn_fix_holes_zblock(ctx, u_gen, new_zblock));
    | Some((new_block, Some(Block(new_suffix, new_e)), u_gen)) =>
      let new_e_line: UHExp.t =
        switch (e_line) {
        | Inj(err_status, side, _) => Inj(err_status, side, new_block)
        | _ => Parenthesized(new_block)
        };
      let new_zblock =
        ZExp.BlockZL(
          (prefix, ExpLineZ(CursorE(cursor, new_e_line)), new_suffix),
          new_e,
        );
      Succeeded(Statics.syn_fix_holes_zblock(ctx, u_gen, new_zblock));
    };
  | (
      ShiftUp | ShiftDown,
      BlockZL(
        (
          prefix,
          ExpLineZ(
            CursorE(
              Staging(1) as cursor,
              Case(err_status, scrut, rules, None),
            ),
          ),
          suffix,
        ),
        e,
      ),
    ) =>
    switch (rules |> split_last) {
    | None => Failed // shouldn't ever see empty rule list
    | Some((leading_rules, Rule(last_p, last_clause))) =>
      let shift_line =
        switch (a) {
        | ShiftUp => UHExp.shift_line_to_suffix_block
        | _ => UHExp.shift_line_from_suffix_block(~is_node_terminal=true)
        };
      switch (last_clause |> shift_line(~u_gen, Some(Block(suffix, e)))) {
      | None => CantShift
      | Some((new_last_clause, None, u_gen)) =>
        let new_e =
          UHExp.Case(
            err_status,
            scrut,
            leading_rules @ [Rule(last_p, new_last_clause)],
            None,
          );
        let new_zblock = ZExp.BlockZE(prefix, CursorE(cursor, new_e));
        Succeeded(Statics.syn_fix_holes_zblock(ctx, u_gen, new_zblock));
      | Some((new_last_clause, Some(Block(new_suffix, new_e)), u_gen)) =>
        let new_e_line =
          UHExp.Case(
            err_status,
            scrut,
            leading_rules @ [Rule(last_p, new_last_clause)],
            None,
          );
        let new_zblock =
          ZExp.BlockZL(
            (prefix, ExpLineZ(CursorE(cursor, new_e_line)), new_suffix),
            new_e,
          );
        Succeeded(Statics.syn_fix_holes_zblock(ctx, u_gen, new_zblock));
      };
    }
  | (
      ShiftRight,
      BlockZE(
        leading,
        CursorE(
          Staging(0) as cursor,
          (Parenthesized(Block([], body)) | Inj(_, _, Block([], body))) as staged,
        ),
      ),
    ) =>
    switch (body |> OpSeqUtil.Exp.shift_optm_to_prefix(~surround=None)) {
    | None => CantShift
    | Some((new_body, new_surround)) =>
      let new_ztm =
        ZExp.CursorE(
          cursor,
          switch (staged) {
          | Inj(err_status, side, _) =>
            Inj(err_status, side, new_body |> UHExp.wrap_in_block)
          | _parenthesized => Parenthesized(new_body |> UHExp.wrap_in_block)
          },
        );
      let new_ze =
        switch (new_surround) {
        | None => new_ztm
        | Some(surround) => OpSeqUtil.Exp.mk_OpSeqZ(new_ztm, surround)
        };
      let new_zblock = ZExp.BlockZE(leading, new_ze);
      Succeeded(Statics.syn_fix_holes_zblock(ctx, u_gen, new_zblock));
    }
  | (
      ShiftUp,
      BlockZL(
        (
          leading_prefix,
          ExpLineZ(
            OpSeqZ(
              _,
              CursorE(Staging(1) as cursor, Parenthesized(Block([], body))),
              EmptySuffix(prefix),
            ),
          ),
          leading_suffix,
        ),
        conclusion,
      ),
    ) =>
    // skip over remaining left shifts, then apply ShiftUp to result
    let skipped_body = OpSeqUtil.Exp.prepend(prefix, body);
    let skipped_zblock =
      ZExp.BlockZL(
        (
          leading_prefix,
          ExpLineZ(
            CursorE(cursor, Parenthesized(Block([], skipped_body))),
          ),
          leading_suffix,
        ),
        conclusion,
      );
    syn_perform_block(
      ~ci,
      ctx,
      ShiftUp,
      Statics.syn_fix_holes_zblock(ctx, u_gen, skipped_zblock),
    );
  | (
      ShiftDown,
      BlockZL(
        (
          leading_prefix,
          ExpLineZ(
            OpSeqZ(
              _,
              CursorE(Staging(1) as cursor, Parenthesized(Block([], body))),
              EmptyPrefix(suffix),
            ),
          ),
          leading_suffix,
        ),
        conclusion,
      ),
    ) =>
    // skip over remaining right shifts, then apply ShiftDown to result
    let skipped_body = OpSeqUtil.Exp.append(body, suffix);
    let skipped_zblock =
      ZExp.BlockZL(
        (
          leading_prefix,
          ExpLineZ(
            CursorE(cursor, Parenthesized(Block([], skipped_body))),
          ),
          leading_suffix,
        ),
        conclusion,
      );
    syn_perform_block(
      ~ci,
      ctx,
      ShiftDown,
      Statics.syn_fix_holes_zblock(ctx, u_gen, skipped_zblock),
    );
  | (
      ShiftUp,
      BlockZE(
        leading,
        OpSeqZ(
          _,
          CursorE(Staging(1) as cursor, Parenthesized(Block([], body))),
          EmptySuffix(prefix),
        ),
      ),
    ) =>
    // skip over remaining left shifts, then apply ShiftUp to result
    let skipped_body = OpSeqUtil.Exp.prepend(prefix, body);
    let skipped_zblock =
      ZExp.BlockZE(
        leading,
        CursorE(cursor, Parenthesized(Block([], skipped_body))),
      );
    syn_perform_block(
      ~ci,
      ctx,
      ShiftUp,
      Statics.syn_fix_holes_zblock(ctx, u_gen, skipped_zblock),
    );
  | (
      ShiftUp,
      BlockZE(leading, CursorE(Staging(1) as cursor, Parenthesized(body))),
    ) =>
    switch (body |> UHExp.shift_line_to_suffix_block(~u_gen, None)) {
    | None => CantShift
    | Some((_, None, _)) => assert(false)
    | Some((
        new_body,
        Some(Block(new_suffix_leading, new_suffix_conclusion)),
        u_gen,
      )) =>
      let new_zblock =
        ZExp.BlockZL(
          (
            leading,
            ExpLineZ(CursorE(cursor, Parenthesized(new_body))),
            new_suffix_leading,
          ),
          new_suffix_conclusion,
        );
      Succeeded(Statics.syn_fix_holes_zblock(ctx, u_gen, new_zblock));
    }
  | (
      ShiftLeft,
      BlockZE(
        leading,
        CursorE(
          Staging(1) as cursor,
          (Parenthesized(Block([], body)) | Inj(_, _, Block([], body))) as staged,
        ),
      ),
    ) =>
    switch (body |> OpSeqUtil.Exp.shift_optm_to_suffix(~surround=None)) {
    | None => CantShift
    | Some((new_body, new_surround)) =>
      let new_ztm =
        ZExp.CursorE(
          cursor,
          switch (staged) {
          | Inj(err_status, side, _) =>
            Inj(err_status, side, new_body |> UHExp.wrap_in_block)
          | _parenthesized => Parenthesized(new_body |> UHExp.wrap_in_block)
          },
        );
      let new_ze =
        switch (new_surround) {
        | None => new_ztm
        | Some(surround) => OpSeqUtil.Exp.mk_OpSeqZ(new_ztm, surround)
        };
      let new_zblock = ZExp.BlockZE(leading, new_ze);
      Succeeded(Statics.syn_fix_holes_zblock(ctx, u_gen, new_zblock));
    }
  | (
      ShiftUp | ShiftDown,
      BlockZE(
        leading,
        CursorE(
          Staging(0) as cursor,
          (Parenthesized(block) | Inj(_, _, block) | Case(_, block, _, _)) as conclusion,
        ),
      ),
    ) =>
    let shift_line =
      switch (a) {
      | ShiftUp => UHExp.shift_line_from_prefix
      | _ => UHExp.shift_line_to_prefix
      };
    switch (block |> shift_line(~u_gen, leading)) {
    | None => CantShift
    | Some((new_leading, new_block, u_gen)) =>
      let new_conclusion =
        switch (conclusion) {
        | Inj(err_status, side, _) => UHExp.Inj(err_status, side, new_block)
        | Case(err_status, _, rules, ann) =>
          Case(err_status, new_block, rules, ann)
        | _ => Parenthesized(new_block)
        };
      let new_zblock =
        ZExp.BlockZE(new_leading, CursorE(cursor, new_conclusion));
      Succeeded(Statics.syn_fix_holes_zblock(ctx, u_gen, new_zblock));
    };
  /* Movement */
  | (MoveTo(path), _) =>
    let block = ZExp.erase_block(zblock);
    switch (Path.follow_block(path, block)) {
    | None => Failed
    | Some(zblock) => Succeeded((zblock, ty, u_gen))
    };
  | (MoveToBefore(steps), _) =>
    let block = ZExp.erase_block(zblock);
    switch (Path.follow_block_and_place_before(steps, block)) {
    | None => Failed
    | Some(zblock) => Succeeded((zblock, ty, u_gen))
    };
  | (MoveToPrevHole, _) =>
    switch (Path.prev_hole_path_zblock(zblock)) {
    | None => Failed
    | Some(path) =>
      syn_perform_block(~ci, ctx, MoveTo(path), (zblock, ty, u_gen))
    }
  | (MoveToNextHole, _) =>
    switch (Path.next_hole_path_zblock(zblock)) {
    | None => Failed
    | Some(path) =>
      syn_perform_block(~ci, ctx, MoveTo(path), (zblock, ty, u_gen))
    }
  | (MoveLeft, _) =>
    ZExp.move_cursor_left_block(zblock)
    |> Opt.map_default(~default=CursorEscaped(Before), zblock =>
         Succeeded((zblock, ty, u_gen))
       )
  | (MoveRight, _) =>
    ZExp.move_cursor_right_block(zblock)
    |> Opt.map_default(~default=CursorEscaped(After), zblock =>
         Succeeded((zblock, ty, u_gen))
       )
  /* Backspace & Delete */
  | (Backspace, _) when ZExp.is_before_block(zblock) =>
    CursorEscaped(Before)
  | (Delete, _) when ZExp.is_after_block(zblock) => CursorEscaped(After)
  | (Delete, BlockZL((prefix, CursorL(_, EmptyLine), []), e)) =>
    let ze = ZExp.place_before_exp(e);
    let zblock = ZExp.BlockZE(prefix, ze);
    Succeeded((zblock, ty, u_gen));
  | (Backspace, BlockZE(leading, zconclusion))
      when ZExp.is_before_exp(zconclusion) =>
    switch (leading |> split_last, zconclusion |> ZExp.erase) {
    | (None, _) => CursorEscaped(Before)
    | (Some((leading_prefix, EmptyLine)), _) =>
      Succeeded((BlockZE(leading_prefix, zconclusion), ty, u_gen))
    | (Some((leading_prefix, ExpLine(e))), EmptyHole(_)) =>
      let new_zconclusion = ZExp.place_after_exp(e);
      Succeeded(
        Statics.syn_fix_holes_zblock(
          ctx,
          u_gen,
          BlockZE(leading_prefix, new_zconclusion),
        ),
      );
    | (Some((leading_prefix, leading_last)), conclusion) =>
      let zleading_last = ZExp.place_after_line(leading_last);
      let zblock =
        ZExp.BlockZL((leading_prefix, zleading_last, []), conclusion);
      Succeeded((zblock, ty, u_gen));
    }
  | (Delete, BlockZL((prefix, ExpLineZ(ze), []), EmptyHole(_)))
      when ZExp.is_after_exp(ze) =>
    switch (Statics.syn_exp(ctx, ZExp.erase(ze))) {
    | None => Failed
    | Some(ty) =>
      let zblock = ZExp.BlockZE(prefix, ze);
      Succeeded((zblock, ty, u_gen));
    }
  | (
      Backspace | Delete,
      BlockZL((prefix, CursorL(Staging(k), _), suffix), conclusion),
    ) =>
    let new_zblock: option(ZExp.zblock) =
      switch (ci |> CursorInfo.preserved_child_term_of_node, suffix) {
      | (Some((_, Type(_) | Pattern(_))), _) => None
      | (None, []) =>
        // If deleted line is followed by an empty hole,
        // then they are on the same visual line. Don't bother
        // leaving behind an empty line, instead let the
        // the empty hole take the deleted line's place.
        switch (conclusion) {
        | EmptyHole(_) =>
          Some(BlockZE(prefix, conclusion |> ZExp.place_before_exp))
        | _ =>
          Some(
            BlockZL(
              (prefix, ZExp.place_before_line(EmptyLine), []),
              conclusion,
            ),
          )
        }
      | (None, [_, ..._]) =>
        Some(
          BlockZL(
            (prefix, ZExp.place_before_line(EmptyLine), suffix),
            conclusion,
          ),
        )
      | (Some((_, Expression(block))), _) =>
        let place_cursor =
          // here we're depending on the fact that
          // only let lines can preserve children
          switch (k) {
          | 0
          | 1
          | 2 => ZExp.place_before_block
          | _three => ZExp.place_after_block
          };
        let (inner_prefix, zline, inner_suffix) =
          block
          |> place_cursor
          |> ZExp.zblock_to_zlines
          |> ZExp.prune_empty_hole_lines;
        Some(
          BlockZL(
            (prefix @ inner_prefix, zline, inner_suffix @ suffix),
            conclusion,
          ),
        );
      };
    new_zblock
    |> Opt.map_default(~default=Failed, zblock =>
         Succeeded(Statics.syn_fix_holes_zblock(ctx, u_gen, zblock))
       );
  /* Construction */
  | (
      Construct(SLine),
      BlockZL((prefix, CursorL(Staging(k), line), suffix), e),
    ) =>
    Succeeded(
      Statics.syn_fix_holes_zblock(
        ctx,
        u_gen,
        BlockZL((prefix, CursorL(OnDelim(k, After), line), suffix), e),
      ),
    )
  | (
      Construct(SLine),
      BlockZL((prefix, ExpLineZ(CursorE(Staging(k), e_line)), suffix), e),
    ) =>
    Succeeded(
      Statics.syn_fix_holes_zblock(
        ctx,
        u_gen,
        BlockZL(
          (prefix, ExpLineZ(CursorE(OnDelim(k, After), e_line)), suffix),
          e,
        ),
      ),
    )
  | (Construct(_), BlockZL((_, CursorL(Staging(_), _), _), _)) => Failed
  | (Construct(SLine), BlockZE(lines, ze)) when ZExp.is_before_exp(ze) =>
    let zblock = ZExp.BlockZE(lines @ [EmptyLine], ze);
    Succeeded((zblock, ty, u_gen));
  | (Construct(SLine), BlockZE(lines, ze)) when ZExp.is_after_exp(ze) =>
    let (zhole, u_gen) = ZExp.new_EmptyHole(u_gen);
    let line = UHExp.prune_empty_hole_line(ExpLine(ZExp.erase(ze)));
    let zblock = ZExp.BlockZE(lines @ [line], zhole);
    Succeeded((zblock, Hole, u_gen));
  | (Construct(SLet), BlockZE(lines, ze1)) when ZExp.is_before_exp(ze1) =>
    let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
    let e1 = ZExp.erase(ze1);
    let zline = ZExp.LetLineZP(zp, None, UHExp.wrap_in_block(e1));
    let zlines = (lines, zline, []);
    let (e2, u_gen) = UHExp.new_EmptyHole(u_gen);
    let zblock = ZExp.BlockZL(zlines, e2);
    Succeeded((zblock, HTyp.Hole, u_gen));
  | (
      Construct(SCase),
      BlockZL(
        (prefix, (CursorL(_, EmptyLine) | ExpLineZ(_)) as zline, suffix),
        e2,
      ),
    )
      when ZExp.is_before_line(zline) =>
    let (e1, u_gen) =
      switch (zline) {
      | ExpLineZ(ze1) => (ZExp.erase(ze1), u_gen)
      | _ =>
        let (u, u_gen) = MetaVarGen.next(u_gen);
        (EmptyHole(u), u_gen);
      };
    let rule_block = UHExp.Block(suffix, e2);
    let (ze, u_gen) =
      switch (e1) {
      | EmptyHole(_) =>
        let (p, u_gen) = UHPat.new_EmptyHole(u_gen);
        let rule = UHExp.Rule(p, rule_block);
        let scrut_zblock = ZExp.BlockZE([], ZExp.place_before_exp(e1));
        (ZExp.CaseZE(NotInHole, scrut_zblock, [rule], Some(Hole)), u_gen);
      | _ =>
        let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
        let zrule = ZExp.RuleZP(zp, rule_block);
        let zrules = ZList.singleton(zrule);
        let scrut_block = UHExp.wrap_in_block(e1);
        (ZExp.CaseZR(NotInHole, scrut_block, zrules, Some(Hole)), u_gen);
      };
    let zblock = ZExp.BlockZE(prefix, ze);
    Succeeded((zblock, Hole, u_gen));
  | (
      Construct(SOp(SSpace)),
      BlockZL(
        (
          prefix,
          ExpLineZ(
            OpSeqZ(
              _,
              CursorE(_, Var(_, InVHole(Keyword(k), _), _)) as ze0,
              EmptyPrefix(opseq_suffix),
            ),
          ),
          suffix,
        ),
        e2,
      ),
    )
      when ZExp.is_after_exp(ze0) =>
    let (e, u_gen) = keyword_suffix_to_exp(opseq_suffix, u_gen);
    let ze = ZExp.place_before_exp(e);
    let zlines = (prefix, ZExp.ExpLineZ(ze), suffix);
    let zblock = ZExp.BlockZL(zlines, e2);
    syn_perform_block(~ci, ctx, keyword_action(k), (zblock, ty, u_gen));
  | (
      Construct(SOp(SSpace)),
      BlockZL(
        (
          prefix,
          ExpLineZ(CursorE(_, Var(_, InVHole(Keyword(k), _), _)) as ze0),
          suffix,
        ),
        e2,
      ),
    )
      when ZExp.is_after_exp(ze0) =>
    let zlines = (prefix, ZExp.place_before_line(EmptyLine), suffix);
    let zblock = ZExp.BlockZL(zlines, e2);
    syn_perform_block(~ci, ctx, keyword_action(k), (zblock, ty, u_gen));
  | (
      Construct(SOp(SSpace)),
      BlockZE(
        lines,
        OpSeqZ(
          _,
          CursorE(_, Var(_, InVHole(Keyword(k), _), _)) as ze0,
          EmptyPrefix(suffix),
        ),
      ),
    )
      when ZExp.is_after_exp(ze0) =>
    let (e, u_gen) = keyword_suffix_to_exp(suffix, u_gen);
    switch (Statics.syn_exp(ctx, e)) {
    | None => Failed
    | Some(ty) =>
      let ze = ZExp.place_before_exp(e);
      let zblock = ZExp.BlockZE(lines, ze);
      syn_perform_block(~ci, ctx, keyword_action(k), (zblock, ty, u_gen));
    };
  | (
      Construct(SOp(SSpace)),
      BlockZE(lines, CursorE(_, Var(_, InVHole(Keyword(k), _), _)) as ze0),
    )
      when ZExp.is_after_exp(ze0) =>
    let (ze, u_gen) = ZExp.new_EmptyHole(u_gen);
    let zblock = ZExp.BlockZE(lines, ze);
    syn_perform_block(~ci, ctx, keyword_action(k), (zblock, Hole, u_gen));
  /* Zipper Cases */
  | (
      Backspace | Delete | Construct(_) | UpdateApPalette(_) | ShiftLeft |
      ShiftRight |
      ShiftUp |
      ShiftDown,
      BlockZL(zlines, e),
    ) =>
    switch (syn_perform_lines(~ci, ctx, a, (zlines, u_gen))) {
    | Failed => Failed
    | CantShift => CantShift
    | CursorEscaped(Before) =>
      syn_perform_block(~ci, ctx, MoveLeft, (zblock, ty, u_gen))
    | CursorEscaped(After) =>
      syn_perform_block(~ci, ctx, MoveRight, (zblock, ty, u_gen))
    | Succeeded((zlines, ctx, u_gen)) =>
      let (e, ty, u_gen) = Statics.syn_fix_holes_exp(ctx, u_gen, e);
      let zblock = ZExp.BlockZL(zlines, e);
      Succeeded((zblock, ty, u_gen));
    }
  | (
      Backspace | Delete | Construct(_) | UpdateApPalette(_) | ShiftLeft |
      ShiftRight |
      ShiftUp |
      ShiftDown,
      BlockZE(lines, ze),
    ) =>
    switch (Statics.syn_lines(ctx, lines)) {
    | None => Failed
    | Some(ctx) =>
      switch (syn_perform_exp(~ci, ctx, a, (ze, ty, u_gen))) {
      | Failed => Failed
      | CantShift => CantShift
      | CursorEscaped(Before) =>
        syn_perform_block(~ci, ctx, MoveLeft, (zblock, ty, u_gen))
      | CursorEscaped(After) =>
        syn_perform_block(~ci, ctx, MoveRight, (zblock, ty, u_gen))
      | Succeeded((E(ze), ty, u_gen)) =>
        Succeeded((BlockZE(lines, ze), ty, u_gen))
      | Succeeded((B(zblock), _, u_gen)) =>
        switch (zblock) {
        | BlockZL((prefix, zline, suffix), e) =>
          let zblock = ZExp.BlockZL((lines @ prefix, zline, suffix), e);
          Succeeded(Statics.syn_fix_holes_zblock(ctx, u_gen, zblock));
        | BlockZE(ls, ze) =>
          let zblock = ZExp.BlockZE(lines @ ls, ze);
          Succeeded(Statics.syn_fix_holes_zblock(ctx, u_gen, zblock));
        }
      }
    }
  }
and syn_perform_lines =
    (
      ~ci: CursorInfo.t,
      ctx: Contexts.t,
      a: t,
      (zlines, u_gen) as edit_state: (ZExp.zlines, MetaVarGen.t),
    )
    : result((ZExp.zlines, Contexts.t, MetaVarGen.t)) =>
  switch (a, zlines) {
  /* Staging */
  | (ShiftUp | ShiftDown | ShiftLeft | ShiftRight, (_, CursorL(_, _), _)) =>
    // handled at block level
    Failed
  /* Movement */
  | (MoveTo(_), _)
  | (MoveToBefore(_), _)
  | (MoveToPrevHole, _)
  | (MoveToNextHole, _) =>
    /* TODO implement when we have cells, which
     * will be modeled as lists of lines
     */
    Failed
  /* Backspace & Delete */
  | (Backspace, _) when ZExp.is_before_lines(zlines) =>
    CursorEscaped(Before)
  | (Delete, _) when ZExp.is_after_lines(zlines) => CursorEscaped(After)
  | (Delete, (prefix, CursorL(_, EmptyLine), suffix)) =>
    switch (suffix) {
    | [] => Failed
    | [line, ...suffix] =>
      let zlines = (prefix, ZExp.place_before_line(line), suffix);
      switch (Statics.syn_zlines(ctx, zlines)) {
      | None => Failed
      | Some(ctx) => Succeeded((zlines, ctx, u_gen))
      };
    }
  | (Backspace, (prefix, CursorL(_, EmptyLine), suffix)) =>
    switch (split_last(prefix)) {
    | None => Failed
    | Some((prefix, line)) =>
      let zlines = (prefix, ZExp.place_after_line(line), suffix);
      switch (Statics.syn_zlines(ctx, zlines)) {
      | None => Failed
      | Some(ctx) => Succeeded((zlines, ctx, u_gen))
      };
    }
  | (Delete, (prefix, zline1, suffix)) when ZExp.is_after_line(zline1) =>
    switch (suffix) {
    | [] => Failed
    | [line2, ...suffix] =>
      switch (line2) {
      | ExpLine(_) => Failed
      | LetLine(_, _, _) => Failed
      | EmptyLine =>
        let zlines = (prefix, zline1, suffix);
        switch (Statics.syn_zlines(ctx, zlines)) {
        | None => Failed
        | Some(ctx) => Succeeded((zlines, ctx, u_gen))
        };
      }
    }
  | (Backspace, (prefix, zline2, suffix)) when ZExp.is_before_line(zline2) =>
    switch (split_last(prefix)) {
    | None => Failed
    | Some((prefix, line1)) =>
      switch (line1) {
      | ExpLine(_) => Failed
      | LetLine(_, _, _) => Failed
      | EmptyLine =>
        let zlines = (prefix, zline2, suffix);
        switch (Statics.syn_zlines(ctx, zlines)) {
        | None => Failed
        | Some(ctx) => Succeeded((zlines, ctx, u_gen))
        };
      }
    }
  /* Construction */
  | (
      Construct(SOp(SSpace)),
      (_, CursorL(OnDelim(_, After), LetLine(_, _, _)), _),
    ) =>
    syn_perform_lines(~ci, ctx, MoveRight, edit_state)
  | (Construct(SLine), (prefix, zline, suffix))
      when ZExp.is_before_line(zline) =>
    let zlines = (prefix @ [EmptyLine], zline, suffix);
    switch (Statics.syn_zlines(ctx, zlines)) {
    | None => Failed
    | Some(ctx) => Succeeded((zlines, ctx, u_gen))
    };
  | (Construct(SLine), (prefix, zline, suffix))
      when ZExp.is_after_line(zline) =>
    let line = ZExp.erase_line(zline);
    let zlines = (
      prefix @ [line],
      ZExp.place_before_line(EmptyLine),
      suffix,
    );
    switch (Statics.syn_zlines(ctx, zlines)) {
    | None => Failed
    | Some(ctx) => Succeeded((zlines, ctx, u_gen))
    };
  | (Construct(_), (_, CursorL(_, LetLine(_, _, _)), _)) => Failed
  | (Construct(_), (prefix, CursorL(_, EmptyLine), suffix)) =>
    let (e, u_gen) = UHExp.new_EmptyHole(u_gen);
    let ze = ZExp.place_before_exp(e);
    syn_perform_lines(
      ~ci,
      ctx,
      a,
      ((prefix, ExpLineZ(ze), suffix), u_gen),
    );
  | (Construct(SLet), (prefix, ExpLineZ(ze), suffix))
      when ZExp.is_before_exp(ze) =>
    let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
    let block = UHExp.wrap_in_block(ZExp.erase(ze));
    let zline = ZExp.LetLineZP(zp, None, block);
    let zlines = (prefix, zline, suffix);
    switch (Statics.syn_zlines(ctx, zlines)) {
    | None => Failed
    | Some(ctx) => Succeeded((zlines, ctx, u_gen))
    };
  | (Construct(SCase), (prefix, ExpLineZ(ze1), suffix))
      when ZExp.is_before_exp(ze1) =>
    let e1 = ZExp.erase(ze1);
    let (rule_block, u_gen) =
      /* check if we need to generate concluding expression */
      switch (split_last(suffix)) {
      | None =>
        let (e2, u_gen) = UHExp.new_EmptyHole(u_gen);
        (UHExp.wrap_in_block(e2), u_gen);
      | Some((lines, last_line)) =>
        switch (last_line) {
        | EmptyLine
        | LetLine(_, _, _) =>
          let (e2, u_gen) = UHExp.new_EmptyHole(u_gen);
          (UHExp.Block(suffix, e2), u_gen);
        | ExpLine(e2) => (UHExp.Block(lines, e2), u_gen)
        }
      };
    let (ze, u_gen) =
      switch (e1) {
      | EmptyHole(_) =>
        let (p, u_gen) = UHPat.new_EmptyHole(u_gen);
        let rule = UHExp.Rule(p, rule_block);
        let scrut_zblock = ZExp.BlockZE([], ze1);
        (ZExp.CaseZE(NotInHole, scrut_zblock, [rule], Some(Hole)), u_gen);
      | _ =>
        let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
        let zrule = ZExp.RuleZP(zp, rule_block);
        let zrules = ZList.singleton(zrule);
        let scrut_block = UHExp.wrap_in_block(e1);
        (ZExp.CaseZR(NotInHole, scrut_block, zrules, Some(Hole)), u_gen);
      };
    let zlines = (prefix, ZExp.ExpLineZ(ze), []);
    switch (Statics.syn_zlines(ctx, zlines)) {
    | None => Failed
    | Some(ctx) => Succeeded((zlines, ctx, u_gen))
    };
  | (
      Construct(SOp(SSpace)),
      (
        prefix,
        ExpLineZ(
          OpSeqZ(
            _,
            CursorE(_, Var(_, InVHole(Keyword(k), _), _)) as ze0,
            EmptyPrefix(opseq_suffix),
          ),
        ),
        suffix,
      ),
    )
      when ZExp.is_after_exp(ze0) =>
    let (e, u_gen) = keyword_suffix_to_exp(opseq_suffix, u_gen);
    let ze = ZExp.place_before_exp(e);
    let zlines = (prefix, ZExp.ExpLineZ(ze), suffix);
    syn_perform_lines(~ci, ctx, keyword_action(k), (zlines, u_gen));
  | (
      Construct(SOp(SSpace)),
      (
        prefix,
        ExpLineZ(CursorE(_, Var(_, InVHole(Keyword(k), _), _)) as ze0),
        suffix,
      ),
    )
      when ZExp.is_after_exp(ze0) =>
    let zlines = (prefix, ZExp.place_before_line(EmptyLine), suffix);
    syn_perform_lines(~ci, ctx, keyword_action(k), (zlines, u_gen));
  /* Zipper Cases */
  | (_, (prefix, zline, suffix)) =>
    switch (Statics.syn_lines(ctx, prefix)) {
    | None => Failed
    | Some(ctx) =>
      switch (syn_perform_line(~ci, ctx, a, (zline, u_gen))) {
      | Failed => Failed
      | CantShift => CantShift
      | CursorEscaped(Before) =>
        switch (ZExp.place_after_lines(prefix)) {
        | None => CursorEscaped(Before)
        | Some((new_prefix, new_zline, _)) =>
          let new_suffix = [ZExp.erase_line(zline), ...suffix];
          let (new_suffix, ctx, u_gen) =
            Statics.syn_fix_holes_lines(ctx, u_gen, new_suffix);
          let zlines = (new_prefix, new_zline, new_suffix);
          Succeeded((zlines, ctx, u_gen));
        }
      | CursorEscaped(After) =>
        let (suffix, ctx, u_gen) =
          Statics.syn_fix_holes_lines(ctx, u_gen, suffix);
        switch (ZExp.place_before_lines(suffix)) {
        | None => CursorEscaped(After)
        | Some(zorphans) =>
          let (new_prefix_end, ctx, u_gen) =
            Statics.syn_fix_holes_line(ctx, u_gen, ZExp.erase_line(zline));
          let new_prefix = prefix @ [new_prefix_end];
          let ((_, new_zline, new_suffix), ctx, u_gen) =
            Statics.syn_fix_holes_zlines(ctx, u_gen, zorphans);
          let zlines = (new_prefix, new_zline, new_suffix);
          Succeeded((zlines, ctx, u_gen));
        };
      | Succeeded(((prefix', zline, suffix'), ctx, u_gen)) =>
        let (suffix, ctx, u_gen) =
          Statics.syn_fix_holes_lines(ctx, u_gen, suffix);
        let zlines = (prefix @ prefix', zline, suffix' @ suffix);
        Succeeded((zlines, ctx, u_gen));
      }
    }
  }
and syn_perform_line =
    (
      ~ci: CursorInfo.t,
      ctx: Contexts.t,
      a: t,
      (zline, u_gen): (ZExp.zline, MetaVarGen.t),
    )
    : result((ZExp.zlines, Contexts.t, MetaVarGen.t)) =>
  switch (a, zline) {
  | (
      _,
      CursorL(OnDelim(_, _) | Staging(_), EmptyLine) |
      CursorL(OnText(_), LetLine(_, _, _)) |
      CursorL(_, ExpLine(_)),
    ) =>
    Failed
  | (_, CursorL(cursor, line)) when !ZExp.is_valid_cursor_line(cursor, line) =>
    Failed
  /* Staging */
  | (ShiftUp | ShiftDown | ShiftLeft | ShiftRight, CursorL(_, _)) =>
    // handled at block level
    Failed
  /* Movement */
  | (MoveLeft, _) =>
    zline
    |> ZExp.move_cursor_left_line
    |> Opt.map_default(~default=CursorEscaped(Before), zline =>
         switch (Statics.syn_line(ctx, zline |> ZExp.erase_line)) {
         | None => Failed
         | Some(ctx) => Succeeded((([], zline, []), ctx, u_gen))
         }
       )
  | (MoveRight, _) =>
    zline
    |> ZExp.move_cursor_right_line
    |> Opt.map_default(~default=CursorEscaped(After), zline =>
         switch (Statics.syn_line(ctx, zline |> ZExp.erase_line)) {
         | None => Failed
         | Some(ctx) => Succeeded((([], zline, []), ctx, u_gen))
         }
       )
  | (MoveTo(_) | MoveToBefore(_) | MoveToPrevHole | MoveToNextHole, _) =>
    /* handled at block or lines level */
    Failed
  /* Backspace & Delete */
  | (Backspace, _) when ZExp.is_before_line(zline) => CursorEscaped(Before)
  | (Delete, _) when ZExp.is_after_line(zline) => CursorEscaped(After)
  | (Backspace | Delete, CursorL(Staging(_), _)) =>
    // handled at blocks level
    Failed
  | (Backspace, CursorL(_, EmptyLine)) => CursorEscaped(Before)
  | (Delete, CursorL(_, EmptyLine)) => CursorEscaped(After)
  /* let x :<| Num = 2   ==>   let x| = 2 */
  | (Backspace, CursorL(OnDelim(1, After), LetLine(p, Some(_), block))) =>
    let (block, ty, u_gen) = Statics.syn_fix_holes_block(ctx, u_gen, block);
    let (p, ctx, u_gen) = Statics.ana_fix_holes_pat(ctx, u_gen, p, ty);
    let zp = ZPat.place_after(p);
    let zline = ZExp.LetLineZP(zp, None, block);
    Succeeded((([], zline, []), ctx, u_gen));
  | (Backspace, CursorL(OnDelim(k, After), LetLine(_, _, _) as li)) =>
    switch (Statics.syn_line(ctx, li)) {
    | None => Failed
    | Some(ctx) =>
      Succeeded((([], CursorL(Staging(k), li), []), ctx, u_gen))
    }
  /* let x <|= 2   ==>   let x| = 2 */
  | (Backspace, CursorL(OnDelim(_, Before), LetLine(_, _, _))) =>
    syn_perform_line(~ci, ctx, MoveLeft, (zline, u_gen))
  /* let x =|> 2   ==>   let x = |2 */
  | (Delete, CursorL(OnDelim(_, After), LetLine(_, _, _))) =>
    syn_perform_line(~ci, ctx, MoveRight, (zline, u_gen))
  /* Delete before delimiter == Backspace after delimiter */
  | (Delete, CursorL(OnDelim(k, Before), LetLine(_, _, _) as li)) =>
    syn_perform_line(
      ~ci,
      ctx,
      Backspace,
      (CursorL(OnDelim(k, After), li), u_gen),
    )
  /* Construction */
  | (Construct(_), CursorL(_, _)) =>
    /* handled at lines level */
    Failed
  | (Construct(SAsc), LetLineZP(zp, None, block)) =>
    switch (Statics.syn_block(ctx, block)) {
    | None => Failed
    | Some(ty) =>
      let p = ZPat.erase(zp);
      switch (Statics.ana_pat(ctx, p, ty)) {
      | None => Failed
      | Some(ctx) =>
        let uty = UHTyp.contract(ty);
        let zty = ZTyp.place_before(uty);
        let zline = ZExp.LetLineZA(p, zty, block);
        Succeeded((([], zline, []), ctx, u_gen));
      };
    }
  | (Construct(SAsc), LetLineZP(zp, Some(uty), block)) =>
    /* just move the cursor over if there is already an ascription */
    let p = ZPat.erase(zp);
    switch (Statics.ana_pat(ctx, p, UHTyp.elab(uty))) {
    | None => Failed
    | Some(ctx) =>
      let zty = ZTyp.place_before(uty);
      let zline = ZExp.LetLineZA(p, zty, block);
      Succeeded((([], zline, []), ctx, u_gen));
    };
  /* Zipper Cases */
  | (_, ExpLineZ(ze)) =>
    switch (Statics.syn_exp(ctx, ZExp.erase(ze))) {
    | None => Failed
    | Some(ty) =>
      switch (syn_perform_exp(~ci, ctx, a, (ze, ty, u_gen))) {
      | (Failed | CantShift | CursorEscaped(_)) as err => err
      | Succeeded((E(ze), _, u_gen)) =>
        let zline = ZExp.prune_empty_hole_line(ExpLineZ(ze));
        Succeeded((([], zline, []), ctx, u_gen));
      | Succeeded((B(zblock), _, u_gen)) =>
        let zlines =
          ZExp.prune_empty_hole_lines(ZExp.zblock_to_zlines(zblock));
        Succeeded((zlines, ctx, u_gen));
      }
    }
  | (_, LetLineZP(zp, ann, block)) =>
    switch (ann) {
    | Some(uty) =>
      let ty = UHTyp.elab(uty);
      switch (ana_perform_pat(ctx, u_gen, a, zp, ty)) {
      | Failed => Failed
      | CantShift => CantShift
      | CursorEscaped(Before) =>
        syn_perform_line(~ci, ctx, MoveLeft, (zline, u_gen))
      | CursorEscaped(After) =>
        syn_perform_line(~ci, ctx, MoveRight, (zline, u_gen))
      | Succeeded((zp, ctx_after, u_gen)) =>
        let p = ZPat.erase(zp);
        let ctx_block = Statics.ctx_for_let(ctx, p, ty, block);
        let (block, u_gen) =
          Statics.ana_fix_holes_block(ctx_block, u_gen, block, ty);
        let zline = ZExp.LetLineZP(zp, ann, block);
        Succeeded((([], zline, []), ctx_after, u_gen));
      };
    | None =>
      switch (Statics.syn_block(ctx, block)) {
      | None => Failed
      | Some(ty) =>
        switch (ana_perform_pat(ctx, u_gen, a, zp, ty)) {
        | Failed => Failed
        | CantShift => CantShift
        | CursorEscaped(Before) =>
          syn_perform_line(~ci, ctx, MoveLeft, (zline, u_gen))
        | CursorEscaped(After) =>
          syn_perform_line(~ci, ctx, MoveRight, (zline, u_gen))
        | Succeeded((zp, ctx, u_gen)) =>
          let (block, _, u_gen) =
            Statics.syn_fix_holes_block(ctx, u_gen, block);
          let zline = ZExp.LetLineZP(zp, ann, block);
          Succeeded((([], zline, []), ctx, u_gen));
        }
      }
    }
  | (_, LetLineZA(p, zann, block)) =>
    switch (perform_ty(a, zann)) {
    | Failed => Failed
    | CantShift => CantShift
    | CursorEscaped(Before) =>
      syn_perform_line(~ci, ctx, MoveLeft, (zline, u_gen))
    | CursorEscaped(After) =>
      syn_perform_line(~ci, ctx, MoveRight, (zline, u_gen))
    | Succeeded(zann) =>
      let ty = UHTyp.elab(ZTyp.erase(zann));
      let (p, ctx_after, u_gen) =
        Statics.ana_fix_holes_pat(ctx, u_gen, p, ty);
      let ctx_block = Statics.ctx_for_let(ctx, p, ty, block);
      let (block, u_gen) =
        Statics.ana_fix_holes_block(ctx_block, u_gen, block, ty);
      let zline = ZExp.LetLineZA(p, zann, block);
      Succeeded((([], zline, []), ctx_after, u_gen));
    }
  | (_, LetLineZE(p, ann, zblock)) =>
    switch (ann) {
    | Some(uty) =>
      let ty = UHTyp.elab(uty);
      let ctx_block =
        Statics.ctx_for_let(ctx, p, ty, ZExp.erase_block(zblock));
      switch (ana_perform_block(~ci, ctx_block, a, (zblock, u_gen), ty)) {
      | Failed => Failed
      | CantShift => CantShift
      | CursorEscaped(Before) =>
        syn_perform_line(~ci, ctx, MoveLeft, (zline, u_gen))
      | CursorEscaped(After) =>
        syn_perform_line(~ci, ctx, MoveRight, (zline, u_gen))
      | Succeeded((zblock, u_gen)) =>
        switch (Statics.ana_pat(ctx, p, ty)) {
        | None => Failed
        | Some(ctx) =>
          let zline = ZExp.LetLineZE(p, ann, zblock);
          Succeeded((([], zline, []), ctx, u_gen));
        }
      };
    | None =>
      let block = ZExp.erase_block(zblock);
      switch (Statics.syn_block(ctx, block)) {
      | None => Failed
      | Some(ty) =>
        switch (syn_perform_block(~ci, ctx, a, (zblock, ty, u_gen))) {
        | Failed => Failed
        | CantShift => CantShift
        | CursorEscaped(Before) =>
          syn_perform_line(~ci, ctx, MoveLeft, (zline, u_gen))
        | CursorEscaped(After) =>
          syn_perform_line(~ci, ctx, MoveRight, (zline, u_gen))
        | Succeeded((zblock, ty, u_gen)) =>
          let (p, ctx, u_gen) = Statics.ana_fix_holes_pat(ctx, u_gen, p, ty);
          let zline = ZExp.LetLineZE(p, ann, zblock);
          Succeeded((([], zline, []), ctx, u_gen));
        }
      };
    }
  | (UpdateApPalette(_), _) => Failed
  }
and syn_perform_exp =
    (
      ~ci: CursorInfo.t,
      ctx: Contexts.t,
      a: t,
      (ze, ty, u_gen) as edit_state: (ZExp.t, HTyp.t, MetaVarGen.t),
    )
    : result((zexp_or_zblock, HTyp.t, MetaVarGen.t)) =>
  switch (a, ze) {
  | (
      _,
      CursorE(
        OnDelim(_, _),
        Var(_, _, _) | NumLit(_, _) | BoolLit(_, _) | ApPalette(_, _, _, _),
      ) |
      CursorE(
        OnText(_),
        EmptyHole(_) | ListNil(_) | Lam(_, _, _, _) | Inj(_, _, _) |
        Case(_, _, _, _) |
        Parenthesized(_) |
        OpSeq(_, _) |
        ApPalette(_, _, _, _),
      ) |
      CursorE(
        Staging(_),
        EmptyHole(_) | Var(_, _, _) | NumLit(_, _) | BoolLit(_, _) |
        ListNil(_) |
        OpSeq(_, _) |
        ApPalette(_, _, _, _),
      ),
    ) =>
    Failed
  | (_, CursorE(cursor, e)) when !ZExp.is_valid_cursor_exp(cursor, e) =>
    Failed
  /* Staging */
  | (ShiftUp | ShiftDown, CursorE(_, _)) =>
    // handled at block level
    Failed
  | (ShiftLeft | ShiftRight, CursorE(OnText(_) | OnDelim(_, _), _)) =>
    Failed
  | (
      ShiftLeft | ShiftRight,
      CursorE(
        Staging(k),
        (Parenthesized(Block([], body)) | Inj(_, _, Block([], body))) as staged,
      ) |
      OpSeqZ(
        _,
        CursorE(
          Staging(k),
          (Parenthesized(Block([], body)) | Inj(_, _, Block([], body))) as staged,
        ),
        _,
      ),
    ) =>
    let shift_optm =
      switch (k, a) {
      | (0, ShiftLeft) => OpSeqUtil.Exp.shift_optm_from_prefix
      | (0, ShiftRight) => OpSeqUtil.Exp.shift_optm_to_prefix
      | (1, ShiftLeft) => OpSeqUtil.Exp.shift_optm_to_suffix
      | (_one, _shift_right) => OpSeqUtil.Exp.shift_optm_from_suffix
      };
    let surround =
      switch (ze) {
      | OpSeqZ(_, _, surround) => Some(surround)
      | _cursor_e => None
      };
    switch (body |> shift_optm(~surround)) {
    | None => CantShift
    | Some((new_body, new_surround)) =>
      let new_ztm =
        ZExp.CursorE(
          Staging(k),
          switch (staged) {
          | Inj(err_status, side, _) =>
            Inj(err_status, side, new_body |> UHExp.wrap_in_block)
          | _parenthesized => Parenthesized(new_body |> UHExp.wrap_in_block)
          },
        );
      let new_ze =
        switch (new_surround) {
        | None => new_ztm
        | Some(surround) => OpSeqUtil.Exp.mk_OpSeqZ(new_ztm, surround)
        };
      let (new_ze, ty, u_gen) =
        Statics.syn_fix_holes_zexp(ctx, u_gen, new_ze);
      Succeeded((E(new_ze), ty, u_gen));
    };
  | (
      ShiftLeft | ShiftRight,
      CursorE(
        Staging(_),
        Parenthesized(_) | Inj(_) | Lam(_, _, _, _) | Case(_, _, _, _),
      ),
    ) =>
    // line shifting is handled at block level
    CantShift
  /* Movement */
  | (MoveTo(path), _) =>
    let e = ZExp.erase(ze);
    switch (Path.follow_exp(path, e)) {
    | None => Failed
    | Some(ze) => Succeeded((E(ze), ty, u_gen))
    };
  | (MoveToBefore(steps), _) =>
    let e = ZExp.erase(ze);
    switch (Path.follow_exp_and_place_before(steps, e)) {
    | None => Failed
    | Some(ze) => Succeeded((E(ze), ty, u_gen))
    };
  | (MoveToPrevHole, _) =>
    let holes = Path.holes_ze(ze, []);
    switch (Path.prev_hole_path(holes)) {
    | None => Failed
    | Some(path) =>
      syn_perform_exp(~ci, ctx, MoveTo(path), (ze, ty, u_gen))
    };
  | (MoveToNextHole, _) =>
    let holes = Path.holes_ze(ze, []);
    switch (Path.next_hole_path(holes)) {
    | None => Failed
    | Some(path) =>
      syn_perform_exp(~ci, ctx, MoveTo(path), (ze, ty, u_gen))
    };
  | (MoveLeft, _) =>
    ZExp.move_cursor_left_exp(ze)
    |> Opt.map_default(~default=CursorEscaped(Before), ze =>
         Succeeded((ZExp.E(ze), ty, u_gen))
       )
  | (MoveRight, _) =>
    ZExp.move_cursor_right_exp(ze)
    |> Opt.map_default(~default=CursorEscaped(After), ze =>
         Succeeded((ZExp.E(ze), ty, u_gen))
       )
  /* Backspace & Deletion */
  | (Backspace, _) when ZExp.is_before_exp(ze) => CursorEscaped(Before)
  | (Delete, _) when ZExp.is_after_exp(ze) => CursorEscaped(After)
  | (Backspace, CursorE(_, EmptyHole(_) as e)) =>
    ZExp.is_after_exp(ze)
      ? Succeeded((E(ZExp.place_before_exp(e)), Hole, u_gen))
      : CursorEscaped(Before)
  | (Delete, CursorE(_, EmptyHole(_) as e)) =>
    ZExp.is_before_exp(ze)
      ? Succeeded((E(ZExp.place_after_exp(e)), Hole, u_gen))
      : CursorEscaped(After)
  | (
      Backspace | Delete,
      CursorE(OnText(_), Var(_, _, _) | NumLit(_, _) | BoolLit(_, _)),
    )
  | (Backspace | Delete, CursorE(OnDelim(_, _), ListNil(_))) =>
    let (ze, u_gen) = ZExp.new_EmptyHole(u_gen);
    Succeeded((E(ze), Hole, u_gen));
  /* ( _ <|)   ==>   ( _| ) */
  /* ... + [k-1] <|+ [k] + ...   ==>   ... + [k-1]| + [k] + ... */
  | (
      Backspace,
      CursorE(
        OnDelim(_, Before),
        Lam(_, _, _, _) | Inj(_, _, _) | Case(_, _, _, _) | Parenthesized(_) |
        OpSeq(_, _),
      ),
    ) =>
    syn_perform_exp(~ci, ctx, MoveLeft, (ze, ty, u_gen))
  /* (|> _ )   ==>   ( |_ ) */
  /* ... + [k-1] +|> [k] + ...   ==>   ... + [k-1] + |[k] + ... */
  | (
      Delete,
      CursorE(
        OnDelim(_, After),
        Lam(_, _, _, _) | Inj(_, _, _) | Case(_, _, _, _) | Parenthesized(_) |
        OpSeq(_, _),
      ),
    ) =>
    syn_perform_exp(~ci, ctx, MoveRight, (ze, ty, u_gen))
  /* Delete before delimiter == Backspace after delimiter */
  | (
      Delete,
      CursorE(
        OnDelim(k, Before),
        (
          Lam(_, _, _, _) | Inj(_, _, _) | Case(_, _, _, _) |
          Parenthesized(_) |
          OpSeq(_, _)
        ) as e,
      ),
    ) =>
    syn_perform_exp(
      ~ci,
      ctx,
      Backspace,
      (CursorE(OnDelim(k, After), e), ty, u_gen),
    )
  /* \x :<| Num . x + 1   ==>   \x| . x + 1 */
  | (Backspace, CursorE(OnDelim(1, After), Lam(_, p, Some(_), block))) =>
    let (p, ctx, u_gen) = Statics.ana_fix_holes_pat(ctx, u_gen, p, Hole);
    let (block, ty2, u_gen) = Statics.syn_fix_holes_block(ctx, u_gen, block);
    let ze = ZExp.LamZP(NotInHole, ZPat.place_after(p), None, block);
    Succeeded((E(ze), Arrow(Hole, ty2), u_gen));
  | (
      Backspace,
      CursorE(
        OnDelim(k, After),
        (
          Lam(_, _, _, _) | Inj(_, _, _) | Case(_, _, _, _) |
          Parenthesized(_)
        ) as e,
      ),
    ) =>
    Succeeded((E(CursorE(Staging(k), e)), ty, u_gen))
  | (
      Backspace | Delete,
      CursorE(Staging(k), Parenthesized(Block(lines, e) as body)),
    ) =>
    let (result, u_gen) =
      switch (ci.frame, lines, e, e |> UHExp.bidelimited) {
      | (ExpFrame(_, None, _), _, _, _)
      | (_, _, OpSeq(_, _), _)
      | (ExpFrame(_, Some(_), _), [], _, true) => (
          body
          |> (
            switch (k) {
            | 0 => ZExp.place_before_block
            | _one => ZExp.place_after_block
            }
          ),
          u_gen,
        )
      | (_exp_frame_some, _, _, _) =>
        let (hole, u_gen) = u_gen |> ZExp.new_EmptyHole;
        (hole |> ZExp.wrap_in_block, u_gen);
      };
    Succeeded((B(result), ty, u_gen));
  | (Backspace | Delete, CursorE(Staging(k), Case(_, scrut, _, _))) =>
    let result =
      scrut
      |> (
        switch (k) {
        | 0 => ZExp.place_before_block
        | _one => ZExp.place_after_block
        }
      );
    let (result, ty, u_gen) =
      Statics.syn_fix_holes_zblock(ctx, u_gen, result);
    Succeeded((B(result), ty, u_gen));
  | (Backspace | Delete, CursorE(Staging(k), Inj(_, _, body))) =>
    let result =
      body
      |> (
        switch (k) {
        | 0 => ZExp.place_before_block
        | _one => ZExp.place_after_block
        }
      );
    let (result, ty, u_gen) =
      Statics.syn_fix_holes_zblock(ctx, u_gen, result);
    Succeeded((B(result), ty, u_gen));
  | (Backspace | Delete, CursorE(Staging(k), Lam(_, _, _, body))) =>
    let result =
      body
      |> (
        switch (k) {
        | 0 => ZExp.place_before_block
        | _one => ZExp.place_after_block
        }
      );
    let (result, ty, u_gen) =
      Statics.syn_fix_holes_zblock(ctx, u_gen, result);
    Succeeded((B(result), ty, u_gen));
  /* TODO consider deletion of type ascription on case */
  | (Backspace, CaseZR(_, _, (_, CursorR(OnDelim(_, Before), _), _), _)) =>
    syn_perform_exp(~ci, ctx, MoveLeft, edit_state)
  | (Delete, CaseZR(_, _, (_, CursorR(OnDelim(_, After), _), _), _)) =>
    syn_perform_exp(~ci, ctx, MoveRight, edit_state)
  // Delete before delim == Backspace after delim
  | (
      Delete,
      CaseZR(
        err_status,
        scrut,
        (prefix, CursorR(OnDelim(k, Before), rule), suffix),
        ann,
      ),
    ) =>
    syn_perform_exp(
      ~ci=ci |> CursorInfo.update_position(OnDelim(k, After)),
      ctx,
      Backspace,
      (
        ZExp.CaseZR(
          err_status,
          scrut,
          (prefix, CursorR(OnDelim(k, After), rule), suffix),
          ann,
        ),
        ty,
        u_gen,
      ),
    )
  | (
      Backspace,
      CaseZR(
        err_status,
        scrut,
        (prefix, CursorR(OnDelim(k, After), rule), suffix),
        ann,
      ),
    ) =>
    Succeeded((
      E(
        CaseZR(
          err_status,
          scrut,
          (prefix, CursorR(Staging(k), rule), suffix),
          ann,
        ),
      ),
      ty,
      u_gen,
    ))
  | (
      Backspace | Delete,
      CaseZR(_, scrut, (prefix, CursorR(Staging(_), _), suffix), ann),
    ) =>
    switch (suffix, prefix |> split_last) {
    | ([], None) =>
      let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
      Succeeded((
        E(CaseZR(NotInHole, scrut, ([], zrule, []), ann)),
        ty,
        u_gen,
      ));
    | ([first, ...rest], _) =>
      let zrule = ZExp.place_before_rule(first);
      let ze = ZExp.CaseZR(NotInHole, scrut, (prefix, zrule, rest), ann);
      Succeeded((E(ze), ty, u_gen));
    | (_, Some((prefix_prefix, prefix_last))) =>
      let zrule = ZExp.place_after_rule(prefix_last);
      let ze =
        ZExp.CaseZR(NotInHole, scrut, (prefix_prefix, zrule, suffix), ann);
      Succeeded((E(ze), ty, u_gen));
    }
  /* ... + [k-1] +<| [k] + ... */
  | (Backspace, CursorE(OnDelim(k, After), OpSeq(_, seq))) =>
    /* validity check at top of switch statement ensures
     * that op between [k-1] and [k] is not Space */
    switch (OperatorSeq.split(k - 1, seq), OperatorSeq.split(k, seq)) {
    /* invalid cursor position */
    | (None, _)
    | (_, None) => Failed
    /* ... + [k-1] +<| _ + ... */
    | (_, Some((EmptyHole(_), surround))) =>
      switch (surround) {
      /* invalid */
      | EmptyPrefix(_) => Failed
      /* ... + [k-1] +<| _   ==>   ... + [k-1]| */
      | EmptySuffix(prefix) =>
        let ze: ZExp.t =
          switch (prefix) {
          | ExpPrefix(e, _) => ZExp.place_after_exp(e)
          | SeqPrefix(seq, _) =>
            let skel = Associator.associate_exp(seq);
            ZExp.place_after_exp(OpSeq(skel, seq));
          };
        let (ze, ty, u_gen) = Statics.syn_fix_holes_zexp(ctx, u_gen, ze);
        Succeeded((E(ze), ty, u_gen));
      /* ... + [k-1] +<| _ + ...   ==>   ... + [k-1]| + ... */
      | BothNonEmpty(prefix, suffix) =>
        let (ze0: ZExp.t, surround: ZExp.opseq_surround) =
          switch (prefix) {
          | ExpPrefix(e, _) => (
              ZExp.place_after_exp(e),
              EmptyPrefix(suffix),
            )
          | SeqPrefix(ExpOpExp(e1, op, e2), _) => (
              ZExp.place_after_exp(e2),
              BothNonEmpty(ExpPrefix(e1, op), suffix),
            )
          | SeqPrefix(SeqOpExp(seq, op, e), _) => (
              ZExp.place_after_exp(e),
              BothNonEmpty(SeqPrefix(seq, op), suffix),
            )
          };
        let skel =
          Associator.associate_exp(
            OperatorSeq.opseq_of_exp_and_surround(ZExp.erase(ze0), surround),
          );
        let ze = ZExp.OpSeqZ(skel, ze0, surround);
        let (ze, ty, u_gen) = Statics.syn_fix_holes_zexp(ctx, u_gen, ze);
        Succeeded((E(ze), ty, u_gen));
      }
    /* ... + _ +<| [k] + ... */
    | (Some((EmptyHole(_), surround)), _) =>
      switch (surround) {
      /* invalid */
      | EmptySuffix(_) => Failed
      /* _ +<| [k] + ...   ==>   |[k] + ... */
      | EmptyPrefix(suffix) =>
        let ze: ZExp.t =
          switch (suffix) {
          | ExpSuffix(_, e) => ZExp.place_before_exp(e)
          | SeqSuffix(_, seq) =>
            let skel = Associator.associate_exp(seq);
            ZExp.place_before_exp(OpSeq(skel, seq));
          };
        let (ze, ty, u_gen) = Statics.syn_fix_holes_zexp(ctx, u_gen, ze);
        Succeeded((E(ze), ty, u_gen));
      /* ... + [k-2] + _ +<| [k] + ...   ==>   ... + [k-2] +| [k] + ... */
      | BothNonEmpty(prefix, suffix) =>
        let seq =
          switch (suffix) {
          | ExpSuffix(_, e) => OperatorSeq.opseq_of_prefix_and_exp(prefix, e)
          | SeqSuffix(_, seq) =>
            OperatorSeq.opseq_of_prefix_and_seq(prefix, seq)
          };
        let skel = Associator.associate_exp(seq);
        let ze = ZExp.CursorE(OnDelim(k - 1, After), OpSeq(skel, seq));
        let (ze, ty, u_gen) = Statics.syn_fix_holes_zexp(ctx, u_gen, ze);
        Succeeded((E(ze), ty, u_gen));
      }
    /* ... + [k-1] +<| [k] + ...   ==>   ... + [k-1]| [k] + ... */
    | (Some((e0, surround)), _) =>
      switch (OperatorSeq.replace_following_op(surround, UHExp.Space)) {
      | None => Failed /* invalid */
      | Some(surround) =>
        let (ze, ty, u_gen) =
          make_and_syn_OpSeqZ(
            ctx,
            u_gen,
            ZExp.place_after_exp(e0),
            surround,
          );
        Succeeded((E(ze), ty, u_gen));
      }
    }
  /* ... + [k-1]  <|_ + [k+1] + ...  ==>   ... + [k-1]| + [k+1] + ... */
  | (
      Backspace,
      OpSeqZ(
        _,
        CursorE(_, EmptyHole(_)) as ze0,
        (
          EmptySuffix(ExpPrefix(_, Space) | SeqPrefix(_, Space)) |
          BothNonEmpty(ExpPrefix(_, Space) | SeqPrefix(_, Space), _)
        ) as surround,
      ),
    )
      when ZExp.is_before_exp(ze0) =>
    switch (surround) {
    | EmptyPrefix(_) => CursorEscaped(Before) /* should never happen */
    | EmptySuffix(prefix) =>
      let e: UHExp.t =
        switch (prefix) {
        | ExpPrefix(e1, _space) => e1
        | SeqPrefix(seq, _space) =>
          let skel = Associator.associate_exp(seq);
          OpSeq(skel, seq);
        };
      let (ze, ty, u_gen) =
        Statics.syn_fix_holes_zexp(ctx, u_gen, ZExp.place_after_exp(e));
      Succeeded((E(ze), ty, u_gen));
    | BothNonEmpty(prefix, suffix) =>
      switch (prefix) {
      | ExpPrefix(e1, _space) =>
        let ze1 = ZExp.place_after_exp(e1);
        let seq = OperatorSeq.opseq_of_exp_and_suffix(e1, suffix);
        let skel = Associator.associate_exp(seq);
        let ze = ZExp.OpSeqZ(skel, ze1, EmptyPrefix(suffix));
        let (ze, ty, u_gen) = Statics.syn_fix_holes_zexp(ctx, u_gen, ze);
        Succeeded((E(ze), ty, u_gen));
      | SeqPrefix(seq, _space) =>
        let (prefix: ZExp.opseq_prefix, e0) =
          switch (seq) {
          | ExpOpExp(e1, op, e2) => (ExpPrefix(e1, op), e2)
          | SeqOpExp(seq, op, e1) => (SeqPrefix(seq, op), e1)
          };
        let ze0 = ZExp.place_after_exp(e0);
        let surround = OperatorSeq.BothNonEmpty(prefix, suffix);
        let seq = OperatorSeq.opseq_of_exp_and_surround(e0, surround);
        let skel = Associator.associate_exp(seq);
        let ze = ZExp.OpSeqZ(skel, ze0, surround);
        let (ze, ty, u_gen) = Statics.syn_fix_holes_zexp(ctx, u_gen, ze);
        Succeeded((E(ze), ty, u_gen));
      }
    }
  /* ... + [k-1] + _|>  [k+1] + ...  ==>   ... + [k-1] + |[k+1] + ... */
  | (
      Delete,
      OpSeqZ(
        _,
        CursorE(_, EmptyHole(_)) as ze0,
        (
          EmptyPrefix(ExpSuffix(Space, _) | SeqSuffix(Space, _)) |
          BothNonEmpty(_, ExpSuffix(Space, _) | SeqSuffix(Space, _))
        ) as surround,
      ),
    )
      when ZExp.is_after_exp(ze0) =>
    switch (surround) {
    | EmptySuffix(_) => CursorEscaped(After) /* should never happen */
    | EmptyPrefix(suffix) =>
      let e =
        switch (suffix) {
        | ExpSuffix(_space, e1) => e1
        | SeqSuffix(_space, seq) =>
          let skel = Associator.associate_exp(seq);
          OpSeq(skel, seq);
        };
      let (ze, ty, u_gen) =
        Statics.syn_fix_holes_zexp(ctx, u_gen, ZExp.place_before_exp(e));
      Succeeded((E(ze), ty, u_gen));
    | BothNonEmpty(prefix, suffix) =>
      switch (suffix) {
      | ExpSuffix(_space, e1) =>
        let ze1 = ZExp.place_before_exp(e1);
        let seq = OperatorSeq.opseq_of_prefix_and_exp(prefix, e1);
        let skel = Associator.associate_exp(seq);
        let ze = ZExp.OpSeqZ(skel, ze1, EmptySuffix(prefix));
        let (ze, ty, u_gen) = Statics.syn_fix_holes_zexp(ctx, u_gen, ze);
        Succeeded((E(ze), ty, u_gen));
      | SeqSuffix(_space, seq) =>
        let (e0, suffix: ZExp.opseq_suffix) =
          switch (seq) {
          | ExpOpExp(e1, op, e2) => (e1, ExpSuffix(op, e2))
          | SeqOpExp(seq, op, e1) => (e1, SeqSuffix(op, seq))
          };
        let ze0 = ZExp.place_before_exp(e0);
        let surround = OperatorSeq.BothNonEmpty(prefix, suffix);
        let seq = OperatorSeq.opseq_of_exp_and_surround(e0, surround);
        let skel = Associator.associate_exp(seq);
        let ze = ZExp.OpSeqZ(skel, ze0, surround);
        let (ze, ty, u_gen) = Statics.syn_fix_holes_zexp(ctx, u_gen, ze);
        Succeeded((E(ze), ty, u_gen));
      }
    }
  /* Construction */
  | (Construct(SLine), CursorE(Staging(k), e)) =>
    let (new_ze, ty, u_gen) =
      Statics.syn_fix_holes_zexp(ctx, u_gen, CursorE(OnDelim(k, After), e));
    Succeeded((E(new_ze), ty, u_gen));
  | (Construct(_), CursorE(Staging(_), _)) => Failed
  | (
      Construct(SOp(SSpace)),
      CursorE(OnDelim(_, After), _) |
      CaseZR(_, _, (_, CursorR(OnDelim(_, After), _), _), _),
    )
      when !ZExp.is_after_exp(ze) =>
    syn_perform_exp(~ci, ctx, MoveRight, edit_state)
  | (Construct(_) as a, CursorE(OnDelim(_, side), _))
      when !ZExp.is_before_exp(ze) && !ZExp.is_after_exp(ze) =>
    let move_then_perform = move_action =>
      switch (syn_perform_exp(~ci, ctx, move_action, edit_state)) {
      | Failed
      | CantShift
      | CursorEscaped(_)
      | Succeeded((B(_), _, _)) => assert(false)
      | Succeeded((E(ze), ty, u_gen)) =>
        CursorInfo.syn_cursor_info(
          ~frame=ci |> CursorInfo.force_get_exp_frame,
          ctx,
          ze,
        )
        |> Opt.map_default(~default=Failed, ci =>
             syn_perform_exp(~ci, ctx, a, (ze, ty, u_gen))
           )
      };
    switch (side) {
    | Before => move_then_perform(MoveLeft)
    | After => move_then_perform(MoveRight)
    };
  | (Construct(SLine), CursorE(_, _))
  | (Construct(SLet), CursorE(_, _)) =>
    /* handled at block or line level */
    Failed
  | (
      Construct(SLine),
      CaseZR(_, e1, (prefix, RuleZP(zp, re), suffix), ann),
    )
      when ZPat.is_before(zp) =>
    let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
    let prev_rule = UHExp.Rule(ZPat.erase(zp), re);
    let suffix = [prev_rule, ...suffix];
    let ze = ZExp.CaseZR(NotInHole, e1, (prefix, zrule, suffix), ann);
    Succeeded((E(ze), ty, u_gen));
  | (
      Construct(SLine),
      CaseZR(_, e1, (prefix, RuleZE(_, ze) as zrule, suffix), ann),
    )
      when ZExp.is_after_block(ze) =>
    let prev_rule = ZExp.erase_rule(zrule);
    let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
    let prefix = prefix @ [prev_rule];
    let ze = ZExp.CaseZR(NotInHole, e1, (prefix, zrule, suffix), ann);
    Succeeded((E(ze), ty, u_gen));
  | (
      Construct(SLine),
      CaseZR(_, e1, (prefix, RuleZP(zp, _) as zrule, suffix), ann),
    )
      when ZPat.is_after(zp) =>
    let prev_rule = ZExp.erase_rule(zrule);
    let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
    let prefix = prefix @ [prev_rule];
    let ze = ZExp.CaseZR(NotInHole, e1, (prefix, zrule, suffix), ann);
    Succeeded((E(ze), ty, u_gen));
  | (Construct(SCase), ze1) when ZExp.is_before_exp(ze1) =>
    let e1 = ZExp.erase(ze1);
    let (ze, u_gen) =
      switch (e1) {
      | EmptyHole(_) =>
        let (rule, u_gen) = UHExp.empty_rule(u_gen);
        (
          ZExp.CaseZE(
            NotInHole,
            ZExp.wrap_in_block(ze1),
            [rule],
            Some(Hole),
          ),
          u_gen,
        );
      | _ =>
        let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
        let zrules = ZList.singleton(zrule);
        (
          ZExp.CaseZR(
            NotInHole,
            UHExp.wrap_in_block(e1),
            zrules,
            Some(Hole),
          ),
          u_gen,
        );
      };
    Succeeded((E(ze), Hole, u_gen));
  | (Construct(SCase), CursorE(_, _)) => Failed
  | (Construct(SParenthesized), CursorE(_, _)) =>
    let zblock = ZExp.BlockZE([], ze);
    Succeeded((E(ParenthesizedZ(zblock)), ty, u_gen));
  | (Construct(SAsc), LamZP(err_status, zp, None, e1)) =>
    let ze =
      ZExp.LamZA(err_status, ZPat.erase(zp), ZTyp.place_before(Hole), e1);
    Succeeded((E(ze), ty, u_gen));
  | (Construct(SAsc), LamZP(err_status, zp, Some(uty1), e1)) =>
    /* just move the cursor over if there is already an ascription */
    let ze =
      ZExp.LamZA(err_status, ZPat.erase(zp), ZTyp.place_before(uty1), e1);
    Succeeded((E(ze), ty, u_gen));
  | (Construct(SAsc), CursorE(_, Case(_, e1, rules, Some(uty)))) =>
    /* just move the cursor over if there is already an ascription */
    let ze = ZExp.CaseZA(NotInHole, e1, rules, ZTyp.place_before(uty));
    Succeeded((E(ze), ty, u_gen));
  | (Construct(SAsc), CursorE(_, _)) => Failed
  | (Construct(SVar(x, cursor)), CursorE(_, EmptyHole(_)))
  | (Construct(SVar(x, cursor)), CursorE(_, Var(_, _, _)))
  | (Construct(SVar(x, cursor)), CursorE(_, NumLit(_, _)))
  | (Construct(SVar(x, cursor)), CursorE(_, BoolLit(_, _))) =>
    if (String.equal(x, "true")) {
      Succeeded((
        E(CursorE(cursor, BoolLit(NotInHole, true))),
        Bool,
        u_gen,
      ));
    } else if (String.equal(x, "false")) {
      Succeeded((
        E(CursorE(cursor, BoolLit(NotInHole, false))),
        Bool,
        u_gen,
      ));
    } else if (Var.is_let(x)) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      Succeeded((
        E(CursorE(cursor, Var(NotInHole, InVHole(Keyword(Let), u), x))),
        Hole,
        u_gen,
      ));
    } else if (Var.is_case(x)) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      Succeeded((
        E(CursorE(cursor, Var(NotInHole, InVHole(Keyword(Case), u), x))),
        Hole,
        u_gen,
      ));
    } else {
      check_valid(
        x,
        {
          let gamma = Contexts.gamma(ctx);
          switch (VarMap.lookup(gamma, x)) {
          | Some(xty) =>
            Succeeded((
              ZExp.E(ZExp.CursorE(cursor, Var(NotInHole, NotInVHole, x))),
              xty,
              u_gen,
            ))
          | None =>
            let (u, u_gen) = MetaVarGen.next(u_gen);
            Succeeded((
              ZExp.E(
                ZExp.CursorE(cursor, Var(NotInHole, InVHole(Free, u), x)),
              ),
              HTyp.Hole,
              u_gen,
            ));
          };
        },
      );
    }
  | (Construct(SVar(_, _)), CursorE(_, _)) => Failed
  | (Construct(SLam), CursorE(_, _) as ze1) =>
    let e1 = ZExp.erase(ze1);
    let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
    let block = UHExp.wrap_in_block(e1);
    let ze = ZExp.LamZP(NotInHole, zp, Some(Hole), block);
    let ty' = HTyp.Arrow(Hole, ty);
    Succeeded((E(ze), ty', u_gen));
  | (Construct(SNumLit(n, cursor)), CursorE(_, EmptyHole(_)))
  | (Construct(SNumLit(n, cursor)), CursorE(_, NumLit(_, _)))
  | (Construct(SNumLit(n, cursor)), CursorE(_, BoolLit(_, _)))
  | (Construct(SNumLit(n, cursor)), CursorE(_, Var(_, _, _))) =>
    Succeeded((E(CursorE(cursor, NumLit(NotInHole, n))), Num, u_gen))
  | (Construct(SNumLit(_, _)), CursorE(_, _)) => Failed
  | (Construct(SInj(side)), CursorE(_, _)) =>
    let zblock = ZExp.BlockZE([], ze);
    let ze' = ZExp.InjZ(NotInHole, side, zblock);
    let ty' =
      switch (side) {
      | L => HTyp.Sum(ty, Hole)
      | R => HTyp.Sum(Hole, ty)
      };
    Succeeded((E(ze'), ty', u_gen));
  | (Construct(SListNil), CursorE(_, EmptyHole(_))) =>
    let ze = ZExp.place_after_exp(ListNil(NotInHole));
    let ty = HTyp.List(Hole);
    Succeeded((E(ze), ty, u_gen));
  | (Construct(SListNil), CursorE(_, _)) => Failed
  | (
      Construct(SOp(SSpace)),
      OpSeqZ(_, CursorE(OnDelim(_, After), _) as ze0, _),
    )
      when !ZExp.is_after_exp(ze0) =>
    syn_perform_exp(~ci, ctx, MoveRight, edit_state)
  | (Construct(SOp(os)), OpSeqZ(_, ze0, surround))
      when ZExp.is_after_exp(ze0) =>
    switch (exp_op_of(os)) {
    | None => Failed
    | Some(op) =>
      let (ze, ty, u_gen) =
        abs_perform_Construct_SOp_After_surround(
          UHExp.new_EmptyHole,
          make_and_syn_OpSeq,
          make_and_syn_OpSeqZ,
          UHExp.is_Space,
          UHExp.Space,
          ZExp.place_before_exp,
          ctx,
          u_gen,
          ZExp.erase(ze0),
          op,
          surround,
        );
      Succeeded((E(ze), ty, u_gen));
    }
  | (Construct(SOp(os)), OpSeqZ(_, ze0, surround))
      when ZExp.is_before_exp(ze0) =>
    switch (exp_op_of(os)) {
    | None => Failed
    | Some(op) =>
      let (ze, ty, u_gen) =
        abs_perform_Construct_SOp_Before_surround(
          UHExp.new_EmptyHole,
          make_and_syn_OpSeq,
          make_and_syn_OpSeqZ,
          UHExp.is_Space,
          UHExp.Space,
          ZExp.place_before_exp,
          ctx,
          u_gen,
          ze0 |> ZExp.erase,
          op,
          surround,
        );
      Succeeded((E(ze), ty, u_gen));
    }
  | (Construct(SOp(os)), CursorE(_, _)) =>
    switch (exp_op_of(os)) {
    | None => Failed
    | Some(op) =>
      if (ZExp.is_before_exp(ze)) {
        let (ze, ty, u_gen) =
          abs_perform_Construct_SOp_Before(
            UHExp.bidelimit,
            UHExp.new_EmptyHole,
            make_and_syn_OpSeq,
            make_and_syn_OpSeqZ,
            UHExp.is_Space,
            ZExp.place_before_exp,
            ctx,
            u_gen,
            ZExp.erase(ze),
            op,
          );
        Succeeded((E(ze), ty, u_gen));
      } else if (ZExp.is_after_exp(ze)) {
        let (ze, ty, u_gen) =
          abs_perform_Construct_SOp_After(
            UHExp.bidelimit,
            UHExp.new_EmptyHole,
            make_and_syn_OpSeq,
            make_and_syn_OpSeqZ,
            UHExp.is_Space,
            ZExp.place_before_exp,
            ctx,
            u_gen,
            ZExp.erase(ze),
            op,
          );
        Succeeded((E(ze), ty, u_gen));
      } else {
        Failed;
      }
    }
  | (Construct(SApPalette(name)), CursorE(_, EmptyHole(_))) =>
    let livelit_ctx = Contexts.livelit_ctx(ctx);
    switch (LivelitCtx.lookup(livelit_ctx, name)) {
    | None => Failed
    | Some(livelit_defn) =>
      let init_model_cmd = livelit_defn.init_model;
      let (init_model, init_splice_info, u_gen) =
        SpliceGenMonad.exec(init_model_cmd, SpliceInfo.empty, u_gen);
      switch (Statics.ana_splice_map(ctx, init_splice_info.splice_map)) {
      | None => Failed
      | Some(splice_ctx) =>
        let expansion_ty = livelit_defn.expansion_ty;
        let expand = livelit_defn.expand;
        let expansion = expand(init_model);
        switch (Statics.ana_block(splice_ctx, expansion, expansion_ty)) {
        | None => Failed
        | Some(_) =>
          Succeeded((
            E(
              ZExp.place_before_exp(
                ApPalette(NotInHole, name, init_model, init_splice_info),
              ),
            ),
            expansion_ty,
            u_gen,
          ))
        };
      };
    };
  | (Construct(SApPalette(_)), CursorE(_, _)) => Failed
  /* TODO
     | (UpdateApPalette(_), CursorE(_, ApPalette(_, _name, _, _hole_data))) =>
        let (_, livelit_ctx) = ctx;
        switch (LivelitCtx.lookup(livelit_ctx, name)) {
        | Some(livelit_defn) =>
          let (q, u_gen') = UHExp.HoleRefs.exec(monad, hole_data, u_gen);
          let (serialized_model, hole_data') = q;
          let expansion_ty = UHExp.LivelitDefinition.expansion_ty(livelit_defn);
          let expansion =
            (UHExp.LivelitDefinition.to_exp(livelit_defn))(serialized_model);
          let (_, hole_map') = hole_data';
          let expansion_ctx =
            UHExp.PaletteHoleData.extend_ctx_with_hole_map(ctx, hole_map');
          switch (Statics.ana(expansion_ctx, expansion, expansion_ty)) {
          | Some(_) =>
            Succeeded((
              CursorE(
                After,
                Tm(
                  NotInHole,
                  ApPalette(name, serialized_model, hole_data'),
                ),
              ),
              expansion_ty,
              u_gen,
            ))
          | None => Failed
          };
        | None => Failed
        }; */
  | (UpdateApPalette(_), CursorE(_, _)) => Failed
  /* Zipper Cases */
  | (_, ParenthesizedZ(zblock)) =>
    switch (syn_perform_block(~ci, ctx, a, (zblock, ty, u_gen))) {
    | Failed => Failed
    | CantShift => CantShift
    | CursorEscaped(Before) =>
      syn_perform_exp(~ci, ctx, MoveLeft, edit_state)
    | CursorEscaped(After) =>
      syn_perform_exp(~ci, ctx, MoveRight, edit_state)
    | Succeeded((ze1', ty', u_gen')) =>
      Succeeded((E(ParenthesizedZ(ze1')), ty', u_gen'))
    }
  | (_, LamZP(_, zp, ann, block)) =>
    let ty1 =
      switch (ann) {
      | Some(uty1) => UHTyp.elab(uty1)
      | None => HTyp.Hole
      };
    switch (ana_perform_pat(ctx, u_gen, a, zp, ty1)) {
    | Failed => Failed
    | CantShift => CantShift
    | CursorEscaped(Before) =>
      syn_perform_exp(~ci, ctx, MoveLeft, edit_state)
    | CursorEscaped(After) =>
      syn_perform_exp(~ci, ctx, MoveRight, edit_state)
    | Succeeded((zp, ctx, u_gen)) =>
      let (block, ty2, u_gen) =
        Statics.syn_fix_holes_block(ctx, u_gen, block);
      let ty = HTyp.Arrow(ty1, ty2);
      let ze = ZExp.LamZP(NotInHole, zp, ann, block);
      Succeeded((E(ze), ty, u_gen));
    };
  | (_, LamZA(_, p, zann, block)) =>
    switch (perform_ty(a, zann)) {
    | Failed => Failed
    | CantShift => CantShift
    | CursorEscaped(Before) =>
      syn_perform_exp(~ci, ctx, MoveLeft, edit_state)
    | CursorEscaped(After) =>
      syn_perform_exp(~ci, ctx, MoveRight, edit_state)
    | Succeeded(zann) =>
      let ty1 = UHTyp.elab(ZTyp.erase(zann));
      let (p, ctx, u_gen) = Statics.ana_fix_holes_pat(ctx, u_gen, p, ty1);
      let (block, ty2, u_gen) =
        Statics.syn_fix_holes_block(ctx, u_gen, block);
      let ze = ZExp.LamZA(NotInHole, p, zann, block);
      Succeeded((E(ze), Arrow(ty1, ty2), u_gen));
    }
  | (_, LamZE(_, p, ann, zblock)) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => Failed
    | Some((_, ty2)) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.elab(uty1)
        | None => HTyp.Hole
        };
      switch (Statics.ana_pat(ctx, p, ty1)) {
      | None => Failed
      | Some(ctx_body) =>
        switch (syn_perform_block(~ci, ctx_body, a, (zblock, ty2, u_gen))) {
        | Failed => Failed
        | CantShift => CantShift
        | CursorEscaped(Before) =>
          syn_perform_exp(~ci, ctx, MoveLeft, edit_state)
        | CursorEscaped(After) =>
          syn_perform_exp(~ci, ctx, MoveRight, edit_state)
        | Succeeded((zblock, ty2, u_gen)) =>
          let ze = ZExp.LamZE(NotInHole, p, ann, zblock);
          Succeeded((E(ze), Arrow(ty1, ty2), u_gen));
        }
      };
    }
  | (_, InjZ(_, side, zblock)) =>
    switch (ty) {
    | Sum(ty1, ty2) =>
      let ty_side = pick_side(side, ty1, ty2);
      switch (syn_perform_block(~ci, ctx, a, (zblock, ty_side, u_gen))) {
      | Failed => Failed
      | CantShift => CantShift
      | CursorEscaped(Before) =>
        syn_perform_exp(~ci, ctx, MoveLeft, edit_state)
      | CursorEscaped(After) =>
        syn_perform_exp(~ci, ctx, MoveRight, edit_state)
      | Succeeded((zblock, ty_side', u_gen)) =>
        let ty' =
          switch (side) {
          | L => HTyp.Sum(ty_side', ty2)
          | R => HTyp.Sum(ty1, ty_side')
          };
        Succeeded((E(InjZ(NotInHole, side, zblock)), ty', u_gen));
      };
    | _ => Failed /* should never happen */
    }
  | (_, OpSeqZ(_, ze0, surround)) =>
    let i = OperatorSeq.surround_prefix_length(surround);
    switch (ZExp.erase(ze)) {
    | OpSeq(skel, seq) =>
      switch (Statics.syn_skel(ctx, skel, seq, Some(i))) {
      | Some((_, Some(mode))) =>
        switch (mode) {
        | AnalyzedAgainst(ty0) =>
          switch (ana_perform_exp(~ci, ctx, a, (ze0, u_gen), ty0)) {
          | Failed => Failed
          | CantShift => CantShift
          | CursorEscaped(Before) =>
            syn_perform_exp(~ci, ctx, MoveLeft, edit_state)
          | CursorEscaped(After) =>
            syn_perform_exp(~ci, ctx, MoveRight, edit_state)
          | Succeeded((ze_zb, u_gen)) =>
            let ze0 =
              switch (ze_zb) {
              | E(ze) => ZExp.bidelimit(ze)
              | B(zblock) =>
                switch (zblock) {
                | BlockZL(_, _)
                | BlockZE([_, ..._], _) => ParenthesizedZ(zblock)
                | BlockZE([], ze) => ze
                }
              };
            let (ze0, surround) = OpSeqUtil.Exp.resurround(ze0, surround);
            let (ze, ty, u_gen) =
              make_and_syn_OpSeqZ(ctx, u_gen, ze0, surround);
            Succeeded((E(ze), ty, u_gen));
          }
        | Synthesized(ty0) =>
          switch (syn_perform_exp(~ci, ctx, a, (ze0, ty0, u_gen))) {
          | Failed => Failed
          | CantShift => CantShift
          | CursorEscaped(Before) =>
            syn_perform_exp(~ci, ctx, MoveLeft, edit_state)
          | CursorEscaped(After) =>
            syn_perform_exp(~ci, ctx, MoveRight, edit_state)
          | Succeeded((ze_or_zblock, _, u_gen)) =>
            let ze0 =
              switch (ze_or_zblock) {
              | E(ze) => ZExp.bidelimit(ze)
              | B(zblock) =>
                switch (zblock) {
                | BlockZL(_, _)
                | BlockZE([_, ..._], _) => ParenthesizedZ(zblock)
                | BlockZE([], ze) => ze
                }
              };
            let (ze0, surround) = OpSeqUtil.Exp.resurround(ze0, surround);
            let (ze, ty, u_gen) =
              make_and_syn_OpSeqZ(ctx, u_gen, ze0, surround);
            Succeeded((E(ze), ty, u_gen));
          }
        }
      | Some(_) => Failed /* should never happen */
      | None => Failed /* should never happen */
      }
    | _ => Failed /* should never happen */
    };
  | (_, ApPaletteZ(_, _name, _serialized_model, _z_hole_data)) => Failed
  /* TODO let (next_lbl, z_nat_map) = z_hole_data;
     let (rest_map, z_data) = z_nat_map;
     let (cell_lbl, cell_data) = z_data;
     let (cell_ty, cell_ze) = cell_data;
     switch (ana_perform_exp(~ci, ctx, a, (cell_ze, u_gen), cell_ty)) {
     | Failed => Failed
     | CantShift => CantShift
     | Succeeded((cell_ze', u_gen')) =>
       let z_hole_data' = (
         next_lbl,
         (rest_map, (cell_lbl, (cell_ty, cell_ze'))),
       );
       Succeeded((
         ApPaletteZ(NotInHole, name, serialized_model, z_hole_data'),
         ty,
         u_gen',
       ));
     }; */
  | (_, CaseZE(_, _, _, None))
  | (_, CaseZR(_, _, _, None)) => Failed
  | (_, CaseZE(_, zblock, rules, Some(uty) as ann)) =>
    switch (Statics.syn_block(ctx, ZExp.erase_block(zblock))) {
    | None => Failed
    | Some(ty1) =>
      switch (syn_perform_block(~ci, ctx, a, (zblock, ty1, u_gen))) {
      | Failed => Failed
      | CantShift => CantShift
      | CursorEscaped(Before) =>
        syn_perform_exp(~ci, ctx, MoveLeft, edit_state)
      | CursorEscaped(After) =>
        syn_perform_exp(~ci, ctx, MoveRight, edit_state)
      | Succeeded((zblock, ty1, u_gen)) =>
        let ty = UHTyp.elab(uty);
        let (rules, u_gen) =
          Statics.ana_fix_holes_rules(ctx, u_gen, rules, ty1, ty);
        let ze = ZExp.CaseZE(NotInHole, zblock, rules, ann);
        Succeeded((E(ze), ty, u_gen));
      }
    }
  | (_, CaseZR(_, block, zrules, Some(uty) as ann)) =>
    switch (Statics.syn_block(ctx, block)) {
    | None => Failed
    | Some(ty1) =>
      switch (ZList.prj_z(zrules)) {
      | CursorR(_, _) => Failed /* handled in earlier case */
      | RuleZP(zp, clause) =>
        switch (ana_perform_pat(ctx, u_gen, a, zp, ty1)) {
        | Failed => Failed
        | CantShift => CantShift
        | CursorEscaped(Before) =>
          syn_perform_exp(~ci, ctx, MoveLeft, edit_state)
        | CursorEscaped(After) =>
          syn_perform_exp(~ci, ctx, MoveRight, edit_state)
        | Succeeded((zp, ctx, u_gen)) =>
          let ty = UHTyp.elab(uty);
          let (clause, u_gen) =
            Statics.ana_fix_holes_block(ctx, u_gen, clause, ty);
          let zrule = ZExp.RuleZP(zp, clause);
          let ze =
            ZExp.CaseZR(
              NotInHole,
              block,
              ZList.replace_z(zrules, zrule),
              ann,
            );
          Succeeded((E(ze), ty, u_gen));
        }
      | RuleZE(p, zclause) =>
        switch (Statics.ana_pat(ctx, p, ty1)) {
        | None => Failed
        | Some(ctx) =>
          let ty = UHTyp.elab(uty);
          switch (ana_perform_block(~ci, ctx, a, (zclause, u_gen), ty)) {
          | Failed => Failed
          | CantShift => CantShift
          | CursorEscaped(Before) =>
            syn_perform_exp(~ci, ctx, MoveLeft, edit_state)
          | CursorEscaped(After) =>
            syn_perform_exp(~ci, ctx, MoveRight, edit_state)
          | Succeeded((zclause, u_gen)) =>
            let zrule = ZExp.RuleZE(p, zclause);
            let ze =
              ZExp.CaseZR(
                NotInHole,
                block,
                ZList.replace_z(zrules, zrule),
                ann,
              );
            Succeeded((E(ze), ty, u_gen));
          };
        }
      }
    }
  | (_, CaseZA(_, block, rules, zann)) =>
    switch (Statics.syn_block(ctx, block)) {
    | None => Failed
    | Some(ty1) =>
      switch (perform_ty(a, zann)) {
      | Failed => Failed
      | CantShift => CantShift
      | CursorEscaped(Before) =>
        syn_perform_exp(~ci, ctx, MoveLeft, edit_state)
      | CursorEscaped(After) =>
        syn_perform_exp(~ci, ctx, MoveRight, edit_state)
      | Succeeded(zann) =>
        let ty = UHTyp.elab(ZTyp.erase(zann));
        let (rules, u_gen) =
          Statics.ana_fix_holes_rules(ctx, u_gen, rules, ty1, ty);
        let ze = ZExp.CaseZA(NotInHole, block, rules, zann);
        Succeeded((E(ze), ty, u_gen));
      }
    }
  /* Invalid actions at expression level */
  | (Construct(SNum), _)
  | (Construct(SBool), _)
  | (Construct(SList), _)
  | (Construct(SWild), _) => Failed
  }
and ana_perform_block =
    (
      ~ci: CursorInfo.t,
      ctx: Contexts.t,
      a: t,
      (zblock, u_gen): (ZExp.zblock, MetaVarGen.t),
      ty: HTyp.t,
    )
    : result((ZExp.zblock, MetaVarGen.t)) =>
  switch (a, zblock) {
  /* Staging */
  | (
      ShiftUp | ShiftDown,
      BlockZL(
        (prefix, CursorL(Staging(3), LetLine(p, ann, def)), suffix),
        e,
      ),
    ) =>
    let shift_line =
      switch (a) {
      | ShiftUp => UHExp.shift_line_to_suffix_block
      | _ => UHExp.shift_line_from_suffix_block(~is_node_terminal=false)
      };
    switch (def |> shift_line(~u_gen, Some(Block(suffix, e)))) {
    | None => CantShift
    | Some((_, None, _)) => assert(false)
    | Some((new_def, Some(Block(new_suffix, new_e)), u_gen)) =>
      let new_zblock =
        ZExp.BlockZL(
          (
            prefix,
            CursorL(Staging(3), LetLine(p, ann, new_def)),
            new_suffix,
          ),
          new_e,
        );
      Succeeded(Statics.ana_fix_holes_zblock(ctx, u_gen, new_zblock, ty));
    };
  | (
      ShiftUp | ShiftDown | ShiftLeft | ShiftRight,
      BlockZL((_, CursorL(Staging(_), LetLine(_, _, _)), _), _),
    ) =>
    CantShift
  | (
      ShiftLeft | ShiftRight,
      BlockZL((_, CursorL(Staging(_), EmptyLine | ExpLine(_)), _), _),
    ) =>
    Failed
  | (
      ShiftUp | ShiftDown,
      BlockZL(
        (
          prefix,
          ExpLineZ(
            CursorE(
              Staging(0),
              (
                Parenthesized(block) | Inj(_, _, block) |
                Case(_, block, _, _)
              ) as e_line,
            ),
          ),
          suffix,
        ),
        e,
      ),
    ) =>
    let shift_line =
      switch (a) {
      | ShiftLeft => UHExp.shift_line_from_prefix
      | _ => UHExp.shift_line_to_prefix
      };
    switch (block |> shift_line(~u_gen, prefix)) {
    | None => CantShift
    | Some((new_prefix, new_block, u_gen)) =>
      let new_e_line =
        switch (e_line) {
        | Inj(err_status, side, _) => UHExp.Inj(err_status, side, new_block)
        | Case(err_status, _, rules, ann) =>
          Case(err_status, new_block, rules, ann)
        | _ => Parenthesized(new_block)
        };
      let new_zblock =
        ZExp.BlockZL(
          (new_prefix, ExpLineZ(CursorE(Staging(0), new_e_line)), suffix),
          e,
        );
      Succeeded(Statics.ana_fix_holes_zblock(ctx, u_gen, new_zblock, ty));
    };
  | (
      ShiftUp | ShiftDown,
      BlockZL(
        (
          prefix,
          ExpLineZ(
            CursorE(
              Staging(1) as cursor,
              (Parenthesized(block) | Inj(_, _, block)) as e_line,
            ),
          ),
          suffix,
        ),
        e,
      ),
    ) =>
    let shift_line =
      switch (a) {
      | ShiftUp => UHExp.shift_line_to_suffix_block
      | _ => UHExp.shift_line_from_suffix_block(~is_node_terminal=true)
      };
    switch (block |> shift_line(~u_gen, Some(Block(suffix, e)))) {
    | None => CantShift
    | Some((new_block, None, u_gen)) =>
      let new_conclusion =
        switch (e_line) {
        | Inj(err_status, side, _) => UHExp.Inj(err_status, side, new_block)
        | _ => Parenthesized(new_block)
        };
      let new_zblock = ZExp.BlockZE(prefix, CursorE(cursor, new_conclusion));
      Succeeded(Statics.ana_fix_holes_zblock(ctx, u_gen, new_zblock, ty));
    | Some((new_block, Some(Block(new_suffix, new_e)), u_gen)) =>
      let new_e_line =
        switch (e_line) {
        | Inj(err_status, side, _) => UHExp.Inj(err_status, side, new_block)
        | _ => Parenthesized(new_block)
        };
      let new_zblock =
        ZExp.BlockZL(
          (prefix, ExpLineZ(CursorE(Staging(1), new_e_line)), new_suffix),
          new_e,
        );
      Succeeded(Statics.ana_fix_holes_zblock(ctx, u_gen, new_zblock, ty));
    };
  | (
      ShiftUp | ShiftDown,
      BlockZL(
        (
          prefix,
          ExpLineZ(
            CursorE(
              Staging(1) as cursor,
              Case(err_status, scrut, rules, None),
            ),
          ),
          suffix,
        ),
        e,
      ),
    ) =>
    switch (rules |> split_last) {
    | None => Failed // shouldn't ever see empty rule list
    | Some((leading_rules, Rule(last_p, last_clause))) =>
      let shift_line =
        switch (a) {
        | ShiftUp => UHExp.shift_line_to_suffix_block
        | _ => UHExp.shift_line_from_suffix_block(~is_node_terminal=true)
        };
      switch (last_clause |> shift_line(~u_gen, Some(Block(suffix, e)))) {
      | None => CantShift
      | Some((new_last_clause, None, u_gen)) =>
        let new_conclusion =
          UHExp.Case(
            err_status,
            scrut,
            leading_rules @ [Rule(last_p, new_last_clause)],
            None,
          );
        let new_zblock =
          ZExp.BlockZE(prefix, CursorE(cursor, new_conclusion));
        Succeeded(Statics.ana_fix_holes_zblock(ctx, u_gen, new_zblock, ty));
      | Some((new_last_clause, Some(Block(new_suffix, new_e)), u_gen)) =>
        let new_zblock =
          ZExp.BlockZL(
            (
              prefix,
              ExpLineZ(
                CursorE(
                  Staging(1),
                  Case(
                    err_status,
                    scrut,
                    leading_rules @ [Rule(last_p, new_last_clause)],
                    None,
                  ),
                ),
              ),
              new_suffix,
            ),
            new_e,
          );
        Succeeded(Statics.ana_fix_holes_zblock(ctx, u_gen, new_zblock, ty));
      };
    }
  | (
      ShiftRight,
      BlockZE(
        leading,
        CursorE(
          Staging(0) as cursor,
          (Parenthesized(Block([], body)) | Inj(_, _, Block([], body))) as staged,
        ),
      ),
    ) =>
    switch (body |> OpSeqUtil.Exp.shift_optm_to_prefix(~surround=None)) {
    | None => CantShift
    | Some((new_body, new_surround)) =>
      let new_ztm =
        ZExp.CursorE(
          cursor,
          switch (staged) {
          | Inj(err_status, side, _) =>
            Inj(err_status, side, new_body |> UHExp.wrap_in_block)
          | _parenthesized => Parenthesized(new_body |> UHExp.wrap_in_block)
          },
        );
      let new_ze =
        switch (new_surround) {
        | None => new_ztm
        | Some(surround) => OpSeqUtil.Exp.mk_OpSeqZ(new_ztm, surround)
        };
      let new_zblock = ZExp.BlockZE(leading, new_ze);
      Succeeded(Statics.ana_fix_holes_zblock(ctx, u_gen, new_zblock, ty));
    }
  | (
      ShiftUp,
      BlockZL(
        (
          leading_prefix,
          ExpLineZ(
            OpSeqZ(
              _,
              CursorE(Staging(1) as cursor, Parenthesized(Block([], body))),
              EmptySuffix(prefix),
            ),
          ),
          leading_suffix,
        ),
        conclusion,
      ),
    ) =>
    // skip over remaining left shifts, then apply ShiftUp to result
    let skipped_body = OpSeqUtil.Exp.prepend(prefix, body);
    let skipped_zblock =
      ZExp.BlockZL(
        (
          leading_prefix,
          ExpLineZ(
            CursorE(cursor, Parenthesized(Block([], skipped_body))),
          ),
          leading_suffix,
        ),
        conclusion,
      );
    ana_perform_block(
      ~ci,
      ctx,
      ShiftUp,
      Statics.ana_fix_holes_zblock(ctx, u_gen, skipped_zblock, ty),
      ty,
    );
  | (
      ShiftDown,
      BlockZL(
        (
          leading_prefix,
          ExpLineZ(
            OpSeqZ(
              _,
              CursorE(Staging(1) as cursor, Parenthesized(Block([], body))),
              EmptyPrefix(suffix),
            ),
          ),
          leading_suffix,
        ),
        conclusion,
      ),
    ) =>
    // skip over remaining right shifts, then apply ShiftDown to result
    let skipped_body = OpSeqUtil.Exp.append(body, suffix);
    let skipped_zblock =
      ZExp.BlockZL(
        (
          leading_prefix,
          ExpLineZ(
            CursorE(cursor, Parenthesized(Block([], skipped_body))),
          ),
          leading_suffix,
        ),
        conclusion,
      );
    ana_perform_block(
      ~ci,
      ctx,
      ShiftDown,
      Statics.ana_fix_holes_zblock(ctx, u_gen, skipped_zblock, ty),
      ty,
    );
  | (
      ShiftUp,
      BlockZE(
        leading,
        OpSeqZ(
          _,
          CursorE(Staging(1) as cursor, Parenthesized(Block([], body))),
          EmptySuffix(prefix),
        ),
      ),
    ) =>
    // skip over remaining left shifts, then apply ShiftUp to result
    let skipped_body = OpSeqUtil.Exp.prepend(prefix, body);
    let skipped_zblock =
      ZExp.BlockZE(
        leading,
        CursorE(cursor, Parenthesized(Block([], skipped_body))),
      );
    ana_perform_block(
      ~ci,
      ctx,
      ShiftUp,
      Statics.ana_fix_holes_zblock(ctx, u_gen, skipped_zblock, ty),
      ty,
    );
  | (
      ShiftUp,
      BlockZE(leading, CursorE(Staging(1) as cursor, Parenthesized(body))),
    ) =>
    switch (body |> UHExp.shift_line_to_suffix_block(~u_gen, None)) {
    | None => CantShift
    | Some((_, None, _)) => assert(false)
    | Some((
        new_body,
        Some(Block(new_suffix_leading, new_suffix_conclusion)),
        u_gen,
      )) =>
      let new_zblock =
        ZExp.BlockZL(
          (
            leading,
            ExpLineZ(CursorE(cursor, Parenthesized(new_body))),
            new_suffix_leading,
          ),
          new_suffix_conclusion,
        );
      Succeeded(Statics.ana_fix_holes_zblock(ctx, u_gen, new_zblock, ty));
    }
  | (
      ShiftLeft,
      BlockZE(
        leading,
        CursorE(
          Staging(1) as cursor,
          (Parenthesized(Block([], body)) | Inj(_, _, Block([], body))) as staged,
        ),
      ),
    ) =>
    switch (body |> OpSeqUtil.Exp.shift_optm_to_suffix(~surround=None)) {
    | None => CantShift
    | Some((new_body, new_surround)) =>
      let new_ztm =
        ZExp.CursorE(
          cursor,
          switch (staged) {
          | Inj(err_status, side, _) =>
            Inj(err_status, side, new_body |> UHExp.wrap_in_block)
          | _parenthesized => Parenthesized(new_body |> UHExp.wrap_in_block)
          },
        );
      let new_ze =
        switch (new_surround) {
        | None => new_ztm
        | Some(surround) => OpSeqUtil.Exp.mk_OpSeqZ(new_ztm, surround)
        };
      let new_zblock = ZExp.BlockZE(leading, new_ze);
      Succeeded(Statics.ana_fix_holes_zblock(ctx, u_gen, new_zblock, ty));
    }
  | (
      ShiftUp | ShiftDown,
      BlockZE(
        leading,
        CursorE(
          Staging(0) as cursor,
          (Parenthesized(block) | Inj(_, _, block) | Case(_, block, _, _)) as conclusion,
        ),
      ),
    ) =>
    let shift_line =
      switch (a) {
      | ShiftUp => UHExp.shift_line_from_prefix
      | _ => UHExp.shift_line_to_prefix
      };
    switch (block |> shift_line(~u_gen, leading)) {
    | None => CantShift
    | Some((new_leading, new_block, u_gen)) =>
      let new_conclusion =
        switch (conclusion) {
        | Inj(err_status, side, _) => UHExp.Inj(err_status, side, new_block)
        | Case(err_status, _, rules, ann) =>
          Case(err_status, new_block, rules, ann)
        | _ => Parenthesized(new_block)
        };
      let new_zblock =
        ZExp.BlockZE(new_leading, CursorE(cursor, new_conclusion));
      Succeeded(Statics.ana_fix_holes_zblock(ctx, u_gen, new_zblock, ty));
    };
  /* Movement */
  | (MoveTo(path), _) =>
    let block = ZExp.erase_block(zblock);
    switch (Path.follow_block(path, block)) {
    | None => Failed
    | Some(zblock) => Succeeded((zblock, u_gen))
    };
  | (MoveToBefore(steps), _) =>
    let block = ZExp.erase_block(zblock);
    switch (Path.follow_block_and_place_before(steps, block)) {
    | None => Failed
    | Some(zblock) => Succeeded((zblock, u_gen))
    };
  | (MoveToPrevHole, _) =>
    switch (Path.prev_hole_path_zblock(zblock)) {
    | None => Failed
    | Some(path) =>
      ana_perform_block(~ci, ctx, MoveTo(path), (zblock, u_gen), ty)
    }
  | (MoveToNextHole, _) =>
    switch (Path.next_hole_path_zblock(zblock)) {
    | None => Failed
    | Some(path) =>
      ana_perform_block(~ci, ctx, MoveTo(path), (zblock, u_gen), ty)
    }
  | (MoveLeft, _) =>
    switch (ZExp.move_cursor_left_block(zblock)) {
    | None => CursorEscaped(Before)
    | Some(zblock) => Succeeded((zblock, u_gen))
    }
  | (MoveRight, _) =>
    switch (ZExp.move_cursor_right_block(zblock)) {
    | None => CursorEscaped(After)
    | Some(zblock) => Succeeded((zblock, u_gen))
    }
  /* Backspace & Delete */
  | (Backspace, _) when ZExp.is_before_block(zblock) =>
    CursorEscaped(Before)
  | (Delete, _) when ZExp.is_after_block(zblock) => CursorEscaped(After)
  | (Delete, BlockZL((prefix, CursorL(_, EmptyLine), []), e)) =>
    let ze = ZExp.place_before_exp(e);
    let zblock = ZExp.BlockZE(prefix, ze);
    Succeeded((zblock, u_gen));
  | (Backspace, BlockZE(leading, zconclusion))
      when ZExp.is_before_exp(zconclusion) =>
    switch (leading |> split_last, zconclusion |> ZExp.erase) {
    | (None, _) => CursorEscaped(Before)
    | (Some((leading_prefix, EmptyLine)), _) =>
      Succeeded((BlockZE(leading_prefix, zconclusion), u_gen))
    | (Some((leading_prefix, ExpLine(e))), EmptyHole(_)) =>
      let new_zconclusion = ZExp.place_after_exp(e);
      Succeeded(
        Statics.ana_fix_holes_zblock(
          ctx,
          u_gen,
          BlockZE(leading_prefix, new_zconclusion),
          ty,
        ),
      );
    | (Some((leading_prefix, leading_last)), conclusion) =>
      let zleading_last = ZExp.place_after_line(leading_last);
      let zblock =
        ZExp.BlockZL((leading_prefix, zleading_last, []), conclusion);
      Succeeded((zblock, u_gen));
    }
  | (Delete, BlockZL((prefix, ExpLineZ(ze), []), EmptyHole(_)))
      when ZExp.is_after_exp(ze) =>
    let zblock = ZExp.BlockZE(prefix, ze);
    Succeeded(Statics.ana_fix_holes_zblock(ctx, u_gen, zblock, ty));
  | (
      Backspace | Delete,
      BlockZL((prefix, CursorL(Staging(k), _), suffix), conclusion),
    ) =>
    let new_zblock: option(ZExp.zblock) =
      switch (ci |> CursorInfo.preserved_child_term_of_node, suffix) {
      | (Some((_, Type(_) | Pattern(_))), _) => None
      | (None, []) =>
        // If deleted line is followed by an empty hole,
        // then they are on the same visual line. Don't bother
        // leaving behind an empty line, instead let the
        // the empty hole take the deleted line's place.
        switch (conclusion) {
        | EmptyHole(_) =>
          Some(BlockZE(prefix, conclusion |> ZExp.place_before_exp))
        | _ =>
          Some(
            BlockZL(
              (prefix, ZExp.place_before_line(EmptyLine), []),
              conclusion,
            ),
          )
        }
      | (None, [_, ..._]) =>
        Some(
          BlockZL(
            (prefix, ZExp.place_before_line(EmptyLine), suffix),
            conclusion,
          ),
        )
      | (Some((_, Expression(block))), _) =>
        let place_cursor =
          // here we're depending on the fact that
          // only let lines can preserve children
          switch (k) {
          | 0
          | 1
          | 2 => ZExp.place_before_block
          | _three => ZExp.place_after_block
          };
        let (inner_prefix, zline, inner_suffix) =
          block
          |> place_cursor
          |> ZExp.zblock_to_zlines
          |> ZExp.prune_empty_hole_lines;
        Some(
          BlockZL(
            (prefix @ inner_prefix, zline, inner_suffix @ suffix),
            conclusion,
          ),
        );
      };
    new_zblock
    |> Opt.map_default(~default=Failed, zblock =>
         Succeeded(Statics.ana_fix_holes_zblock(ctx, u_gen, zblock, ty))
       );
  /* Construction */
  | (
      Construct(SLine),
      BlockZL((prefix, CursorL(Staging(k), line), suffix), e),
    ) =>
    Succeeded(
      Statics.ana_fix_holes_zblock(
        ctx,
        u_gen,
        BlockZL((prefix, CursorL(OnDelim(k, After), line), suffix), e),
        ty,
      ),
    )
  | (
      Construct(SLine),
      BlockZL((prefix, ExpLineZ(CursorE(Staging(k), e_line)), suffix), e),
    ) =>
    Succeeded(
      Statics.ana_fix_holes_zblock(
        ctx,
        u_gen,
        BlockZL(
          (prefix, ExpLineZ(CursorE(OnDelim(k, After), e_line)), suffix),
          e,
        ),
        ty,
      ),
    )
  | (Construct(_), BlockZL((_, CursorL(Staging(_), _), _), _)) => Failed
  | (Construct(SLine), BlockZE(lines, ze)) when ZExp.is_before_exp(ze) =>
    let zblock = ZExp.BlockZE(lines @ [EmptyLine], ze);
    Succeeded((zblock, u_gen));
  | (Construct(SLine), BlockZE(lines, ze)) when ZExp.is_after_exp(ze) =>
    switch (Statics.syn_lines(ctx, lines)) {
    | None => Failed
    | Some(ctx) =>
      let (e, _, u_gen) =
        Statics.syn_fix_holes_exp(ctx, u_gen, ZExp.erase(ze));
      let line = UHExp.prune_empty_hole_line(ExpLine(e));
      let (zhole, u_gen) = ZExp.new_EmptyHole(u_gen);
      let zblock = ZExp.BlockZE(lines @ [line], zhole);
      Succeeded((zblock, u_gen));
    }
  | (Construct(SLet), BlockZE(lines, ze1)) when ZExp.is_before_exp(ze1) =>
    let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
    let block = UHExp.wrap_in_block(ZExp.erase(ze1));
    let zline = ZExp.LetLineZP(zp, None, block);
    let zlines = (lines, zline, []);
    let (e2, u_gen) = UHExp.new_EmptyHole(u_gen);
    let zblock = ZExp.BlockZL(zlines, e2);
    Succeeded((zblock, u_gen));
  | (
      Construct(SCase),
      BlockZL(
        (prefix, (CursorL(_, EmptyLine) | ExpLineZ(_)) as zline, suffix),
        e2,
      ),
    )
      when ZExp.is_before_line(zline) =>
    let (e1, u_gen) =
      switch (zline) {
      | ExpLineZ(ze1) => (ZExp.erase(ze1), u_gen)
      | _ =>
        let (u, u_gen) = MetaVarGen.next(u_gen);
        (EmptyHole(u), u_gen);
      };
    let clause = UHExp.Block(suffix, e2);
    let (ze, u_gen) =
      switch (e1) {
      | EmptyHole(_) =>
        let (p, u_gen) = UHPat.new_EmptyHole(u_gen);
        let rule = UHExp.Rule(p, clause);
        (
          ZExp.CaseZE(
            NotInHole,
            ZExp.BlockZE([], ZExp.place_before_exp(e1)),
            [rule],
            None,
          ),
          u_gen,
        );
      | _ =>
        let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
        let zrule = ZExp.RuleZP(zp, clause);
        let zrules = ZList.singleton(zrule);
        (
          ZExp.CaseZR(NotInHole, UHExp.wrap_in_block(e1), zrules, None),
          u_gen,
        );
      };
    let zblock = ZExp.BlockZE(prefix, ze);
    Succeeded(Statics.ana_fix_holes_zblock(ctx, u_gen, zblock, ty));
  | (
      Construct(SOp(SSpace)),
      BlockZL(
        (
          prefix,
          ExpLineZ(
            OpSeqZ(
              _,
              CursorE(_, Var(_, InVHole(Keyword(k), _), _)) as ze0,
              EmptyPrefix(opseq_suffix),
            ),
          ),
          suffix,
        ),
        e2,
      ),
    )
      when ZExp.is_after_exp(ze0) =>
    let (e, u_gen) = keyword_suffix_to_exp(opseq_suffix, u_gen);
    let ze = ZExp.place_before_exp(e);
    let zlines = (prefix, ZExp.ExpLineZ(ze), suffix);
    let zblock = ZExp.BlockZL(zlines, e2);
    ana_perform_block(~ci, ctx, keyword_action(k), (zblock, u_gen), ty);
  | (
      Construct(SOp(SSpace)),
      BlockZL(
        (
          prefix,
          ExpLineZ(CursorE(_, Var(_, InVHole(Keyword(k), _), _)) as ze0),
          suffix,
        ),
        e2,
      ),
    )
      when ZExp.is_after_exp(ze0) =>
    let zlines = (prefix, ZExp.place_before_line(EmptyLine), suffix);
    let zblock = ZExp.BlockZL(zlines, e2);
    ana_perform_block(~ci, ctx, keyword_action(k), (zblock, u_gen), ty);
  | (
      Construct(SOp(SSpace)),
      BlockZE(
        lines,
        OpSeqZ(
          _,
          CursorE(_, Var(_, InVHole(Keyword(k), _), _)) as ze0,
          EmptyPrefix(suffix),
        ),
      ),
    )
      when ZExp.is_after_exp(ze0) =>
    let (e, u_gen) = keyword_suffix_to_exp(suffix, u_gen);
    let ze = ZExp.place_before_exp(e);
    let zblock = ZExp.BlockZE(lines, ze);
    ana_perform_block(~ci, ctx, keyword_action(k), (zblock, u_gen), ty);
  | (
      Construct(SOp(SSpace)),
      BlockZE(lines, CursorE(_, Var(_, InVHole(Keyword(k), _), _)) as ze0),
    )
      when ZExp.is_after_exp(ze0) =>
    let (ze, u_gen) = ZExp.new_EmptyHole(u_gen);
    let zblock = ZExp.BlockZE(lines, ze);
    ana_perform_block(~ci, ctx, keyword_action(k), (zblock, u_gen), ty);
  /* Zipper Cases */
  | (
      Backspace | Delete | Construct(_) | UpdateApPalette(_) | ShiftLeft |
      ShiftRight |
      ShiftUp |
      ShiftDown,
      BlockZL(zlines, e),
    ) =>
    switch (syn_perform_lines(~ci, ctx, a, (zlines, u_gen))) {
    | Failed => Failed
    | CantShift => CantShift
    | CursorEscaped(Before) => CursorEscaped(Before)
    | CursorEscaped(After) =>
      Succeeded((
        BlockZE(ZExp.erase_lines(zlines), ZExp.place_before_exp(e)),
        u_gen,
      ))
    | Succeeded((zlines, _, u_gen)) =>
      let zblock = ZExp.BlockZL(zlines, e);
      Succeeded(Statics.ana_fix_holes_zblock(ctx, u_gen, zblock, ty));
    }
  | (
      Backspace | Delete | Construct(_) | UpdateApPalette(_) | ShiftLeft |
      ShiftRight |
      ShiftUp |
      ShiftDown,
      BlockZE(lines, ze),
    ) =>
    switch (Statics.syn_lines(ctx, lines)) {
    | None => Failed
    | Some(ctx1) =>
      switch (ana_perform_exp(~ci, ctx1, a, (ze, u_gen), ty)) {
      | Failed => Failed
      | CantShift => CantShift
      | CursorEscaped(Before) =>
        switch (ZExp.place_after_lines(lines)) {
        | None => CursorEscaped(Before)
        | Some(zlines) =>
          Succeeded((BlockZL(zlines, ZExp.erase(ze)), u_gen))
        }
      | CursorEscaped(After) => CursorEscaped(After)
      | Succeeded((E(ze), u_gen)) => Succeeded((BlockZE(lines, ze), u_gen))
      | Succeeded((B(zblock), u_gen)) =>
        switch (zblock) {
        | BlockZL((prefix, zline, suffix), e) =>
          let zblock = ZExp.BlockZL((lines @ prefix, zline, suffix), e);
          Succeeded(Statics.ana_fix_holes_zblock(ctx, u_gen, zblock, ty));
        | BlockZE(ls, ze) =>
          let zblock = ZExp.BlockZE(lines @ ls, ze);
          Succeeded(Statics.ana_fix_holes_zblock(ctx, u_gen, zblock, ty));
        }
      }
    }
  }
and ana_perform_exp =
    (
      ~ci: CursorInfo.t,
      ctx: Contexts.t,
      a: t,
      (ze, u_gen) as edit_state: (ZExp.t, MetaVarGen.t),
      ty: HTyp.t,
    )
    : result((zexp_or_zblock, MetaVarGen.t)) =>
  switch (a, ze) {
  | (
      _,
      CursorE(
        OnDelim(_, _),
        Var(_, _, _) | NumLit(_, _) | BoolLit(_, _) | ApPalette(_, _, _, _),
      ) |
      CursorE(
        OnText(_),
        EmptyHole(_) | ListNil(_) | Lam(_, _, _, _) | Inj(_, _, _) |
        Case(_, _, _, _) |
        Parenthesized(_) |
        OpSeq(_, _) |
        ApPalette(_, _, _, _),
      ) |
      CursorE(
        Staging(_),
        EmptyHole(_) | Var(_, _, _) | NumLit(_, _) | BoolLit(_, _) |
        ListNil(_) |
        OpSeq(_, _) |
        ApPalette(_, _, _, _),
      ),
    ) =>
    Failed
  | (_, CursorE(cursor, e)) when !ZExp.is_valid_cursor_exp(cursor, e) =>
    Failed
  | (_, _) when ZExp.is_inconsistent(ze) =>
    let err = ze |> ZExp.get_err_status_t;
    let ze' = ZExp.set_err_status_t(NotInHole, ze);
    let e' = ZExp.erase(ze');
    switch (Statics.syn_exp(ctx, e')) {
    | None => Failed
    | Some(ty1) =>
      switch (syn_perform_exp(~ci, ctx, a, (ze', ty1, u_gen))) {
      | (Failed | CursorEscaped(_) | CantShift) as result => result
      | Succeeded((ze', ty1', u_gen')) =>
        if (HTyp.consistent(ty1', ty)) {
          Succeeded((ze', u_gen'));
        } else {
          Succeeded((set_err_status_zexp_or_zblock(err, ze'), u_gen'));
        }
      }
    };
  /* Staging */
  | (ShiftUp | ShiftDown, CursorE(_, _)) =>
    // handled at block level
    Failed
  | (ShiftLeft | ShiftRight, CursorE(OnText(_) | OnDelim(_, _), _)) =>
    Failed
  | (
      ShiftLeft | ShiftRight,
      CursorE(
        Staging(k),
        (Parenthesized(Block([], body)) | Inj(_, _, Block([], body))) as staged,
      ) |
      OpSeqZ(
        _,
        CursorE(
          Staging(k),
          (Parenthesized(Block([], body)) | Inj(_, _, Block([], body))) as staged,
        ),
        _,
      ),
    ) =>
    let shift_optm =
      switch (k, a) {
      | (0, ShiftLeft) => OpSeqUtil.Exp.shift_optm_from_prefix
      | (0, ShiftRight) => OpSeqUtil.Exp.shift_optm_to_prefix
      | (1, ShiftLeft) => OpSeqUtil.Exp.shift_optm_to_suffix
      | (_one, _shift_right) => OpSeqUtil.Exp.shift_optm_from_suffix
      };
    let surround =
      switch (ze) {
      | OpSeqZ(_, _, surround) => Some(surround)
      | _cursor_e => None
      };
    switch (body |> shift_optm(~surround)) {
    | None => CantShift
    | Some((new_body, new_surround)) =>
      let new_ztm =
        ZExp.CursorE(
          Staging(k),
          switch (staged) {
          | Inj(err_status, side, _) =>
            Inj(err_status, side, new_body |> UHExp.wrap_in_block)
          | _parenthesized => Parenthesized(new_body |> UHExp.wrap_in_block)
          },
        );
      let new_ze =
        switch (new_surround) {
        | None => new_ztm
        | Some(surround) => OpSeqUtil.Exp.mk_OpSeqZ(new_ztm, surround)
        };
      let (new_ze, u_gen) =
        Statics.ana_fix_holes_zexp(ctx, u_gen, new_ze, ty);
      Succeeded((E(new_ze), u_gen));
    };
  | (
      ShiftLeft | ShiftRight,
      CursorE(
        Staging(_),
        Parenthesized(_) | Inj(_) | Lam(_, _, _, _) | Case(_, _, _, _),
      ),
    ) =>
    // line shifting is handled at block level
    CantShift
  /* Movement */
  | (MoveTo(path), _) =>
    let e = ZExp.erase(ze);
    switch (Path.follow_exp(path, e)) {
    | Some(ze') => Succeeded((E(ze'), u_gen))
    | None => Failed
    };
  | (MoveToBefore(steps), _) =>
    let e = ZExp.erase(ze);
    switch (Path.follow_exp_and_place_before(steps, e)) {
    | Some(ze') => Succeeded((E(ze'), u_gen))
    | None => Failed
    };
  | (MoveToPrevHole, _) =>
    switch (Path.prev_hole_path(Path.holes_ze(ze, []))) {
    | None => Failed
    | Some(path) =>
      ana_perform_exp(~ci, ctx, MoveTo(path), (ze, u_gen), ty)
    }
  | (MoveToNextHole, _) =>
    switch (Path.next_hole_path(Path.holes_ze(ze, []))) {
    | None => Failed
    | Some(path) =>
      ana_perform_exp(~ci, ctx, MoveTo(path), (ze, u_gen), ty)
    }
  | (MoveLeft, _) =>
    ZExp.move_cursor_left_exp(ze)
    |> Opt.map_default(~default=CursorEscaped(Before), ze =>
         Succeeded((ZExp.E(ze), u_gen))
       )
  | (MoveRight, _) =>
    ZExp.move_cursor_right_exp(ze)
    |> Opt.map_default(~default=CursorEscaped(After), ze =>
         Succeeded((ZExp.E(ze), u_gen))
       )
  /* Backspace & Delete */
  | (Backspace, _) when ZExp.is_before_exp(ze) => CursorEscaped(Before)
  | (Delete, _) when ZExp.is_after_exp(ze) => CursorEscaped(After)
  | (Backspace, CursorE(_, EmptyHole(_) as e)) =>
    ZExp.is_after_exp(ze)
      ? Succeeded((E(ZExp.place_before_exp(e)), u_gen))
      : CursorEscaped(Before)
  | (Delete, CursorE(_, EmptyHole(_) as e)) =>
    ZExp.is_before_exp(ze)
      ? Succeeded((E(ZExp.place_after_exp(e)), u_gen))
      : CursorEscaped(After)
  | (
      Backspace | Delete,
      CursorE(OnText(_), Var(_, _, _) | NumLit(_, _) | BoolLit(_, _)),
    )
  | (Backspace | Delete, CursorE(OnDelim(_, _), ListNil(_))) =>
    let (ze, u_gen) = ZExp.new_EmptyHole(u_gen);
    Succeeded((E(ze), u_gen));
  /* ( _ <|)   ==>   ( _| ) */
  /* ... + [k-1] <|+ [k] + ...   ==>   ... + [k-1]| + [k] + ... */
  | (
      Backspace,
      CursorE(
        OnDelim(_, Before),
        Lam(_, _, _, _) | Inj(_, _, _) | Case(_, _, _, _) | Parenthesized(_) |
        OpSeq(_, _),
      ),
    ) =>
    ana_perform_exp(~ci, ctx, MoveLeft, edit_state, ty)
  /* (|> _ )   ==>   ( |_ ) */
  /* ... + [k-1] +|> [k] + ...   ==>   ... + [k-1] + |[k] + ... */
  | (
      Delete,
      CursorE(
        OnDelim(_, After),
        Lam(_, _, _, _) | Inj(_, _, _) | Case(_, _, _, _) | Parenthesized(_) |
        OpSeq(_, _),
      ),
    ) =>
    ana_perform_exp(~ci, ctx, MoveRight, edit_state, ty)
  /* Delete before delimiter == Backspace after delimiter */
  | (
      Delete,
      CursorE(
        OnDelim(k, Before),
        (
          Lam(_, _, _, _) | Inj(_, _, _) | Case(_, _, _, _) |
          Parenthesized(_) |
          OpSeq(_, _)
        ) as e,
      ),
    ) =>
    ana_perform_exp(
      ~ci,
      ctx,
      Backspace,
      (CursorE(OnDelim(k, After), e), u_gen),
      ty,
    )
  /* \x :<| Num . x + 1   ==>   \x| . x + 1 */
  | (Backspace, CursorE(OnDelim(1, After), Lam(_, p, Some(_), block))) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => Failed
    | Some((ty1, ty2)) =>
      let (p, ctx, u_gen) = Statics.ana_fix_holes_pat(ctx, u_gen, p, ty1);
      let (block, u_gen) =
        Statics.ana_fix_holes_block(ctx, u_gen, block, ty2);
      let ze = ZExp.LamZP(NotInHole, ZPat.place_after(p), None, block);
      Succeeded((E(ze), u_gen));
    }
  | (
      Backspace,
      CursorE(
        OnDelim(k, After),
        (
          Lam(_, _, _, _) | Inj(_, _, _) | Case(_, _, _, _) |
          Parenthesized(_)
        ) as e,
      ),
    ) =>
    Succeeded((E(CursorE(Staging(k), e)), u_gen))
  | (
      Backspace | Delete,
      CursorE(Staging(k), Parenthesized(Block(lines, e) as body)),
    ) =>
    let (result, u_gen) =
      switch (ci.frame, lines, e, e |> UHExp.bidelimited) {
      | (ExpFrame(_, None, _), _, _, _)
      | (_, _, OpSeq(_, _), _)
      | (ExpFrame(_, Some(_), _), [], _, true) => (
          body
          |> (
            switch (k) {
            | 0 => ZExp.place_before_block
            | _one => ZExp.place_after_block
            }
          ),
          u_gen,
        )
      | (_exp_frame_some, _, _, _) =>
        let (hole, u_gen) = u_gen |> ZExp.new_EmptyHole;
        (hole |> ZExp.wrap_in_block, u_gen);
      };
    Succeeded((B(result), u_gen));
  | (Backspace | Delete, CursorE(Staging(k), Case(_, scrut, _, _))) =>
    let result =
      scrut
      |> (
        switch (k) {
        | 0 => ZExp.place_before_block
        | _one => ZExp.place_after_block
        }
      );
    let (result, u_gen) =
      Statics.ana_fix_holes_zblock(ctx, u_gen, result, ty);
    Succeeded((B(result), u_gen));
  | (Backspace | Delete, CursorE(Staging(k), Inj(_, _, body))) =>
    let result =
      body
      |> (
        switch (k) {
        | 0 => ZExp.place_before_block
        | _one => ZExp.place_after_block
        }
      );
    let (result, u_gen) =
      Statics.ana_fix_holes_zblock(ctx, u_gen, result, ty);
    Succeeded((B(result), u_gen));
  | (Backspace | Delete, CursorE(Staging(k), Lam(_, _, _, body))) =>
    let result =
      body
      |> (
        switch (k) {
        | 0 => ZExp.place_before_block
        | _one => ZExp.place_after_block
        }
      );
    let (result, u_gen) =
      Statics.ana_fix_holes_zblock(ctx, u_gen, result, ty);
    Succeeded((B(result), u_gen));
  /* TODO consider deletion of type ascription on case */
  | (Backspace, CaseZR(_, _, (_, CursorR(OnDelim(_, Before), _), _), _)) =>
    ana_perform_exp(~ci, ctx, MoveLeft, edit_state, ty)
  | (Delete, CaseZR(_, _, (_, CursorR(OnDelim(_, After), _), _), _)) =>
    ana_perform_exp(~ci, ctx, MoveRight, edit_state, ty)
  // Delete before delim == Backspace after delim
  | (
      Delete,
      CaseZR(
        err_status,
        scrut,
        (prefix, CursorR(OnDelim(k, Before), rule), suffix),
        ann,
      ),
    ) =>
    ana_perform_exp(
      ~ci=ci |> CursorInfo.update_position(OnDelim(k, After)),
      ctx,
      Backspace,
      (
        ZExp.CaseZR(
          err_status,
          scrut,
          (prefix, CursorR(OnDelim(k, After), rule), suffix),
          ann,
        ),
        u_gen,
      ),
      ty,
    )
  | (
      Backspace,
      CaseZR(
        err_status,
        scrut,
        (prefix, CursorR(OnDelim(k, After), rule), suffix),
        ann,
      ),
    ) =>
    Succeeded((
      E(
        CaseZR(
          err_status,
          scrut,
          (prefix, CursorR(Staging(k), rule), suffix),
          ann,
        ),
      ),
      u_gen,
    ))
  | (
      Backspace | Delete,
      CaseZR(_, scrut, (prefix, CursorR(Staging(_), _), suffix), ann),
    ) =>
    switch (suffix, prefix |> split_last) {
    | ([], None) =>
      let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
      Succeeded((
        E(CaseZR(NotInHole, scrut, ([], zrule, []), ann)),
        u_gen,
      ));
    | ([first, ...rest], _) =>
      let zrule = ZExp.place_before_rule(first);
      let ze = ZExp.CaseZR(NotInHole, scrut, (prefix, zrule, rest), ann);
      Succeeded((E(ze), u_gen));
    | (_, Some((prefix_prefix, prefix_last))) =>
      let zrule = ZExp.place_after_rule(prefix_last);
      let ze =
        ZExp.CaseZR(NotInHole, scrut, (prefix_prefix, zrule, suffix), ann);
      Succeeded((E(ze), u_gen));
    }
  /* ... + [k-1] +<| [k] + ... */
  | (Backspace, CursorE(OnDelim(k, After), OpSeq(_, seq))) =>
    /* validity check at top of switch statement ensures
     * that op between [k-1] and [k] is not Space */
    switch (OperatorSeq.split(k - 1, seq), OperatorSeq.split(k, seq)) {
    /* invalid cursor position */
    | (None, _)
    | (_, None) => Failed
    /* ... + [k-1] +<| _ + ... */
    | (_, Some((EmptyHole(_), surround))) =>
      switch (surround) {
      /* invalid */
      | EmptyPrefix(_) => Failed
      /* ... + [k-1] +<| _   ==>   ... + [k-1]| */
      | EmptySuffix(prefix) =>
        let ze: ZExp.t =
          switch (prefix) {
          | ExpPrefix(e, _) => ZExp.place_after_exp(e)
          | SeqPrefix(seq, _) =>
            let skel = Associator.associate_exp(seq);
            ZExp.place_after_exp(OpSeq(skel, seq));
          };
        let (ze, u_gen) = Statics.ana_fix_holes_zexp(ctx, u_gen, ze, ty);
        Succeeded((E(ze), u_gen));
      /* ... + [k-1] +<| _ + ...   ==>   ... + [k-1]| + ... */
      | BothNonEmpty(prefix, suffix) =>
        let (ze0: ZExp.t, surround: ZExp.opseq_surround) =
          switch (prefix) {
          | ExpPrefix(e, _) => (
              ZExp.place_after_exp(e),
              EmptyPrefix(suffix),
            )
          | SeqPrefix(ExpOpExp(e1, op, e2), _) => (
              ZExp.place_after_exp(e2),
              BothNonEmpty(ExpPrefix(e1, op), suffix),
            )
          | SeqPrefix(SeqOpExp(seq, op, e), _) => (
              ZExp.place_after_exp(e),
              BothNonEmpty(SeqPrefix(seq, op), suffix),
            )
          };
        let skel =
          Associator.associate_exp(
            OperatorSeq.opseq_of_exp_and_surround(ZExp.erase(ze0), surround),
          );
        let ze = ZExp.OpSeqZ(skel, ze0, surround);
        let (ze, u_gen) = Statics.ana_fix_holes_zexp(ctx, u_gen, ze, ty);
        Succeeded((E(ze), u_gen));
      }
    /* ... + _ +<| [k] + ... */
    | (Some((EmptyHole(_), surround)), _) =>
      switch (surround) {
      /* invalid */
      | EmptySuffix(_) => Failed
      /* _ +<| [k] + ...   ==>   |[k] + ... */
      | EmptyPrefix(suffix) =>
        let ze: ZExp.t =
          switch (suffix) {
          | ExpSuffix(_, e) => ZExp.place_before_exp(e)
          | SeqSuffix(_, seq) =>
            let skel = Associator.associate_exp(seq);
            ZExp.place_before_exp(OpSeq(skel, seq));
          };
        let (ze, u_gen) = Statics.ana_fix_holes_zexp(ctx, u_gen, ze, ty);
        Succeeded((E(ze), u_gen));
      /* ... + [k-2] + _ +<| [k] + ...   ==>   ... + [k-2] +| [k] + ... */
      | BothNonEmpty(prefix, suffix) =>
        let seq =
          switch (suffix) {
          | ExpSuffix(_, e) => OperatorSeq.opseq_of_prefix_and_exp(prefix, e)
          | SeqSuffix(_, seq) =>
            OperatorSeq.opseq_of_prefix_and_seq(prefix, seq)
          };
        let skel = Associator.associate_exp(seq);
        let ze = ZExp.CursorE(OnDelim(k - 1, After), OpSeq(skel, seq));
        let (ze, u_gen) = Statics.ana_fix_holes_zexp(ctx, u_gen, ze, ty);
        Succeeded((E(ze), u_gen));
      }
    /* ... + [k-1] +<| [k] + ...   ==>   ... + [k-1]| [k] + ... */
    | (Some((e0, surround)), _) =>
      switch (OperatorSeq.replace_following_op(surround, UHExp.Space)) {
      | None => Failed /* invalid */
      | Some(surround) =>
        let (ze, u_gen) =
          make_and_ana_OpSeqZ(
            ctx,
            u_gen,
            ZExp.place_after_exp(e0),
            surround,
            ty,
          );
        Succeeded((E(ze), u_gen));
      }
    }
  /* ... + [k-1]  <|_ + [k+1] + ...  ==>   ... + [k-1]| + [k+1] + ... */
  | (
      Backspace,
      OpSeqZ(
        _,
        CursorE(_, EmptyHole(_)) as ze0,
        (
          EmptySuffix(ExpPrefix(_, Space) | SeqPrefix(_, Space)) |
          BothNonEmpty(ExpPrefix(_, Space) | SeqPrefix(_, Space), _)
        ) as surround,
      ),
    )
      when ZExp.is_before_exp(ze0) =>
    switch (surround) {
    | EmptyPrefix(_) => CursorEscaped(Before) /* should never happen */
    | EmptySuffix(prefix) =>
      let e: UHExp.t =
        switch (prefix) {
        | ExpPrefix(e1, _space) => e1
        | SeqPrefix(seq, _space) =>
          let skel = Associator.associate_exp(seq);
          OpSeq(skel, seq);
        };
      let (ze, u_gen) =
        Statics.ana_fix_holes_zexp(ctx, u_gen, ZExp.place_after_exp(e), ty);
      Succeeded((E(ze), u_gen));
    | BothNonEmpty(prefix, suffix) =>
      switch (prefix) {
      | ExpPrefix(e1, _space) =>
        let ze1 = ZExp.place_after_exp(e1);
        let seq = OperatorSeq.opseq_of_exp_and_suffix(e1, suffix);
        let skel = Associator.associate_exp(seq);
        let ze = ZExp.OpSeqZ(skel, ze1, EmptyPrefix(suffix));
        let (ze, u_gen) = Statics.ana_fix_holes_zexp(ctx, u_gen, ze, ty);
        Succeeded((E(ze), u_gen));
      | SeqPrefix(seq, _space) =>
        let (prefix: ZExp.opseq_prefix, e0) =
          switch (seq) {
          | ExpOpExp(e1, op, e2) => (ExpPrefix(e1, op), e2)
          | SeqOpExp(seq, op, e1) => (SeqPrefix(seq, op), e1)
          };
        let ze0 = ZExp.place_after_exp(e0);
        let surround = OperatorSeq.BothNonEmpty(prefix, suffix);
        let seq = OperatorSeq.opseq_of_exp_and_surround(e0, surround);
        let skel = Associator.associate_exp(seq);
        let ze = ZExp.OpSeqZ(skel, ze0, surround);
        let (ze, u_gen) = Statics.ana_fix_holes_zexp(ctx, u_gen, ze, ty);
        Succeeded((E(ze), u_gen));
      }
    }
  /* ... + [k-1] + _|>  [k+1] + ...  ==>   ... + [k-1] + |[k+1] + ... */
  | (
      Delete,
      OpSeqZ(
        _,
        CursorE(_, EmptyHole(_)) as ze0,
        (
          EmptyPrefix(ExpSuffix(Space, _) | SeqSuffix(Space, _)) |
          BothNonEmpty(_, ExpSuffix(Space, _) | SeqSuffix(Space, _))
        ) as surround,
      ),
    )
      when ZExp.is_after_exp(ze0) =>
    switch (surround) {
    | EmptySuffix(_) => CursorEscaped(After) /* should never happen */
    | EmptyPrefix(suffix) =>
      let e =
        switch (suffix) {
        | ExpSuffix(_space, e1) => e1
        | SeqSuffix(_space, seq) =>
          let skel = Associator.associate_exp(seq);
          OpSeq(skel, seq);
        };
      let (ze, u_gen) =
        Statics.ana_fix_holes_zexp(ctx, u_gen, ZExp.place_before_exp(e), ty);
      Succeeded((E(ze), u_gen));
    | BothNonEmpty(prefix, suffix) =>
      switch (suffix) {
      | ExpSuffix(_space, e1) =>
        let ze1 = ZExp.place_before_exp(e1);
        let seq = OperatorSeq.opseq_of_prefix_and_exp(prefix, e1);
        let skel = Associator.associate_exp(seq);
        let ze = ZExp.OpSeqZ(skel, ze1, EmptySuffix(prefix));
        let (ze, u_gen) = Statics.ana_fix_holes_zexp(ctx, u_gen, ze, ty);
        Succeeded((E(ze), u_gen));
      | SeqSuffix(_space, seq) =>
        let (e0, suffix: ZExp.opseq_suffix) =
          switch (seq) {
          | ExpOpExp(e1, op, e2) => (e1, ExpSuffix(op, e2))
          | SeqOpExp(seq, op, e1) => (e1, SeqSuffix(op, seq))
          };
        let ze0 = ZExp.place_before_exp(e0);
        let surround = OperatorSeq.BothNonEmpty(prefix, suffix);
        let seq = OperatorSeq.opseq_of_exp_and_surround(e0, surround);
        let skel = Associator.associate_exp(seq);
        let ze = ZExp.OpSeqZ(skel, ze0, surround);
        let (ze, u_gen) = Statics.ana_fix_holes_zexp(ctx, u_gen, ze, ty);
        Succeeded((E(ze), u_gen));
      }
    }
  /* Construction */
  | (Construct(SLine), CursorE(Staging(k), e)) =>
    let (new_ze, u_gen) =
      Statics.ana_fix_holes_zexp(
        ctx,
        u_gen,
        CursorE(OnDelim(k, After), e),
        ty,
      );
    Succeeded((E(new_ze), u_gen));
  | (Construct(_), CursorE(Staging(_), _)) => Failed
  | (
      Construct(SOp(SSpace)),
      CursorE(OnDelim(_, After), _) |
      CaseZR(_, _, (_, CursorR(OnDelim(_, After), _), _), _),
    )
      when !ZExp.is_after_exp(ze) =>
    ana_perform_exp(~ci, ctx, MoveRight, edit_state, ty)
  | (Construct(_) as a, CursorE(OnDelim(_, side), _))
      when !ZExp.is_before_exp(ze) && !ZExp.is_after_exp(ze) =>
    let move_then_perform = move_action =>
      switch (ana_perform_exp(~ci, ctx, move_action, edit_state, ty)) {
      | Failed
      | CantShift
      | CursorEscaped(_)
      | Succeeded((B(_), _)) => assert(false)
      | Succeeded((E(ze), u_gen)) =>
        CursorInfo.ana_cursor_info(
          ~frame=ci |> CursorInfo.force_get_exp_frame,
          ctx,
          ze,
          ty,
        )
        |> Opt.map_default(~default=Failed, ci =>
             ana_perform_exp(~ci, ctx, a, (ze, u_gen), ty)
           )
      };
    switch (side) {
    | Before => move_then_perform(MoveLeft)
    | After => move_then_perform(MoveRight)
    };
  | (Construct(SLine), CursorE(_, _))
  | (Construct(SLet), CursorE(_, _)) =>
    /* handled at block or line level */
    Failed
  | (
      Construct(SLine),
      CaseZR(err, e1, (prefix, RuleZP(zp, re), suffix), ann),
    )
      when ZPat.is_before(zp) =>
    let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
    let prev_rule = UHExp.Rule(ZPat.erase(zp), re);
    let suffix = [prev_rule, ...suffix];
    let ze = ZExp.CaseZR(err, e1, (prefix, zrule, suffix), ann);
    Succeeded((E(ze), u_gen));
  | (
      Construct(SLine),
      CaseZR(err, e1, (prefix, RuleZE(_, ze) as zrule, suffix), ann),
    )
      when ZExp.is_after_block(ze) =>
    let prev_rule = ZExp.erase_rule(zrule);
    let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
    let prefix = prefix @ [prev_rule];
    let ze = ZExp.CaseZR(err, e1, (prefix, zrule, suffix), ann);
    Succeeded((E(ze), u_gen));
  | (
      Construct(SLine),
      CaseZR(err, e1, (prefix, RuleZP(zp, _) as zrule, suffix), ann),
    )
      when ZPat.is_after(zp) =>
    let prev_rule = ZExp.erase_rule(zrule);
    let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
    let prefix = prefix @ [prev_rule];
    let ze = ZExp.CaseZR(err, e1, (prefix, zrule, suffix), ann);
    Succeeded((E(ze), u_gen));
  | (Construct(SCase), ze1) when ZExp.is_before_exp(ze1) =>
    let e1 = ZExp.erase(ze1);
    let (ze, u_gen) =
      switch (e1) {
      | EmptyHole(_) =>
        let (rule, u_gen) = UHExp.empty_rule(u_gen);
        (
          ZExp.CaseZE(NotInHole, ZExp.wrap_in_block(ze1), [rule], None),
          u_gen,
        );
      | _ =>
        let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
        let zrules = ZList.singleton(zrule);
        (
          ZExp.CaseZR(NotInHole, UHExp.wrap_in_block(e1), zrules, None),
          u_gen,
        );
      };
    Succeeded((E(ze), u_gen));
  | (Construct(SCase), CursorE(_, _)) => Failed
  | (Construct(SParenthesized), CursorE(_, _)) =>
    Succeeded((E(ParenthesizedZ(ZExp.wrap_in_block(ze))), u_gen))
  | (Construct(SAsc), LamZP(err_status, zp, None, e1)) =>
    let ze =
      ZExp.LamZA(err_status, ZPat.erase(zp), ZTyp.place_before(Hole), e1);
    Succeeded((E(ze), u_gen));
  | (Construct(SAsc), LamZP(err_status, zp, Some(uty1), e1)) =>
    /* just move the cursor over if there is already an ascription */
    let ze =
      ZExp.LamZA(err_status, ZPat.erase(zp), ZTyp.place_before(uty1), e1);
    Succeeded((E(ze), u_gen));
  | (Construct(SAsc), CursorE(_, Case(_, e1, rules, None))) =>
    let ze = ZExp.CaseZA(NotInHole, e1, rules, ZTyp.place_before(Hole));
    Succeeded((E(ze), u_gen));
  | (Construct(SAsc), CursorE(_, Case(_, e1, rules, Some(uty)))) =>
    /* just move the cursor over if there is already an ascription */
    let ze = ZExp.CaseZA(NotInHole, e1, rules, ZTyp.place_before(uty));
    Succeeded((E(ze), u_gen));
  | (Construct(SAsc), CursorE(_, _)) => Failed
  | (Construct(SLam), CursorE(_, _)) =>
    let e = ZExp.erase(ze);
    switch (HTyp.matched_arrow(ty)) {
    | Some((_, ty2)) =>
      let (e, u_gen) = Statics.ana_fix_holes_exp(ctx, u_gen, e, ty2);
      let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
      let ze = ZExp.LamZP(NotInHole, zp, None, UHExp.wrap_in_block(e));
      Succeeded((E(ze), u_gen));
    | None =>
      let (e, _, u_gen) = Statics.syn_fix_holes_exp(ctx, u_gen, e);
      let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
      let (u, u_gen) = MetaVarGen.next(u_gen);
      let ze =
        ZExp.LamZP(
          InHole(TypeInconsistent, u),
          zp,
          None,
          UHExp.wrap_in_block(e),
        );
      Succeeded((E(ze), u_gen));
    };
  | (Construct(SInj(side)), CursorE(_, _) as ze1) =>
    switch (HTyp.matched_sum(ty)) {
    | Some((tyL, tyR)) =>
      let ty1 = pick_side(side, tyL, tyR);
      let (ze1, u_gen) = Statics.ana_fix_holes_zexp(ctx, u_gen, ze1, ty1);
      let ze = ZExp.InjZ(NotInHole, side, ZExp.wrap_in_block(ze1));
      Succeeded((E(ze), u_gen));
    | None =>
      let (ze1, _, u_gen) = Statics.syn_fix_holes_zexp(ctx, u_gen, ze1);
      let (u, u_gen) = MetaVarGen.next(u_gen);
      let ze =
        ZExp.InjZ(
          InHole(TypeInconsistent, u),
          side,
          ZExp.wrap_in_block(ze1),
        );
      Succeeded((E(ze), u_gen));
    }
  | (
      Construct(SOp(SSpace)),
      OpSeqZ(_, CursorE(OnDelim(_, After), _) as ze0, _),
    )
      when !ZExp.is_after_exp(ze0) =>
    ana_perform_exp(~ci, ctx, MoveRight, edit_state, ty)
  | (Construct(SOp(os)), OpSeqZ(_, ze0, surround))
      when ZExp.is_after_exp(ze0) =>
    switch (exp_op_of(os)) {
    | None => Failed
    | Some(op) =>
      let (ze, u_gen) =
        abs_perform_Construct_SOp_After_surround(
          UHExp.new_EmptyHole,
          (ctx, u_gen, cursor, seq) =>
            make_and_ana_OpSeq(ctx, u_gen, cursor, seq, ty),
          (ctx, u_gen, ze, surround) =>
            make_and_ana_OpSeqZ(ctx, u_gen, ze, surround, ty),
          UHExp.is_Space,
          UHExp.Space,
          ZExp.place_before_exp,
          ctx,
          u_gen,
          ZExp.erase(ze0),
          op,
          surround,
        );
      Succeeded((E(ze), u_gen));
    }
  | (Construct(SOp(os)), OpSeqZ(_, ze0, surround))
      when ZExp.is_before_exp(ze0) =>
    switch (exp_op_of(os)) {
    | None => Failed
    | Some(op) =>
      let (ze, u_gen) =
        abs_perform_Construct_SOp_Before_surround(
          UHExp.new_EmptyHole,
          (ctx, u_gen, cursor, seq) =>
            make_and_ana_OpSeq(ctx, u_gen, cursor, seq, ty),
          (ctx, u_gen, ze, surround) =>
            make_and_ana_OpSeqZ(ctx, u_gen, ze, surround, ty),
          UHExp.is_Space,
          UHExp.Space,
          ZExp.place_before_exp,
          ctx,
          u_gen,
          ze0 |> ZExp.erase,
          op,
          surround,
        );
      Succeeded((E(ze), u_gen));
    }
  | (Construct(SOp(os)), CursorE(_, _)) =>
    switch (exp_op_of(os)) {
    | None => Failed
    | Some(op) =>
      if (ZExp.is_before_exp(ze)) {
        let (ze, u_gen) =
          abs_perform_Construct_SOp_Before(
            UHExp.bidelimit,
            UHExp.new_EmptyHole,
            (ctx, u_gen, cursor, seq) =>
              make_and_ana_OpSeq(ctx, u_gen, cursor, seq, ty),
            (ctx, u_gen, ze, surround) =>
              make_and_ana_OpSeqZ(ctx, u_gen, ze, surround, ty),
            UHExp.is_Space,
            ZExp.place_before_exp,
            ctx,
            u_gen,
            ZExp.erase(ze),
            op,
          );
        Succeeded((E(ze), u_gen));
      } else if (ZExp.is_after_exp(ze)) {
        let (ze, u_gen) =
          abs_perform_Construct_SOp_After(
            UHExp.bidelimit,
            UHExp.new_EmptyHole,
            (ctx, u_gen, cursor, seq) =>
              make_and_ana_OpSeq(ctx, u_gen, cursor, seq, ty),
            (ctx, u_gen, ze, surround) =>
              make_and_ana_OpSeqZ(ctx, u_gen, ze, surround, ty),
            UHExp.is_Space,
            ZExp.place_before_exp,
            ctx,
            u_gen,
            ZExp.erase(ze),
            op,
          );
        Succeeded((E(ze), u_gen));
      } else {
        Failed;
      }
    }
  /* Zipper Cases */
  | (_, ParenthesizedZ(zblock)) =>
    switch (ana_perform_block(~ci, ctx, a, (zblock, u_gen), ty)) {
    | Failed => Failed
    | CantShift => CantShift
    | CursorEscaped(Before) =>
      ana_perform_exp(~ci, ctx, MoveLeft, edit_state, ty)
    | CursorEscaped(After) =>
      ana_perform_exp(~ci, ctx, MoveRight, edit_state, ty)
    | Succeeded((zblock, u_gen)) =>
      Succeeded((E(ParenthesizedZ(zblock)), u_gen))
    }
  | (_, LamZP(err, zp, ann, block)) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => Failed
    | Some((ty1_given, ty2)) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.elab(uty1)
        | None => ty1_given
        };
      switch (ana_perform_pat(ctx, u_gen, a, zp, ty1)) {
      | Failed => Failed
      | CantShift => CantShift
      | CursorEscaped(Before) =>
        ana_perform_exp(~ci, ctx, MoveLeft, edit_state, ty)
      | CursorEscaped(After) =>
        ana_perform_exp(~ci, ctx, MoveRight, edit_state, ty)
      | Succeeded((zp, ctx, u_gen)) =>
        let (block, u_gen) =
          Statics.ana_fix_holes_block(ctx, u_gen, block, ty2);
        let ze = ZExp.LamZP(err, zp, ann, block);
        Succeeded((E(ze), u_gen));
      };
    }
  | (_, LamZA(_, p, zann, block)) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => Failed
    | Some((ty1_given, ty2)) =>
      switch (perform_ty(a, zann)) {
      | Failed => Failed
      | CantShift => CantShift
      | CursorEscaped(Before) =>
        ana_perform_exp(~ci, ctx, MoveLeft, edit_state, ty)
      | CursorEscaped(After) =>
        ana_perform_exp(~ci, ctx, MoveRight, edit_state, ty)
      | Succeeded(zann) =>
        let ty1 = UHTyp.elab(ZTyp.erase(zann));
        HTyp.consistent(ty1, ty1_given)
          ? {
            let (p, ctx, u_gen) =
              Statics.ana_fix_holes_pat(ctx, u_gen, p, ty1);
            let (block, u_gen) =
              Statics.ana_fix_holes_block(ctx, u_gen, block, ty2);
            let ze = ZExp.LamZA(NotInHole, p, zann, block);
            Succeeded((E(ze), u_gen));
          }
          : {
            let (p, ctx, u_gen) =
              Statics.ana_fix_holes_pat(ctx, u_gen, p, ty1);
            let (block, _, u_gen) =
              Statics.syn_fix_holes_block(ctx, u_gen, block);
            let (u, u_gen) = MetaVarGen.next(u_gen);
            let ze = ZExp.LamZA(InHole(TypeInconsistent, u), p, zann, block);
            Succeeded((E(ze), u_gen));
          };
      }
    }
  | (_, LamZE(err, p, ann, zblock)) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => Failed
    | Some((ty1_given, ty2)) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.elab(uty1)
        | None => ty1_given
        };
      switch (Statics.ana_pat(ctx, p, ty1)) {
      | None => Failed
      | Some(ctx_body) =>
        switch (ana_perform_block(~ci, ctx_body, a, (zblock, u_gen), ty2)) {
        | Failed => Failed
        | CantShift => CantShift
        | CursorEscaped(Before) =>
          ana_perform_exp(~ci, ctx, MoveLeft, edit_state, ty)
        | CursorEscaped(After) =>
          ana_perform_exp(~ci, ctx, MoveRight, edit_state, ty)
        | Succeeded((zblock, u_gen)) =>
          let ze = ZExp.LamZE(err, p, ann, zblock);
          Succeeded((E(ze), u_gen));
        }
      };
    }
  | (_, InjZ(err, side, zblock)) =>
    switch (HTyp.matched_sum(ty)) {
    | None => Failed
    | Some((ty1, ty2)) =>
      let picked = pick_side(side, ty1, ty2);
      switch (ana_perform_block(~ci, ctx, a, (zblock, u_gen), picked)) {
      | Failed => Failed
      | CantShift => CantShift
      | CursorEscaped(Before) =>
        ana_perform_exp(~ci, ctx, MoveLeft, edit_state, ty)
      | CursorEscaped(After) =>
        ana_perform_exp(~ci, ctx, MoveRight, edit_state, ty)
      | Succeeded((zblock, u_gen)) =>
        Succeeded((E(InjZ(err, side, zblock)), u_gen))
      };
    }
  | (_, CaseZE(_, zblock, rules, ann)) =>
    switch (Statics.syn_block(ctx, ZExp.erase_block(zblock))) {
    | None => Failed
    | Some(ty1) =>
      switch (syn_perform_block(~ci, ctx, a, (zblock, ty1, u_gen))) {
      | Failed => Failed
      | CantShift => CantShift
      | CursorEscaped(Before) =>
        ana_perform_exp(~ci, ctx, MoveLeft, edit_state, ty)
      | CursorEscaped(After) =>
        ana_perform_exp(~ci, ctx, MoveRight, edit_state, ty)
      | Succeeded((zblock, ty1, u_gen)) =>
        let (rules, u_gen) =
          Statics.ana_fix_holes_rules(ctx, u_gen, rules, ty1, ty);
        let ze = ZExp.CaseZE(NotInHole, zblock, rules, ann);
        Succeeded((E(ze), u_gen));
      }
    }
  | (_, CaseZR(_, block, zrules, ann)) =>
    switch (Statics.syn_block(ctx, block)) {
    | None => Failed
    | Some(ty1) =>
      switch (ZList.prj_z(zrules)) {
      | CursorR(_, _) => Failed /* handled in earlier case */
      | RuleZP(zp, clause) =>
        switch (ana_perform_pat(ctx, u_gen, a, zp, ty1)) {
        | Failed => Failed
        | CantShift => CantShift
        | CursorEscaped(Before) =>
          ana_perform_exp(~ci, ctx, MoveLeft, edit_state, ty)
        | CursorEscaped(After) =>
          ana_perform_exp(~ci, ctx, MoveRight, edit_state, ty)
        | Succeeded((zp, ctx, u_gen)) =>
          let (clause, u_gen) =
            Statics.ana_fix_holes_block(ctx, u_gen, clause, ty);
          let zrule = ZExp.RuleZP(zp, clause);
          let ze =
            ZExp.CaseZR(
              NotInHole,
              block,
              ZList.replace_z(zrules, zrule),
              ann,
            );
          Succeeded((E(ze), u_gen));
        }
      | RuleZE(p, zclause) =>
        switch (Statics.ana_pat(ctx, p, ty1)) {
        | None => Failed
        | Some(ctx) =>
          switch (ana_perform_block(~ci, ctx, a, (zclause, u_gen), ty)) {
          | Failed => Failed
          | CantShift => CantShift
          | CursorEscaped(Before) =>
            ana_perform_exp(~ci, ctx, MoveLeft, edit_state, ty)
          | CursorEscaped(After) =>
            ana_perform_exp(~ci, ctx, MoveRight, edit_state, ty)
          | Succeeded((zclause, u_gen)) =>
            let zrule = ZExp.RuleZE(p, zclause);
            let ze =
              ZExp.CaseZR(
                NotInHole,
                block,
                ZList.replace_z(zrules, zrule),
                ann,
              );
            Succeeded((E(ze), u_gen));
          }
        }
      }
    }
  | (_, CaseZA(_, block, rules, zann)) =>
    switch (Statics.syn_block(ctx, block)) {
    | None => Failed
    | Some(ty1) =>
      switch (perform_ty(a, zann)) {
      | Failed => Failed
      | CantShift => CantShift
      | CursorEscaped(Before) =>
        ana_perform_exp(~ci, ctx, MoveLeft, edit_state, ty)
      | CursorEscaped(After) =>
        ana_perform_exp(~ci, ctx, MoveRight, edit_state, ty)
      | Succeeded(zann) =>
        let ty2 = UHTyp.elab(ZTyp.erase(zann));
        let (rules, u_gen) =
          Statics.ana_fix_holes_rules(ctx, u_gen, rules, ty1, ty2);
        let ze = ZExp.CaseZA(NotInHole, block, rules, zann);
        let (ze, u_gen) = Statics.ana_fix_holes_zexp(ctx, u_gen, ze, ty);
        Succeeded((E(ze), u_gen));
      }
    }
  | (_, OpSeqZ(_, ze0, surround)) =>
    let i = OperatorSeq.surround_prefix_length(surround);
    switch (ZExp.erase(ze)) {
    | OpSeq(skel, seq) =>
      switch (Statics.ana_skel(ctx, skel, seq, ty, Some(i))) {
      | Some(Some(mode)) =>
        switch (mode) {
        | Statics.AnalyzedAgainst(ty0) =>
          switch (ana_perform_exp(~ci, ctx, a, (ze0, u_gen), ty0)) {
          | Failed => Failed
          | CantShift => CantShift
          | CursorEscaped(Before) =>
            ana_perform_exp(~ci, ctx, MoveLeft, edit_state, ty)
          | CursorEscaped(After) =>
            ana_perform_exp(~ci, ctx, MoveRight, edit_state, ty)
          | Succeeded((ze_zb, u_gen)) =>
            let ze0 =
              switch (ze_zb) {
              | E(ze) => ZExp.bidelimit(ze)
              | B(zblock) =>
                switch (zblock) {
                | BlockZL(_, _)
                | BlockZE([_, ..._], _) => ParenthesizedZ(zblock)
                | BlockZE([], ze) => ze
                }
              };
            let (ze0, surround) = OpSeqUtil.Exp.resurround(ze0, surround);
            Succeeded((E(OpSeqZ(skel, ze0, surround)), u_gen));
          }
        | Statics.Synthesized(ty0) =>
          switch (syn_perform_exp(~ci, ctx, a, (ze0, ty0, u_gen))) {
          | Failed => Failed
          | CantShift => CantShift
          | CursorEscaped(Before) =>
            ana_perform_exp(~ci, ctx, MoveLeft, edit_state, ty)
          | CursorEscaped(After) =>
            ana_perform_exp(~ci, ctx, MoveRight, edit_state, ty)
          | Succeeded((ze_or_zblock, _, u_gen)) =>
            let ze0 =
              switch (ze_or_zblock) {
              | E(ze) => ZExp.bidelimit(ze)
              | B(zblock) =>
                switch (zblock) {
                | BlockZL(_, _)
                | BlockZE([_, ..._], _) => ParenthesizedZ(zblock)
                | BlockZE([], ze) => ze
                }
              };
            let (ze0, surround) = OpSeqUtil.Exp.resurround(ze0, surround);
            let (ze, u_gen) =
              make_and_ana_OpSeqZ(ctx, u_gen, ze0, surround, ty);
            Succeeded((E(ze), u_gen));
          }
        }
      | Some(_) => Failed /* should never happen */
      | None => Failed /* should never happen */
      }
    | _ => Failed /* should never happen */
    };
  /* Subsumption */
  | (UpdateApPalette(_), _)
  | (Construct(SApPalette(_)), _)
  | (Construct(SLine), _)
  | (Construct(SVar(_, _)), _)
  | (Construct(SNumLit(_, _)), _)
  | (Construct(SListNil), _)
  | (_, ApPaletteZ(_, _, _, _)) =>
    ana_perform_exp_subsume(~ci, ctx, a, (ze, u_gen), ty)
  /* Invalid actions at expression level */
  | (Construct(SNum), _)
  | (Construct(SBool), _)
  | (Construct(SList), _)
  | (Construct(SWild), _) => Failed
  }
and ana_perform_exp_subsume =
    (
      ~ci: CursorInfo.t,
      ctx: Contexts.t,
      a: t,
      (ze, u_gen): (ZExp.t, MetaVarGen.t),
      ty: HTyp.t,
    )
    : result((zexp_or_zblock, MetaVarGen.t)) =>
  switch (Statics.syn_exp(ctx, ZExp.erase(ze))) {
  | None => Failed
  | Some(ty1) =>
    switch (syn_perform_exp(~ci, ctx, a, (ze, ty1, u_gen))) {
    | (Failed | CantShift | CursorEscaped(_)) as err => err
    | Succeeded((ze_zb, ty1, u_gen)) =>
      if (HTyp.consistent(ty, ty1)) {
        Succeeded((ze_zb, u_gen));
      } else {
        let (ze_zb, u_gen) = make_zexp_or_zblock_inconsistent(u_gen, ze_zb);
        Succeeded((ze_zb, u_gen));
      }
    }
  };

let can_perform =
    (
      ctx: Contexts.t,
      edit_state: (ZExp.zblock, HTyp.t, MetaVarGen.t),
      ci: CursorInfo.t,
      a: t,
    )
    : bool =>
  switch (a) {
  | Construct(SParenthesized) => true
  | Construct(SLine)
  | Construct(SLet)
  | Construct(SCase) =>
    switch (ci.node) {
    | Line(_) => true
    | Exp(_) => true
    | Rule(_) => false
    | Pat(_) => false
    | Typ(_) => false
    }
  | Construct(SInj(_)) =>
    switch (ci.node) {
    | Line(_) => true
    | Exp(_) => true
    | Rule(_) => false
    | Pat(_) => true
    | Typ(_) => false
    }
  | Construct(SListNil) =>
    switch (ci.node) {
    | Line(EmptyLine) => true
    | Line(ExpLine(EmptyHole(_))) => true
    | Line(_) => false
    | Exp(EmptyHole(_)) => true
    | Exp(_) => false
    | Pat(EmptyHole(_)) => true
    | Pat(_) => false
    | Typ(_) => false
    | Rule(_) => false
    }
  | Construct(SOp(SArrow))
  | Construct(SOp(SVBar))
  | Construct(SList) =>
    switch (ci.node) {
    | Typ(_) => true
    | Line(_)
    | Exp(_)
    | Rule(_)
    | Pat(_) => false
    }
  | Construct(SAsc)
  | Construct(SApPalette(_))
  | Construct(SLam)
  | Construct(SVar(_, _)) /* see can_enter_varchar below */
  | Construct(SWild)
  | Construct(SNumLit(_, _)) /* see can_enter_numeral below */
  | Construct(SOp(_))
  | Construct(SNum) /* TODO enrich cursor_info to allow simplifying these type cases */
  | Construct(SBool) /* TODO enrich cursor_info to allow simplifying these type cases */
  | MoveTo(_)
  | MoveToBefore(_)
  | MoveToNextHole
  | MoveToPrevHole
  | MoveLeft
  | MoveRight
  | UpdateApPalette(_)
  | Delete
  | Backspace
  | ShiftLeft
  | ShiftRight
  | ShiftUp
  | ShiftDown =>
    _TEST_PERFORM
      ? switch (syn_perform_block(~ci, ctx, a, edit_state)) {
        | Succeeded(_) => true
        | CantShift
        | CursorEscaped(_)
        | Failed => false
        }
      : true
  };

let can_enter_varchar = (ci: CursorInfo.t): bool =>
  switch (ci.node) {
  | Line(EmptyLine)
  | Line(ExpLine(EmptyHole(_)))
  | Exp(Var(_, _, _))
  | Exp(EmptyHole(_))
  | Exp(BoolLit(_, _))
  | Pat(Var(_, _, _))
  | Pat(EmptyHole(_))
  | Pat(BoolLit(_, _)) => true
  | Exp(NumLit(_, _))
  | Pat(NumLit(_, _)) =>
    switch (ci.position) {
    | OnText(_) => true
    | _ => false
    }
  | Line(_)
  | Exp(_)
  | Rule(_)
  | Pat(_)
  | Typ(_) => false
  };

let can_enter_numeral = (ci: CursorInfo.t): bool =>
  switch (ci.node) {
  | Line(EmptyLine)
  | Line(ExpLine(EmptyHole(_)))
  | Exp(NumLit(_, _))
  | Exp(EmptyHole(_))
  | Pat(NumLit(_, _))
  | Pat(EmptyHole(_)) => true
  | Line(_)
  | Exp(_)
  | Rule(_)
  | Pat(_)
  | Typ(_) => false
  };

let can_construct_palette = (ci: CursorInfo.t): bool =>
  switch (ci.node) {
  | Line(EmptyLine)
  | Line(ExpLine(EmptyHole(_)))
  | Exp(EmptyHole(_)) => true
  | _ => false
  };
