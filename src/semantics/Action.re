let _TEST_PERFORM = false;
open SemanticsCommon;
open HazelUtil;

[@deriving show({with_path: false})]
type op_shape =
  | SPlus
  | STimes
  | SLessThan
  | SSpace
  | SComma
  | SArrow
  | SVBar
  | SCons;

let ty_op_of = (os: op_shape): option(UHTyp.op) =>
  switch (os) {
  | SArrow => Some(Arrow)
  | SComma => Some(Prod)
  | SVBar => Some(Sum)
  | SPlus
  | STimes
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
  | STimes => Some(Times)
  | SLessThan => Some(LessThan)
  | SSpace => Some(Space)
  | SComma => Some(Comma)
  | SCons => Some(Cons)
  | SArrow
  | SVBar => None
  };

let op_shape_of_exp_op = (op: UHExp.op): op_shape =>
  switch (op) {
  | Plus => SPlus
  | Times => STimes
  | LessThan => SLessThan
  | Space => SSpace
  | Comma => SComma
  | Cons => SCons
  };

[@deriving show({with_path: false})]
type shape =
  | SParenthesized
  /* type shapes */
  | SNum
  | SBool
  | SList
  /* expression shapes */
  | SAsc
  | SVar(Var.t, ZExp.cursor_side)
  | SLam
  | SNumLit(int, ZExp.cursor_side)
  | SListNil
  | SInj(inj_side)
  | SLet
  | SLine
  | SCase
  | SOp(op_shape)
  | SApPalette(PaletteName.t)
  /* pattern-only shapes */
  | SWild;

[@deriving show({with_path: false})]
type t =
  | MoveTo(Path.t)
  | MoveToNextHole
  | MoveToPrevHole
  | UpdateApPalette(SpliceGenMonad.t(SerializedModel.t))
  | Delete
  | Backspace
  | Construct(shape);

let make_ty_OpSeqZ = (zty0: ZTyp.t, surround: ZTyp.opseq_surround): ZTyp.t => {
  let uty0 = ZTyp.erase(zty0);
  let seq = OperatorSeq.opseq_of_exp_and_surround(uty0, surround);
  let skel = Associator.associate_ty(seq);
  OpSeqZ(skel, zty0, surround);
};

let rec perform_ty = (a: t, zty: ZTyp.t): option(ZTyp.t) =>
  switch (a, zty) {
  /* Movement */
  | (MoveTo(path), _) =>
    let ty = ZTyp.erase(zty);
    Path.follow_ty(path, ty);
  | (MoveToPrevHole, _) =>
    switch (Path.prev_hole_path(Path.holes_zty(zty, []))) {
    | None => None
    | Some(path) => perform_ty(MoveTo(path), zty)
    }
  | (MoveToNextHole, _) =>
    switch (Path.next_hole_path(Path.holes_zty(zty, []))) {
    | None => None
    | Some(path) =>
      /* [debug] let path = Helper.log_path path in */
      perform_ty(MoveTo(path), zty)
    }
  /* Backspace and Delete */
  | (Backspace, CursorT(After, uty))
  | (Backspace, CursorT(In(_), uty)) => Some(CursorT(Before, Hole))
  | (Backspace, CursorT(Before, _)) => None
  | (Delete, CursorT(Before, uty))
  | (Delete, CursorT(In(_), uty)) =>
    switch (uty) {
    | Hole => Some(CursorT(After, uty))
    | _ => Some(CursorT(Before, Hole))
    }
  | (Delete, CursorT(After, uty)) => None
  | (Backspace, OpSeqZ(_, CursorT(Before, uty0) as zty0, surround)) =>
    switch (surround) {
    | EmptyPrefix(_) => None
    | EmptySuffix(prefix) =>
      switch (prefix) {
      | ExpPrefix(uty1, op1) =>
        switch (uty0) {
        | Hole =>
          /* uty1 op1 |_ -> uty1| */
          Some(CursorT(After, uty1))
        | _ =>
          /* uty1 op1 |uty0 -> |uty0 */
          Some(zty0)
        }
      | SeqPrefix(seq1, op1) =>
        let (uty1, prefix') = OperatorSeq.split_tail(seq1);
        switch (uty0) {
        | Hole =>
          /* prefix' uty1 op1 |_ --> prefix' uty1| */
          let surround' = OperatorSeq.EmptySuffix(prefix');
          let ze1 = ZTyp.CursorT(After, uty1);
          Some(make_ty_OpSeqZ(ze1, surround'));
        | _ =>
          /* prefix' uty1 op |uty0 --> prefix' |uty0 */
          let surround' = OperatorSeq.EmptySuffix(prefix');
          Some(make_ty_OpSeqZ(zty0, surround'));
        };
      }
    | BothNonEmpty(prefix, suffix) =>
      switch (prefix) {
      | ExpPrefix(uty1, op1) =>
        switch (uty0) {
        | Hole =>
          /* uty1 op1 |_ suffix -> uty1| suffix */
          let surround' = OperatorSeq.EmptyPrefix(suffix);
          let zty1 = ZTyp.CursorT(After, uty1);
          Some(make_ty_OpSeqZ(zty1, surround'));
        | _ =>
          /* uty1 op1 |uty0 suffix -> |uty0 suffix */
          let surround' = OperatorSeq.EmptyPrefix(suffix);
          Some(make_ty_OpSeqZ(zty0, surround'));
        }
      | SeqPrefix(seq1, op1) =>
        let (uty1, prefix') = OperatorSeq.split_tail(seq1);
        switch (uty0) {
        | Hole =>
          /* prefix' uty1 op1 |_ suffix --> prefix' uty1| suffix */
          let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
          let ze1 = ZTyp.CursorT(After, uty1);
          Some(make_ty_OpSeqZ(ze1, surround'));
        | _ =>
          /* prefix' uty1 op |uty0 suffix --> prefix' |uty0 suffix */
          let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
          Some(make_ty_OpSeqZ(zty0, surround'));
        };
      }
    }
  | (Delete, OpSeqZ(_, CursorT(After, uty0) as zty0, surround)) =>
    switch (surround) {
    | EmptySuffix(_) => None
    | EmptyPrefix(suffix) =>
      switch (suffix) {
      | ExpSuffix(op1, uty1) =>
        switch (uty0) {
        | Hole =>
          /* _| op1 uty1 -> |uty1 */
          Some(CursorT(Before, uty1))
        | _ =>
          /* uty0| op1 uty0 -> uty0| */
          Some(zty0)
        }
      | SeqSuffix(op1, seq1) =>
        let (uty1, suffix') = OperatorSeq.split0(seq1);
        switch (uty0) {
        | Hole =>
          /* _| op1 uty1 suffix' --> |uty1 suffix' */
          let surround' = OperatorSeq.EmptyPrefix(suffix');
          let ze1 = ZTyp.CursorT(Before, uty1);
          Some(make_ty_OpSeqZ(ze1, surround'));
        | _ =>
          /* uty0| op1 uty1 suffix' --> uty0| suffix' */
          let surround' = OperatorSeq.EmptyPrefix(suffix');
          Some(make_ty_OpSeqZ(zty0, surround'));
        };
      }
    | BothNonEmpty(prefix, suffix) =>
      switch (suffix) {
      | ExpSuffix(op1, uty1) =>
        switch (uty0) {
        | Hole =>
          /* prefix _| op1 uty1 -> prefix |uty1 */
          let surround' = OperatorSeq.EmptySuffix(prefix);
          let zty1 = ZTyp.CursorT(Before, uty1);
          Some(make_ty_OpSeqZ(zty1, surround'));
        | _ =>
          /* prefix uty0| op1 uty0 -> prefix uty0| */
          let surround' = OperatorSeq.EmptySuffix(prefix);
          Some(make_ty_OpSeqZ(zty0, surround'));
        }
      | SeqSuffix(op1, seq1) =>
        let (uty1, suffix') = OperatorSeq.split0(seq1);
        switch (uty0) {
        | Hole =>
          /* prefix _| op1 uty1 suffix' --> prefix |uty1 suffix' */
          let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
          let ze1 = ZTyp.CursorT(Before, uty1);
          Some(make_ty_OpSeqZ(ze1, surround'));
        | _ =>
          /* prefix uty0| op1 uty1 suffix' --> prefix uty0| suffix' */
          let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
          Some(make_ty_OpSeqZ(zty0, surround'));
        };
      }
    }
  /* Construction */
  | (Construct(SParenthesized), CursorT(_, _)) =>
    Some(ParenthesizedZ(zty))
  | (Construct(SNum), CursorT(_, Hole)) => Some(CursorT(After, Num))
  | (Construct(SNum), CursorT(_, _)) => None
  | (Construct(SBool), CursorT(_, Hole)) => Some(CursorT(After, Bool))
  | (Construct(SBool), CursorT(_, _)) => None
  | (Construct(SList), CursorT(_, ty1)) => Some(ListZ(zty))
  | (Construct(SOp(os)), CursorT(After, uty1))
  | (Construct(SOp(os)), CursorT(In(_), uty1)) =>
    switch (ty_op_of(os)) {
    | None => None
    | Some(op) =>
      let surround = OperatorSeq.EmptySuffix(ExpPrefix(uty1, op));
      let zty0 = ZTyp.CursorT(Before, Hole);
      Some(make_ty_OpSeqZ(zty0, surround));
    }
  | (Construct(SOp(os)), CursorT(Before, uty1)) =>
    switch (ty_op_of(os)) {
    | None => None
    | Some(op) =>
      let surround = OperatorSeq.EmptyPrefix(ExpSuffix(op, uty1));
      let zty0 = ZTyp.CursorT(Before, Hole);
      Some(make_ty_OpSeqZ(zty0, surround));
    }
  | (Construct(SOp(os)), OpSeqZ(_, CursorT(After, uty0), surround))
  | (Construct(SOp(os)), OpSeqZ(_, CursorT(In(_), uty0), surround)) =>
    switch (ty_op_of(os)) {
    | None => None
    | Some(op) =>
      switch (surround) {
      | EmptyPrefix(suffix) =>
        /* zty0| suffix -> uty0 op |_ suffix */
        let prefix' = OperatorSeq.ExpPrefix(uty0, op);
        let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
        let zty0' = ZTyp.CursorT(Before, Hole);
        Some(make_ty_OpSeqZ(zty0', surround'));
      | EmptySuffix(prefix) =>
        /* prefix zty0| -> prefix uty0 op |_ */
        let prefix' = OperatorSeq.prefix_append_exp(prefix, uty0, op);
        let surround' = OperatorSeq.EmptySuffix(prefix');
        let zty0' = ZTyp.CursorT(Before, Hole);
        Some(make_ty_OpSeqZ(zty0', surround'));
      | BothNonEmpty(prefix, suffix) =>
        /* prefix zty0| suffix -> prefix uty0 op |_ suffix */
        let prefix' = OperatorSeq.prefix_append_exp(prefix, uty0, op);
        let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
        let zty0' = ZTyp.CursorT(Before, Hole);
        Some(make_ty_OpSeqZ(zty0', surround'));
      }
    }
  | (Construct(SOp(os)), OpSeqZ(_, CursorT(Before, uty0), surround)) =>
    switch (ty_op_of(os)) {
    | None => None
    | Some(op) =>
      switch (surround) {
      | EmptyPrefix(suffix) =>
        /* |zty0 suffix -> |_ op uty0 suffix */
        let suffix' = OperatorSeq.suffix_prepend_exp(suffix, op, uty0);
        let surround' = OperatorSeq.EmptyPrefix(suffix');
        let zty0' = ZTyp.CursorT(Before, Hole);
        Some(make_ty_OpSeqZ(zty0', surround'));
      | EmptySuffix(prefix) =>
        /* prefix |zty0 -> prefix |_ op uty0 */
        let suffix' = OperatorSeq.ExpSuffix(op, uty0);
        let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
        let zty0' = ZTyp.CursorT(Before, Hole);
        Some(make_ty_OpSeqZ(zty0', surround'));
      | BothNonEmpty(prefix, suffix) =>
        /* prefix |zty0 suffix -> prefix |_ op uty0 suffix */
        let suffix' = OperatorSeq.suffix_prepend_exp(suffix, op, uty0);
        let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
        let zty0' = ZTyp.CursorT(Before, Hole);
        Some(make_ty_OpSeqZ(zty0', surround'));
      }
    }
  /* Zipper Cases */
  | (a, ParenthesizedZ(zty1)) =>
    switch (perform_ty(a, zty1)) {
    | Some(zty1') => Some(ParenthesizedZ(zty1'))
    | None => None
    }
  | (a, ListZ(zty1)) =>
    switch (perform_ty(a, zty1)) {
    | Some(zty1) => Some(ListZ(zty1))
    | None => None
    }
  | (a, OpSeqZ(skel, zty0, surround)) =>
    switch (perform_ty(a, zty0)) {
    | Some(zty0') => Some(OpSeqZ(skel, zty0', surround))
    | None => None
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
  | (Construct(SWild), _) => None
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
      _Cursor: (cursor_side, 'e) => 'z,
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
          let ze0' = _Cursor(After, e1);
          Some(z_typecheck_fix_holes(ctx, u_gen, ze0'));
        | (true, _) =>
          /* _1 op1 |e0 --> |e0 */
          Some(z_typecheck_fix_holes(ctx, u_gen, ze0))
        | (false, true) =>
          /* e1 op1 |_0 --> e1| */
          let ze0' = _Cursor(After, e1);
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
            let ze0' = _Cursor(After, e1);
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
            let ze0' = _Cursor(After, e1);
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
            let ze0' = _Cursor(After, e1);
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
      _Cursor: (cursor_side, 'e) => 'z,
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
            let ze1 = _Cursor(Before, e1);
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
            let ze1 = _Cursor(Before, e1);
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
            let ze1 = _Cursor(Before, e1);
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
            let ze1 = _Cursor(Before, e1);
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
      new_EmptyHole: MetaVarGen.t => ('z, MetaVarGen.t),
      make_and_typecheck_OpSeqZ:
        (Contexts.t, MetaVarGen.t, 'z, OperatorSeq.opseq_surround('e, 'op)) =>
        'm,
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      e: 'e,
      op: 'op,
    )
    : 'm => {
  let e' = bidelimit(e);
  let prefix = OperatorSeq.ExpPrefix(e', op);
  let surround = OperatorSeq.EmptySuffix(prefix);
  let (ze0, u_gen) = new_EmptyHole(u_gen);
  make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround);
};

let abs_perform_Construct_SOp_Before =
    (
      bidelimit: 'e => 'e,
      new_EmptyHole: MetaVarGen.t => ('z, MetaVarGen.t),
      make_and_typecheck_OpSeqZ:
        (Contexts.t, MetaVarGen.t, 'z, OperatorSeq.opseq_surround('e, 'op)) =>
        'm,
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      e: 'e,
      op: 'op,
    )
    : 'm => {
  let e' = bidelimit(e);
  let suffix = OperatorSeq.ExpSuffix(op, e');
  let surround = OperatorSeq.EmptyPrefix(suffix);
  let (ze0, u_gen) = new_EmptyHole(u_gen);
  make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround);
};

let abs_perform_Construct_SOp_After_surround =
    (
      new_EmptyHole: MetaVarGen.t => ('z, MetaVarGen.t),
      make_and_typecheck_OpSeqZ:
        (Contexts.t, MetaVarGen.t, 'z, OperatorSeq.opseq_surround('e, 'op)) =>
        'm,
      is_Space: 'op => bool,
      _Space: 'op,
      _Cursor: (cursor_side, 'e) => 'z,
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      e: 'e,
      op: 'op,
      surround: OperatorSeq.opseq_surround('e, 'op),
    )
    : 'm =>
  switch (surround) {
  | EmptySuffix(prefix) =>
    let prefix' = OperatorSeq.prefix_append_exp(prefix, e, op);
    let surround' = OperatorSeq.EmptySuffix(prefix');
    let (ze0, u_gen) = new_EmptyHole(u_gen);
    make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
  | EmptyPrefix(suffix) =>
    switch (suffix) {
    | OperatorSeq.ExpSuffix(op', e') =>
      is_Space(op)
        /* e| op' e' --> e |_ op' e' */
        ? {
          let prefix' = OperatorSeq.ExpPrefix(e, op);
          let suffix' = OperatorSeq.ExpSuffix(op', e');
          let surround' = OperatorSeq.BothNonEmpty(prefix', suffix');
          let (ze0, u_gen) = new_EmptyHole(u_gen);
          make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
        }
        : is_Space(op')
            /* e| e' --> e op |e' */
            ? {
              let prefix' = OperatorSeq.ExpPrefix(e, op);
              let surround' = OperatorSeq.EmptySuffix(prefix');
              let ze0 = _Cursor(Before, e');
              make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
            }
            /* e| op' e' --> e op |_ op' e' */
            : {
              let prefix' = OperatorSeq.ExpPrefix(e, op);
              let suffix' = OperatorSeq.ExpSuffix(op', e');
              let surround' = OperatorSeq.BothNonEmpty(prefix', suffix');
              let (ze0, u_gen) = new_EmptyHole(u_gen);
              make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
            }
    | SeqSuffix(op', seq') =>
      is_Space(op)
        /* e| seq' --> e |_ op' seq' */
        ? {
          let prefix' = OperatorSeq.ExpPrefix(e, op);
          let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
          let (ze0, u_gen) = new_EmptyHole(u_gen);
          make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
        }
        : is_Space(op')
            /* e| seq' --> e op |seq' */
            ? {
              let prefix' = OperatorSeq.ExpPrefix(e, op);
              let (e0', suffix') = OperatorSeq.split0(seq');
              let surround' = OperatorSeq.BothNonEmpty(prefix', suffix');
              let ze0 = _Cursor(Before, e0');
              make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
            }
            /* e| op' seq' --> e op |_ op' seq' */
            : {
              let prefix' = OperatorSeq.ExpPrefix(e, op);
              let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
              let (ze0, u_gen) = new_EmptyHole(u_gen);
              make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
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
          let (ze0, u_gen) = new_EmptyHole(u_gen);
          make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
        }
        : is_Space(op')
            /* prefix e| e' --> prefix e op |e' */
            ? {
              let prefix' = OperatorSeq.prefix_append_exp(prefix, e, op);
              let surround' = OperatorSeq.EmptySuffix(prefix');
              let ze0 = _Cursor(Before, e');
              make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
            }
            /* prefix e| op' e' --> prefix e op |_ op' e' */
            : {
              let prefix' = OperatorSeq.prefix_append_exp(prefix, e, op);
              let suffix' = OperatorSeq.ExpSuffix(op', e');
              let surround' = OperatorSeq.BothNonEmpty(prefix', suffix');
              let (ze0, u_gen) = new_EmptyHole(u_gen);
              make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
            }
    | SeqSuffix(op', seq') =>
      is_Space(op)
        /* prefix e| op' seq' --> prefix e |_ op' seq' */
        ? {
          let prefix' = OperatorSeq.prefix_append_exp(prefix, e, op);
          let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
          let (ze0, u_gen) = new_EmptyHole(u_gen);
          make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
        }
        : is_Space(op')
            /* prefix e| seq' --> prefix e op |seq' */
            ? {
              let prefix' = OperatorSeq.prefix_append_exp(prefix, e, op);
              let (e0', suffix') = OperatorSeq.split0(seq');
              let surround' = OperatorSeq.BothNonEmpty(prefix', suffix');
              let ze0' = _Cursor(Before, e0');
              make_and_typecheck_OpSeqZ(ctx, u_gen, ze0', surround');
            }
            /* prefix e| op' seq' --> prefix e op |_ op' seq' */
            : {
              let prefix' = OperatorSeq.prefix_append_exp(prefix, e, op);
              let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
              let (ze0, u_gen) = new_EmptyHole(u_gen);
              make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
            }
    }
  };

let abs_perform_Construct_SOp_Before_surround =
    (
      erase: 'z => 'e,
      new_EmptyHole: MetaVarGen.t => ('z, MetaVarGen.t),
      make_and_typecheck_OpSeqZ:
        (Contexts.t, MetaVarGen.t, 'z, OperatorSeq.opseq_surround('e, 'op)) =>
        'm,
      is_Space: 'op => bool,
      _Space: 'op,
      _Cursor: (cursor_side, 'e) => 'z,
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ze0: 'z,
      op: 'op,
      surround: OperatorSeq.opseq_surround('e, 'op),
    )
    : 'm =>
  switch (surround) {
  | EmptyPrefix(suffix) =>
    /* |ze0 ... --> |_ op e0 ... */
    let e0 = erase(ze0);
    let suffix' = OperatorSeq.suffix_prepend_exp(suffix, op, e0);
    let surround' = OperatorSeq.EmptyPrefix(suffix');
    let (ze0, u_gen) = new_EmptyHole(u_gen);
    make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
  | EmptySuffix(ExpPrefix(e1, op') as prefix) =>
    is_Space(op')
      ? is_Space(op)
          /* e1 |ze0 --> e1 |_ e0 */
          ? {
            let e0 = erase(ze0);
            let suffix' = OperatorSeq.ExpSuffix(_Space, e0);
            let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
            let (ze0, u_gen) = new_EmptyHole(u_gen);
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
          }
          /* e1 |ze0 --> e1 op |ze0 */
          : {
            let surround' =
              OperatorSeq.EmptySuffix(OperatorSeq.ExpPrefix(e1, op));
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
          }
      /* prefix [^ ] |ze0 --> prefix |_ op e0 */
      : {
        let e0 = erase(ze0);
        let suffix' = OperatorSeq.ExpSuffix(op, e0);
        let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
        let (ze0, u_gen) = new_EmptyHole(u_gen);
        make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
      }
  | EmptySuffix(SeqPrefix(seq1, op') as prefix) =>
    is_Space(op')
      ? is_Space(op)
          /* seq1 |ze0 --> seq1 |_ e0 */
          ? {
            let e0 = erase(ze0);
            let suffix' = OperatorSeq.ExpSuffix(_Space, e0);
            let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
            let (ze0, u_gen) = new_EmptyHole(u_gen);
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
          }
          /* seq1 |ze0 --> seq1 op |ze0 */
          : {
            let surround' = OperatorSeq.EmptySuffix(SeqPrefix(seq1, op));
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
          }
      /* prefix [^ ] |ze0 --> prefix |_ op e0 */
      : {
        let e0 = erase(ze0);
        let suffix' = OperatorSeq.ExpSuffix(op, e0);
        let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
        let (ze0, u_gen) = new_EmptyHole(u_gen);
        make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
      }
  | BothNonEmpty(ExpPrefix(e1, op') as prefix, suffix) =>
    is_Space(op')
      ? is_Space(op)
          /* e1 |ze0 suffix --> e1 |_ e0 suffix */
          ? {
            let e0 = erase(ze0);
            let suffix' = OperatorSeq.suffix_prepend_exp(suffix, _Space, e0);
            let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
            let (ze0, u_gen) = new_EmptyHole(u_gen);
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
          }
          /* e1 |ze0 suffix --> e1 op |ze0 suffix */
          : {
            let prefix' = OperatorSeq.ExpPrefix(e1, op);
            let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
          }
      /* prefix [^ ] |ze0 suffix --> prefix |_ op e0 suffix */
      : {
        let e0 = erase(ze0);
        let suffix' = OperatorSeq.suffix_prepend_exp(suffix, op, e0);
        let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
        let (ze0, u_gen) = new_EmptyHole(u_gen);
        make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
      }
  | BothNonEmpty(SeqPrefix(seq1, op') as prefix, suffix) =>
    is_Space(op')
      ? is_Space(op)
          /* seq1 |ze0 suffix --> seq1 |_ e0 suffix */
          ? {
            let e0 = erase(ze0);
            let suffix' = OperatorSeq.suffix_prepend_exp(suffix, _Space, e0);
            let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
            let (ze0, u_gen) = new_EmptyHole(u_gen);
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
          }
          /* seq1 |ze0 suffix --> seq1 op |ze0 suffix */
          : {
            let prefix' = OperatorSeq.SeqPrefix(seq1, op);
            let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
          }
      /* prefix [^ ] |ze0 suffix --> prefix |_ op e0 suffix */
      : {
        let e0 = erase(ze0);
        let suffix' = OperatorSeq.suffix_prepend_exp(suffix, op, e0);
        let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
        let (ze0, u_gen) = new_EmptyHole(u_gen);
        make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
      }
  };

let syn_zpat_fix_holes =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, zp: ZPat.t)
    : (ZPat.t, HTyp.t, Contexts.t, MetaVarGen.t) => {
  let path = Path.of_zpat(zp);
  let p = ZPat.erase(zp);
  let (p, ty, ctx, u_gen) = Statics.syn_pat_fix_holes(ctx, u_gen, false, p);
  let zp = Path.follow_pat_or_fail(path, p);
  (zp, ty, ctx, u_gen);
};

let ana_zpat_fix_holes =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, zp: ZPat.t, ty: HTyp.t)
    : (ZPat.t, Contexts.t, MetaVarGen.t) => {
  let path = Path.of_zpat(zp);
  let p = ZPat.erase(zp);
  let (p, ctx, u_gen) = Statics.ana_pat_fix_holes(ctx, u_gen, false, p, ty);
  let zp = Path.follow_pat_or_fail(path, p);
  (zp, ctx, u_gen);
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
  switch (Statics.syn_skel_pat_fix_holes(ctx, u_gen, false, skel, seq)) {
  | (Placeholder(_), _, _, _, _) =>
    raise(UHPat.SkelInconsistentWithOpSeq(skel, seq))
  | (BinOp(err, _, _, _) as skel, seq, ty, ctx, u_gen) =>
    let p = UHPat.Pat(err, OpSeq(skel, seq));
    let zp = Path.follow_pat_or_fail(path0, p);
    (zp, ty, ctx, u_gen);
  };
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
  switch (Statics.ana_skel_pat_fix_holes(ctx, u_gen, false, skel, seq, ty)) {
  | (Placeholder(_), _, _, _) =>
    raise(UHPat.SkelInconsistentWithOpSeq(skel, seq))
  | (BinOp(err, _, _, _) as skel, seq, ctx, u_gen) =>
    let p = UHPat.Pat(err, OpSeq(skel, seq));
    let zp = Path.follow_pat_or_fail(path0, p);
    (zp, ctx, u_gen);
  };
};

let combine_for_Backspace_Space_pat = (p1: UHPat.t, zp0: ZPat.t): ZPat.t =>
  switch (zp0) {
  | CursorP(_, Pat(_, EmptyHole(_))) =>
    /* p1 |_ --> p1| */
    CursorP(After, p1)
  | _ =>
    /* p1 |zp0 --> |zp0 */
    zp0
  };

let combine_for_Delete_Space_pat = (zp0: ZPat.t, p: UHPat.t): ZPat.t =>
  switch (zp0, p) {
  | (CursorP(After, Pat(_, EmptyHole(_))), Pat(_, EmptyHole(_))) =>
    /* _| _ --> _| */
    zp0
  | (CursorP(After, Pat(_, EmptyHole(_))), _) =>
    /* _| p  --> |p */
    CursorP(Before, p)
  | _ => zp0
  };

let rec perform_syn_pat =
        (ctx: Contexts.t, u_gen: MetaVarGen.t, a: t, zp: ZPat.t)
        : option((ZPat.t, HTyp.t, Contexts.t, MetaVarGen.t)) =>
  switch (a, zp) {
  /* Movement */
  /* NOTE: we don't need to handle movement actions here for the purposes of the UI,
   * since it's handled at the top (expression) level, but for the sake of API completeness
   * we include it */
  | (MoveTo(path), _) =>
    let p = ZPat.erase(zp);
    switch (Statics.syn_pat(ctx, p)) {
    | None => None
    | Some((ty, _)) =>
      switch (Path.follow_pat(path, p)) {
      | None => None
      | Some(zp) => Some((zp, ty, ctx, u_gen))
      }
    };
  | (MoveToPrevHole, _) =>
    switch (Path.prev_hole_path(Path.holes_zpat(zp, []))) {
    | None => None
    | Some(path) => perform_syn_pat(ctx, u_gen, MoveTo(path), zp)
    }
  | (MoveToNextHole, _) =>
    switch (Path.next_hole_path(Path.holes_zpat(zp, []))) {
    | None => None
    | Some(path) => perform_syn_pat(ctx, u_gen, MoveTo(path), zp)
    }
  /* Backspace and Delete */
  | (Backspace, CursorP(After, p)) =>
    switch (p) {
    | Pat(_, EmptyHole(_)) => Some((CursorP(Before, p), Hole, ctx, u_gen))
    | _ =>
      let (p, u_gen) = UHPat.new_EmptyHole(u_gen);
      Some((CursorP(Before, p), Hole, ctx, u_gen));
    }
  | (Backspace, CursorP(Before, _)) => None
  | (Delete, CursorP(Before, p)) =>
    switch (p) {
    | Pat(_, EmptyHole(_)) => Some((CursorP(After, p), Hole, ctx, u_gen))
    | _ =>
      let (p, u_gen) = UHPat.new_EmptyHole(u_gen);
      Some((CursorP(Before, p), Hole, ctx, u_gen));
    }
  | (Delete, CursorP(After, _)) => None
  | (Backspace, CursorP(In(_), _))
  | (Delete, CursorP(In(_), _)) =>
    let (p, u_gen) = UHPat.new_EmptyHole(u_gen);
    let zp = ZPat.CursorP(Before, p);
    Some((zp, Hole, ctx, u_gen));
  | (
      Backspace,
      Deeper(
        _,
        OpSeqZ(_, CursorP(Before, p0) as zp0, EmptySuffix(_) as surround),
      ),
    )
  | (
      Backspace,
      Deeper(
        _,
        OpSeqZ(
          _,
          CursorP(Before, p0) as zp0,
          BothNonEmpty(_, _) as surround,
        ),
      ),
    ) =>
    abs_perform_Backspace_Before_op(
      combine_for_Backspace_Space_pat,
      syn_zpat_fix_holes,
      make_and_syn_OpSeqZ_pat,
      UHPat.is_EmptyHole,
      UHPat.is_Space,
      UHPat.Space,
      (side, p) => CursorP(side, p),
      ctx,
      u_gen,
      p0,
      zp0,
      surround,
    )
  | (
      Delete,
      Deeper(
        _,
        OpSeqZ(_, CursorP(After, p0) as zp0, EmptyPrefix(_) as surround),
      ),
    )
  | (
      Delete,
      Deeper(
        _,
        OpSeqZ(
          _,
          CursorP(After, p0) as zp0,
          BothNonEmpty(_, _) as surround,
        ),
      ),
    ) =>
    abs_perform_Delete_After_op(
      combine_for_Delete_Space_pat,
      syn_zpat_fix_holes,
      make_and_syn_OpSeqZ_pat,
      UHPat.is_EmptyHole,
      UHPat.is_Space,
      UHPat.Space,
      (side, p) => CursorP(side, p),
      ctx,
      u_gen,
      p0,
      zp0,
      surround,
    )
  /* Construct */
  | (Construct(SParenthesized), CursorP(_, p)) =>
    switch (Statics.syn_pat(ctx, p)) {
    | None => None
    | Some((ty, ctx)) => Some((ParenthesizedZ(zp), ty, ctx, u_gen))
    }
  | (Construct(SVar(x, side)), CursorP(_, Pat(_, EmptyHole(_))))
  | (Construct(SVar(x, side)), CursorP(_, Pat(_, Wild)))
  | (Construct(SVar(x, side)), CursorP(_, Pat(_, Var(_))))
  | (Construct(SVar(x, side)), CursorP(_, Pat(_, NumLit(_))))
  | (Construct(SVar(x, side)), CursorP(_, Pat(_, BoolLit(_)))) =>
    if (Var.is_true(x)) {
      Some((
        CursorP(side, Pat(NotInHole, BoolLit(true))),
        Bool,
        ctx,
        u_gen,
      ));
    } else if (Var.is_false(x)) {
      Some((
        CursorP(side, Pat(NotInHole, BoolLit(false))),
        Bool,
        ctx,
        u_gen,
      ));
    } else if (Var.is_let(x)) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      Some((
        CursorP(side, Pat(NotInHole, Var(InVHole(Keyword(Let), u), x))),
        Hole,
        ctx,
        u_gen,
      ));
    } else if (Var.is_case(x)) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      Some((
        CursorP(side, Pat(NotInHole, Var(InVHole(Keyword(Case), u), x))),
        Hole,
        ctx,
        u_gen,
      ));
    } else {
      Var.check_valid(
        x,
        {
          let ctx = Contexts.extend_gamma(ctx, (x, Hole));
          Some((
            ZPat.CursorP(side, Pat(NotInHole, Var(NotInVHole, x))),
            HTyp.Hole,
            ctx,
            u_gen,
          ));
        },
      );
    }
  | (Construct(SVar(_, _)), CursorP(_, _)) => None
  | (Construct(SWild), CursorP(_, Pat(_, EmptyHole(_))))
  | (Construct(SWild), CursorP(_, Pat(_, Wild)))
  | (Construct(SWild), CursorP(_, Pat(_, Var(_))))
  | (Construct(SWild), CursorP(_, Pat(_, NumLit(_))))
  | (Construct(SWild), CursorP(_, Pat(_, BoolLit(_)))) =>
    Some((CursorP(After, Pat(NotInHole, Wild)), Hole, ctx, u_gen))
  | (Construct(SWild), CursorP(_, _)) => None
  | (Construct(SNumLit(n, side)), CursorP(_, Pat(_, EmptyHole(_))))
  | (Construct(SNumLit(n, side)), CursorP(_, Pat(_, Wild)))
  | (Construct(SNumLit(n, side)), CursorP(_, Pat(_, Var(_))))
  | (Construct(SNumLit(n, side)), CursorP(_, Pat(_, NumLit(_))))
  | (Construct(SNumLit(n, side)), CursorP(_, Pat(_, BoolLit(_)))) =>
    Some((CursorP(side, Pat(NotInHole, NumLit(n))), Num, ctx, u_gen))
  | (Construct(SNumLit(_, _)), CursorP(_, _)) => None
  | (Construct(SInj(side)), CursorP(_, p1)) =>
    switch (Statics.syn_pat(ctx, p1)) {
    | None => None
    | Some((ty1, ctx)) =>
      let zp = ZPat.Deeper(NotInHole, InjZ(side, zp));
      let ty =
        switch (side) {
        | L => HTyp.Sum(ty1, Hole)
        | R => HTyp.Sum(Hole, ty1)
        };
      Some((zp, ty, ctx, u_gen));
    }
  | (Construct(SListNil), CursorP(_, Pat(_, EmptyHole(_)))) =>
    let zp = ZPat.CursorP(After, Pat(NotInHole, ListNil));
    let ty = HTyp.List(Hole);
    Some((zp, ty, ctx, u_gen));
  | (Construct(SListNil), CursorP(_, _)) => None
  | (
      Construct(SOp(os)),
      Deeper(_, OpSeqZ(_, CursorP(In(_), p), surround)),
    )
  | (
      Construct(SOp(os)),
      Deeper(_, OpSeqZ(_, CursorP(After, p), surround)),
    ) =>
    switch (pat_op_of(os)) {
    | None => None
    | Some(op) =>
      Some(
        abs_perform_Construct_SOp_After_surround(
          ZPat.new_EmptyHole,
          make_and_syn_OpSeqZ_pat,
          UHPat.is_Space,
          UHPat.Space,
          (side, p) => CursorP(side, p),
          ctx,
          u_gen,
          p,
          op,
          surround,
        ),
      )
    }
  | (
      Construct(SOp(os)),
      Deeper(_, OpSeqZ(_, CursorP(Before, _) as zp0, surround)),
    ) =>
    switch (pat_op_of(os)) {
    | None => None
    | Some(op) =>
      Some(
        abs_perform_Construct_SOp_Before_surround(
          ZPat.erase,
          ZPat.new_EmptyHole,
          make_and_syn_OpSeqZ_pat,
          UHPat.is_Space,
          UHPat.Space,
          (side, p) => CursorP(side, p),
          ctx,
          u_gen,
          zp0,
          op,
          surround,
        ),
      )
    }
  | (Construct(SOp(os)), CursorP(In(_), p))
  | (Construct(SOp(os)), CursorP(After, p)) =>
    switch (pat_op_of(os)) {
    | None => None
    | Some(op) =>
      Some(
        abs_perform_Construct_SOp_After(
          UHPat.bidelimit,
          ZPat.new_EmptyHole,
          make_and_syn_OpSeqZ_pat,
          ctx,
          u_gen,
          p,
          op,
        ),
      )
    }
  | (Construct(SOp(os)), CursorP(Before, p)) =>
    switch (pat_op_of(os)) {
    | None => None
    | Some(op) =>
      Some(
        abs_perform_Construct_SOp_Before(
          UHPat.bidelimit,
          ZPat.new_EmptyHole,
          make_and_syn_OpSeqZ_pat,
          ctx,
          u_gen,
          p,
          op,
        ),
      )
    }
  /* Zipper */
  | (_, ParenthesizedZ(zp1)) =>
    switch (perform_syn_pat(ctx, u_gen, a, zp1)) {
    | None => None
    | Some((zp1, ty, ctx, u_gen)) =>
      Some((ParenthesizedZ(zp1), ty, ctx, u_gen))
    }
  | (_, Deeper(_, InjZ(side, zp1))) =>
    switch (perform_syn_pat(ctx, u_gen, a, zp1)) {
    | None => None
    | Some((zp1, ty1, ctx, u_gen)) =>
      let zp = ZPat.Deeper(NotInHole, InjZ(side, zp1));
      let ty =
        switch (side) {
        | L => HTyp.Sum(ty1, Hole)
        | R => HTyp.Sum(Hole, ty1)
        };
      Some((zp, ty, ctx, u_gen));
    }
  | (_, Deeper(err, OpSeqZ(_, zp0, surround))) =>
    let i = OperatorSeq.surround_prefix_length(surround);
    switch (ZPat.erase(zp)) {
    | Pat(_, OpSeq(skel, seq)) =>
      switch (Statics.syn_skel_pat(ctx, skel, seq, Some(i))) {
      | Some((ty, ctx, Some(mode))) =>
        switch (mode) {
        | Statics.AnalyzedAgainst(ty0) =>
          switch (perform_ana_pat(ctx, u_gen, a, zp0, ty0)) {
          | None => None
          | Some((zp0, ctx, u_gen)) =>
            let zp0 = ZPat.bidelimit(zp0);
            Some(make_and_syn_OpSeqZ_pat(ctx, u_gen, zp0, surround));
          }
        | Statics.Synthesized(ty0) =>
          switch (perform_syn_pat(ctx, u_gen, a, zp0)) {
          | Some((zp0, ty0, ctx, u_gen)) =>
            let zp0 = ZPat.bidelimit(zp0);
            Some(make_and_syn_OpSeqZ_pat(ctx, u_gen, zp0, surround));
          | None => None
          }
        }
      | Some(_) => None /* should never happen */
      | None => None /* should never happen */
      }
    | _ => None /* should never happen */
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
  | (Construct(SCase), _) => None
  }
and perform_ana_pat =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, a: t, zp: ZPat.t, ty: HTyp.t)
    : option((ZPat.t, Contexts.t, MetaVarGen.t)) =>
  switch (a, zp) {
  /* Movement */
  /* NOTE: we don't need to handle movement actions here for the purposes of the UI,
   * since it's handled at the top (expression) level, but for the sake of API completeness
   * we include it */
  | (MoveTo(path), _) =>
    let p = ZPat.erase(zp);
    switch (Path.follow_pat(path, p)) {
    | Some(zp) => Some((zp, ctx, u_gen))
    | None => None
    };
  | (MoveToPrevHole, _) =>
    switch (Path.prev_hole_path(Path.holes_zpat(zp, []))) {
    | None => None
    | Some(path) => perform_ana_pat(ctx, u_gen, MoveTo(path), zp, ty)
    }
  | (MoveToNextHole, _) =>
    switch (Path.next_hole_path(Path.holes_zpat(zp, []))) {
    | None => None
    | Some(path) => perform_ana_pat(ctx, u_gen, MoveTo(path), zp, ty)
    }
  /* switch to synthesis if in a hole */
  | (_, Deeper(InHole(TypeInconsistent, u) as err, zp1)) =>
    let zp_not_in_hole = ZPat.set_err_status(NotInHole, zp);
    let p = ZPat.erase(zp_not_in_hole);
    switch (Statics.syn_pat(ctx, p)) {
    | None => None
    | Some((ty1, _)) =>
      switch (perform_syn_pat(ctx, u_gen, a, zp_not_in_hole)) {
      | None => None
      | Some((zp1, ty', ctx, u_gen)) =>
        if (HTyp.consistent(ty, ty')) {
          Some((zp1, ctx, u_gen));
        } else {
          Some((ZPat.set_err_status(err, zp1), ctx, u_gen));
        }
      }
    };
  /* Backspace and Delete */
  | (Backspace, CursorP(After, p)) =>
    switch (p) {
    | Pat(_, EmptyHole(_)) => Some((CursorP(Before, p), ctx, u_gen))
    | _ =>
      let (p, u_gen) = UHPat.new_EmptyHole(u_gen);
      Some((CursorP(Before, p), ctx, u_gen));
    }
  | (Backspace, CursorP(Before, p)) => None
  | (Delete, CursorP(Before, p)) =>
    switch (p) {
    | Pat(_, EmptyHole(_)) => Some((CursorP(After, p), ctx, u_gen))
    | _ =>
      let (p, u_gen) = UHPat.new_EmptyHole(u_gen);
      Some((CursorP(Before, p), ctx, u_gen));
    }
  | (Backspace, CursorP(In(_), _))
  | (Delete, CursorP(In(_), _)) =>
    let (p, u_gen) = UHPat.new_EmptyHole(u_gen);
    let zp = ZPat.CursorP(Before, p);
    Some((zp, ctx, u_gen));
  | (Delete, CursorP(After, _)) => None
  | (
      Backspace,
      Deeper(
        _,
        OpSeqZ(_, CursorP(Before, p0) as zp0, EmptySuffix(_) as surround),
      ),
    )
  | (
      Backspace,
      Deeper(
        _,
        OpSeqZ(
          _,
          CursorP(Before, p0) as zp0,
          BothNonEmpty(_, _) as surround,
        ),
      ),
    ) =>
    abs_perform_Backspace_Before_op(
      combine_for_Backspace_Space_pat,
      (ctx, u_gen, zp) => ana_zpat_fix_holes(ctx, u_gen, zp, ty),
      (ctx, u_gen, zp, surround) =>
        make_and_ana_OpSeqZ_pat(ctx, u_gen, zp, surround, ty),
      UHPat.is_EmptyHole,
      UHPat.is_Space,
      UHPat.Space,
      (side, p) => CursorP(side, p),
      ctx,
      u_gen,
      p0,
      zp0,
      surround,
    )
  | (
      Delete,
      Deeper(
        _,
        OpSeqZ(_, CursorP(After, p0) as zp0, EmptyPrefix(_) as surround),
      ),
    )
  | (
      Delete,
      Deeper(
        _,
        OpSeqZ(
          _,
          CursorP(After, p0) as zp0,
          BothNonEmpty(_, _) as surround,
        ),
      ),
    ) =>
    abs_perform_Delete_After_op(
      combine_for_Delete_Space_pat,
      (ctx, u_gen, zp) => ana_zpat_fix_holes(ctx, u_gen, zp, ty),
      (ctx, u_gen, zp, surround) =>
        make_and_ana_OpSeqZ_pat(ctx, u_gen, zp, surround, ty),
      UHPat.is_EmptyHole,
      UHPat.is_Space,
      UHPat.Space,
      (side, p) => CursorP(side, p),
      ctx,
      u_gen,
      p0,
      zp0,
      surround,
    )
  /* Construct */
  | (Construct(SParenthesized), CursorP(_, p)) =>
    switch (Statics.ana_pat(ctx, p, ty)) {
    | None => None
    | Some(ctx) => Some((ParenthesizedZ(zp), ctx, u_gen))
    }
  | (Construct(SVar("true", side)), _)
  | (Construct(SVar("false", side)), _) =>
    switch (perform_syn_pat(ctx, u_gen, a, zp)) {
    | None => None
    | Some((zp, ty', ctx, u_gen)) =>
      if (HTyp.consistent(ty, ty')) {
        Some((zp, ctx, u_gen));
      } else {
        let (zp, u_gen) = ZPat.make_inconsistent(u_gen, zp);
        Some((zp, ctx, u_gen));
      }
    }
  | (Construct(SVar(x, side)), CursorP(_, Pat(_, EmptyHole(_))))
  | (Construct(SVar(x, side)), CursorP(_, Pat(_, Wild)))
  | (Construct(SVar(x, side)), CursorP(_, Pat(_, Var(_))))
  | (Construct(SVar(x, side)), CursorP(_, Pat(_, NumLit(_))))
  | (Construct(SVar(x, side)), CursorP(_, Pat(_, BoolLit(_)))) =>
    if (Var.is_let(x)) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      Some((
        CursorP(side, Pat(NotInHole, Var(InVHole(Keyword(Let), u), x))),
        ctx,
        u_gen,
      ));
    } else if (Var.is_case(x)) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      Some((
        CursorP(side, Pat(NotInHole, Var(InVHole(Keyword(Case), u), x))),
        ctx,
        u_gen,
      ));
    } else {
      Var.check_valid(
        x,
        {
          let ctx = Contexts.extend_gamma(ctx, (x, ty));
          Some((
            ZPat.CursorP(side, Pat(NotInHole, Var(NotInVHole, x))),
            ctx,
            u_gen,
          ));
        },
      );
    }
  | (Construct(SVar(_, _)), CursorP(_, _)) => None
  | (Construct(SWild), CursorP(_, Pat(_, EmptyHole(_))))
  | (Construct(SWild), CursorP(_, Pat(_, Wild)))
  | (Construct(SWild), CursorP(_, Pat(_, Var(_))))
  | (Construct(SWild), CursorP(_, Pat(_, NumLit(_))))
  | (Construct(SWild), CursorP(_, Pat(_, BoolLit(_)))) =>
    Some((CursorP(After, Pat(NotInHole, Wild)), ctx, u_gen))
  | (Construct(SWild), CursorP(_, _)) => None
  | (Construct(SInj(side)), CursorP(cursor_side, p1)) =>
    switch (HTyp.matched_sum(ty)) {
    | Some((tyL, tyR)) =>
      let ty1 = pick_side(side, tyL, tyR);
      let (p1, ctx, u_gen) =
        Statics.ana_pat_fix_holes(ctx, u_gen, false, p1, ty1);
      let zp = ZPat.Deeper(NotInHole, InjZ(side, CursorP(cursor_side, p1)));
      Some((zp, ctx, u_gen));
    | None =>
      let (p1, _, ctx, u_gen) =
        Statics.syn_pat_fix_holes(ctx, u_gen, false, p1);
      let (u, u_gen) = MetaVarGen.next(u_gen);
      let zp =
        ZPat.Deeper(
          InHole(TypeInconsistent, u),
          InjZ(side, CursorP(cursor_side, p1)),
        );
      Some((zp, ctx, u_gen));
    }
  | (
      Construct(SOp(os)),
      Deeper(_, OpSeqZ(_, CursorP(In(_), p), surround)),
    )
  | (
      Construct(SOp(os)),
      Deeper(_, OpSeqZ(_, CursorP(After, p), surround)),
    ) =>
    switch (pat_op_of(os)) {
    | None => None
    | Some(op) =>
      Some(
        abs_perform_Construct_SOp_After_surround(
          ZPat.new_EmptyHole,
          (ctx, u_gen, zp, surround) =>
            make_and_ana_OpSeqZ_pat(ctx, u_gen, zp, surround, ty),
          UHPat.is_Space,
          UHPat.Space,
          (side, p) => CursorP(side, p),
          ctx,
          u_gen,
          p,
          op,
          surround,
        ),
      )
    }
  | (
      Construct(SOp(os)),
      Deeper(_, OpSeqZ(_, CursorP(Before, _) as zp0, surround)),
    ) =>
    switch (pat_op_of(os)) {
    | None => None
    | Some(op) =>
      Some(
        abs_perform_Construct_SOp_Before_surround(
          ZPat.erase,
          ZPat.new_EmptyHole,
          (ctx, u_gen, zp, surround) =>
            make_and_ana_OpSeqZ_pat(ctx, u_gen, zp, surround, ty),
          UHPat.is_Space,
          UHPat.Space,
          (side, p) => CursorP(side, p),
          ctx,
          u_gen,
          zp0,
          op,
          surround,
        ),
      )
    }
  | (Construct(SOp(os)), CursorP(In(_), p))
  | (Construct(SOp(os)), CursorP(After, p)) =>
    switch (pat_op_of(os)) {
    | None => None
    | Some(op) =>
      Some(
        abs_perform_Construct_SOp_After(
          UHPat.bidelimit,
          ZPat.new_EmptyHole,
          (ctx, u_gen, zp, surround) =>
            make_and_ana_OpSeqZ_pat(ctx, u_gen, zp, surround, ty),
          ctx,
          u_gen,
          p,
          op,
        ),
      )
    }
  | (Construct(SOp(os)), CursorP(Before, p)) =>
    switch (pat_op_of(os)) {
    | None => None
    | Some(op) =>
      Some(
        abs_perform_Construct_SOp_Before(
          UHPat.bidelimit,
          ZPat.new_EmptyHole,
          (ctx, u_gen, zp, surround) =>
            make_and_ana_OpSeqZ_pat(ctx, u_gen, zp, surround, ty),
          ctx,
          u_gen,
          p,
          op,
        ),
      )
    }
  /* Zipper */
  | (_, ParenthesizedZ(zp1)) =>
    switch (perform_ana_pat(ctx, u_gen, a, zp1, ty)) {
    | None => None
    | Some((zp1, ctx, u_gen)) => Some((ParenthesizedZ(zp1), ctx, u_gen))
    }
  | (_, Deeper(_, InjZ(side, zp1))) =>
    switch (HTyp.matched_sum(ty)) {
    | None => None
    | Some((tyL, tyR)) =>
      let ty1 = pick_side(side, tyL, tyR);
      switch (perform_ana_pat(ctx, u_gen, a, zp1, ty1)) {
      | None => None
      | Some((zp1, ctx, u_gen)) =>
        let zp = ZPat.Deeper(NotInHole, InjZ(side, zp1));
        Some((zp, ctx, u_gen));
      };
    }
  | (_, Deeper(err, OpSeqZ(_, zp0, surround))) =>
    let i = OperatorSeq.surround_prefix_length(surround);
    switch (ZPat.erase(zp)) {
    | Pat(_, OpSeq(skel, seq)) =>
      switch (Statics.ana_skel_pat(ctx, skel, seq, ty, Some(i))) {
      | Some((_, Some(mode))) =>
        switch (mode) {
        | Statics.AnalyzedAgainst(ty0) =>
          switch (perform_ana_pat(ctx, u_gen, a, zp0, ty0)) {
          | None => None
          | Some((zp0, _, u_gen)) =>
            let zp0 = ZPat.bidelimit(zp0);
            Some(make_and_ana_OpSeqZ_pat(ctx, u_gen, zp0, surround, ty));
          }
        | Statics.Synthesized(ty0) =>
          switch (perform_syn_pat(ctx, u_gen, a, zp0)) {
          | Some((zp0, ty0, _, u_gen)) =>
            let zp0 = ZPat.bidelimit(zp0);
            Some(make_and_ana_OpSeqZ_pat(ctx, u_gen, zp0, surround, ty));
          | None => None
          }
        }
      | Some(_) => None /* should never happen */
      | None => None /* should never happen */
      }
    | _ => None /* should never happen */
    };
  /* Subsumption */
  | (Construct(SNumLit(_, _)), _)
  | (Construct(SListNil), _) =>
    switch (perform_syn_pat(ctx, u_gen, a, zp)) {
    | None => None
    | Some((zp, ty', ctx, u_gen)) =>
      if (HTyp.consistent(ty, ty')) {
        Some((zp, ctx, u_gen));
      } else {
        let (zp, u_gen) = ZPat.make_inconsistent(u_gen, zp);
        Some((zp, ctx, u_gen));
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
  | (Construct(SCase), _) => None
  };

let zexp_syn_fix_holes =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, ze: ZExp.t)
    : (ZExp.t, HTyp.t, MetaVarGen.t) => {
  let path = Path.of_zexp(ze);
  let e = ZExp.erase(ze);
  let (e, ty, u_gen) = Statics.syn_fix_holes(ctx, u_gen, e);
  let ze = Path.follow_e_or_fail(path, e);
  (ze, ty, u_gen);
};

let zexp_ana_fix_holes =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, ze: ZExp.t, ty: HTyp.t)
    : (ZExp.t, MetaVarGen.t) => {
  let path = Path.of_zexp(ze);
  let e = ZExp.erase(ze);
  let (e, u_gen) = Statics.ana_fix_holes(ctx, u_gen, e, ty);
  let ze = Path.follow_e_or_fail(path, e);
  (ze, u_gen);
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
    Statics.syn_skel_fix_holes(ctx, u_gen, false, skel, seq);
  let e = UHExp.Tm(NotInHole, OpSeq(skel, seq));
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
  switch (Statics.ana_skel_fix_holes(ctx, u_gen, false, skel, seq, ty)) {
  | (Placeholder(_), _, _) =>
    raise(UHExp.SkelInconsistentWithOpSeq(skel, seq))
  | (BinOp(err, _, _, _) as skel, seq, u_gen) =>
    let e = UHExp.Tm(err, OpSeq(skel, seq));
    let ze = Path.follow_e_or_fail(path0, e);
    (ze, u_gen);
  };
};

let combine_for_Backspace_Space = (e1: UHExp.t, ze0: ZExp.t): ZExp.t =>
  switch (e1, ze0) {
  | (_, CursorE(_, Tm(_, EmptyHole(_)))) =>
    /* e1 |_ --> e1| */
    CursorE(After, e1)
  | _ => ze0
  };

let combine_for_Delete_Space = (ze0: ZExp.t, e: UHExp.t): ZExp.t =>
  switch (ze0, e) {
  | (CursorE(After, Tm(_, EmptyHole(_))), Tm(_, EmptyHole(_))) =>
    /* _| _ --> _| */
    ze0
  | (CursorE(After, Tm(_, EmptyHole(_))), _) =>
    /* _| e --> |e */
    CursorE(Before, e)
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
      Tm(NotInHole, OpSeq(Associator.associate_exp(seq), seq)),
      u_gen,
    )
  | ExpSuffix(_, _)
  | SeqSuffix(_, _) =>
    let (hole, u_gen) = UHExp.new_EmptyHole(u_gen);
    let opseq = OperatorSeq.opseq_of_exp_and_suffix(hole, suffix);
    let skel = Associator.associate_exp(opseq);
    (Tm(NotInHole, OpSeq(skel, opseq)), u_gen);
  };

/**
 * Convenience function used as a pattern guard in perform_syn/ana.
 * Returns true when the cursor is on an operand in opseq, but not
 * Before the first operand. Note that this is not quite the complement
 * of ZExp.is_before, since we do not want this pattern guard
 * to prevent an action from flowing into the zipper cases.
 */
let is_not_Before_opseq = (ze: ZExp.t): bool =>
  switch (ze) {
  | Deeper(_, OpSeqZ(_, CursorE(In(_), _), EmptyPrefix(_)))
  | Deeper(_, OpSeqZ(_, CursorE(After, _), EmptyPrefix(_)))
  | Deeper(_, OpSeqZ(_, CursorE(_, _), BothNonEmpty(_, _)))
  | Deeper(_, OpSeqZ(_, CursorE(_, _), EmptySuffix(_))) => true
  | _ => false
  };

let keyword_action = (k: keyword): t =>
  switch (k) {
  | Let => Construct(SLet)
  | Case => Construct(SCase)
  };

let rec perform_syn =
        (ctx: Contexts.t, a: t, ze_ty: (ZExp.t, HTyp.t, MetaVarGen.t))
        : option((ZExp.t, HTyp.t, MetaVarGen.t)) => {
  let (ze, ty, u_gen) = ze_ty;
  switch (a, ze) {
  /* Movement */
  | (MoveTo(path), _) =>
    let e = ZExp.erase(ze);
    switch (Path.follow_e(path, e)) {
    | None => None
    | Some(ze) => Some((ze, ty, u_gen))
    };
  | (MoveToPrevHole, _) =>
    let holes = Path.holes_ze(ze, []);
    switch (Path.prev_hole_path(holes)) {
    | None => None
    | Some(path) => perform_syn(ctx, MoveTo(path), ze_ty)
    };
  | (MoveToNextHole, _) =>
    let holes = Path.holes_ze(ze, []);
    switch (Path.next_hole_path(holes)) {
    | None => None
    | Some(path) => perform_syn(ctx, MoveTo(path), ze_ty)
    };
  /* Backspace & Deletion */
  | (Backspace, CursorE(After, e)) =>
    switch (e) {
    | Tm(_, EmptyHole(_)) => Some((CursorE(Before, e), ty, u_gen))
    | _ =>
      let (e', u_gen') = UHExp.new_EmptyHole(u_gen);
      Some((CursorE(Before, e'), Hole, u_gen'));
    }
  | (Backspace, CursorE(Before, e)) => None
  | (Delete, CursorE(Before, e)) =>
    switch (e) {
    | Tm(_, EmptyHole(_)) => Some((CursorE(After, e), ty, u_gen))
    | _ =>
      let (e', u_gen) = UHExp.new_EmptyHole(u_gen);
      Some((CursorE(Before, e'), Hole, u_gen));
    }
  | (Delete, CursorE(After, e)) => None
  | (
      Backspace,
      Deeper(
        _,
        LineItemZE(li, Deeper(_, LineItemZL(CursorL(_, EmptyLine), e2))),
      ),
    ) =>
    let zli = ZExp.place_after_line_item(li);
    let ze = ZExp.prepend_zline(zli, e2);
    Some((ze, ty, u_gen));
  | (Delete, Deeper(_, LineItemZL(CursorL(_, EmptyLine), e1))) =>
    let ze = ZExp.place_before(e1);
    Some((ze, ty, u_gen));
  | (Backspace, Deeper(_, LineItemZL(CursorL(Before, _), e2))) => None
  | (Backspace, Deeper(_, LineItemZL(CursorL(After, _), e2)))
  | (Backspace, Deeper(_, LineItemZL(CursorL(In(_), _), e2))) =>
    let (e2, ty, u_gen) = Statics.syn_fix_holes(ctx, u_gen, e2);
    let ze = ZExp.prepend_zline(CursorL(After, EmptyLine), e2);
    Some((ze, ty, u_gen));
  | (Delete, Deeper(_, LineItemZL(CursorL(After, _), e2))) => None
  | (Delete, Deeper(_, LineItemZL(CursorL(Before, _), e2)))
  | (Delete, Deeper(_, LineItemZL(CursorL(In(_), _), e2))) =>
    let (e2, ty, u_gen) = Statics.syn_fix_holes(ctx, u_gen, e2);
    let ze = ZExp.prepend_zline(CursorL(Before, EmptyLine), e2);
    Some((ze, ty, u_gen));
  | (Backspace, Deeper(_, LineItemZE(EmptyLine, ze1)))
      when ZExp.is_before(ze1) =>
    Some((ze1, ty, u_gen))
  | (Delete, Deeper(_, LineItemZL(zli, Tm(_, LineItem(EmptyLine, e2)))))
      when ZExp.is_after_line_item(zli) =>
    let ze = ZExp.prepend_zline(zli, e2);
    Some((ze, ty, u_gen));
  | (
      Backspace,
      Deeper(
        _,
        LineItemZE(ExpLine(e1), CursorE(Before, Tm(_, EmptyHole(_)))),
      ),
    ) =>
    switch (Statics.syn(ctx, e1)) {
    | None => None
    | Some(ty) =>
      let ze = ZExp.place_after(e1);
      Some((ze, ty, u_gen));
    }
  | (
      Delete,
      Deeper(_, LineItemZL(DeeperL(ExpLineZ(ze1)), Tm(_, EmptyHole(_)))),
    )
      when ZExp.is_after(ze1) =>
    let e1 = ZExp.erase(ze1);
    switch (Statics.syn(ctx, e1)) {
    | None => None
    | Some(ty) => Some((ze1, ty, u_gen))
    };
  | (Backspace, Deeper(_, LineItemZL(DeeperL(LetLineZA(p, zty, e1)), e2)))
      when ZTyp.is_before(zty) =>
    let (e1, ty1, u_gen) = Statics.syn_fix_holes(ctx, u_gen, e1);
    let (p, ctx, u_gen) =
      Statics.ana_pat_fix_holes(ctx, u_gen, false, p, ty1);
    let (e2, ty, u_gen) = Statics.syn_fix_holes(ctx, u_gen, e2);
    let ze =
      ZExp.(
        prepend_zline(DeeperL(LetLineZP(CursorP(After, p), None, e1)), e2)
      );
    Some((ze, ty, u_gen));
  | (
      Delete,
      Deeper(_, LineItemZL(DeeperL(LetLineZP(zp, Some(_), e1)), e2)),
    )
      when ZPat.is_after(zp) =>
    let (e1, ty1, u_gen) = Statics.syn_fix_holes(ctx, u_gen, e1);
    let (zp, ctx, u_gen) = ana_zpat_fix_holes(ctx, u_gen, zp, ty1);
    let (e2, ty, u_gen) = Statics.syn_fix_holes(ctx, u_gen, e2);
    let ze = ZExp.prepend_zline(DeeperL(LetLineZP(zp, None, e1)), e2);
    Some((ze, ty, u_gen));
  | (Backspace, Deeper(_, LamZA(p, zty, e1))) when ZTyp.is_before(zty) =>
    let (p, ctx, u_gen) =
      Statics.ana_pat_fix_holes(ctx, u_gen, false, p, Hole);
    let (e1, ty2, u_gen) = Statics.syn_fix_holes(ctx, u_gen, e1);
    let ze = ZExp.Deeper(NotInHole, LamZP(CursorP(After, p), None, e1));
    Some((ze, Arrow(Hole, ty2), u_gen));
  | (Delete, Deeper(_, LamZP(zp, Some(_), e1))) when ZPat.is_after(zp) =>
    let (zp, ctx, u_gen) = ana_zpat_fix_holes(ctx, u_gen, zp, Hole);
    let (e1, ty2, u_gen) = Statics.syn_fix_holes(ctx, u_gen, e1);
    let ze = ZExp.Deeper(NotInHole, LamZP(zp, None, e1));
    Some((ze, Arrow(Hole, ty2), u_gen));
  | (Backspace, Deeper(_, CaseZA(e1, rules, zann)))
      when ZTyp.is_before(zann) =>
    /* can't delete annotation on case in synthetic position */
    None
  | (Delete, CursorE(In(_), Tm(_, Case(_, _, _)))) =>
    /* disable attempt to delete type ascription after `end` delimiter
     * until we have more fine-grained In positions for Case */
    None
  | (
      Backspace,
      Deeper(
        _,
        OpSeqZ(_, CursorE(Before, e0) as ze0, EmptySuffix(_) as surround),
      ),
    )
  | (
      Backspace,
      Deeper(
        _,
        OpSeqZ(
          _,
          CursorE(Before, e0) as ze0,
          BothNonEmpty(_, _) as surround,
        ),
      ),
    ) =>
    abs_perform_Backspace_Before_op(
      combine_for_Backspace_Space,
      zexp_syn_fix_holes,
      make_and_syn_OpSeqZ,
      UHExp.is_EmptyHole,
      UHExp.is_Space,
      UHExp.Space,
      (side, e) => CursorE(side, e),
      ctx,
      u_gen,
      e0,
      ze0,
      surround,
    )
  | (
      Delete,
      Deeper(
        _,
        OpSeqZ(_, CursorE(After, e0) as ze0, EmptyPrefix(_) as surround),
      ),
    )
  | (
      Delete,
      Deeper(
        _,
        OpSeqZ(
          _,
          CursorE(After, e0) as ze0,
          BothNonEmpty(_, _) as surround,
        ),
      ),
    ) =>
    abs_perform_Delete_After_op(
      combine_for_Delete_Space,
      zexp_syn_fix_holes,
      make_and_syn_OpSeqZ,
      UHExp.is_EmptyHole,
      UHExp.is_Space,
      UHExp.Space,
      (side, e) => CursorE(side, e),
      ctx,
      u_gen,
      e0,
      ze0,
      surround,
    )
  | (Backspace, CursorE(In(_), e))
  | (Delete, CursorE(In(_), e)) =>
    let (e', u_gen') = UHExp.new_EmptyHole(u_gen);
    let ze' = ZExp.CursorE(Before, e');
    Some((ze', Hole, u_gen'));
  /* Construction */
  | (Construct(SLine), Deeper(_, LineItemZL(zli, e2)))
      when ZExp.is_after_line_item(zli) =>
    let li = ZExp.erase_line_item(zli);
    let ze =
      ZExp.(
        prune_and_prepend_line(
          li,
          prepend_zline(CursorL(Before, EmptyLine), e2),
        )
      );
    Some((ze, ty, u_gen));
  | (Construct(SLine), ze1) when ZExp.is_before(ze1) =>
    let ze = ZExp.prepend_line(EmptyLine, ze1);
    Some((ze, ty, u_gen));
  | (Construct(SLine), ze1) when ZExp.is_after(ze1) =>
    let e1 = ZExp.erase(ze1);
    let (ze2, u_gen) = ZExp.new_EmptyHole(u_gen);
    let ze = ZExp.prune_and_prepend_lines(e1, ze2);
    Some(zexp_syn_fix_holes(ctx, u_gen, ze));
  | (Construct(SLine), CursorE(_, _)) => None
  | (Construct(_) as a, Deeper(_, LineItemZL(CursorL(_, EmptyLine), e2))) =>
    let (e1, u_gen) = UHExp.new_EmptyHole(u_gen);
    let ze1 = ZExp.CursorE(Before, e1);
    let ze = ZExp.prepend_zline(DeeperL(ExpLineZ(ze1)), e2);
    perform_syn(ctx, a, (ze, Hole, u_gen));
  | (
      Construct(_) as a,
      Deeper(_, LineItemZL(CursorL(side, ExpLine(e1)), e2)),
    ) =>
    switch (side) {
    | In(_) => None
    | Before =>
      let ze1 = ZExp.place_before(e1);
      let ze = ZExp.prune_and_prepend_zline(DeeperL(ExpLineZ(ze1)), e2);
      perform_syn(ctx, a, (ze, ty, u_gen));
    | After =>
      let ze1 = ZExp.place_after(e1);
      let ze = ZExp.prune_and_prepend_zline(DeeperL(ExpLineZ(ze1)), e2);
      perform_syn(ctx, a, (ze, ty, u_gen));
    }
  | (Construct(_), Deeper(_, LineItemZL(CursorL(_, LetLine(_, _, _)), _))) =>
    None
  | (
      Construct(SOp(SSpace)),
      Deeper(
        _,
        LineItemZL(
          DeeperL(
            ExpLineZ(
              Deeper(
                _,
                OpSeqZ(
                  _,
                  CursorE(After, Tm(_, Var(InVHole(Keyword(k), _), _))),
                  EmptyPrefix(suffix),
                ),
              ),
            ),
          ),
          e2,
        ),
      ),
    ) =>
    let (e1, u_gen) = keyword_suffix_to_exp(suffix, u_gen);
    let ze1 = ZExp.place_before(e1);
    let ze = ZExp.prepend_zline(DeeperL(ExpLineZ(ze1)), e2);
    perform_syn(ctx, keyword_action(k), (ze, ty, u_gen));
  | (
      Construct(SOp(SSpace)),
      Deeper(
        _,
        LineItemZL(
          DeeperL(
            ExpLineZ(
              CursorE(After, Tm(_, Var(InVHole(Keyword(k), _), _))),
            ),
          ),
          e2,
        ),
      ),
    ) =>
    let ze = ZExp.prepend_zline(CursorL(Before, EmptyLine), e2);
    perform_syn(ctx, keyword_action(k), (ze, ty, u_gen));
  | (
      Construct(SOp(SSpace)),
      Deeper(
        _,
        OpSeqZ(
          _,
          CursorE(After, Tm(_, Var(InVHole(Keyword(k), _), _))),
          EmptyPrefix(suffix),
        ),
      ),
    ) =>
    let (e, u_gen) = keyword_suffix_to_exp(suffix, u_gen);
    switch (Statics.syn(ctx, e)) {
    | None => None
    | Some(ty) =>
      let ze = ZExp.place_before(e);
      perform_syn(ctx, keyword_action(k), (ze, ty, u_gen));
    };
  | (
      Construct(SOp(SSpace)),
      CursorE(After, Tm(_, Var(InVHole(Keyword(k), _), _))),
    ) =>
    let (ze, u_gen) = ZExp.new_EmptyHole(u_gen);
    perform_syn(ctx, keyword_action(k), (ze, Hole, u_gen));
  | (Construct(SParenthesized), CursorE(cursor_side, e)) =>
    Some((ParenthesizedZ(ze), ty, u_gen))
  | (
      Construct(SAsc),
      Deeper(err_status, LineItemZL(DeeperL(LetLineZP(zp, None, e1)), e2)),
    ) =>
    switch (Statics.syn(ctx, e1)) {
    | None => None
    | Some(ty1) =>
      let uty1 = UHTyp.contract(ty1);
      let ze =
        ZExp.Deeper(
          err_status,
          LineItemZL(
            DeeperL(
              LetLineZA(ZPat.erase(zp), ZTyp.place_before(uty1), e1),
            ),
            e2,
          ),
        );
      Some((ze, ty, u_gen));
    }
  | (Construct(SAsc), Deeper(err_status, LamZP(zp, None, e1))) =>
    let ze =
      ZExp.Deeper(
        err_status,
        LamZA(ZPat.erase(zp), ZTyp.place_before(UHTyp.Hole), e1),
      );
    Some((ze, ty, u_gen));
  | (
      Construct(SAsc),
      Deeper(
        err_status,
        LineItemZL(DeeperL(LetLineZP(zp, Some(uty1), e1)), e2),
      ),
    ) =>
    /* just move the cursor over if there is already an ascription */
    let ze =
      ZExp.Deeper(
        err_status,
        LineItemZL(
          DeeperL(LetLineZA(ZPat.erase(zp), ZTyp.place_before(uty1), e1)),
          e2,
        ),
      );
    Some((ze, ty, u_gen));
  | (Construct(SAsc), Deeper(err_status, LamZP(zp, Some(uty1), e1))) =>
    /* just move the cursor over if there is already an ascription */
    let ze =
      ZExp.Deeper(
        err_status,
        LamZA(ZPat.erase(zp), ZTyp.place_before(uty1), e1),
      );
    Some((ze, ty, u_gen));
  | (
      Construct(SAsc),
      CursorE(_, Tm(err_status, Case(e1, rules, Some(uty)))),
    ) =>
    /* just move the cursor over if there is already an ascription */
    let ze =
      ZExp.Deeper(NotInHole, CaseZA(e1, rules, ZTyp.place_before(uty)));
    Some((ze, ty, u_gen));
  | (Construct(SAsc), CursorE(_, _)) => None
  | (Construct(SVar(x, side)), CursorE(_, Tm(_, EmptyHole(_))))
  | (Construct(SVar(x, side)), CursorE(_, Tm(_, Var(_, _))))
  | (Construct(SVar(x, side)), CursorE(_, Tm(_, NumLit(_))))
  | (Construct(SVar(x, side)), CursorE(_, Tm(_, BoolLit(_)))) =>
    if (String.equal(x, "true")) {
      Some((CursorE(side, Tm(NotInHole, BoolLit(true))), Bool, u_gen));
    } else if (String.equal(x, "false")) {
      Some((CursorE(side, Tm(NotInHole, BoolLit(false))), Bool, u_gen));
    } else if (Var.is_let(x)) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      Some((
        CursorE(side, Tm(NotInHole, Var(InVHole(Keyword(Let), u), x))),
        Hole,
        u_gen,
      ));
    } else if (Var.is_case(x)) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      Some((
        CursorE(side, Tm(NotInHole, Var(InVHole(Keyword(Case), u), x))),
        Hole,
        u_gen,
      ));
    } else {
      Var.check_valid(
        x,
        {
          let gamma = Contexts.gamma(ctx);
          switch (VarMap.lookup(gamma, x)) {
          | Some(xty) =>
            Some((
              ZExp.CursorE(side, Tm(NotInHole, Var(NotInVHole, x))),
              xty,
              u_gen,
            ))
          | None =>
            let (u, u_gen) = MetaVarGen.next(u_gen);
            Some((
              ZExp.CursorE(side, Tm(NotInHole, Var(InVHole(Free, u), x))),
              HTyp.Hole,
              u_gen,
            ));
          };
        },
      );
    }
  | (Construct(SVar(_, _)), CursorE(_, _)) => None
  | (Construct(SLet), Deeper(_, LineItemZL(DeeperL(ExpLineZ(ze1)), e2)))
      when ZExp.is_before(ze1) =>
    let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
    let e1 = ZExp.erase(ze1);
    let ze =
      ZExp.Deeper(
        NotInHole,
        LineItemZL(DeeperL(LetLineZP(zp, None, e1)), e2),
      );
    Some((ze, ty, u_gen));
  | (Construct(SLet), ze1) when ZExp.is_before(ze1) =>
    let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
    let e1 = ZExp.erase(ze1);
    let (e2, u_gen) = UHExp.new_EmptyHole(u_gen);
    let ze =
      ZExp.Deeper(
        NotInHole,
        LineItemZL(DeeperL(LetLineZP(zp, None, e1)), e2),
      );
    Some((ze, ty, u_gen));
  | (Construct(SLet), CursorE(_, _)) => None
  /* cannot construct let in middle of opseq without parens */
  | (Construct(SLet), ze) when is_not_Before_opseq(ze) => None
  | (Construct(SLam), CursorE(_, e1)) =>
    let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
    let ze = ZExp.Deeper(NotInHole, LamZP(zp, Some(Hole), e1));
    let ty' = HTyp.Arrow(Hole, ty);
    Some((ze, ty', u_gen));
  | (Construct(SNumLit(n, side)), CursorE(_, Tm(_, EmptyHole(_))))
  | (Construct(SNumLit(n, side)), CursorE(_, Tm(_, NumLit(_))))
  | (Construct(SNumLit(n, side)), CursorE(_, Tm(_, BoolLit(_))))
  | (Construct(SNumLit(n, side)), CursorE(_, Tm(_, Var(_, _)))) =>
    Some((CursorE(side, Tm(NotInHole, NumLit(n))), Num, u_gen))
  | (Construct(SNumLit(_, _)), CursorE(_, _)) => None
  | (Construct(SInj(side)), CursorE(_, e)) =>
    let ze' = ZExp.Deeper(NotInHole, InjZ(side, ze));
    let ty' =
      switch (side) {
      | L => HTyp.Sum(ty, Hole)
      | R => HTyp.Sum(Hole, ty)
      };
    Some((ze', ty', u_gen));
  | (Construct(SListNil), CursorE(_, Tm(_, EmptyHole(_)))) =>
    let ze = ZExp.CursorE(After, Tm(NotInHole, ListNil));
    let ty = HTyp.List(Hole);
    Some((ze, ty, u_gen));
  | (Construct(SListNil), CursorE(_, _)) => None
  | (Construct(SCase), Deeper(_, LineItemZL(DeeperL(ExpLineZ(ze1)), e2)))
      when ZExp.is_before(ze1) =>
    let e1 = ZExp.erase(ze1);
    let ze =
      switch (e1) {
      | Tm(_, EmptyHole(_)) =>
        let (rule_p, u_gen) = UHPat.new_EmptyHole(u_gen);
        let rule = UHExp.Rule(rule_p, e2);
        ZExp.Deeper(NotInHole, CaseZE(ze1, [rule], Some(Hole)));
      | _ =>
        let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
        let zrule = ZExp.RuleZP(zp, e2);
        let zrules = ZList.singleton(zrule);
        ZExp.Deeper(NotInHole, CaseZR(e1, zrules, Some(Hole)));
      };
    Some((ze, Hole, u_gen));
  | (Construct(SCase), ze1) when ZExp.is_before(ze1) =>
    let e1 = ZExp.erase(ze1);
    let ze =
      switch (e1) {
      | Tm(_, EmptyHole(_)) =>
        let (rule_p, u_gen) = UHPat.new_EmptyHole(u_gen);
        let (rule_e, u_gen) = UHExp.new_EmptyHole(u_gen);
        let rule = UHExp.Rule(rule_p, rule_e);
        ZExp.Deeper(NotInHole, CaseZE(ze1, [rule], Some(Hole)));
      | _ =>
        let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
        let (rule_e, u_gen) = UHExp.new_EmptyHole(u_gen);
        let zrule = ZExp.RuleZP(zp, rule_e);
        let zrules = ZList.singleton(zrule);
        ZExp.Deeper(NotInHole, CaseZR(e1, zrules, Some(Hole)));
      };
    Some((ze, Hole, u_gen));
  | (Construct(SCase), CursorE(_, _)) => None
  /* cannot construct case in middle of opseq without parens */
  | (Construct(SCase), ze) when is_not_Before_opseq(ze) => None
  | (
      Construct(SOp(os)),
      Deeper(_, OpSeqZ(_, CursorE(In(_), e), surround)),
    )
  | (
      Construct(SOp(os)),
      Deeper(_, OpSeqZ(_, CursorE(After, e), surround)),
    ) =>
    switch (exp_op_of(os)) {
    | None => None
    | Some(op) =>
      Some(
        abs_perform_Construct_SOp_After_surround(
          ZExp.new_EmptyHole,
          make_and_syn_OpSeqZ,
          UHExp.is_Space,
          UHExp.Space,
          (side, e) => CursorE(side, e),
          ctx,
          u_gen,
          e,
          op,
          surround,
        ),
      )
    }
  | (
      Construct(SOp(os)),
      Deeper(_, OpSeqZ(_, CursorE(Before, _) as ze0, surround)),
    ) =>
    switch (exp_op_of(os)) {
    | None => None
    | Some(op) =>
      Some(
        abs_perform_Construct_SOp_Before_surround(
          ZExp.erase,
          ZExp.new_EmptyHole,
          make_and_syn_OpSeqZ,
          UHExp.is_Space,
          UHExp.Space,
          (side, e) => CursorE(side, e),
          ctx,
          u_gen,
          ze0,
          op,
          surround,
        ),
      )
    }
  | (Construct(SOp(os)), CursorE(In(_), e))
  | (Construct(SOp(os)), CursorE(After, e)) =>
    switch (exp_op_of(os)) {
    | None => None
    | Some(op) =>
      Some(
        abs_perform_Construct_SOp_After(
          UHExp.bidelimit,
          ZExp.new_EmptyHole,
          make_and_syn_OpSeqZ,
          ctx,
          u_gen,
          e,
          op,
        ),
      )
    }
  | (Construct(SOp(os)), CursorE(Before, e)) =>
    switch (exp_op_of(os)) {
    | None => None
    | Some(op) =>
      Some(
        abs_perform_Construct_SOp_Before(
          UHExp.bidelimit,
          ZExp.new_EmptyHole,
          make_and_syn_OpSeqZ,
          ctx,
          u_gen,
          e,
          op,
        ),
      )
    }
  | (Construct(SApPalette(name)), CursorE(_, Tm(_, EmptyHole(_)))) =>
    let palette_ctx = Contexts.palette_ctx(ctx);
    switch (PaletteCtx.lookup(palette_ctx, name)) {
    | None => None
    | Some(palette_defn) =>
      let init_model_cmd = palette_defn.init_model;
      let (init_model, init_splice_info, u_gen) =
        SpliceGenMonad.exec(init_model_cmd, SpliceInfo.empty, u_gen);
      switch (Statics.ana_splice_map(ctx, init_splice_info.splice_map)) {
      | None => None
      | Some(splice_ctx) =>
        let expansion_ty = palette_defn.expansion_ty;
        let expand = palette_defn.expand;
        let expansion = expand(init_model);
        switch (Statics.ana(splice_ctx, expansion, expansion_ty)) {
        | None => None
        | Some(_) =>
          Some((
            CursorE(
              Before,
              Tm(NotInHole, ApPalette(name, init_model, init_splice_info)),
            ),
            expansion_ty,
            u_gen,
          ))
        };
      };
    };
  | (Construct(SApPalette(_)), CursorE(_, _)) => None
  | (
      UpdateApPalette(cmd),
      CursorE(_, Tm(_, ApPalette(name, _, hole_data))),
    ) =>
    None
  /* TODO let (_, palette_ctx) = ctx;
     switch (PaletteCtx.lookup(palette_ctx, name)) {
     | Some(palette_defn) =>
       let (q, u_gen') = UHExp.HoleRefs.exec(monad, hole_data, u_gen);
       let (serialized_model, hole_data') = q;
       let expansion_ty = UHExp.PaletteDefinition.expansion_ty(palette_defn);
       let expansion =
         (UHExp.PaletteDefinition.to_exp(palette_defn))(serialized_model);
       let (_, hole_map') = hole_data';
       let expansion_ctx =
         UHExp.PaletteHoleData.extend_ctx_with_hole_map(ctx, hole_map');
       switch (Statics.ana(expansion_ctx, expansion, expansion_ty)) {
       | Some(_) =>
         Some((
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
       | None => None
       };
     | None => None
     }; */
  | (UpdateApPalette(_), CursorE(_, _))
  | (UpdateApPalette(_), Deeper(_, LineItemZL(CursorL(_, _), _))) => None
  /* Zipper Cases */
  | (_, ParenthesizedZ(ze1)) =>
    switch (perform_syn(ctx, a, (ze1, ty, u_gen))) {
    | Some((ze1', ty', u_gen')) => Some((ParenthesizedZ(ze1'), ty', u_gen'))
    | None => None
    }
  | (_, Deeper(_, LineItemZL(DeeperL(ExpLineZ(ze1)), e2))) =>
    switch (Statics.syn(ctx, ZExp.erase(ze1))) {
    | None => None
    | Some(ty1) =>
      switch (perform_syn(ctx, a, (ze1, ty1, u_gen))) {
      | None => None
      | Some((ze1, _, u_gen)) =>
        let ze = ZExp.prune_and_prepend_zline(DeeperL(ExpLineZ(ze1)), e2);
        Some((ze, ty, u_gen));
      }
    }
  | (_, Deeper(_, LineItemZL(DeeperL(LetLineZP(zp, ann, e1)), e2))) =>
    switch (ann) {
    | Some(uty1) =>
      let ty1 = UHTyp.expand(uty1);
      switch (perform_ana_pat(ctx, u_gen, a, zp, ty1)) {
      | None => None
      | Some((zp, ctx2, u_gen)) =>
        let p = ZPat.erase(zp);
        let ctx1 = Statics.ctx_for_let(ctx, p, ty1, e1);
        let (e1, u_gen) = Statics.ana_fix_holes(ctx1, u_gen, e1, ty1);
        let (e2, ty, u_gen) = Statics.syn_fix_holes(ctx2, u_gen, e2);
        let ze = ZExp.prepend_zline(DeeperL(LetLineZP(zp, ann, e1)), e2);
        Some((ze, ty, u_gen));
      };
    | None =>
      switch (Statics.syn(ctx, e1)) {
      | None => None
      | Some(ty1) =>
        switch (perform_ana_pat(ctx, u_gen, a, zp, ty1)) {
        | None => None
        | Some((zp, ctx2, u_gen)) =>
          let (e2, ty, u_gen) = Statics.syn_fix_holes(ctx2, u_gen, e2);
          let ze = ZExp.prepend_zline(DeeperL(LetLineZP(zp, ann, e1)), e2);
          Some((ze, ty, u_gen));
        }
      }
    }
  | (_, Deeper(_, LineItemZL(DeeperL(LetLineZA(p, zann, e1)), e2))) =>
    /* (ctx) let p (ctx2) : ty = (ctx1) e1 in (ctx2) e2 */
    switch (perform_ty(a, zann)) {
    | None => None
    | Some(zann) =>
      let ty1 = UHTyp.expand(ZTyp.erase(zann));
      let (p, ctx2, u_gen) =
        Statics.ana_pat_fix_holes(ctx, u_gen, false, p, ty1);
      let ctx1 = Statics.ctx_for_let(ctx, p, ty1, e1);
      let (e1, u_gen) = Statics.ana_fix_holes(ctx1, u_gen, e1, ty1);
      let (e2, ty, u_gen) = Statics.syn_fix_holes(ctx2, u_gen, e2);
      let ze = ZExp.prepend_zline(DeeperL(LetLineZA(p, zann, e1)), e2);
      Some((ze, ty, u_gen));
    }
  | (_, Deeper(_, LineItemZL(DeeperL(LetLineZE(p, ann, ze1)), e2))) =>
    switch (ann) {
    | Some(ann_ty) =>
      let ty1 = UHTyp.expand(ann_ty);
      let ctx1 = Statics.ctx_for_let(ctx, p, ty1, ZExp.erase(ze1));
      switch (perform_ana(u_gen, ctx1, a, ze1, ty1)) {
      | None => None
      | Some((ze1, u_gen)) =>
        let ze = ZExp.prepend_zline(DeeperL(LetLineZE(p, ann, ze1)), e2);
        Some((ze, ty, u_gen));
      };
    | None =>
      let e1 = ZExp.erase(ze1);
      switch (Statics.syn(ctx, e1)) {
      | None => None
      | Some(ty1) =>
        switch (perform_syn(ctx, a, (ze1, ty1, u_gen))) {
        | None => None
        | Some((ze1, ty1, u_gen)) =>
          let (p, ctx2, u_gen) =
            Statics.ana_pat_fix_holes(ctx, u_gen, false, p, ty1);
          let (e2, ty, u_gen) = Statics.syn_fix_holes(ctx2, u_gen, e2);
          let ze = ZExp.prepend_zline(DeeperL(LetLineZE(p, ann, ze1)), e2);
          Some((ze, ty, u_gen));
        }
      };
    }
  | (_, Deeper(_, LineItemZE(EmptyLine as li, ze2)))
  | (_, Deeper(_, LineItemZE(ExpLine(_) as li, ze2))) =>
    switch (perform_syn(ctx, a, (ze2, ty, u_gen))) {
    | None => None
    | Some((ze2, ty, u_gen)) =>
      let ze = ZExp.prepend_line(li, ze2);
      Some((ze, ty, u_gen));
    }
  | (_, Deeper(_, LineItemZE(LetLine(p, ann, e1), ze2))) =>
    let ty1 =
      switch (ann) {
      | Some(uty1) => Some(UHTyp.expand(uty1))
      | None => Statics.syn(ctx, e1)
      };
    switch (ty1) {
    | None => None
    | Some(ty1) =>
      switch (Statics.ana_pat(ctx, p, ty1)) {
      | None => None
      | Some(ctx2) =>
        switch (perform_syn(ctx2, a, (ze2, ty, u_gen))) {
        | None => None
        | Some((ze2, ty, u_gen)) =>
          let ze = ZExp.prepend_line(LetLine(p, ann, e1), ze2);
          Some((ze, ty, u_gen));
        }
      }
    };
  | (_, Deeper(_, LamZP(zp, ann, e1))) =>
    let ty1 =
      switch (ann) {
      | Some(uty1) => UHTyp.expand(uty1)
      | None => HTyp.Hole
      };
    switch (perform_ana_pat(ctx, u_gen, a, zp, ty1)) {
    | None => None
    | Some((zp, ctx, u_gen)) =>
      let (e1, ty2, u_gen) = Statics.syn_fix_holes(ctx, u_gen, e1);
      let ty = HTyp.Arrow(ty1, ty2);
      let ze = ZExp.Deeper(NotInHole, LamZP(zp, ann, e1));
      Some((ze, ty, u_gen));
    };
  | (_, Deeper(_, LamZA(p, zann, e1))) =>
    switch (perform_ty(a, zann)) {
    | None => None
    | Some(zann) =>
      let ty1 = UHTyp.expand(ZTyp.erase(zann));
      let (p, ctx, u_gen) =
        Statics.ana_pat_fix_holes(ctx, u_gen, false, p, ty1);
      let (e1, ty2, u_gen) = Statics.syn_fix_holes(ctx, u_gen, e1);
      let ze = ZExp.Deeper(NotInHole, LamZA(p, zann, e1));
      Some((ze, Arrow(ty1, ty2), u_gen));
    }
  | (_, Deeper(_, LamZE(p, ann, ze1))) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => None
    | Some((_, ty2)) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => HTyp.Hole
        };
      switch (Statics.ana_pat(ctx, p, ty1)) {
      | None => None
      | Some(ctx) =>
        switch (perform_syn(ctx, a, (ze1, ty2, u_gen))) {
        | None => None
        | Some((ze1, ty2, u_gen)) =>
          let ze = ZExp.Deeper(NotInHole, LamZE(p, ann, ze1));
          Some((ze, Arrow(ty1, ty2), u_gen));
        }
      };
    }
  | (_, Deeper(_, InjZ(side, ze1))) =>
    switch (ty) {
    | Sum(ty1, ty2) =>
      let ty_side = pick_side(side, ty1, ty2);
      switch (perform_syn(ctx, a, (ze1, ty_side, u_gen))) {
      | None => None
      | Some((ze1', ty_side', u_gen')) =>
        let ty' =
          switch (side) {
          | L => HTyp.Sum(ty_side', ty2)
          | R => HTyp.Sum(ty1, ty_side')
          };
        Some((Deeper(NotInHole, InjZ(side, ze1')), ty', u_gen'));
      };
    | _ => None /* should never happen */
    }
  | (_, Deeper(err, OpSeqZ(_, ze0, surround))) =>
    let i = OperatorSeq.surround_prefix_length(surround);
    switch (ZExp.erase(ze)) {
    | Tm(_, OpSeq(skel, seq)) =>
      switch (Statics.syn_skel(ctx, skel, seq, Some(i))) {
      | Some((ty, Some(mode))) =>
        switch (mode) {
        | Statics.AnalyzedAgainst(ty0) =>
          switch (perform_ana(u_gen, ctx, a, ze0, ty0)) {
          | None => None
          | Some((ze0', u_gen)) =>
            let ze0'' = ZExp.bidelimit(ze0');
            Some((Deeper(err, OpSeqZ(skel, ze0'', surround)), ty, u_gen));
          }
        | Statics.Synthesized(ty0) =>
          switch (perform_syn(ctx, a, (ze0, ty0, u_gen))) {
          | None => None
          | Some((ze0', ty0', u_gen)) =>
            let ze0'' = ZExp.bidelimit(ze0');
            Some(make_and_syn_OpSeqZ(ctx, u_gen, ze0'', surround));
          }
        }
      | Some(_) => None /* should never happen */
      | None => None /* should never happen */
      }
    | _ => None /* should never happen */
    };
  | (_, Deeper(_, ApPaletteZ(name, serialized_model, z_hole_data))) => None
  /* TODO let (next_lbl, z_nat_map) = z_hole_data;
     let (rest_map, z_data) = z_nat_map;
     let (cell_lbl, cell_data) = z_data;
     let (cell_ty, cell_ze) = cell_data;
     switch (perform_ana(u_gen, ctx, a, cell_ze, cell_ty)) {
     | None => None
     | Some((cell_ze', u_gen')) =>
       let z_hole_data' = (
         next_lbl,
         (rest_map, (cell_lbl, (cell_ty, cell_ze'))),
       );
       Some((
         Deeper(
           NotInHole,
           ApPaletteZ(name, serialized_model, z_hole_data'),
         ),
         ty,
         u_gen',
       ));
     }; */
  | (_, Deeper(_, CaseZE(_, _, None)))
  | (_, Deeper(_, CaseZR(_, _, None))) => None
  | (_, Deeper(_, CaseZE(ze1, rules, Some(uty) as ann))) =>
    switch (Statics.syn(ctx, ZExp.erase(ze1))) {
    | None => None
    | Some(ty1) =>
      switch (perform_syn(ctx, a, (ze1, ty1, u_gen))) {
      | None => None
      | Some((ze1, ty1, u_gen)) =>
        let ty = UHTyp.expand(uty);
        let (rules, u_gen) =
          Statics.ana_rules_fix_holes(ctx, u_gen, false, rules, ty1, ty);
        let ze = ZExp.Deeper(NotInHole, CaseZE(ze1, rules, ann));
        Some((ze, ty, u_gen));
      }
    }
  | (_, Deeper(_, CaseZR(e1, zrules, Some(uty) as ann))) =>
    switch (Statics.syn(ctx, e1)) {
    | None => None
    | Some(ty1) =>
      switch (ZList.prj_z(zrules)) {
      | RuleZP(zp, e) =>
        switch (perform_ana_pat(ctx, u_gen, a, zp, ty1)) {
        | None => None
        | Some((zp, ctx, u_gen)) =>
          let ty = UHTyp.expand(uty);
          let (e, u_gen) = Statics.ana_fix_holes(ctx, u_gen, e, ty);
          let zrule = ZExp.RuleZP(zp, e);
          let ze =
            ZExp.Deeper(
              NotInHole,
              CaseZR(e1, ZList.replace_z(zrules, zrule), ann),
            );
          Some((ze, ty, u_gen));
        }
      | RuleZE(p, ze) =>
        switch (Statics.ana_pat(ctx, p, ty1)) {
        | None => None
        | Some(ctx) =>
          let ty = UHTyp.expand(uty);
          switch (perform_ana(u_gen, ctx, a, ze, ty)) {
          | None => None
          | Some((ze, u_gen)) =>
            let zrule = ZExp.RuleZE(p, ze);
            let ze =
              ZExp.Deeper(
                NotInHole,
                CaseZR(e1, ZList.replace_z(zrules, zrule), ann),
              );
            Some((ze, ty, u_gen));
          };
        }
      }
    }
  | (_, Deeper(_, CaseZA(e1, rules, zann))) =>
    switch (Statics.syn(ctx, e1)) {
    | None => None
    | Some(ty1) =>
      switch (perform_ty(a, zann)) {
      | None => None
      | Some(zann) =>
        let ty = UHTyp.expand(ZTyp.erase(zann));
        let (rules, u_gen) =
          Statics.ana_rules_fix_holes(ctx, u_gen, false, rules, ty1, ty);
        let ze = ZExp.Deeper(NotInHole, CaseZA(e1, rules, zann));
        Some((ze, ty, u_gen));
      }
    }
  /* Invalid actions at expression level */
  | (Construct(SNum), _)
  | (Construct(SBool), _)
  | (Construct(SList), _)
  | (Construct(SWild), _) => None
  };
}
and perform_ana =
    (u_gen: MetaVarGen.t, ctx: Contexts.t, a: t, ze: ZExp.t, ty: HTyp.t)
    : option((ZExp.t, MetaVarGen.t)) =>
  switch (a, ze) {
  | (_, Deeper(InHole(TypeInconsistent, u) as err, ze1')) =>
    let ze' = ZExp.set_err_status(NotInHole, ze);
    let e' = ZExp.erase(ze');
    switch (Statics.syn(ctx, e')) {
    | Some(ty1) =>
      switch (perform_syn(ctx, a, (ze', ty1, u_gen))) {
      | Some((ze', ty1', u_gen')) =>
        if (HTyp.consistent(ty1', ty)) {
          Some((ze', u_gen'));
        } else {
          Some((ZExp.set_err_status(err, ze'), u_gen'));
        }
      | None => None
      }
    | None => None
    };
  /* Movement */
  | (MoveTo(path), _) =>
    let e = ZExp.erase(ze);
    switch (Path.follow_e(path, e)) {
    | Some(ze') => Some((ze', u_gen))
    | None => None
    };
  | (MoveToPrevHole, _) =>
    switch (Path.prev_hole_path(Path.holes_ze(ze, []))) {
    | None => None
    | Some(path) => perform_ana(u_gen, ctx, MoveTo(path), ze, ty)
    }
  | (MoveToNextHole, _) =>
    switch (Path.next_hole_path(Path.holes_ze(ze, []))) {
    | None => None
    | Some(path) =>
      /* [debug] let path = Helper.log_path path in */
      perform_ana(u_gen, ctx, MoveTo(path), ze, ty)
    }
  /* Backspace & Delete */
  | (Backspace, CursorE(After, e)) =>
    switch (e) {
    | Tm(_, EmptyHole(_)) => Some((CursorE(Before, e), u_gen))
    | _ =>
      let (e', u_gen) = UHExp.new_EmptyHole(u_gen);
      Some((CursorE(Before, e'), u_gen));
    }
  | (Backspace, CursorE(Before, e)) => None
  | (Delete, CursorE(Before, e)) =>
    switch (e) {
    | Tm(_, EmptyHole(_)) => Some((CursorE(After, e), u_gen))
    | _ =>
      let (e', u_gen) = UHExp.new_EmptyHole(u_gen);
      Some((CursorE(Before, e'), u_gen));
    }
  | (Delete, CursorE(After, e)) => None
  | (Backspace, CursorE(In(_), e))
  | (Delete, CursorE(In(_), e)) =>
    let (e', u_gen) = UHExp.new_EmptyHole(u_gen);
    let ze' = ZExp.CursorE(Before, e');
    Some((ze', u_gen));
  | (
      Backspace,
      Deeper(
        err_status,
        LineItemZE(li, Deeper(_, LineItemZL(CursorL(_, EmptyLine), e2))),
      ),
    ) =>
    let zli = ZExp.place_after_line_item(li);
    let ze = ZExp.prepend_zline(~err_status, zli, e2);
    Some((ze, u_gen));
  | (Delete, Deeper(_, LineItemZL(CursorL(_, EmptyLine), e1))) =>
    let ze = ZExp.place_before(e1);
    Some((ze, u_gen));
  | (Backspace, Deeper(_, LineItemZL(CursorL(Before, _), e2))) => None
  | (Backspace, Deeper(err_status, LineItemZL(CursorL(After, _), e2)))
  | (Backspace, Deeper(err_status, LineItemZL(CursorL(In(_), _), e2))) =>
    let (e2, u_gen) = Statics.ana_fix_holes(ctx, u_gen, e2, ty);
    let ze = ZExp.prepend_zline(~err_status, CursorL(After, EmptyLine), e2);
    Some((ze, u_gen));
  | (Delete, Deeper(_, LineItemZL(CursorL(After, _), e2))) => None
  | (Delete, Deeper(err_status, LineItemZL(CursorL(Before, _), e2)))
  | (Delete, Deeper(err_status, LineItemZL(CursorL(In(_), _), e2))) =>
    let (e2, u_gen) = Statics.ana_fix_holes(ctx, u_gen, e2, ty);
    let ze = ZExp.prepend_zline(~err_status, CursorL(Before, EmptyLine), e2);
    Some((ze, u_gen));
  | (Backspace, Deeper(_, LineItemZE(EmptyLine, ze1)))
      when ZExp.is_before(ze1) =>
    Some((ze1, u_gen))
  | (
      Delete,
      Deeper(err_status, LineItemZL(zli, Tm(_, LineItem(EmptyLine, e2)))),
    )
      when ZExp.is_after_line_item(zli) =>
    let ze = ZExp.prepend_zline(~err_status, zli, e2);
    Some((ze, u_gen));
  | (
      Backspace,
      Deeper(
        _,
        LineItemZE(ExpLine(e1), CursorE(Before, Tm(_, EmptyHole(_)))),
      ),
    ) =>
    let (e, u_gen) = Statics.ana_fix_holes(ctx, u_gen, e1, ty);
    let ze = ZExp.place_after(e);
    Some((ze, u_gen));
  | (
      Delete,
      Deeper(_, LineItemZL(DeeperL(ExpLineZ(ze1)), Tm(_, EmptyHole(_)))),
    )
      when ZExp.is_after(ze1) =>
    Some(zexp_ana_fix_holes(ctx, u_gen, ze1, ty))
  | (Backspace, Deeper(_, LineItemZL(DeeperL(LetLineZA(p, zty, e1)), e2)))
      when ZTyp.is_before(zty) =>
    let (e1, ty1, u_gen) = Statics.syn_fix_holes(ctx, u_gen, e1);
    let (p, ctx, u_gen) =
      Statics.ana_pat_fix_holes(ctx, u_gen, false, p, ty1);
    let (e2, u_gen) = Statics.ana_fix_holes(ctx, u_gen, e2, ty);
    let ze =
      ZExp.(
        prepend_zline(
          DeeperL(LetLineZP(ZPat.place_after(p), None, e1)),
          e2,
        )
      );
    Some((ze, u_gen));
  | (
      Delete,
      Deeper(_, LineItemZL(DeeperL(LetLineZP(zp, Some(_), e1)), e2)),
    )
      when ZPat.is_after(zp) =>
    let (e1, ty1, u_gen) = Statics.syn_fix_holes(ctx, u_gen, e1);
    let (zp, ctx, u_gen) = ana_zpat_fix_holes(ctx, u_gen, zp, ty1);
    let (e2, u_gen) = Statics.ana_fix_holes(ctx, u_gen, e2, ty);
    let ze = ZExp.prepend_zline(DeeperL(LetLineZP(zp, None, e1)), e2);
    Some((ze, u_gen));
  | (Backspace, Deeper(_, LamZA(p, zty, e1))) when ZTyp.is_before(zty) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => None
    | Some((ty1, ty2)) =>
      let (p, ctx, u_gen) =
        Statics.ana_pat_fix_holes(ctx, u_gen, false, p, ty1);
      let (e1, u_gen) = Statics.ana_fix_holes(ctx, u_gen, e1, ty2);
      let zp = ZPat.place_after(p);
      let ze = ZExp.Deeper(NotInHole, LamZP(zp, None, e1));
      Some((ze, u_gen));
    }
  | (Delete, Deeper(_, LamZP(zp, Some(_), e1))) when ZPat.is_after(zp) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => None
    | Some((ty1, ty2)) =>
      let (zp, ctx, u_gen) = ana_zpat_fix_holes(ctx, u_gen, zp, ty1);
      let (e1, u_gen) = Statics.ana_fix_holes(ctx, u_gen, e1, ty2);
      let ze = ZExp.Deeper(NotInHole, LamZP(zp, None, e1));
      Some((ze, u_gen));
    }
  | (
      Backspace,
      Deeper(_, CaseZR(e1, (prefix, RuleZP(zp, _), suffix), ann)),
    )
      when ZPat.is_before(zp) =>
    switch (suffix) {
    | [] =>
      switch (prefix) {
      | [] =>
        let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
        let ze =
          ZExp.Deeper(NotInHole, CaseZR(e1, (prefix, zrule, suffix), ann));
        Some((ze, u_gen));
      | [_, ..._] =>
        switch (List.rev(prefix)) {
        | [] => None
        | [Rule(p2, e2), ...rev_prefix'] =>
          let prefix' = List.rev(rev_prefix');
          let zrule = ZExp.RuleZP(ZPat.place_before(p2), e2);
          let ze =
            ZExp.Deeper(
              NotInHole,
              CaseZR(e1, (prefix', zrule, suffix), ann),
            );
          Some((ze, u_gen));
        }
      }
    | [Rule(p2, e2), ...suffix'] =>
      let zrule = ZExp.RuleZP(ZPat.place_before(p2), e2);
      let ze =
        ZExp.Deeper(NotInHole, CaseZR(e1, (prefix, zrule, suffix'), ann));
      Some((ze, u_gen));
    }
  | (Backspace, Deeper(_, CaseZA(e1, rules, zann)))
      when ZTyp.is_before(zann) =>
    switch (Statics.syn(ctx, e1)) {
    | None => None
    | Some(ty1) =>
      let (rules, u_gen) =
        Statics.ana_rules_fix_holes(ctx, u_gen, false, rules, ty1, ty);
      let ze = ZExp.CursorE(After, Tm(NotInHole, Case(e1, rules, None)));
      Some((ze, u_gen));
    }
  | (
      Backspace,
      Deeper(
        _,
        OpSeqZ(_, CursorE(Before, e0) as ze0, EmptySuffix(_) as surround),
      ),
    )
  | (
      Backspace,
      Deeper(
        _,
        OpSeqZ(
          _,
          CursorE(Before, e0) as ze0,
          BothNonEmpty(_, _) as surround,
        ),
      ),
    ) =>
    abs_perform_Backspace_Before_op(
      combine_for_Backspace_Space,
      (ctx, u_gen, ze) => zexp_ana_fix_holes(ctx, u_gen, ze, ty),
      (ctx, u_gen, ze, surround) =>
        make_and_ana_OpSeqZ(ctx, u_gen, ze, surround, ty),
      UHExp.is_EmptyHole,
      UHExp.is_Space,
      UHExp.Space,
      (side, e) => CursorE(side, e),
      ctx,
      u_gen,
      e0,
      ze0,
      surround,
    )
  | (
      Delete,
      Deeper(
        _,
        OpSeqZ(_, CursorE(After, e0) as ze0, EmptyPrefix(_) as surround),
      ),
    )
  | (
      Delete,
      Deeper(
        _,
        OpSeqZ(
          _,
          CursorE(After, e0) as ze0,
          BothNonEmpty(_, _) as surround,
        ),
      ),
    ) =>
    abs_perform_Delete_After_op(
      combine_for_Delete_Space,
      (ctx, u_gen, ze) => zexp_ana_fix_holes(ctx, u_gen, ze, ty),
      (ctx, u_gen, ze, surround) =>
        make_and_ana_OpSeqZ(ctx, u_gen, ze, surround, ty),
      UHExp.is_EmptyHole,
      UHExp.is_Space,
      UHExp.Space,
      (side, e) => CursorE(side, e),
      ctx,
      u_gen,
      e0,
      ze0,
      surround,
    )
  /* Construction */
  | (
      Construct(SLine),
      Deeper(
        _,
        CaseZR(e1, (prefix, RuleZP(CursorP(Before, p), re), suffix), ann),
      ),
    ) =>
    let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
    let prev_rule = UHExp.Rule(p, re);
    let suffix = [prev_rule, ...suffix];
    let ze =
      ZExp.Deeper(NotInHole, CaseZR(e1, (prefix, zrule, suffix), ann));
    Some((ze, u_gen));
  | (
      Construct(SLine),
      Deeper(_, CaseZR(e1, (prefix, RuleZE(_, ze) as zrule, suffix), ann)),
    )
      when ZExp.is_after(ze) =>
    let prev_rule = ZExp.erase_rule(zrule);
    let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
    let prefix = prefix @ [prev_rule];
    let ze =
      ZExp.Deeper(NotInHole, CaseZR(e1, (prefix, zrule, suffix), ann));
    Some((ze, u_gen));
  | (
      Construct(SLine),
      Deeper(_, CaseZR(e1, (prefix, RuleZP(zp, _) as zrule, suffix), ann)),
    )
      when ZPat.is_after(zp) =>
    let prev_rule = ZExp.erase_rule(zrule);
    let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
    let prefix = prefix @ [prev_rule];
    let ze =
      ZExp.Deeper(NotInHole, CaseZR(e1, (prefix, zrule, suffix), ann));
    Some((ze, u_gen));
  | (Construct(SLine), Deeper(err_status, LineItemZL(zli, e2)))
      when ZExp.is_after_line_item(zli) =>
    let li = ZExp.erase_line_item(zli);
    let ze =
      ZExp.(
        prepend_line(
          ~err_status,
          li,
          prepend_zline(CursorL(Before, EmptyLine), e2),
        )
      );
    Some((ze, u_gen));
  | (Construct(SLine), ze1) when ZExp.is_before(ze1) =>
    let ze = ZExp.prepend_line(EmptyLine, ze1);
    Some((ze, u_gen));
  | (Construct(SLine), ze1) when ZExp.is_after(ze1) =>
    let e1 = ZExp.erase(ze1);
    let (ze2, u_gen) = ZExp.new_EmptyHole(u_gen);
    let ze = ZExp.prune_and_prepend_lines(e1, ze2);
    Some(zexp_ana_fix_holes(ctx, u_gen, ze, ty));
  | (Construct(_) as a, Deeper(_, LineItemZL(CursorL(_, EmptyLine), e2))) =>
    let (e1, u_gen) = UHExp.new_EmptyHole(u_gen);
    let ze1 = ZExp.CursorE(Before, e1);
    let ze = ZExp.prepend_zline(DeeperL(ExpLineZ(ze1)), e2);
    perform_ana(u_gen, ctx, a, ze, ty);
  | (
      Construct(_) as a,
      Deeper(err_status, LineItemZL(CursorL(side, ExpLine(e1)), e2)),
    ) =>
    switch (side) {
    | In(_) => None
    | Before =>
      let ze1 = ZExp.place_before(e1);
      let ze =
        ZExp.(
          prune_and_prepend_zline(~err_status, DeeperL(ExpLineZ(ze1)), e2)
        );
      perform_ana(u_gen, ctx, a, ze, ty);
    | After =>
      let ze1 = ZExp.place_after(e1);
      let ze =
        ZExp.(
          prune_and_prepend_zline(~err_status, DeeperL(ExpLineZ(ze1)), e2)
        );
      perform_ana(u_gen, ctx, a, ze, ty);
    }
  | (Construct(_), Deeper(_, LineItemZL(CursorL(_, LetLine(_, _, _)), _))) =>
    None
  | (
      Construct(SOp(SSpace)),
      Deeper(
        _,
        LineItemZL(
          DeeperL(
            ExpLineZ(
              Deeper(
                _,
                OpSeqZ(
                  _,
                  CursorE(After, Tm(_, Var(InVHole(Keyword(k), _), _))),
                  EmptyPrefix(suffix),
                ),
              ),
            ),
          ),
          e2,
        ),
      ),
    ) =>
    let (e1, u_gen) = keyword_suffix_to_exp(suffix, u_gen);
    let ze1 = ZExp.place_before(e1);
    let ze = ZExp.prepend_zline(DeeperL(ExpLineZ(ze1)), e2);
    perform_ana(u_gen, ctx, keyword_action(k), ze, ty);
  | (
      Construct(SOp(SSpace)),
      Deeper(
        _,
        LineItemZL(
          DeeperL(
            ExpLineZ(
              CursorE(After, Tm(_, Var(InVHole(Keyword(k), _), _))),
            ),
          ),
          e2,
        ),
      ),
    ) =>
    let ze = ZExp.prepend_zline(CursorL(Before, EmptyLine), e2);
    perform_ana(u_gen, ctx, keyword_action(k), ze, ty);
  | (
      Construct(SOp(SSpace)),
      Deeper(
        _,
        OpSeqZ(
          _,
          CursorE(After, Tm(_, Var(InVHole(Keyword(k), _), _))),
          EmptyPrefix(suffix),
        ),
      ),
    ) =>
    let (e, u_gen) = keyword_suffix_to_exp(suffix, u_gen);
    let ze = ZExp.place_before(e);
    perform_ana(u_gen, ctx, keyword_action(k), ze, ty);
  | (
      Construct(SOp(SSpace)),
      CursorE(After, Tm(_, Var(InVHole(Keyword(k), _), _))),
    ) =>
    let (ze, u_gen) = ZExp.new_EmptyHole(u_gen);
    perform_ana(u_gen, ctx, keyword_action(k), ze, ty);
  | (Construct(SParenthesized), CursorE(_, e)) =>
    Some((ParenthesizedZ(ze), u_gen))
  | (
      Construct(SAsc),
      Deeper(err_status, LineItemZL(DeeperL(LetLineZP(zp, None, e1)), e2)),
    ) =>
    switch (Statics.syn(ctx, e1)) {
    | None => None
    | Some(ty1) =>
      let uty1 = UHTyp.contract(ty1);
      let ze =
        ZExp.(
          prepend_zline(
            ~err_status,
            DeeperL(
              LetLineZA(ZPat.erase(zp), ZTyp.place_before(uty1), e1),
            ),
            e2,
          )
        );
      Some((ze, u_gen));
    }
  | (Construct(SAsc), Deeper(err_status, LamZP(zp, None, e1))) =>
    let ze =
      ZExp.Deeper(
        err_status,
        LamZA(ZPat.erase(zp), CursorT(Before, Hole), e1),
      );
    Some((ze, u_gen));
  | (
      Construct(SAsc),
      Deeper(
        err_status,
        LineItemZL(DeeperL(LetLineZP(zp, Some(uty1), e1)), e2),
      ),
    ) =>
    /* just move the cursor over if there is already an ascription */
    let ze =
      ZExp.(
        prepend_zline(
          ~err_status,
          DeeperL(LetLineZA(ZPat.erase(zp), ZTyp.place_before(uty1), e1)),
          e2,
        )
      );
    Some((ze, u_gen));
  | (Construct(SAsc), Deeper(err_status, LamZP(zp, Some(uty1), e1))) =>
    /* just move the cursor over if there is already an ascription */
    let ze =
      ZExp.Deeper(
        err_status,
        LamZA(ZPat.erase(zp), ZTyp.place_before(uty1), e1),
      );
    Some((ze, u_gen));
  | (Construct(SAsc), CursorE(_, Tm(err_status, Case(e1, rules, None)))) =>
    let ze =
      ZExp.Deeper(NotInHole, CaseZA(e1, rules, ZTyp.place_before(Hole)));
    Some((ze, u_gen));
  | (
      Construct(SAsc),
      CursorE(_, Tm(err_status, Case(e1, rules, Some(uty)))),
    ) =>
    /* just move the cursor over if there is already an ascription */
    let ze =
      ZExp.Deeper(NotInHole, CaseZA(e1, rules, ZTyp.place_before(uty)));
    Some((ze, u_gen));
  | (Construct(SAsc), CursorE(_, _)) => None
  | (
      Construct(SLet),
      Deeper(err_status, LineItemZL(DeeperL(ExpLineZ(ze1)), e2)),
    )
      when ZExp.is_before(ze1) =>
    let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
    let e1 = ZExp.erase(ze1);
    let ze =
      ZExp.prepend_zline(~err_status, DeeperL(LetLineZP(zp, None, e1)), e2);
    Some((ze, u_gen));
  | (Construct(SLet), ze1) when ZExp.is_before(ze1) =>
    let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
    let e1 = ZExp.erase(ze1);
    let (e2, u_gen) = UHExp.new_EmptyHole(u_gen);
    let (e1, ty1, u_gen) = Statics.syn_fix_holes(ctx, u_gen, e1);
    let ze = ZExp.prepend_zline(DeeperL(LetLineZP(zp, None, e1)), e2);
    Some((ze, u_gen));
  | (Construct(SLet), CursorE(_, _)) => None
  /* cannot construct let in middle of opseq without parens */
  | (Construct(SLet), ze) when is_not_Before_opseq(ze) => None
  | (Construct(SLam), CursorE(_, e)) =>
    switch (HTyp.matched_arrow(ty)) {
    | Some((_, ty2)) =>
      let (e, u_gen) = Statics.ana_fix_holes(ctx, u_gen, e, ty2);
      let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
      let ze = ZExp.Deeper(NotInHole, LamZP(zp, None, e));
      Some((ze, u_gen));
    | None =>
      let (e, _, u_gen) = Statics.syn_fix_holes(ctx, u_gen, e);
      let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
      let (u, u_gen) = MetaVarGen.next(u_gen);
      let ze = ZExp.Deeper(InHole(TypeInconsistent, u), LamZP(zp, None, e));
      Some((ze, u_gen));
    }
  | (Construct(SInj(side)), CursorE(cursor_side, e1)) =>
    switch (HTyp.matched_sum(ty)) {
    | Some((tyL, tyR)) =>
      let ty1 = pick_side(side, tyL, tyR);
      let (e1, u_gen) = Statics.ana_fix_holes(ctx, u_gen, e1, ty1);
      let ze = ZExp.Deeper(NotInHole, InjZ(side, CursorE(cursor_side, e1)));
      Some((ze, u_gen));
    | None =>
      let (e1, _, u_gen) = Statics.syn_fix_holes(ctx, u_gen, e1);
      let (u, u_gen) = MetaVarGen.next(u_gen);
      let ze =
        ZExp.Deeper(
          InHole(TypeInconsistent, u),
          InjZ(side, CursorE(cursor_side, e1)),
        );
      Some((ze, u_gen));
    }
  | (Construct(SCase), Deeper(_, LineItemZL(DeeperL(ExpLineZ(ze1)), e2)))
      when ZExp.is_before(ze1) =>
    let e1 = ZExp.erase(ze1);
    switch (e1) {
    | Tm(_, EmptyHole(_)) =>
      let (rule_p, u_gen) = UHPat.new_EmptyHole(u_gen);
      let rule = UHExp.Rule(rule_p, e2);
      let ze = ZExp.Deeper(NotInHole, CaseZE(ze1, [rule], None));
      Some((ze, u_gen));
    | _ =>
      let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
      let zrule = ZExp.RuleZP(zp, e2);
      let zrules = ZList.singleton(zrule);
      let (e1, _, u_gen) = Statics.syn_fix_holes(ctx, u_gen, e1);
      let ze = ZExp.Deeper(NotInHole, CaseZR(e1, zrules, None));
      Some((ze, u_gen));
    };
  | (Construct(SCase), ze1) when ZExp.is_before(ze1) =>
    let e1 = ZExp.erase(ze1);
    switch (e1) {
    | Tm(_, EmptyHole(_)) =>
      let (rule, u_gen) = UHExp.empty_rule(u_gen);
      let ze = ZExp.Deeper(NotInHole, CaseZE(ze1, [rule], None));
      Some((ze, u_gen));
    | _ =>
      let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
      let zrules = ZList.singleton(zrule);
      let (e1, _, u_gen) = Statics.syn_fix_holes(ctx, u_gen, e1);
      let ze = ZExp.Deeper(NotInHole, CaseZR(e1, zrules, None));
      Some((ze, u_gen));
    };
  | (Construct(SCase), CursorE(_, _)) => None
  /* cannot construct case in middle of opseq without parens */
  | (Construct(SCase), ze) when is_not_Before_opseq(ze) => None
  | (
      Construct(SOp(os)),
      Deeper(_, OpSeqZ(_, CursorE(In(_), e), surround)),
    )
  | (
      Construct(SOp(os)),
      Deeper(_, OpSeqZ(_, CursorE(After, e), surround)),
    ) =>
    switch (exp_op_of(os)) {
    | None => None
    | Some(op) =>
      Some(
        abs_perform_Construct_SOp_After_surround(
          ZExp.new_EmptyHole,
          (ctx, u_gen, ze, surround) =>
            make_and_ana_OpSeqZ(ctx, u_gen, ze, surround, ty),
          UHExp.is_Space,
          UHExp.Space,
          (side, e) => CursorE(side, e),
          ctx,
          u_gen,
          e,
          op,
          surround,
        ),
      )
    }
  | (
      Construct(SOp(os)),
      Deeper(_, OpSeqZ(_, CursorE(Before, _) as ze0, surround)),
    ) =>
    switch (exp_op_of(os)) {
    | None => None
    | Some(op) =>
      Some(
        abs_perform_Construct_SOp_Before_surround(
          ZExp.erase,
          ZExp.new_EmptyHole,
          (ctx, u_gen, ze, surround) =>
            make_and_ana_OpSeqZ(ctx, u_gen, ze, surround, ty),
          UHExp.is_Space,
          UHExp.Space,
          (side, e) => CursorE(side, e),
          ctx,
          u_gen,
          ze0,
          op,
          surround,
        ),
      )
    }
  | (Construct(SOp(os)), CursorE(In(_), e))
  | (Construct(SOp(os)), CursorE(After, e)) =>
    switch (exp_op_of(os)) {
    | None => None
    | Some(op) =>
      Some(
        abs_perform_Construct_SOp_After(
          UHExp.bidelimit,
          ZExp.new_EmptyHole,
          (ctx, u_gen, ze, surround) =>
            make_and_ana_OpSeqZ(ctx, u_gen, ze, surround, ty),
          ctx,
          u_gen,
          e,
          op,
        ),
      )
    }
  | (Construct(SOp(os)), CursorE(Before, e)) =>
    switch (exp_op_of(os)) {
    | None => None
    | Some(op) =>
      Some(
        abs_perform_Construct_SOp_Before(
          UHExp.bidelimit,
          ZExp.new_EmptyHole,
          (ctx, u_gen, ze, surround) =>
            make_and_ana_OpSeqZ(ctx, u_gen, ze, surround, ty),
          ctx,
          u_gen,
          e,
          op,
        ),
      )
    }
  /* Zipper Cases */
  | (_, ParenthesizedZ(ze1)) =>
    switch (perform_ana(u_gen, ctx, a, ze1, ty)) {
    | Some((ze1', u_gen')) => Some((ParenthesizedZ(ze1'), u_gen'))
    | None => None
    }
  | (_, Deeper(err_status, LineItemZL(CursorL(_, EmptyLine), e2))) =>
    let (ze1, u_gen) = ZExp.new_EmptyHole(u_gen);
    let ze = ZExp.prepend_zline(~err_status, DeeperL(ExpLineZ(ze1)), e2);
    perform_ana(u_gen, ctx, a, ze, ty);
  | (_, Deeper(err_status, LineItemZL(DeeperL(ExpLineZ(ze1)), e2))) =>
    switch (Statics.syn(ctx, ZExp.erase(ze1))) {
    | None => None
    | Some(ty1) =>
      switch (perform_syn(ctx, a, (ze1, ty1, u_gen))) {
      | None => None
      | Some((ze1, _, u_gen)) =>
        let ze =
          ZExp.(
            prune_and_prepend_zline(~err_status, DeeperL(ExpLineZ(ze1)), e2)
          );
        Some((ze, u_gen));
      }
    }
  | (_, Deeper(_, LineItemZL(DeeperL(LetLineZP(zp, ann, e1)), e2))) =>
    switch (ann) {
    | Some(uty1) =>
      let ty1 = UHTyp.expand(uty1);
      switch (perform_ana_pat(ctx, u_gen, a, zp, ty1)) {
      | None => None
      | Some((zp, ctx2, u_gen)) =>
        let p = ZPat.erase(zp);
        let ctx1 = Statics.ctx_for_let(ctx, p, ty1, e1);
        let (e1, u_gen) = Statics.ana_fix_holes(ctx1, u_gen, e1, ty1);
        let (e2, u_gen) = Statics.ana_fix_holes(ctx2, u_gen, e2, ty);
        let ze = ZExp.prepend_zline(DeeperL(LetLineZP(zp, ann, e1)), e2);
        Some((ze, u_gen));
      };
    | None =>
      switch (Statics.syn(ctx, e1)) {
      | None => None
      | Some(ty1) =>
        switch (perform_ana_pat(ctx, u_gen, a, zp, ty1)) {
        | None => None
        | Some((zp, ctx2, u_gen)) =>
          let (e2, u_gen) = Statics.ana_fix_holes(ctx2, u_gen, e2, ty);
          let ze = ZExp.prepend_zline(DeeperL(LetLineZP(zp, ann, e1)), e2);
          Some((ze, u_gen));
        }
      }
    }
  | (_, Deeper(_, LineItemZL(DeeperL(LetLineZA(p, zann, e1)), e2))) =>
    /* (ctx) let p (ctx2) : ty = (ctx1) e1 in (ctx2) e2 */
    switch (perform_ty(a, zann)) {
    | None => None
    | Some(zann) =>
      let ty1 = UHTyp.expand(ZTyp.erase(zann));
      let (p, ctx2, u_gen) =
        Statics.ana_pat_fix_holes(ctx, u_gen, false, p, ty1);
      let ctx1 = Statics.ctx_for_let(ctx, p, ty1, e1);
      let (e1, u_gen) = Statics.ana_fix_holes(ctx1, u_gen, e1, ty1);
      let (e2, u_gen) = Statics.ana_fix_holes(ctx2, u_gen, e2, ty);
      let ze = ZExp.prepend_zline(DeeperL(LetLineZA(p, zann, e1)), e2);
      Some((ze, u_gen));
    }
  | (_, Deeper(_, LineItemZL(DeeperL(LetLineZE(p, ann, ze1)), e2))) =>
    switch (ann) {
    | Some(ann_ty) =>
      let ty1 = UHTyp.expand(ann_ty);
      let ctx1 = Statics.ctx_for_let(ctx, p, ty1, ZExp.erase(ze1));
      switch (perform_ana(u_gen, ctx1, a, ze1, ty1)) {
      | None => None
      | Some((ze1, u_gen)) =>
        let ze = ZExp.prepend_zline(DeeperL(LetLineZE(p, ann, ze1)), e2);
        Some((ze, u_gen));
      };
    | None =>
      let e1 = ZExp.erase(ze1);
      switch (Statics.syn(ctx, e1)) {
      | None => None
      | Some(ty1) =>
        switch (perform_syn(ctx, a, (ze1, ty1, u_gen))) {
        | None => None
        | Some((ze1, ty1, u_gen)) =>
          let (p, ctx2, u_gen) =
            Statics.ana_pat_fix_holes(ctx, u_gen, false, p, ty1);
          let (e2, u_gen) = Statics.ana_fix_holes(ctx2, u_gen, e2, ty);
          let ze = ZExp.prepend_zline(DeeperL(LetLineZE(p, ann, ze1)), e2);
          Some((ze, u_gen));
        }
      };
    }
  | (_, Deeper(_, LineItemZE(EmptyLine as li, ze2)))
  | (_, Deeper(_, LineItemZE(ExpLine(_) as li, ze2))) =>
    switch (perform_ana(u_gen, ctx, a, ze2, ty)) {
    | None => None
    | Some((ze2, u_gen)) =>
      let ze = ZExp.prepend_line(li, ze2);
      Some((ze, u_gen));
    }
  | (_, Deeper(_, LineItemZE(LetLine(p, ann, e1), ze2))) =>
    let ty1 =
      switch (ann) {
      | Some(uty1) => Some(UHTyp.expand(uty1))
      | None => Statics.syn(ctx, e1)
      };
    switch (ty1) {
    | None => None
    | Some(ty1) =>
      switch (Statics.ana_pat(ctx, p, ty1)) {
      | None => None
      | Some(ctx2) =>
        switch (perform_ana(u_gen, ctx2, a, ze2, ty)) {
        | None => None
        | Some((ze2, u_gen)) =>
          let ze = ZExp.prepend_line(LetLine(p, ann, e1), ze2);
          Some((ze, u_gen));
        }
      }
    };
  | (_, Deeper(err, LamZP(zp, ann, e1))) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => None
    | Some((ty1_given, ty2)) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => ty1_given
        };
      switch (perform_ana_pat(ctx, u_gen, a, zp, ty1)) {
      | None => None
      | Some((zp, ctx, u_gen)) =>
        let (e1, u_gen) = Statics.ana_fix_holes(ctx, u_gen, e1, ty2);
        let ze = ZExp.Deeper(err, LamZP(zp, ann, e1));
        Some((ze, u_gen));
      };
    }
  | (_, Deeper(_, LamZA(p, zann, e1))) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => None
    | Some((ty1_given, ty2)) =>
      switch (perform_ty(a, zann)) {
      | None => None
      | Some(zann) =>
        let ty1 = UHTyp.expand(ZTyp.erase(zann));
        HTyp.consistent(ty1, ty1_given)
          ? {
            let (p, ctx, u_gen) =
              Statics.ana_pat_fix_holes(ctx, u_gen, false, p, ty1);
            let (e1, u_gen) = Statics.ana_fix_holes(ctx, u_gen, e1, ty2);
            let ze = ZExp.Deeper(NotInHole, LamZA(p, zann, e1));
            Some((ze, u_gen));
          }
          : {
            let (p, ctx, u_gen) =
              Statics.ana_pat_fix_holes(ctx, u_gen, false, p, ty1);
            let (e1, _, u_gen) = Statics.syn_fix_holes(ctx, u_gen, e1);
            let (u, u_gen) = MetaVarGen.next(u_gen);
            let ze =
              ZExp.Deeper(InHole(TypeInconsistent, u), LamZA(p, zann, e1));
            Some((ze, u_gen));
          };
      }
    }
  | (_, Deeper(err, LamZE(p, ann, ze1))) =>
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
        switch (perform_ana(u_gen, ctx, a, ze1, ty2)) {
        | None => None
        | Some((ze1, u_gen)) =>
          let ze = ZExp.Deeper(err, LamZE(p, ann, ze1));
          Some((ze, u_gen));
        }
      };
    }
  | (_, Deeper(err, InjZ(side, ze))) =>
    switch (HTyp.matched_sum(ty)) {
    | Some((ty1, ty2)) =>
      let picked = pick_side(side, ty1, ty2);
      switch (perform_ana(u_gen, ctx, a, ze, picked)) {
      | Some((ze', u_gen)) => Some((Deeper(err, InjZ(side, ze')), u_gen))
      | None => None
      };
    | None => None
    }
  | (_, Deeper(_, CaseZE(ze1, rules, ann))) =>
    switch (Statics.syn(ctx, ZExp.erase(ze1))) {
    | None => None
    | Some(ty1) =>
      switch (perform_syn(ctx, a, (ze1, ty1, u_gen))) {
      | None => None
      | Some((ze1, ty1, u_gen)) =>
        let (rules, u_gen) =
          Statics.ana_rules_fix_holes(ctx, u_gen, false, rules, ty1, ty);
        let ze = ZExp.Deeper(NotInHole, CaseZE(ze1, rules, ann));
        Some((ze, u_gen));
      }
    }
  | (_, Deeper(_, CaseZR(e1, zrules, ann))) =>
    switch (Statics.syn(ctx, e1)) {
    | None => None
    | Some(ty1) =>
      switch (ZList.prj_z(zrules)) {
      | RuleZP(zp, e) =>
        switch (perform_ana_pat(ctx, u_gen, a, zp, ty1)) {
        | None => None
        | Some((zp, ctx, u_gen)) =>
          let (e, u_gen) = Statics.ana_fix_holes(ctx, u_gen, e, ty);
          let zrule = ZExp.RuleZP(zp, e);
          let ze =
            ZExp.Deeper(
              NotInHole,
              CaseZR(e1, ZList.replace_z(zrules, zrule), ann),
            );
          Some((ze, u_gen));
        }
      | RuleZE(p, ze) =>
        switch (Statics.ana_pat(ctx, p, ty1)) {
        | None => None
        | Some(ctx) =>
          switch (perform_ana(u_gen, ctx, a, ze, ty)) {
          | None => None
          | Some((ze, u_gen)) =>
            let zrule = ZExp.RuleZE(p, ze);
            let ze =
              ZExp.Deeper(
                NotInHole,
                CaseZR(e1, ZList.replace_z(zrules, zrule), ann),
              );
            Some((ze, u_gen));
          }
        }
      }
    }
  | (_, Deeper(_, CaseZA(e1, rules, zann))) =>
    switch (Statics.syn(ctx, e1)) {
    | None => None
    | Some(ty1) =>
      switch (perform_ty(a, zann)) {
      | None => None
      | Some(zann) =>
        let ty2 = UHTyp.expand(ZTyp.erase(zann));
        let (rules, u_gen) =
          Statics.ana_rules_fix_holes(ctx, u_gen, false, rules, ty1, ty2);
        let ze = ZExp.Deeper(NotInHole, CaseZA(e1, rules, zann));
        Some(zexp_ana_fix_holes(ctx, u_gen, ze, ty));
      }
    }
  | (_, Deeper(err, OpSeqZ(_, ze0, surround))) =>
    let i = OperatorSeq.surround_prefix_length(surround);
    switch (ZExp.erase(ze)) {
    | Tm(_, OpSeq(skel, seq)) =>
      switch (Statics.ana_skel(ctx, skel, seq, ty, Some(i))) {
      | Some(Some(mode)) =>
        switch (mode) {
        | Statics.AnalyzedAgainst(ty0) =>
          switch (perform_ana(u_gen, ctx, a, ze0, ty0)) {
          | None => None
          | Some((ze0', u_gen)) =>
            let ze0'' = ZExp.bidelimit(ze0');
            Some((ZExp.Deeper(err, OpSeqZ(skel, ze0'', surround)), u_gen));
          }
        | Statics.Synthesized(ty0) =>
          switch (perform_syn(ctx, a, (ze0, ty0, u_gen))) {
          | None => None
          | Some((ze0', ty0', u_gen)) =>
            let ze0'' = ZExp.bidelimit(ze0');
            Some(make_and_ana_OpSeqZ(ctx, u_gen, ze0'', surround, ty));
          }
        }
      | Some(_) => None /* should never happen */
      | None => None /* should never happen */
      }
    | _ => None /* should never happen */
    };
  /* Subsumption (post zipper cases) */
  | (UpdateApPalette(_), _)
  | (Construct(SApPalette(_)), _)
  | (Construct(SLine), _)
  | (Construct(SVar(_, _)), _)
  | (Construct(SNumLit(_, _)), _)
  | (Construct(SListNil), _)
  | (_, Deeper(_, ApPaletteZ(_, _, _))) =>
    perform_ana_subsume(u_gen, ctx, a, ze, ty)
  /* Invalid actions at expression level */
  | (Construct(SNum), _)
  | (Construct(SBool), _)
  | (Construct(SList), _)
  | (Construct(SWild), _) => None
  }
and perform_ana_subsume =
    (u_gen: MetaVarGen.t, ctx: Contexts.t, a: t, ze: ZExp.t, ty: HTyp.t)
    : option((ZExp.t, MetaVarGen.t)) =>
  switch (Statics.syn(ctx, ZExp.erase(ze))) {
  | Some(ty1) =>
    switch (perform_syn(ctx, a, (ze, ty1, u_gen))) {
    | Some((ze', ty1', u_gen')) =>
      if (HTyp.consistent(ty, ty1')) {
        Some((ze', u_gen'));
      } else {
        let (ze'', u_gen'') = ZExp.make_inconsistent(u_gen', ze');
        Some((ze'', u_gen''));
      }
    | None => None
    }
  | None => None
  };

let can_perform =
    (
      ctx: Contexts.t,
      edit_state: (ZExp.t, HTyp.t, MetaVarGen.t),
      ci: CursorInfo.t,
      a: t,
    )
    : bool =>
  switch (a) {
  | Construct(SParenthesized) => true
  | Construct(SLine)
  | Construct(SLet)
  | Construct(SCase) =>
    switch (ci.sort) {
    | IsLineItem(_) => true
    | IsExpr(_) => true
    | IsPat(_) => false
    | IsType => false
    }
  | Construct(SInj(_)) =>
    switch (ci.sort) {
    | IsLineItem(_) => true
    | IsExpr(_) => true
    | IsPat(_) => true
    | IsType => false
    }
  | Construct(SListNil) =>
    switch (ci.sort) {
    | IsLineItem(EmptyLine) => true
    | IsLineItem(ExpLine(Tm(_, EmptyHole(_)))) => true
    | IsLineItem(_) => false
    | IsExpr(Tm(_, EmptyHole(_))) => true
    | IsExpr(_) => false
    | IsPat(Pat(_, EmptyHole(_))) => true
    | IsPat(_) => false
    | IsType => false
    }
  | Construct(SOp(SArrow))
  | Construct(SOp(SVBar))
  | Construct(SList) =>
    switch (ci.sort) {
    | IsType => true
    | IsLineItem(_)
    | IsExpr(_)
    | IsPat(_) => false
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
  | MoveToNextHole
  | MoveToPrevHole
  | UpdateApPalette(_)
  | Delete
  | Backspace =>
    _TEST_PERFORM
      ? switch (perform_syn(ctx, a, edit_state)) {
        | Some(_) => true
        | None => false
        }
      : true
  };

let can_enter_varchar = (ci: CursorInfo.t): bool =>
  switch (ci.sort) {
  | IsLineItem(EmptyLine)
  | IsLineItem(ExpLine(Tm(_, EmptyHole(_))))
  | IsExpr(Tm(_, Var(_, _)))
  | IsExpr(Tm(_, EmptyHole(_)))
  | IsExpr(Tm(_, BoolLit(_)))
  | IsPat(Pat(_, Var(_)))
  | IsPat(Pat(_, EmptyHole(_)))
  | IsPat(Pat(_, BoolLit(_))) => true
  | IsExpr(Tm(_, NumLit(_)))
  | IsPat(Pat(_, NumLit(_))) =>
    switch (ci.side) {
    | Before => true
    | In(_)
    | After => false
    }
  | IsLineItem(_)
  | IsExpr(_)
  | IsPat(_)
  | IsType => false
  };

let can_enter_numeral = (ci: CursorInfo.t): bool =>
  switch (ci.sort) {
  | IsLineItem(EmptyLine)
  | IsLineItem(ExpLine(Tm(_, EmptyHole(_))))
  | IsExpr(Tm(_, NumLit(_)))
  | IsExpr(Tm(_, EmptyHole(_)))
  | IsPat(Pat(_, NumLit(_)))
  | IsPat(Pat(_, EmptyHole(_))) => true
  | IsLineItem(_)
  | IsExpr(_)
  | IsPat(_)
  | IsType => false
  };

let can_construct_palette = (ci: CursorInfo.t): bool =>
  switch (ci.sort) {
  | IsLineItem(EmptyLine)
  | IsLineItem(ExpLine(Tm(_, EmptyHole(_))))
  | IsExpr(Tm(_, EmptyHole(_))) => true
  | _ => false
  };
