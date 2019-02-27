let _TEST_PERFORM = false;
open SemanticsCommon;
open HazelUtil;

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
  | SArrow => Some(UHTyp.Arrow)
  | SComma => Some(UHTyp.Prod)
  | SVBar => Some(UHTyp.Sum)
  | SPlus
  | STimes
  | SLessThan
  | SSpace
  | SCons => None
  };

let op_shape_of_ty_op = (op: UHTyp.op): op_shape =>
  switch (op) {
  | UHTyp.Arrow => SArrow
  | UHTyp.Prod => SComma
  | UHTyp.Sum => SVBar
  };

let pat_op_of = (os: op_shape): option(UHPat.op) =>
  switch (os) {
  | SComma => Some(UHPat.Comma)
  | SSpace => Some(UHPat.Space)
  | SCons => Some(UHPat.Cons)
  | SPlus
  | STimes
  | SLessThan
  | SArrow
  | SVBar => None
  };

let op_shape_of_pat_op = (op: UHPat.op): op_shape =>
  switch (op) {
  | UHPat.Comma => SComma
  | UHPat.Space => SSpace
  | UHPat.Cons => SCons
  };

let exp_op_of = (os: op_shape): option(UHExp.op) =>
  switch (os) {
  | SPlus => Some(UHExp.Plus)
  | STimes => Some(UHExp.Times)
  | SLessThan => Some(UHExp.LessThan)
  | SSpace => Some(UHExp.Space)
  | SComma => Some(UHExp.Comma)
  | SCons => Some(UHExp.Cons)
  | SArrow
  | SVBar => None
  };

let op_shape_of_exp_op = (op: UHExp.op): op_shape =>
  switch (op) {
  | UHExp.Plus => SPlus
  | UHExp.Times => STimes
  | UHExp.LessThan => SLessThan
  | UHExp.Space => SSpace
  | UHExp.Comma => SComma
  | UHExp.Cons => SCons
  };

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
  ZTyp.OpSeqZ(skel, zty0, surround);
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
  | (Backspace, ZTyp.CursorT(After, uty))
  | (Backspace, ZTyp.CursorT(In(_), uty)) =>
    Some(ZTyp.CursorT(Before, UHTyp.Hole))
  | (Backspace, ZTyp.CursorT(Before, _)) => None
  | (Delete, ZTyp.CursorT(Before, uty))
  | (Delete, ZTyp.CursorT(In(_), uty)) =>
    switch (uty) {
    | UHTyp.Hole => Some(ZTyp.CursorT(After, uty))
    | _ => Some(ZTyp.CursorT(Before, UHTyp.Hole))
    }
  | (Delete, ZTyp.CursorT(After, uty)) => None
  | (
      Backspace,
      ZTyp.OpSeqZ(_, ZTyp.CursorT(Before, uty0) as zty0, surround),
    ) =>
    switch (surround) {
    | OperatorSeq.EmptyPrefix(_) => None
    | OperatorSeq.EmptySuffix(prefix) =>
      switch (prefix) {
      | OperatorSeq.ExpPrefix(uty1, op1) =>
        switch (uty0) {
        | UHTyp.Hole =>
          /* uty1 op1 |_ -> uty1| */
          Some(ZTyp.CursorT(After, uty1))
        | _ =>
          /* uty1 op1 |uty0 -> |uty0 */
          Some(zty0)
        }
      | OperatorSeq.SeqPrefix(seq1, op1) =>
        let (uty1, prefix') = OperatorSeq.split_tail(seq1);
        switch (uty0) {
        | UHTyp.Hole =>
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
    | OperatorSeq.BothNonEmpty(prefix, suffix) =>
      switch (prefix) {
      | OperatorSeq.ExpPrefix(uty1, op1) =>
        switch (uty0) {
        | UHTyp.Hole =>
          /* uty1 op1 |_ suffix -> uty1| suffix */
          let surround' = OperatorSeq.EmptyPrefix(suffix);
          let zty1 = ZTyp.CursorT(After, uty1);
          Some(make_ty_OpSeqZ(zty1, surround'));
        | _ =>
          /* uty1 op1 |uty0 suffix -> |uty0 suffix */
          let surround' = OperatorSeq.EmptyPrefix(suffix);
          Some(make_ty_OpSeqZ(zty0, surround'));
        }
      | OperatorSeq.SeqPrefix(seq1, op1) =>
        let (uty1, prefix') = OperatorSeq.split_tail(seq1);
        switch (uty0) {
        | UHTyp.Hole =>
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
  | (Delete, ZTyp.OpSeqZ(_, ZTyp.CursorT(After, uty0) as zty0, surround)) =>
    switch (surround) {
    | OperatorSeq.EmptySuffix(_) => None
    | OperatorSeq.EmptyPrefix(suffix) =>
      switch (suffix) {
      | OperatorSeq.ExpSuffix(op1, uty1) =>
        switch (uty0) {
        | UHTyp.Hole =>
          /* _| op1 uty1 -> |uty1 */
          Some(ZTyp.CursorT(Before, uty1))
        | _ =>
          /* uty0| op1 uty0 -> uty0| */
          Some(zty0)
        }
      | OperatorSeq.SeqSuffix(op1, seq1) =>
        let (uty1, suffix') = OperatorSeq.split0(seq1);
        switch (uty0) {
        | UHTyp.Hole =>
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
    | OperatorSeq.BothNonEmpty(prefix, suffix) =>
      switch (suffix) {
      | OperatorSeq.ExpSuffix(op1, uty1) =>
        switch (uty0) {
        | UHTyp.Hole =>
          /* prefix _| op1 uty1 -> prefix |uty1 */
          let surround' = OperatorSeq.EmptySuffix(prefix);
          let zty1 = ZTyp.CursorT(Before, uty1);
          Some(make_ty_OpSeqZ(zty1, surround'));
        | _ =>
          /* prefix uty0| op1 uty0 -> prefix uty0| */
          let surround' = OperatorSeq.EmptySuffix(prefix);
          Some(make_ty_OpSeqZ(zty0, surround'));
        }
      | OperatorSeq.SeqSuffix(op1, seq1) =>
        let (uty1, suffix') = OperatorSeq.split0(seq1);
        switch (uty0) {
        | UHTyp.Hole =>
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
  | (Construct(SParenthesized), ZTyp.CursorT(_, _)) =>
    Some(ZTyp.ParenthesizedZ(zty))
  | (Construct(SNum), ZTyp.CursorT(_, UHTyp.Hole)) =>
    Some(ZTyp.CursorT(After, UHTyp.Num))
  | (Construct(SNum), ZTyp.CursorT(_, _)) => None
  | (Construct(SBool), ZTyp.CursorT(_, UHTyp.Hole)) =>
    Some(ZTyp.CursorT(After, UHTyp.Bool))
  | (Construct(SBool), ZTyp.CursorT(_, _)) => None
  | (Construct(SList), ZTyp.CursorT(_, ty1)) => Some(ZTyp.ListZ(zty))
  | (Construct(SOp(os)), ZTyp.CursorT(After, uty1))
  | (Construct(SOp(os)), ZTyp.CursorT(In(_), uty1)) =>
    switch (ty_op_of(os)) {
    | None => None
    | Some(op) =>
      let surround =
        OperatorSeq.EmptySuffix(OperatorSeq.ExpPrefix(uty1, op));
      let zty0 = ZTyp.CursorT(Before, UHTyp.Hole);
      Some(make_ty_OpSeqZ(zty0, surround));
    }
  | (Construct(SOp(os)), ZTyp.CursorT(Before, uty1)) =>
    switch (ty_op_of(os)) {
    | None => None
    | Some(op) =>
      let surround =
        OperatorSeq.EmptyPrefix(OperatorSeq.ExpSuffix(op, uty1));
      let zty0 = ZTyp.CursorT(Before, UHTyp.Hole);
      Some(make_ty_OpSeqZ(zty0, surround));
    }
  | (
      Construct(SOp(os)),
      ZTyp.OpSeqZ(_, ZTyp.CursorT(After, uty0), surround),
    )
  | (
      Construct(SOp(os)),
      ZTyp.OpSeqZ(_, ZTyp.CursorT(In(_), uty0), surround),
    ) =>
    switch (ty_op_of(os)) {
    | None => None
    | Some(op) =>
      switch (surround) {
      | OperatorSeq.EmptyPrefix(suffix) =>
        /* zty0| suffix -> uty0 op |_ suffix */
        let prefix' = OperatorSeq.ExpPrefix(uty0, op);
        let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
        let zty0' = ZTyp.CursorT(Before, UHTyp.Hole);
        Some(make_ty_OpSeqZ(zty0', surround'));
      | OperatorSeq.EmptySuffix(prefix) =>
        /* prefix zty0| -> prefix uty0 op |_ */
        let prefix' = OperatorSeq.prefix_append_exp(prefix, uty0, op);
        let surround' = OperatorSeq.EmptySuffix(prefix');
        let zty0' = ZTyp.CursorT(Before, UHTyp.Hole);
        Some(make_ty_OpSeqZ(zty0', surround'));
      | OperatorSeq.BothNonEmpty(prefix, suffix) =>
        /* prefix zty0| suffix -> prefix uty0 op |_ suffix */
        let prefix' = OperatorSeq.prefix_append_exp(prefix, uty0, op);
        let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
        let zty0' = ZTyp.CursorT(Before, UHTyp.Hole);
        Some(make_ty_OpSeqZ(zty0', surround'));
      }
    }
  | (
      Construct(SOp(os)),
      ZTyp.OpSeqZ(_, ZTyp.CursorT(Before, uty0), surround),
    ) =>
    switch (ty_op_of(os)) {
    | None => None
    | Some(op) =>
      switch (surround) {
      | OperatorSeq.EmptyPrefix(suffix) =>
        /* |zty0 suffix -> |_ op uty0 suffix */
        let suffix' = OperatorSeq.suffix_prepend_exp(suffix, op, uty0);
        let surround' = OperatorSeq.EmptyPrefix(suffix');
        let zty0' = ZTyp.CursorT(Before, UHTyp.Hole);
        Some(make_ty_OpSeqZ(zty0', surround'));
      | OperatorSeq.EmptySuffix(prefix) =>
        /* prefix |zty0 -> prefix |_ op uty0 */
        let suffix' = OperatorSeq.ExpSuffix(op, uty0);
        let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
        let zty0' = ZTyp.CursorT(Before, UHTyp.Hole);
        Some(make_ty_OpSeqZ(zty0', surround'));
      | OperatorSeq.BothNonEmpty(prefix, suffix) =>
        /* prefix |zty0 suffix -> prefix |_ op uty0 suffix */
        let suffix' = OperatorSeq.suffix_prepend_exp(suffix, op, uty0);
        let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
        let zty0' = ZTyp.CursorT(Before, UHTyp.Hole);
        Some(make_ty_OpSeqZ(zty0', surround'));
      }
    }
  /* Zipper Cases */
  | (a, ZTyp.ParenthesizedZ(zty1)) =>
    switch (perform_ty(a, zty1)) {
    | Some(zty1') => Some(ZTyp.ParenthesizedZ(zty1'))
    | None => None
    }
  | (a, ZTyp.ListZ(zty1)) =>
    switch (perform_ty(a, zty1)) {
    | Some(zty1) => Some(ZTyp.ListZ(zty1))
    | None => None
    }
  | (a, ZTyp.OpSeqZ(skel, zty0, surround)) =>
    switch (perform_ty(a, zty0)) {
    | Some(zty0') => Some(ZTyp.OpSeqZ(skel, zty0', surround))
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
      z_typecheck_fix_holes: (Contexts.t, MetaVarGen.t, 'z) => option('m),
      make_and_typecheck_OpSeqZ:
        (Contexts.t, MetaVarGen.t, 'z, OperatorSeq.opseq_surround('e, 'op)) =>
        option('m),
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
  | OperatorSeq.EmptyPrefix(_) => None
  | OperatorSeq.EmptySuffix(prefix) =>
    switch (prefix) {
    | OperatorSeq.ExpPrefix(e1, op1) =>
      /* e1 op1 |ze0 */
      if (is_Space(op1)) {
        /* e1 |ze0 */
        let ze0' = combine_for_Backspace_Space(e1, ze0);
        z_typecheck_fix_holes(ctx, u_gen, ze0');
      } else {
        switch (is_EmptyHole(e1), is_EmptyHole(e0)) {
        | (true, true) =>
          /* _1 op1 |_0 --> _1| */
          let ze0' = _Cursor(After, e1);
          z_typecheck_fix_holes(ctx, u_gen, ze0');
        | (true, _) =>
          /* _1 op1 |e0 --> |e0 */
          z_typecheck_fix_holes(ctx, u_gen, ze0)
        | (false, true) =>
          /* e1 op1 |_0 --> e1| */
          let ze0' = _Cursor(After, e1);
          z_typecheck_fix_holes(ctx, u_gen, ze0');
        | (false, false) =>
          /* e1 op1 |ze0 --> e1 |ze0 */
          let surround' =
            OperatorSeq.EmptySuffix(OperatorSeq.ExpPrefix(e1, _Space));
          make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
        };
      }
    | OperatorSeq.SeqPrefix(seq1, op1) =>
      /* seq1 op1 |ze0 */
      is_Space(op1)
        /* seq1 |ze0 */
        ? {
          let (e1, prefix') = OperatorSeq.split_tail(seq1);
          let surround' = OperatorSeq.EmptySuffix(prefix');
          let ze0' = combine_for_Backspace_Space(e1, ze0);
          make_and_typecheck_OpSeqZ(ctx, u_gen, ze0', surround');
        }
        : {
          let (e1, prefix') = OperatorSeq.split_tail(seq1);
          if (is_EmptyHole(e0)) {
            /* prefix' e1 op1 |_0 --> prefix' e1| */
            let surround' = OperatorSeq.EmptySuffix(prefix');
            let ze0' = _Cursor(After, e1);
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0', surround');
          } else if (is_EmptyHole(e1)) {
            /* prefix' _1 op1 |e0 --> prefix' |e0 */
            let surround' = OperatorSeq.EmptySuffix(prefix');
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
          } else {
            /* seq1 op1 |ze0 --> seq1 |ze0 */
            let prefix' = OperatorSeq.SeqPrefix(seq1, _Space);
            let surround' = OperatorSeq.EmptySuffix(prefix');
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
          };
        }
    }
  | OperatorSeq.BothNonEmpty(prefix, suffix) =>
    switch (prefix) {
    | OperatorSeq.ExpPrefix(e1, op1) =>
      /* e1 op1 |ze0 ...suffix */
      is_Space(op1)
        /* e1 |ze0 ...suffix */
        ? {
          let ze0' = combine_for_Backspace_Space(e1, ze0);
          let surround' = OperatorSeq.EmptyPrefix(suffix);
          make_and_typecheck_OpSeqZ(ctx, u_gen, ze0', surround');
        }
        : (
          if (is_EmptyHole(e0)) {
            /* e1 op1 |_0 suffix --> e1| suffix */
            let surround' = OperatorSeq.EmptyPrefix(suffix);
            let ze0' = _Cursor(After, e1);
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0', surround');
          } else if (is_EmptyHole(e1)) {
            /* _1 op1 |e0 suffix --> |e0 suffix */
            let surround' = OperatorSeq.EmptyPrefix(suffix);
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
          } else {
            /* e1 op1 |ze0 --> e1 |ze0 ...suffix */
            let surround' =
              OperatorSeq.BothNonEmpty(
                OperatorSeq.ExpPrefix(e1, _Space),
                suffix,
              );
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
          }
        )
    | OperatorSeq.SeqPrefix(seq1, op1) =>
      /* seq1 op1 |ze0 ...suffix */
      is_Space(op1)
        /* seq1 |ze0 ...suffix */
        ? {
          let (e1, prefix') = OperatorSeq.split_tail(seq1);
          let ze0' = combine_for_Backspace_Space(e1, ze0);
          let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
          make_and_typecheck_OpSeqZ(ctx, u_gen, ze0', surround');
        }
        : {
          let (e1, prefix') = OperatorSeq.split_tail(seq1);
          if (is_EmptyHole(e0)) {
            /* prefix' e1 op1 |_0 suffix --> prefix' e1| suffix */
            let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
            let ze0' = _Cursor(After, e1);
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0', surround');
          } else if (is_EmptyHole(e1)) {
            /* prefix' _1 op1 |e0 suffix --> prefix' |e0 suffix */
            let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
          } else {
            /* seq1 op1 |ze0 suffix --> seq1 |ze0 suffix */
            let prefix' = OperatorSeq.SeqPrefix(seq1, _Space);
            let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
          };
        }
    }
  };

let abs_perform_Delete_After_op =
    (
      combine_for_Delete_Space: ('z, 'e) => 'z,
      z_typecheck_fix_holes: (Contexts.t, MetaVarGen.t, 'z) => option('m),
      make_and_typecheck_OpSeqZ:
        (Contexts.t, MetaVarGen.t, 'z, OperatorSeq.opseq_surround('e, 'op)) =>
        option('m),
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
  | OperatorSeq.EmptySuffix(_) => None /* precluded by pattern begin match above */
  | OperatorSeq.EmptyPrefix(suffix) =>
    switch (suffix) {
    | OperatorSeq.ExpSuffix(op, e1) =>
      is_Space(op)
        ? {
          let ze0' = combine_for_Delete_Space(ze0, e1);
          z_typecheck_fix_holes(ctx, u_gen, ze0');
        }
        : (
          switch (is_EmptyHole(e0), is_EmptyHole(e1)) {
          | (true, true) =>
            /* _0| op _1 --> _0| */
            z_typecheck_fix_holes(ctx, u_gen, ze0)
          | (true, false) =>
            /* _0| op e1 --> |e1 */
            let ze1 = _Cursor(Before, e1);
            z_typecheck_fix_holes(ctx, u_gen, ze1);
          | (false, true) =>
            /* e0| op _ --> e0| */
            z_typecheck_fix_holes(ctx, u_gen, ze0)
          | (false, false) =>
            /* e0| op e1 --> e0| e1 */
            let surround' =
              OperatorSeq.EmptyPrefix(OperatorSeq.ExpSuffix(_Space, e1));
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
          }
        )
    | OperatorSeq.SeqSuffix(op, seq) =>
      is_Space(op)
        ? {
          let (e, suffix') = OperatorSeq.split0(seq);
          let surround' = OperatorSeq.EmptyPrefix(suffix');
          let ze0' = combine_for_Delete_Space(ze0, e);
          make_and_typecheck_OpSeqZ(ctx, u_gen, ze0', surround');
        }
        : {
          let (e1, suffix') = OperatorSeq.split0(seq);
          if (is_EmptyHole(e1)) {
            /* e0| op _ suffix' --> e0| suffix' */
            let surround' = OperatorSeq.EmptyPrefix(suffix');
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
          } else if (is_EmptyHole(e0)) {
            /* _0| op e1 suffix' --> |e1 suffix' */
            let surround' = OperatorSeq.EmptyPrefix(suffix');
            let ze1 = _Cursor(Before, e1);
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze1, surround');
          } else {
            /* e0| op seq --> e0| seq */
            let suffix' = OperatorSeq.SeqSuffix(_Space, seq);
            let surround' = OperatorSeq.EmptyPrefix(suffix');
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
          };
        }
    }
  | OperatorSeq.BothNonEmpty(prefix, suffix) =>
    switch (suffix) {
    | OperatorSeq.ExpSuffix(op, e1) =>
      is_Space(op)
        ? {
          let ze0' = combine_for_Delete_Space(ze0, e1);
          let surround' = OperatorSeq.EmptySuffix(prefix);
          make_and_typecheck_OpSeqZ(ctx, u_gen, ze0', surround');
        }
        : (
          if (is_EmptyHole(e1)) {
            /* prefix e0| op _ --> prefix e0| */
            let surround' = OperatorSeq.EmptySuffix(prefix);
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
          } else if (is_EmptyHole(e0)) {
            /* prefix _0| op e1 --> prefix |e1 */
            let surround' = OperatorSeq.EmptySuffix(prefix);
            let ze1 = _Cursor(Before, e1);
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze1, surround');
          } else {
            /* prefix e0| op e1 --> e0| e1 */
            let surround' =
              OperatorSeq.BothNonEmpty(
                prefix,
                OperatorSeq.ExpSuffix(_Space, e1),
              );
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
          }
        )
    | OperatorSeq.SeqSuffix(op, seq) =>
      is_Space(op)
        ? {
          let (e, suffix') = OperatorSeq.split0(seq);
          let ze0' = combine_for_Delete_Space(ze0, e);
          let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
          make_and_typecheck_OpSeqZ(ctx, u_gen, ze0', surround');
        }
        : {
          let (e1, suffix') = OperatorSeq.split0(seq);
          if (is_EmptyHole(e1)) {
            /* prefix e0| op _ suffix' --> prefix e0| suffix' */
            let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
          } else if (is_EmptyHole(e0)) {
            /* prefix _0| op e1 suffix' --> prefix |e1 suffix' */
            let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
            let ze1 = _Cursor(Before, e1);
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze1, surround');
          } else {
            /* prefix e| op seq --> e| seq */
            let suffix' = OperatorSeq.SeqSuffix(_Space, seq);
            let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
            make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
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
        option('m),
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      e: 'e,
      op: 'op,
    )
    : option('m) => {
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
        option('m),
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      e: 'e,
      op: 'op,
    )
    : option('m) => {
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
        option('m),
      is_Space: 'op => bool,
      _Space: 'op,
      _Cursor: (cursor_side, 'e) => 'z,
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      e: 'e,
      op: 'op,
      surround: OperatorSeq.opseq_surround('e, 'op),
    )
    : option('m) =>
  switch (surround) {
  | OperatorSeq.EmptySuffix(prefix) =>
    let prefix' = OperatorSeq.prefix_append_exp(prefix, e, op);
    let surround' = OperatorSeq.EmptySuffix(prefix');
    let (ze0, u_gen) = new_EmptyHole(u_gen);
    make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
  | OperatorSeq.EmptyPrefix(suffix) =>
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
    | OperatorSeq.SeqSuffix(op', seq') =>
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
  | OperatorSeq.BothNonEmpty(prefix, suffix) =>
    switch (suffix) {
    | OperatorSeq.ExpSuffix(op', e') =>
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
    | OperatorSeq.SeqSuffix(op', seq') =>
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
        option('m),
      is_Space: 'op => bool,
      _Space: 'op,
      _Cursor: (cursor_side, 'e) => 'z,
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ze0: 'z,
      op: 'op,
      surround: OperatorSeq.opseq_surround('e, 'op),
    )
    : option('m) =>
  switch (surround) {
  | OperatorSeq.EmptyPrefix(suffix) =>
    /* |ze0 ... --> |_ op e0 ... */
    let e0 = erase(ze0);
    let suffix' = OperatorSeq.suffix_prepend_exp(suffix, op, e0);
    let surround' = OperatorSeq.EmptyPrefix(suffix');
    let (ze0, u_gen) = new_EmptyHole(u_gen);
    make_and_typecheck_OpSeqZ(ctx, u_gen, ze0, surround');
  | OperatorSeq.EmptySuffix(OperatorSeq.ExpPrefix(e1, op') as prefix) =>
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
  | OperatorSeq.EmptySuffix(OperatorSeq.SeqPrefix(seq1, op') as prefix) =>
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
            let surround' =
              OperatorSeq.EmptySuffix(OperatorSeq.SeqPrefix(seq1, op));
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
  | OperatorSeq.BothNonEmpty(
      OperatorSeq.ExpPrefix(e1, op') as prefix,
      suffix,
    ) =>
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
  | OperatorSeq.BothNonEmpty(
      OperatorSeq.SeqPrefix(seq1, op') as prefix,
      suffix,
    ) =>
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
    : option((ZPat.t, HTyp.t, Contexts.t, MetaVarGen.t)) => {
  let path = Path.of_zpat(zp);
  let p = ZPat.erase(zp);
  switch (Statics.syn_pat_fix_holes(ctx, u_gen, false, p)) {
  | None => None
  | Some((p, ty, ctx, u_gen)) =>
    switch (Path.follow_pat(path, p)) {
    | None => None
    | Some(zp) => Some((zp, ty, ctx, u_gen))
    }
  };
};

let ana_zpat_fix_holes =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, zp: ZPat.t, ty: HTyp.t)
    : option((ZPat.t, Contexts.t, MetaVarGen.t)) => {
  let path = Path.of_zpat(zp);
  let p = ZPat.erase(zp);
  switch (Statics.ana_pat_fix_holes(ctx, u_gen, false, p, ty)) {
  | None => None
  | Some((p, ctx, u_gen)) =>
    switch (Path.follow_pat(path, p)) {
    | None => None
    | Some(zp) => Some((zp, ctx, u_gen))
    }
  };
};

let make_and_syn_OpSeqZ_pat =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      zp0: ZPat.t,
      surround: ZPat.opseq_surround,
    )
    : option((ZPat.t, HTyp.t, Contexts.t, MetaVarGen.t)) => {
  /* figure out the current path so that we can follow it again
   * to reconstitute the Z-exp after calling into the UHExp hole
   * insertion logic (otherwise we'd have to do a version of that
   * logic specific to Z-exps) */
  let path0 = Path.of_OpSeqZ_pat(zp0, surround);
  let p0 = ZPat.erase(zp0);
  let seq = OperatorSeq.opseq_of_exp_and_surround(p0, surround);
  let skel = Associator.associate_pat(seq);
  switch (Statics.syn_skel_pat_fix_holes(ctx, u_gen, false, skel, seq)) {
  | Some((Skel.BinOp(err, _, _, _) as skel, seq, ty, ctx, u_gen)) =>
    let p = UHPat.Pat(err, UHPat.OpSeq(skel, seq));
    switch (Path.follow_pat(path0, p)) {
    | Some(zp) => Some((zp, ty, ctx, u_gen))
    | None => None
    };
  | Some((Skel.Placeholder(_), _, _, _, _))
  | None => None
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
    : option((ZPat.t, Contexts.t, MetaVarGen.t)) => {
  /* figure out the current path so that we can follow it again
   * to reconstitute the Z-exp after calling into the UHExp hole
   * insertion logic (otherwise we'd have to do a version of that
   * logic specific to Z-exps) */
  let path0 = Path.of_OpSeqZ_pat(zp0, surround);
  let p0 = ZPat.erase(zp0);
  let seq = OperatorSeq.opseq_of_exp_and_surround(p0, surround);
  let skel = Associator.associate_pat(seq);
  switch (Statics.ana_skel_pat_fix_holes(ctx, u_gen, false, skel, seq, ty)) {
  | Some((Skel.BinOp(err, _, _, _) as skel, seq, ctx, u_gen)) =>
    let p = UHPat.Pat(err, UHPat.OpSeq(skel, seq));
    switch (Path.follow_pat(path0, p)) {
    | Some(zp) => Some((zp, ctx, u_gen))
    | None => None
    };
  | Some((Skel.Placeholder(_), _, _, _))
  | None => None
  };
};

let combine_for_Backspace_Space_pat = (p1, zp0) =>
  switch (zp0) {
  | ZPat.CursorP(_, UHPat.Pat(_, UHPat.EmptyHole(_))) =>
    /* p1 |_ --> p1| */
    ZPat.CursorP(After, p1)
  | _ =>
    /* p1 |zp0 --> |zp0 */
    zp0
  };

let combine_for_Delete_Space_pat = (zp0, p) =>
  switch (zp0, p) {
  | (
      ZPat.CursorP(After, UHPat.Pat(_, UHPat.EmptyHole(_))),
      UHPat.Pat(_, UHPat.EmptyHole(_)),
    ) =>
    /* _| _ --> _| */
    zp0
  | (ZPat.CursorP(After, UHPat.Pat(_, UHPat.EmptyHole(_))), _) =>
    /* _| p  --> |p */
    ZPat.CursorP(Before, p)
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
      | Some(zp) => Some((zp, ty, ctx, u_gen))
      | None => None
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
  | (Backspace, ZPat.CursorP(After, p)) =>
    switch (p) {
    | UHPat.Pat(_, UHPat.EmptyHole(_)) =>
      Some((ZPat.CursorP(Before, p), HTyp.Hole, ctx, u_gen))
    | _ =>
      let (p, u_gen) = UHPat.new_EmptyHole(u_gen);
      Some((ZPat.CursorP(Before, p), HTyp.Hole, ctx, u_gen));
    }
  | (Backspace, ZPat.CursorP(Before, _)) => None
  | (Delete, ZPat.CursorP(Before, p)) =>
    switch (p) {
    | UHPat.Pat(_, UHPat.EmptyHole(_)) =>
      Some((ZPat.CursorP(After, p), HTyp.Hole, ctx, u_gen))
    | _ =>
      let (p, u_gen) = UHPat.new_EmptyHole(u_gen);
      Some((ZPat.CursorP(Before, p), HTyp.Hole, ctx, u_gen));
    }
  | (Delete, ZPat.CursorP(After, _)) => None
  | (Backspace, ZPat.CursorP(In(_), _))
  | (Delete, ZPat.CursorP(In(_), _)) =>
    let (p, u_gen) = UHPat.new_EmptyHole(u_gen);
    let zp = ZPat.CursorP(Before, p);
    Some((zp, HTyp.Hole, ctx, u_gen));
  | (
      Backspace,
      ZPat.Deeper(
        _,
        ZPat.OpSeqZ(
          _,
          ZPat.CursorP(Before, p0) as zp0,
          OperatorSeq.EmptySuffix(_) as surround,
        ),
      ),
    )
  | (
      Backspace,
      ZPat.Deeper(
        _,
        ZPat.OpSeqZ(
          _,
          ZPat.CursorP(Before, p0) as zp0,
          OperatorSeq.BothNonEmpty(_, _) as surround,
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
      (side, p) => ZPat.CursorP(side, p),
      ctx,
      u_gen,
      p0,
      zp0,
      surround,
    )
  | (
      Delete,
      ZPat.Deeper(
        _,
        ZPat.OpSeqZ(
          _,
          ZPat.CursorP(After, p0) as zp0,
          OperatorSeq.EmptyPrefix(_) as surround,
        ),
      ),
    )
  | (
      Delete,
      ZPat.Deeper(
        _,
        ZPat.OpSeqZ(
          _,
          ZPat.CursorP(After, p0) as zp0,
          OperatorSeq.BothNonEmpty(_, _) as surround,
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
      (side, p) => ZPat.CursorP(side, p),
      ctx,
      u_gen,
      p0,
      zp0,
      surround,
    )
  /* Construct */
  | (Construct(SParenthesized), ZPat.CursorP(_, p)) =>
    switch (Statics.syn_pat(ctx, p)) {
    | None => None
    | Some((ty, ctx)) => Some((ZPat.ParenthesizedZ(zp), ty, ctx, u_gen))
    }
  | (
      Construct(SVar(x, side)),
      ZPat.CursorP(_, UHPat.Pat(_, UHPat.EmptyHole(_))),
    )
  | (Construct(SVar(x, side)), ZPat.CursorP(_, UHPat.Pat(_, UHPat.Wild)))
  | (
      Construct(SVar(x, side)),
      ZPat.CursorP(_, UHPat.Pat(_, UHPat.Var(_))),
    )
  | (
      Construct(SVar(x, side)),
      ZPat.CursorP(_, UHPat.Pat(_, UHPat.NumLit(_))),
    )
  | (
      Construct(SVar(x, side)),
      ZPat.CursorP(_, UHPat.Pat(_, UHPat.BoolLit(_))),
    ) =>
    if (String.equal(x, "true")) {
      Some((
        ZPat.CursorP(side, UHPat.Pat(NotInHole, UHPat.BoolLit(true))),
        HTyp.Bool,
        ctx,
        u_gen,
      ));
    } else if (String.equal(x, "false")) {
      Some((
        ZPat.CursorP(side, UHPat.Pat(NotInHole, UHPat.BoolLit(false))),
        HTyp.Bool,
        ctx,
        u_gen,
      ));
    } else {
      Var.check_valid(
        x,
        {
          let ctx = Contexts.extend_gamma(ctx, (x, HTyp.Hole));
          Some((
            ZPat.CursorP(side, UHPat.Pat(NotInHole, UHPat.Var(x))),
            HTyp.Hole,
            ctx,
            u_gen,
          ));
        },
      );
    }
  | (Construct(SVar(_, _)), ZPat.CursorP(_, _)) => None
  | (Construct(SWild), ZPat.CursorP(_, UHPat.Pat(_, UHPat.EmptyHole(_))))
  | (Construct(SWild), ZPat.CursorP(_, UHPat.Pat(_, UHPat.Wild)))
  | (Construct(SWild), ZPat.CursorP(_, UHPat.Pat(_, UHPat.Var(_))))
  | (Construct(SWild), ZPat.CursorP(_, UHPat.Pat(_, UHPat.NumLit(_))))
  | (Construct(SWild), ZPat.CursorP(_, UHPat.Pat(_, UHPat.BoolLit(_)))) =>
    Some((
      ZPat.CursorP(After, UHPat.Pat(NotInHole, UHPat.Wild)),
      HTyp.Hole,
      ctx,
      u_gen,
    ))
  | (Construct(SWild), ZPat.CursorP(_, _)) => None
  | (
      Construct(SNumLit(n, side)),
      ZPat.CursorP(_, UHPat.Pat(_, UHPat.EmptyHole(_))),
    )
  | (
      Construct(SNumLit(n, side)),
      ZPat.CursorP(_, UHPat.Pat(_, UHPat.Wild)),
    )
  | (
      Construct(SNumLit(n, side)),
      ZPat.CursorP(_, UHPat.Pat(_, UHPat.Var(_))),
    )
  | (
      Construct(SNumLit(n, side)),
      ZPat.CursorP(_, UHPat.Pat(_, UHPat.NumLit(_))),
    )
  | (
      Construct(SNumLit(n, side)),
      ZPat.CursorP(_, UHPat.Pat(_, UHPat.BoolLit(_))),
    ) =>
    Some((
      ZPat.CursorP(side, UHPat.Pat(NotInHole, UHPat.NumLit(n))),
      HTyp.Num,
      ctx,
      u_gen,
    ))
  | (Construct(SNumLit(_, _)), ZPat.CursorP(_, _)) => None
  | (Construct(SInj(side)), ZPat.CursorP(_, p1)) =>
    switch (Statics.syn_pat(ctx, p1)) {
    | None => None
    | Some((ty1, ctx)) =>
      let zp = ZPat.Deeper(NotInHole, ZPat.InjZ(side, zp));
      let ty =
        switch (side) {
        | L => HTyp.Sum(ty1, HTyp.Hole)
        | R => HTyp.Sum(HTyp.Hole, ty1)
        };
      Some((zp, ty, ctx, u_gen));
    }
  | (
      Construct(SListNil),
      ZPat.CursorP(_, UHPat.Pat(_, UHPat.EmptyHole(_))),
    ) =>
    let zp = ZPat.CursorP(After, UHPat.Pat(NotInHole, UHPat.ListNil));
    let ty = HTyp.List(HTyp.Hole);
    Some((zp, ty, ctx, u_gen));
  | (Construct(SListNil), ZPat.CursorP(_, _)) => None
  | (
      Construct(SOp(os)),
      ZPat.Deeper(_, ZPat.OpSeqZ(_, ZPat.CursorP(In(_), p), surround)),
    )
  | (
      Construct(SOp(os)),
      ZPat.Deeper(_, ZPat.OpSeqZ(_, ZPat.CursorP(After, p), surround)),
    ) =>
    switch (pat_op_of(os)) {
    | None => None
    | Some(op) =>
      abs_perform_Construct_SOp_After_surround(
        ZPat.new_EmptyHole,
        make_and_syn_OpSeqZ_pat,
        UHPat.is_Space,
        UHPat.Space,
        (side, p) => ZPat.CursorP(side, p),
        ctx,
        u_gen,
        p,
        op,
        surround,
      )
    }
  | (
      Construct(SOp(os)),
      ZPat.Deeper(
        _,
        ZPat.OpSeqZ(_, ZPat.CursorP(Before, _) as zp0, surround),
      ),
    ) =>
    switch (pat_op_of(os)) {
    | None => None
    | Some(op) =>
      abs_perform_Construct_SOp_Before_surround(
        ZPat.erase,
        ZPat.new_EmptyHole,
        make_and_syn_OpSeqZ_pat,
        UHPat.is_Space,
        UHPat.Space,
        (side, p) => ZPat.CursorP(side, p),
        ctx,
        u_gen,
        zp0,
        op,
        surround,
      )
    }
  | (Construct(SOp(os)), ZPat.CursorP(In(_), p))
  | (Construct(SOp(os)), ZPat.CursorP(After, p)) =>
    switch (pat_op_of(os)) {
    | None => None
    | Some(op) =>
      abs_perform_Construct_SOp_After(
        UHPat.bidelimit,
        ZPat.new_EmptyHole,
        make_and_syn_OpSeqZ_pat,
        ctx,
        u_gen,
        p,
        op,
      )
    }
  | (Construct(SOp(os)), ZPat.CursorP(Before, p)) =>
    switch (pat_op_of(os)) {
    | None => None
    | Some(op) =>
      abs_perform_Construct_SOp_Before(
        UHPat.bidelimit,
        ZPat.new_EmptyHole,
        make_and_syn_OpSeqZ_pat,
        ctx,
        u_gen,
        p,
        op,
      )
    }
  /* Zipper */
  | (_, ZPat.ParenthesizedZ(zp1)) =>
    switch (perform_syn_pat(ctx, u_gen, a, zp1)) {
    | None => None
    | Some((zp1, ty, ctx, u_gen)) =>
      Some((ZPat.ParenthesizedZ(zp1), ty, ctx, u_gen))
    }
  | (_, ZPat.Deeper(_, ZPat.InjZ(side, zp1))) =>
    switch (perform_syn_pat(ctx, u_gen, a, zp1)) {
    | None => None
    | Some((zp1, ty1, ctx, u_gen)) =>
      let zp = ZPat.Deeper(NotInHole, ZPat.InjZ(side, zp1));
      let ty =
        switch (side) {
        | L => HTyp.Sum(ty1, HTyp.Hole)
        | R => HTyp.Sum(HTyp.Hole, ty1)
        };
      Some((zp, ty, ctx, u_gen));
    }
  | (_, ZPat.Deeper(err, ZPat.OpSeqZ(_, zp0, surround))) =>
    let i = OperatorSeq.surround_prefix_length(surround);
    switch (ZPat.erase(zp)) {
    | UHPat.Pat(_, UHPat.OpSeq(skel, seq)) =>
      switch (Statics.syn_skel_pat(ctx, skel, seq, Some(i))) {
      | Some((ty, ctx, Some(mode))) =>
        switch (mode) {
        | Statics.AnalyzedAgainst(ty0) =>
          switch (perform_ana_pat(ctx, u_gen, a, zp0, ty0)) {
          | None => None
          | Some((zp0, ctx, u_gen)) =>
            let zp0 = ZPat.bidelimit(zp0);
            make_and_syn_OpSeqZ_pat(ctx, u_gen, zp0, surround);
          }
        | Statics.Synthesized(ty0) =>
          switch (perform_syn_pat(ctx, u_gen, a, zp0)) {
          | Some((zp0, ty0, ctx, u_gen)) =>
            let zp0 = ZPat.bidelimit(zp0);
            make_and_syn_OpSeqZ_pat(ctx, u_gen, zp0, surround);
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
  | (_, ZPat.Deeper(InHole(TypeInconsistent, u) as err, zp1)) =>
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
  | (Backspace, ZPat.CursorP(After, p)) =>
    switch (p) {
    | UHPat.Pat(_, UHPat.EmptyHole(_)) =>
      Some((ZPat.CursorP(Before, p), ctx, u_gen))
    | _ =>
      let (p, u_gen) = UHPat.new_EmptyHole(u_gen);
      Some((ZPat.CursorP(Before, p), ctx, u_gen));
    }
  | (Backspace, ZPat.CursorP(Before, p)) => None
  | (Delete, ZPat.CursorP(Before, p)) =>
    switch (p) {
    | UHPat.Pat(_, UHPat.EmptyHole(_)) =>
      Some((ZPat.CursorP(After, p), ctx, u_gen))
    | _ =>
      let (p, u_gen) = UHPat.new_EmptyHole(u_gen);
      Some((ZPat.CursorP(Before, p), ctx, u_gen));
    }
  | (Backspace, ZPat.CursorP(In(_), _))
  | (Delete, ZPat.CursorP(In(_), _)) =>
    let (p, u_gen) = UHPat.new_EmptyHole(u_gen);
    let zp = ZPat.CursorP(Before, p);
    Some((zp, ctx, u_gen));
  | (Delete, ZPat.CursorP(After, _)) => None
  | (
      Backspace,
      ZPat.Deeper(
        _,
        ZPat.OpSeqZ(
          _,
          ZPat.CursorP(Before, p0) as zp0,
          OperatorSeq.EmptySuffix(_) as surround,
        ),
      ),
    )
  | (
      Backspace,
      ZPat.Deeper(
        _,
        ZPat.OpSeqZ(
          _,
          ZPat.CursorP(Before, p0) as zp0,
          OperatorSeq.BothNonEmpty(_, _) as surround,
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
      (side, p) => ZPat.CursorP(side, p),
      ctx,
      u_gen,
      p0,
      zp0,
      surround,
    )
  | (
      Delete,
      ZPat.Deeper(
        _,
        ZPat.OpSeqZ(
          _,
          ZPat.CursorP(After, p0) as zp0,
          OperatorSeq.EmptyPrefix(_) as surround,
        ),
      ),
    )
  | (
      Delete,
      ZPat.Deeper(
        _,
        ZPat.OpSeqZ(
          _,
          ZPat.CursorP(After, p0) as zp0,
          OperatorSeq.BothNonEmpty(_, _) as surround,
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
      (side, p) => ZPat.CursorP(side, p),
      ctx,
      u_gen,
      p0,
      zp0,
      surround,
    )
  /* Construct */
  | (Construct(SParenthesized), ZPat.CursorP(_, p)) =>
    switch (Statics.ana_pat(ctx, p, ty)) {
    | None => None
    | Some(ctx) => Some((ZPat.ParenthesizedZ(zp), ctx, u_gen))
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
  | (
      Construct(SVar(x, side)),
      ZPat.CursorP(_, UHPat.Pat(_, UHPat.EmptyHole(_))),
    )
  | (Construct(SVar(x, side)), ZPat.CursorP(_, UHPat.Pat(_, UHPat.Wild)))
  | (
      Construct(SVar(x, side)),
      ZPat.CursorP(_, UHPat.Pat(_, UHPat.Var(_))),
    )
  | (
      Construct(SVar(x, side)),
      ZPat.CursorP(_, UHPat.Pat(_, UHPat.NumLit(_))),
    )
  | (
      Construct(SVar(x, side)),
      ZPat.CursorP(_, UHPat.Pat(_, UHPat.BoolLit(_))),
    ) =>
    Var.check_valid(
      x,
      {
        let ctx = Contexts.extend_gamma(ctx, (x, ty));
        Some((
          ZPat.CursorP(side, UHPat.Pat(NotInHole, UHPat.Var(x))),
          ctx,
          u_gen,
        ));
      },
    )
  | (Construct(SVar(_, _)), ZPat.CursorP(_, _)) => None
  | (Construct(SWild), ZPat.CursorP(_, UHPat.Pat(_, UHPat.EmptyHole(_))))
  | (Construct(SWild), ZPat.CursorP(_, UHPat.Pat(_, UHPat.Wild)))
  | (Construct(SWild), ZPat.CursorP(_, UHPat.Pat(_, UHPat.Var(_))))
  | (Construct(SWild), ZPat.CursorP(_, UHPat.Pat(_, UHPat.NumLit(_))))
  | (Construct(SWild), ZPat.CursorP(_, UHPat.Pat(_, UHPat.BoolLit(_)))) =>
    Some((
      ZPat.CursorP(After, UHPat.Pat(NotInHole, UHPat.Wild)),
      ctx,
      u_gen,
    ))
  | (Construct(SWild), ZPat.CursorP(_, _)) => None
  | (Construct(SInj(side)), ZPat.CursorP(cursor_side, p1)) =>
    switch (HTyp.matched_sum(ty)) {
    | Some((tyL, tyR)) =>
      let ty1 = pick_side(side, tyL, tyR);
      switch (Statics.ana_pat_fix_holes(ctx, u_gen, false, p1, ty1)) {
      | None => None
      | Some((p1, ctx, u_gen)) =>
        let zp =
          ZPat.Deeper(
            NotInHole,
            ZPat.InjZ(side, ZPat.CursorP(cursor_side, p1)),
          );
        Some((zp, ctx, u_gen));
      };
    | None =>
      switch (Statics.syn_pat_fix_holes(ctx, u_gen, false, p1)) {
      | None => None
      | Some((p1, _, ctx, u_gen)) =>
        let (u, u_gen) = MetaVarGen.next(u_gen);
        let zp =
          ZPat.Deeper(
            InHole(TypeInconsistent, u),
            ZPat.InjZ(side, ZPat.CursorP(cursor_side, p1)),
          );
        Some((zp, ctx, u_gen));
      }
    }
  | (
      Construct(SOp(os)),
      ZPat.Deeper(_, ZPat.OpSeqZ(_, ZPat.CursorP(In(_), p), surround)),
    )
  | (
      Construct(SOp(os)),
      ZPat.Deeper(_, ZPat.OpSeqZ(_, ZPat.CursorP(After, p), surround)),
    ) =>
    switch (pat_op_of(os)) {
    | None => None
    | Some(op) =>
      abs_perform_Construct_SOp_After_surround(
        ZPat.new_EmptyHole,
        (ctx, u_gen, zp, surround) =>
          make_and_ana_OpSeqZ_pat(ctx, u_gen, zp, surround, ty),
        UHPat.is_Space,
        UHPat.Space,
        (side, p) => ZPat.CursorP(side, p),
        ctx,
        u_gen,
        p,
        op,
        surround,
      )
    }
  | (
      Construct(SOp(os)),
      ZPat.Deeper(
        _,
        ZPat.OpSeqZ(_, ZPat.CursorP(Before, _) as zp0, surround),
      ),
    ) =>
    switch (pat_op_of(os)) {
    | None => None
    | Some(op) =>
      abs_perform_Construct_SOp_Before_surround(
        ZPat.erase,
        ZPat.new_EmptyHole,
        (ctx, u_gen, zp, surround) =>
          make_and_ana_OpSeqZ_pat(ctx, u_gen, zp, surround, ty),
        UHPat.is_Space,
        UHPat.Space,
        (side, p) => ZPat.CursorP(side, p),
        ctx,
        u_gen,
        zp0,
        op,
        surround,
      )
    }
  | (Construct(SOp(os)), ZPat.CursorP(In(_), p))
  | (Construct(SOp(os)), ZPat.CursorP(After, p)) =>
    switch (pat_op_of(os)) {
    | None => None
    | Some(op) =>
      abs_perform_Construct_SOp_After(
        UHPat.bidelimit,
        ZPat.new_EmptyHole,
        (ctx, u_gen, zp, surround) =>
          make_and_ana_OpSeqZ_pat(ctx, u_gen, zp, surround, ty),
        ctx,
        u_gen,
        p,
        op,
      )
    }
  | (Construct(SOp(os)), ZPat.CursorP(Before, p)) =>
    switch (pat_op_of(os)) {
    | None => None
    | Some(op) =>
      abs_perform_Construct_SOp_Before(
        UHPat.bidelimit,
        ZPat.new_EmptyHole,
        (ctx, u_gen, zp, surround) =>
          make_and_ana_OpSeqZ_pat(ctx, u_gen, zp, surround, ty),
        ctx,
        u_gen,
        p,
        op,
      )
    }
  /* Zipper */
  | (_, ZPat.ParenthesizedZ(zp1)) =>
    switch (perform_ana_pat(ctx, u_gen, a, zp1, ty)) {
    | None => None
    | Some((zp1, ctx, u_gen)) =>
      Some((ZPat.ParenthesizedZ(zp1), ctx, u_gen))
    }
  | (_, ZPat.Deeper(_, ZPat.InjZ(side, zp1))) =>
    switch (HTyp.matched_sum(ty)) {
    | None => None
    | Some((tyL, tyR)) =>
      let ty1 = pick_side(side, tyL, tyR);
      switch (perform_ana_pat(ctx, u_gen, a, zp1, ty1)) {
      | None => None
      | Some((zp1, ctx, u_gen)) =>
        let zp = ZPat.Deeper(NotInHole, ZPat.InjZ(side, zp1));
        Some((zp, ctx, u_gen));
      };
    }
  | (_, ZPat.Deeper(err, ZPat.OpSeqZ(_, zp0, surround))) =>
    let i = OperatorSeq.surround_prefix_length(surround);
    switch (ZPat.erase(zp)) {
    | UHPat.Pat(_, UHPat.OpSeq(skel, seq)) =>
      switch (Statics.ana_skel_pat(ctx, skel, seq, ty, Some(i))) {
      | Some((_, Some(mode))) =>
        switch (mode) {
        | Statics.AnalyzedAgainst(ty0) =>
          switch (perform_ana_pat(ctx, u_gen, a, zp0, ty0)) {
          | None => None
          | Some((zp0, _, u_gen)) =>
            let zp0 = ZPat.bidelimit(zp0);
            make_and_ana_OpSeqZ_pat(ctx, u_gen, zp0, surround, ty);
          }
        | Statics.Synthesized(ty0) =>
          switch (perform_syn_pat(ctx, u_gen, a, zp0)) {
          | Some((zp0, ty0, _, u_gen)) =>
            let zp0 = ZPat.bidelimit(zp0);
            make_and_ana_OpSeqZ_pat(ctx, u_gen, zp0, surround, ty);
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
    : option((ZExp.t, HTyp.t, MetaVarGen.t)) => {
  let path = Path.of_zexp(ze);
  let e = ZExp.erase(ze);
  switch (Statics.syn_fix_holes(ctx, u_gen, e)) {
  | Some((e', ty, u_gen')) =>
    switch (Path.follow_e(path, e')) {
    | Some(ze') => Some((ze', ty, u_gen'))
    | None => None
    }
  | None => None
  };
};

let zexp_ana_fix_holes =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, ze: ZExp.t, ty: HTyp.t)
    : option((ZExp.t, MetaVarGen.t)) => {
  let path = Path.of_zexp(ze);
  let e = ZExp.erase(ze);
  switch (Statics.ana_fix_holes(ctx, u_gen, e, ty)) {
  | Some((e', u_gen')) =>
    switch (Path.follow_e(path, e')) {
    | Some(ze') => Some((ze', u_gen'))
    | None => None
    }
  | None => None
  };
};

let make_and_syn_OpSeqZ =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ze0: ZExp.t,
      surround: ZExp.opseq_surround,
    )
    : option((ZExp.t, HTyp.t, MetaVarGen.t)) => {
  /* figure out the current path so that we can follow it again
   * to reconstitute the Z-exp after calling into the UHExp hole
   * insertion logic (otherwise we'd have to do a version of that
   * logic specific to Z-exps) */
  let path0 = Path.of_OpSeqZ(ze0, surround);
  let e0 = ZExp.erase(ze0);
  let seq = OperatorSeq.opseq_of_exp_and_surround(e0, surround);
  let skel = Associator.associate_exp(seq);
  switch (Statics.syn_skel_fix_holes(ctx, u_gen, false, skel, seq)) {
  | Some((skel', seq', ty, u_gen')) =>
    let e' = UHExp.Tm(NotInHole, UHExp.OpSeq(skel', seq'));
    switch (Path.follow_e(path0, e')) {
    | Some(ze') => Some((ze', ty, u_gen'))
    | None => None
    };
  | None => None
  };
};

let make_and_ana_OpSeqZ =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ze0: ZExp.t,
      surround: ZExp.opseq_surround,
      ty: HTyp.t,
    )
    : option((ZExp.t, MetaVarGen.t)) => {
  /* figure out the current path so that we can follow it again
   * to reconstitute the Z-exp after calling into the UHExp hole
   * insertion logic (otherwise we'd have to do a version of that
   * logic specific to Z-exps) */
  let path0 = Path.of_OpSeqZ(ze0, surround);
  let e0 = ZExp.erase(ze0);
  let seq = OperatorSeq.opseq_of_exp_and_surround(e0, surround);
  let skel = Associator.associate_exp(seq);
  switch (Statics.ana_skel_fix_holes(ctx, u_gen, false, skel, seq, ty)) {
  | Some((Skel.BinOp(err, _, _, _) as skel, seq, u_gen)) =>
    let e = UHExp.Tm(err, UHExp.OpSeq(skel, seq));
    switch (Path.follow_e(path0, e)) {
    | Some(ze) => Some((ze, u_gen))
    | None => None
    };
  | Some((Skel.Placeholder(_), _, _))
  | None => None
  };
};

let combine_for_Backspace_Space = (e1, ze0) =>
  switch (e1, ze0) {
  | (_, ZExp.CursorE(_, UHExp.Tm(_, UHExp.EmptyHole(_)))) =>
    /* e1 |_ --> e1| */
    ZExp.CursorE(After, e1)
  | _ => ze0
  };

let combine_for_Delete_Space = (ze0, e) =>
  switch (ze0, e) {
  | (
      ZExp.CursorE(After, UHExp.Tm(_, UHExp.EmptyHole(_))),
      UHExp.Tm(_, UHExp.EmptyHole(_)),
    ) =>
    /* _| _ --> _| */
    ze0
  | (ZExp.CursorE(After, UHExp.Tm(_, UHExp.EmptyHole(_))), _) =>
    /* _| e --> |e */
    ZExp.CursorE(Before, e)
  | _ => ze0
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
    | Some(ze') => Some((ze', ty, u_gen))
    | None => None
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
  | (Backspace, ZExp.CursorE(After, e)) =>
    switch (e) {
    | UHExp.Tm(_, UHExp.EmptyHole(_)) =>
      Some((ZExp.CursorE(Before, e), ty, u_gen))
    | _ =>
      let (e', u_gen') = UHExp.new_EmptyHole(u_gen);
      Some((ZExp.CursorE(Before, e'), HTyp.Hole, u_gen'));
    }
  | (Backspace, ZExp.CursorE(Before, e)) => None
  | (Delete, ZExp.CursorE(Before, e)) =>
    switch (e) {
    | UHExp.Tm(_, UHExp.EmptyHole(_)) =>
      Some((ZExp.CursorE(After, e), ty, u_gen))
    | _ =>
      let (e', u_gen) = UHExp.new_EmptyHole(u_gen);
      Some((ZExp.CursorE(Before, e'), HTyp.Hole, u_gen));
    }
  | (Delete, ZExp.CursorE(After, e)) => None
  | (Backspace, ZExp.Deeper(_, ZExp.AscZ2(e1, zty)))
      when ZTyp.cursor_at_start(zty) =>
    let ze' = ZExp.CursorE(After, e1);
    zexp_syn_fix_holes(ctx, u_gen, ze');
  | (Delete, ZExp.Deeper(_, ZExp.AscZ1(ze1, _)))
      when ZExp.cursor_at_end(ze1) =>
    let e1 = ZExp.erase(ze1);
    switch (Statics.syn_fix_holes(ctx, u_gen, e1)) {
    | Some((e1', ty', u_gen)) =>
      let ze' = ZExp.CursorE(After, e1');
      Some((ze', ty', u_gen));
    | None => None
    };
  | (
      Backspace,
      ZExp.Deeper(
        _,
        ZExp.LineItemZE(
          li,
          ZExp.Deeper(
            _,
            ZExp.LineItemZL(ZExp.CursorL(_, UHExp.EmptyLine), e2),
          ),
        ),
      ),
    ) =>
    let zli = ZExp.place_After_line_item(li);
    let ze = ZExp.prepend_zline(zli, e2);
    Some((ze, ty, u_gen));
  | (
      Delete,
      ZExp.Deeper(_, ZExp.LineItemZL(ZExp.CursorL(_, UHExp.EmptyLine), e1)),
    ) =>
    let ze = ZExp.place_Before(e1);
    Some((ze, ty, u_gen));
  | (
      Backspace,
      ZExp.Deeper(_, ZExp.LineItemZL(ZExp.CursorL(Before, _), e2)),
    ) =>
    None
  | (
      Backspace,
      ZExp.Deeper(_, ZExp.LineItemZL(ZExp.CursorL(After, _), e2)),
    )
  | (
      Backspace,
      ZExp.Deeper(_, ZExp.LineItemZL(ZExp.CursorL(In(_), _), e2)),
    ) =>
    switch (Statics.syn_fix_holes(ctx, u_gen, e2)) {
    | None => None
    | Some((e2, ty, u_gen)) =>
      let ze = ZExp.prepend_zline(ZExp.CursorL(After, UHExp.EmptyLine), e2);
      Some((ze, ty, u_gen));
    }
  | (Delete, ZExp.Deeper(_, ZExp.LineItemZL(ZExp.CursorL(After, _), e2))) =>
    None
  | (Delete, ZExp.Deeper(_, ZExp.LineItemZL(ZExp.CursorL(Before, _), e2)))
  | (Delete, ZExp.Deeper(_, ZExp.LineItemZL(ZExp.CursorL(In(_), _), e2))) =>
    switch (Statics.syn_fix_holes(ctx, u_gen, e2)) {
    | None => None
    | Some((e2, ty, u_gen)) =>
      let ze = ZExp.prepend_zline(ZExp.CursorL(Before, UHExp.EmptyLine), e2);
      Some((ze, ty, u_gen));
    }
  | (Backspace, ZExp.Deeper(_, ZExp.LineItemZE(UHExp.EmptyLine, ze1)))
      when ZExp.cursor_at_start(ze1) =>
    Some((ze1, ty, u_gen))
  | (
      Delete,
      ZExp.Deeper(
        _,
        ZExp.LineItemZL(
          zli,
          UHExp.Tm(_, UHExp.LineItem(UHExp.EmptyLine, e2)),
        ),
      ),
    )
      when ZExp.cursor_at_end_line_item(zli) =>
    let ze = ZExp.prepend_zline(zli, e2);
    Some((ze, ty, u_gen));
  | (
      Backspace,
      ZExp.Deeper(
        _,
        ZExp.LineItemZE(
          UHExp.ExpLine(e1),
          ZExp.CursorE(Before, UHExp.Tm(_, UHExp.EmptyHole(_))),
        ),
      ),
    ) =>
    switch (Statics.syn(ctx, e1)) {
    | None => None
    | Some(ty) =>
      let ze = ZExp.place_After(e1);
      Some((ze, ty, u_gen));
    }
  | (
      Delete,
      ZExp.Deeper(
        _,
        ZExp.LineItemZL(
          ZExp.DeeperL(ZExp.ExpLineZ(ze1)),
          UHExp.Tm(_, UHExp.EmptyHole(_)),
        ),
      ),
    )
      when ZExp.cursor_at_end(ze1) =>
    let e1 = ZExp.erase(ze1);
    switch (Statics.syn(ctx, e1)) {
    | None => None
    | Some(ty) => Some((ze1, ty, u_gen))
    };
  | (
      Backspace,
      ZExp.Deeper(
        _,
        ZExp.LineItemZL(ZExp.DeeperL(ZExp.LetLineZA(p, zty, e1)), e2),
      ),
    )
      when ZTyp.cursor_at_start(zty) =>
    switch (Statics.syn_fix_holes(ctx, u_gen, e1)) {
    | None => None
    | Some((e1, ty1, u_gen)) =>
      switch (Statics.ana_pat_fix_holes(ctx, u_gen, false, p, ty1)) {
      | None => None
      | Some((p, ctx, u_gen)) =>
        switch (Statics.syn_fix_holes(ctx, u_gen, e2)) {
        | None => None
        | Some((e2, ty, u_gen)) =>
          let ze =
            ZExp.(
              prepend_zline(
                DeeperL(LetLineZP(ZPat.CursorP(After, p), None, e1)),
                e2,
              )
            );
          Some((ze, ty, u_gen));
        }
      }
    }
  | (
      Delete,
      ZExp.Deeper(
        _,
        ZExp.LineItemZL(ZExp.DeeperL(ZExp.LetLineZP(zp, Some(_), e1)), e2),
      ),
    )
      when ZPat.cursor_at_end(zp) =>
    switch (Statics.syn_fix_holes(ctx, u_gen, e1)) {
    | None => None
    | Some((e1, ty1, u_gen)) =>
      switch (ana_zpat_fix_holes(ctx, u_gen, zp, ty1)) {
      | None => None
      | Some((zp, ctx, u_gen)) =>
        switch (Statics.syn_fix_holes(ctx, u_gen, e2)) {
        | None => None
        | Some((e2, ty, u_gen)) =>
          let ze =
            ZExp.(prepend_zline(DeeperL(LetLineZP(zp, None, e1)), e2));
          Some((ze, ty, u_gen));
        }
      }
    }
  | (Backspace, ZExp.Deeper(_, ZExp.LamZA(p, zty, e1)))
      when ZTyp.cursor_at_start(zty) =>
    switch (Statics.ana_pat_fix_holes(ctx, u_gen, false, p, HTyp.Hole)) {
    | None => None
    | Some((p, ctx, u_gen)) =>
      switch (Statics.syn_fix_holes(ctx, u_gen, e1)) {
      | None => None
      | Some((e1, ty2, u_gen)) =>
        let ze =
          ZExp.Deeper(
            NotInHole,
            ZExp.LamZP(ZPat.CursorP(After, p), None, e1),
          );
        Some((ze, HTyp.Arrow(HTyp.Hole, ty2), u_gen));
      }
    }
  | (Delete, ZExp.Deeper(_, ZExp.LamZP(zp, Some(_), e1)))
      when ZPat.cursor_at_end(zp) =>
    switch (ana_zpat_fix_holes(ctx, u_gen, zp, HTyp.Hole)) {
    | None => None
    | Some((zp, ctx, u_gen)) =>
      switch (Statics.syn_fix_holes(ctx, u_gen, e1)) {
      | None => None
      | Some((e1, ty2, u_gen)) =>
        let ze = ZExp.Deeper(NotInHole, ZExp.LamZP(zp, None, e1));
        Some((ze, HTyp.Arrow(HTyp.Hole, ty2), u_gen));
      }
    }
  | (
      Backspace,
      ZExp.Deeper(
        _,
        ZExp.OpSeqZ(
          _,
          ZExp.CursorE(Before, e0) as ze0,
          OperatorSeq.EmptySuffix(_) as surround,
        ),
      ),
    )
  | (
      Backspace,
      ZExp.Deeper(
        _,
        ZExp.OpSeqZ(
          _,
          ZExp.CursorE(Before, e0) as ze0,
          OperatorSeq.BothNonEmpty(_, _) as surround,
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
      (side, e) => ZExp.CursorE(side, e),
      ctx,
      u_gen,
      e0,
      ze0,
      surround,
    )
  | (
      Delete,
      ZExp.Deeper(
        _,
        ZExp.OpSeqZ(
          _,
          ZExp.CursorE(After, e0) as ze0,
          OperatorSeq.EmptyPrefix(_) as surround,
        ),
      ),
    )
  | (
      Delete,
      ZExp.Deeper(
        _,
        ZExp.OpSeqZ(
          _,
          ZExp.CursorE(After, e0) as ze0,
          OperatorSeq.BothNonEmpty(_, _) as surround,
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
      (side, e) => ZExp.CursorE(side, e),
      ctx,
      u_gen,
      e0,
      ze0,
      surround,
    )
  | (Backspace, ZExp.CursorE(In(_), e))
  | (Delete, ZExp.CursorE(In(_), e)) =>
    let (e', u_gen') = UHExp.new_EmptyHole(u_gen);
    let ze' = ZExp.CursorE(Before, e');
    Some((ze', HTyp.Hole, u_gen'));
  /* Construction */
  | (Construct(SParenthesized), ZExp.CursorE(cursor_side, e)) =>
    Some((ZExp.ParenthesizedZ(ze), ty, u_gen))
  | (Construct(SAsc), ZExp.CursorE(_, e)) =>
    let e' = UHExp.bidelimit(e);
    Some((
      ZExp.Deeper(
        NotInHole,
        ZExp.AscZ2(e', ZTyp.CursorT(Before, UHTyp.Hole)),
      ),
      ty,
      u_gen,
    ));
  | (
      Construct(SAsc),
      ZExp.Deeper(
        err_status,
        ZExp.LineItemZL(ZExp.DeeperL(ZExp.LetLineZP(zp, None, e1)), e2),
      ),
    ) =>
    switch (Statics.syn(ctx, e1)) {
    | None => None
    | Some(ty1) =>
      let uty1 = UHTyp.contract(ty1);
      let ze =
        ZExp.Deeper(
          err_status,
          ZExp.LineItemZL(
            ZExp.DeeperL(
              ZExp.LetLineZA(ZPat.erase(zp), ZTyp.place_Before(uty1), e1),
            ),
            e2,
          ),
        );
      Some((ze, ty, u_gen));
    }
  | (Construct(SAsc), ZExp.Deeper(err_status, ZExp.LamZP(zp, None, e1))) =>
    let ze =
      ZExp.Deeper(
        err_status,
        ZExp.LamZA(ZPat.erase(zp), ZTyp.place_Before(UHTyp.Hole), e1),
      );
    Some((ze, ty, u_gen));
  | (
      Construct(SAsc),
      ZExp.Deeper(
        err_status,
        ZExp.LineItemZL(
          ZExp.DeeperL(ZExp.LetLineZP(zp, Some(uty1), e1)),
          e2,
        ),
      ),
    ) =>
    /* just move the cursor over if there is already an ascription */
    let ze =
      ZExp.Deeper(
        err_status,
        ZExp.LineItemZL(
          ZExp.DeeperL(
            ZExp.LetLineZA(ZPat.erase(zp), ZTyp.place_Before(uty1), e1),
          ),
          e2,
        ),
      );
    Some((ze, ty, u_gen));
  | (
      Construct(SAsc),
      ZExp.Deeper(err_status, ZExp.LamZP(zp, Some(uty1), e1)),
    ) =>
    /* just move the cursor over if there is already an ascription */
    let ze =
      ZExp.Deeper(
        err_status,
        ZExp.LamZA(ZPat.erase(zp), ZTyp.place_Before(uty1), e1),
      );
    Some((ze, ty, u_gen));
  | (
      Construct(SVar(x, side)),
      ZExp.CursorE(_, UHExp.Tm(_, UHExp.EmptyHole(_))),
    )
  | (
      Construct(SVar(x, side)),
      ZExp.CursorE(_, UHExp.Tm(_, UHExp.Var(_, _))),
    )
  | (
      Construct(SVar(x, side)),
      ZExp.CursorE(_, UHExp.Tm(_, UHExp.NumLit(_))),
    )
  | (
      Construct(SVar(x, side)),
      ZExp.CursorE(_, UHExp.Tm(_, UHExp.BoolLit(_))),
    ) =>
    if (String.equal(x, "true")) {
      Some((
        ZExp.CursorE(side, UHExp.Tm(NotInHole, UHExp.BoolLit(true))),
        HTyp.Bool,
        u_gen,
      ));
    } else if (String.equal(x, "false")) {
      Some((
        ZExp.CursorE(side, UHExp.Tm(NotInHole, UHExp.BoolLit(false))),
        HTyp.Bool,
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
              ZExp.CursorE(
                side,
                UHExp.Tm(NotInHole, UHExp.Var(NotInVHole, x)),
              ),
              xty,
              u_gen,
            ))
          | None =>
            let (u, u_gen) = MetaVarGen.next(u_gen);
            Some((
              ZExp.CursorE(
                side,
                UHExp.Tm(NotInHole, UHExp.Var(InVHole(u), x)),
              ),
              HTyp.Hole,
              u_gen,
            ));
          };
        },
      );
    }
  | (Construct(SVar(_, _)), ZExp.CursorE(_, _)) => None
  | (Construct(SLine), ZExp.Deeper(_, ZExp.LineItemZL(zli, e2)))
      when ZExp.cursor_at_end_line_item(zli) =>
    let li = ZExp.erase_line_item(zli);
    let ze =
      ZExp.(
        prune_and_prepend_line(
          li,
          prepend_zline(CursorL(Before, UHExp.EmptyLine), e2),
        )
      );
    Some((ze, ty, u_gen));
  | (Construct(SLine), ze1) when ZExp.cursor_at_start(ze1) =>
    let ze = ZExp.prepend_line(UHExp.EmptyLine, ze1);
    Some((ze, ty, u_gen));
  | (Construct(SLine), ze1) when ZExp.cursor_at_end(ze1) =>
    let e1 = ZExp.erase(ze1);
    let (ze2, u_gen) = ZExp.new_EmptyHole(u_gen);
    let ze = ZExp.prune_and_prepend_lines(e1, ze2);
    zexp_syn_fix_holes(ctx, u_gen, ze);
  | (Construct(SLine), ZExp.CursorE(_, _)) => None
  | (Construct(SLet), ZExp.CursorE(_, _) as ze1)
  | (
      Construct(SLet),
      ZExp.Deeper(
        _,
        ZExp.OpSeqZ(_, ZExp.CursorE(Before, _), OperatorSeq.EmptyPrefix(_)),
      ) as ze1,
    ) =>
    let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
    let e1 = ZExp.erase(ze1);
    let (e2, u_gen) = UHExp.new_EmptyHole(u_gen);
    let ze =
      ZExp.Deeper(
        NotInHole,
        ZExp.LineItemZL(ZExp.DeeperL(ZExp.LetLineZP(zp, None, e1)), e2),
      );
    Some((ze, ty, u_gen));
  | (
      Construct(SLet),
      ZExp.Deeper(_, ZExp.LineItemZL(ZExp.CursorL(_, UHExp.EmptyLine), e2)),
    ) =>
    let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
    let (e1, u_gen) = UHExp.new_EmptyHole(u_gen);
    let ze =
      ZExp.Deeper(
        NotInHole,
        ZExp.LineItemZL(ZExp.DeeperL(ZExp.LetLineZP(zp, None, e1)), e2),
      );
    Some((ze, ty, u_gen));
  | (
      Construct(SLet),
      ZExp.Deeper(_, ZExp.LineItemZL(ZExp.DeeperL(ZExp.ExpLineZ(ze1)), e2)),
    )
      when ZExp.cursor_at_start(ze1) =>
    let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
    let e1 = ZExp.erase(ze1);
    let ze =
      ZExp.Deeper(
        NotInHole,
        ZExp.LineItemZL(ZExp.DeeperL(ZExp.LetLineZP(zp, None, e1)), e2),
      );
    Some((ze, ty, u_gen));
  | (
      Construct(_) as a,
      ZExp.Deeper(_, ZExp.LineItemZL(ZExp.CursorL(_, UHExp.EmptyLine), e2)),
    ) =>
    let (e1, u_gen) = UHExp.new_EmptyHole(u_gen);
    let ze1 = ZExp.CursorE(Before, e1);
    switch (perform_syn(ctx, a, (ze1, HTyp.Hole, u_gen))) {
    | None => None
    | Some((ze1, _, u_gen)) =>
      let ze = ZExp.(prune_and_prepend_zline(DeeperL(ExpLineZ(ze1)), e2));
      Some((ze, ty, u_gen));
    };
  | (
      Construct(_) as a,
      ZExp.Deeper(
        _,
        ZExp.LineItemZL(ZExp.CursorL(side, UHExp.ExpLine(e1)), e2),
      ),
    ) =>
    switch (side) {
    | In(_) => None
    | Before =>
      let ze1 = ZExp.place_Before(e1);
      let ze = ZExp.(prune_and_prepend_zline(DeeperL(ExpLineZ(ze1)), e2));
      perform_syn(ctx, a, (ze, ty, u_gen));
    | After =>
      let ze1 = ZExp.place_After(e1);
      let ze = ZExp.(prune_and_prepend_zline(DeeperL(ExpLineZ(ze1)), e2));
      perform_syn(ctx, a, (ze, ty, u_gen));
    }
  | (
      Construct(_) as a,
      ZExp.Deeper(
        _,
        ZExp.LineItemZL(
          ZExp.CursorL(side, UHExp.LetLine(_, _, _) as li),
          e2,
        ),
      ),
    ) =>
    switch (Statics.syn_fix_holes(ctx, u_gen, e2)) {
    | None => None
    | Some((e2, ty, u_gen)) =>
      let (hole, u_gen) = UHExp.new_EmptyHole(u_gen);
      let ze1 = ZExp.CursorE(side, UHExp.prepend_line(li, hole));
      let ze = ZExp.(prepend_zline(DeeperL(ExpLineZ(ze1)), e2));
      perform_syn(ctx, a, (ze, ty, u_gen));
    }
  | (Construct(SLam), ZExp.CursorE(_, e1)) =>
    let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
    let ze = ZExp.Deeper(NotInHole, ZExp.LamZP(zp, Some(UHTyp.Hole), e1));
    let ty' = HTyp.Arrow(HTyp.Hole, ty);
    Some((ze, ty', u_gen));
  | (
      Construct(SNumLit(n, side)),
      ZExp.CursorE(_, UHExp.Tm(_, UHExp.EmptyHole(_))),
    )
  | (
      Construct(SNumLit(n, side)),
      ZExp.CursorE(_, UHExp.Tm(_, UHExp.NumLit(_))),
    )
  | (
      Construct(SNumLit(n, side)),
      ZExp.CursorE(_, UHExp.Tm(_, UHExp.BoolLit(_))),
    )
  | (
      Construct(SNumLit(n, side)),
      ZExp.CursorE(_, UHExp.Tm(_, UHExp.Var(_, _))),
    ) =>
    Some((
      ZExp.CursorE(side, UHExp.Tm(NotInHole, UHExp.NumLit(n))),
      HTyp.Num,
      u_gen,
    ))
  | (Construct(SNumLit(_, _)), ZExp.CursorE(_, _)) => None
  | (Construct(SInj(side)), ZExp.CursorE(_, e)) =>
    let ze' = ZExp.Deeper(NotInHole, ZExp.InjZ(side, ze));
    let ty' =
      switch (side) {
      | L => HTyp.Sum(ty, HTyp.Hole)
      | R => HTyp.Sum(HTyp.Hole, ty)
      };
    Some((ze', ty', u_gen));
  | (Construct(SListNil), ZExp.CursorE(_, UHExp.Tm(_, UHExp.EmptyHole(_)))) =>
    let ze = ZExp.CursorE(After, UHExp.Tm(NotInHole, UHExp.ListNil));
    let ty = HTyp.List(HTyp.Hole);
    Some((ze, ty, u_gen));
  | (Construct(SListNil), ZExp.CursorE(_, _)) => None
  | (Construct(SCase), ZExp.CursorE(_, e1)) =>
    switch (e1) {
    | UHExp.Tm(_, UHExp.EmptyHole(_)) =>
      let (rule_p, u_gen) = UHPat.new_EmptyHole(u_gen);
      let (rule_e, u_gen) = UHExp.new_EmptyHole(u_gen);
      let rule = UHExp.Rule(rule_p, rule_e);
      let rules = [rule];
      let caseze = ZExp.Deeper(NotInHole, ZExp.CaseZE(ze, rules));
      let ze = ZExp.Deeper(NotInHole, ZExp.AscZ1(caseze, UHTyp.Hole));
      Some((ze, HTyp.Hole, u_gen));
    | _ =>
      let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
      let (rule_e, u_gen) = UHExp.new_EmptyHole(u_gen);
      let zrule = ZExp.RuleZP(zp, rule_e);
      let zrules = ZList.singleton(zrule);
      let caseze = ZExp.Deeper(NotInHole, ZExp.CaseZR(e1, zrules));
      let ze = ZExp.Deeper(NotInHole, ZExp.AscZ1(caseze, UHTyp.Hole));
      Some((ze, HTyp.Hole, u_gen));
    }
  | (
      Construct(SOp(os)),
      ZExp.Deeper(_, ZExp.OpSeqZ(_, ZExp.CursorE(In(_), e), surround)),
    )
  | (
      Construct(SOp(os)),
      ZExp.Deeper(_, ZExp.OpSeqZ(_, ZExp.CursorE(After, e), surround)),
    ) =>
    switch (exp_op_of(os)) {
    | None => None
    | Some(op) =>
      abs_perform_Construct_SOp_After_surround(
        ZExp.new_EmptyHole,
        make_and_syn_OpSeqZ,
        UHExp.is_Space,
        UHExp.Space,
        (side, e) => ZExp.CursorE(side, e),
        ctx,
        u_gen,
        e,
        op,
        surround,
      )
    }
  | (
      Construct(SOp(os)),
      ZExp.Deeper(
        _,
        ZExp.OpSeqZ(_, ZExp.CursorE(Before, _) as ze0, surround),
      ),
    ) =>
    switch (exp_op_of(os)) {
    | None => None
    | Some(op) =>
      abs_perform_Construct_SOp_Before_surround(
        ZExp.erase,
        ZExp.new_EmptyHole,
        make_and_syn_OpSeqZ,
        UHExp.is_Space,
        UHExp.Space,
        (side, e) => ZExp.CursorE(side, e),
        ctx,
        u_gen,
        ze0,
        op,
        surround,
      )
    }
  | (Construct(SOp(os)), ZExp.CursorE(In(_), e))
  | (Construct(SOp(os)), ZExp.CursorE(After, e)) =>
    switch (exp_op_of(os)) {
    | None => None
    | Some(op) =>
      abs_perform_Construct_SOp_After(
        UHExp.bidelimit,
        ZExp.new_EmptyHole,
        make_and_syn_OpSeqZ,
        ctx,
        u_gen,
        e,
        op,
      )
    }
  | (Construct(SOp(os)), ZExp.CursorE(Before, e)) =>
    switch (exp_op_of(os)) {
    | None => None
    | Some(op) =>
      abs_perform_Construct_SOp_Before(
        UHExp.bidelimit,
        ZExp.new_EmptyHole,
        make_and_syn_OpSeqZ,
        ctx,
        u_gen,
        e,
        op,
      )
    }
  | (
      Construct(SApPalette(name)),
      ZExp.CursorE(_, UHExp.Tm(_, UHExp.EmptyHole(_))),
    ) =>
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
            ZExp.CursorE(
              Before,
              UHExp.Tm(
                NotInHole,
                UHExp.ApPalette(name, init_model, init_splice_info),
              ),
            ),
            expansion_ty,
            u_gen,
          ))
        };
      };
    };
  | (Construct(SApPalette(_)), ZExp.CursorE(_, _)) => None
  | (
      UpdateApPalette(cmd),
      ZExp.CursorE(_, UHExp.Tm(_, UHExp.ApPalette(name, _, hole_data))),
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
           ZExp.CursorE(
             After,
             UHExp.Tm(
               NotInHole,
               UHExp.ApPalette(name, serialized_model, hole_data'),
             ),
           ),
           expansion_ty,
           u_gen,
         ))
       | None => None
       };
     | None => None
     }; */
  | (UpdateApPalette(_), ZExp.CursorE(_, _))
  | (
      UpdateApPalette(_),
      ZExp.Deeper(_, ZExp.LineItemZL(ZExp.CursorL(_, _), _)),
    ) =>
    None
  /* Zipper Cases */
  | (_, ZExp.ParenthesizedZ(ze1)) =>
    switch (perform_syn(ctx, a, (ze1, ty, u_gen))) {
    | Some((ze1', ty', u_gen')) =>
      Some((ZExp.ParenthesizedZ(ze1'), ty', u_gen'))
    | None => None
    }
  | (_, ZExp.Deeper(_, ZExp.AscZ1(ze, uty1))) =>
    let ty1 = UHTyp.expand(uty1);
    switch (perform_ana(u_gen, ctx, a, ze, ty1)) {
    | Some((ze', u_gen')) =>
      let ze'' = ZExp.bidelimit(ze');
      Some((ZExp.Deeper(NotInHole, ZExp.AscZ1(ze'', uty1)), ty, u_gen'));
    | None => None
    };
  | (_, ZExp.Deeper(_, ZExp.AscZ2(e, zty))) =>
    switch (perform_ty(a, zty)) {
    | Some(zty') =>
      let uty' = ZTyp.erase(zty');
      let ty' = UHTyp.expand(uty');
      switch (Statics.ana_fix_holes(ctx, u_gen, e, ty')) {
      | None => None
      | Some((e', u_gen')) =>
        Some((ZExp.Deeper(NotInHole, ZExp.AscZ2(e', zty')), ty', u_gen'))
      };
    | None => None
    }
  | (
      _,
      ZExp.Deeper(_, ZExp.LineItemZL(ZExp.DeeperL(ZExp.ExpLineZ(ze1)), e2)),
    ) =>
    switch (Statics.syn(ctx, ZExp.erase(ze1))) {
    | None => None
    | Some(ty1) =>
      switch (perform_syn(ctx, a, (ze1, ty1, u_gen))) {
      | None => None
      | Some((ze1, _, u_gen)) =>
        let ze = ZExp.(prune_and_prepend_zline(DeeperL(ExpLineZ(ze1)), e2));
        Some((ze, ty, u_gen));
      }
    }
  | (
      _,
      ZExp.Deeper(
        _,
        ZExp.LineItemZL(ZExp.DeeperL(ZExp.LetLineZP(zp, ann, e1)), e2),
      ),
    ) =>
    switch (ann) {
    | Some(uty1) =>
      let ty1 = UHTyp.expand(uty1);
      switch (perform_ana_pat(ctx, u_gen, a, zp, ty1)) {
      | None => None
      | Some((zp, ctx2, u_gen)) =>
        let p = ZPat.erase(zp);
        let ctx1 = Statics.ctx_for_let(ctx, p, ty1, e1);
        switch (Statics.ana_fix_holes(ctx1, u_gen, e1, ty1)) {
        | None => None
        | Some((e1, u_gen)) =>
          switch (Statics.syn_fix_holes(ctx2, u_gen, e2)) {
          | None => None
          | Some((e2, ty, u_gen)) =>
            let ze =
              ZExp.(prepend_zline(DeeperL(LetLineZP(zp, ann, e1)), e2));
            Some((ze, ty, u_gen));
          }
        };
      };
    | None =>
      switch (Statics.syn(ctx, e1)) {
      | None => None
      | Some(ty1) =>
        switch (perform_ana_pat(ctx, u_gen, a, zp, ty1)) {
        | None => None
        | Some((zp, ctx2, u_gen)) =>
          switch (Statics.syn_fix_holes(ctx2, u_gen, e2)) {
          | None => None
          | Some((e2, ty, u_gen)) =>
            let ze =
              ZExp.(prepend_zline(DeeperL(LetLineZP(zp, ann, e1)), e2));
            Some((ze, ty, u_gen));
          }
        }
      }
    }
  | (
      _,
      ZExp.Deeper(
        _,
        ZExp.LineItemZL(ZExp.DeeperL(ZExp.LetLineZA(p, zann, e1)), e2),
      ),
    ) =>
    /* (ctx) let p (ctx2) : ty = (ctx1) e1 in (ctx2) e2 */
    switch (perform_ty(a, zann)) {
    | None => None
    | Some(zann) =>
      let ty1 = UHTyp.expand(ZTyp.erase(zann));
      switch (Statics.ana_pat_fix_holes(ctx, u_gen, false, p, ty1)) {
      | None => None
      | Some((p, ctx2, u_gen)) =>
        let ctx1 = Statics.ctx_for_let(ctx, p, ty1, e1);
        switch (Statics.ana_fix_holes(ctx1, u_gen, e1, ty1)) {
        | None => None
        | Some((e1, u_gen)) =>
          switch (Statics.syn_fix_holes(ctx2, u_gen, e2)) {
          | None => None
          | Some((e2, ty, u_gen)) =>
            let ze =
              ZExp.(prepend_zline(DeeperL(LetLineZA(p, zann, e1)), e2));
            Some((ze, ty, u_gen));
          }
        };
      };
    }
  | (
      _,
      ZExp.Deeper(
        _,
        ZExp.LineItemZL(ZExp.DeeperL(ZExp.LetLineZE(p, ann, ze1)), e2),
      ),
    ) =>
    switch (ann) {
    | Some(ann_ty) =>
      let ty1 = UHTyp.expand(ann_ty);
      let ctx1 = Statics.ctx_for_let(ctx, p, ty1, ZExp.erase(ze1));
      switch (perform_ana(u_gen, ctx1, a, ze1, ty1)) {
      | None => None
      | Some((ze1, u_gen)) =>
        let ze = ZExp.(prepend_zline(DeeperL(LetLineZE(p, ann, ze1)), e2));
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
          switch (Statics.ana_pat_fix_holes(ctx, u_gen, false, p, ty1)) {
          | None => None
          | Some((p, ctx2, u_gen)) =>
            switch (Statics.syn_fix_holes(ctx2, u_gen, e2)) {
            | None => None
            | Some((e2, ty, u_gen)) =>
              let ze =
                ZExp.(prepend_zline(DeeperL(LetLineZE(p, ann, ze1)), e2));
              Some((ze, ty, u_gen));
            }
          }
        }
      };
    }
  | (_, ZExp.Deeper(_, ZExp.LineItemZE(UHExp.EmptyLine as li, ze2)))
  | (_, ZExp.Deeper(_, ZExp.LineItemZE(UHExp.ExpLine(_) as li, ze2))) =>
    switch (perform_syn(ctx, a, (ze2, ty, u_gen))) {
    | None => None
    | Some((ze2, ty, u_gen)) =>
      let ze = ZExp.prepend_line(li, ze2);
      Some((ze, ty, u_gen));
    }
  | (_, ZExp.Deeper(_, ZExp.LineItemZE(UHExp.LetLine(p, ann, e1), ze2))) =>
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
          let ze = ZExp.prepend_line(UHExp.LetLine(p, ann, e1), ze2);
          Some((ze, ty, u_gen));
        }
      }
    };
  | (_, ZExp.Deeper(_, ZExp.LamZP(zp, ann, e1))) =>
    let ty1 =
      switch (ann) {
      | Some(uty1) => UHTyp.expand(uty1)
      | None => HTyp.Hole
      };
    switch (perform_ana_pat(ctx, u_gen, a, zp, ty1)) {
    | None => None
    | Some((zp, ctx, u_gen)) =>
      switch (Statics.syn_fix_holes(ctx, u_gen, e1)) {
      | None => None
      | Some((e1, ty2, u_gen)) =>
        let ty = HTyp.Arrow(ty1, ty2);
        let ze = ZExp.Deeper(NotInHole, ZExp.LamZP(zp, ann, e1));
        Some((ze, ty, u_gen));
      }
    };
  | (_, ZExp.Deeper(_, ZExp.LamZA(p, zann, e1))) =>
    switch (perform_ty(a, zann)) {
    | None => None
    | Some(zann) =>
      let ty1 = UHTyp.expand(ZTyp.erase(zann));
      switch (Statics.ana_pat_fix_holes(ctx, u_gen, false, p, ty1)) {
      | None => None
      | Some((p, ctx, u_gen)) =>
        switch (Statics.syn_fix_holes(ctx, u_gen, e1)) {
        | None => None
        | Some((e1, ty2, u_gen)) =>
          let ze = ZExp.Deeper(NotInHole, ZExp.LamZA(p, zann, e1));
          Some((ze, HTyp.Arrow(ty1, ty2), u_gen));
        }
      };
    }
  | (_, ZExp.Deeper(_, ZExp.LamZE(p, ann, ze1))) =>
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
          let ze = ZExp.Deeper(NotInHole, ZExp.LamZE(p, ann, ze1));
          Some((ze, HTyp.Arrow(ty1, ty2), u_gen));
        }
      };
    }
  | (_, ZExp.Deeper(_, ZExp.InjZ(side, ze1))) =>
    switch (ty) {
    | HTyp.Sum(ty1, ty2) =>
      let ty_side = pick_side(side, ty1, ty2);
      switch (perform_syn(ctx, a, (ze1, ty_side, u_gen))) {
      | None => None
      | Some((ze1', ty_side', u_gen')) =>
        let ty' =
          switch (side) {
          | L => HTyp.Sum(ty_side', ty2)
          | R => HTyp.Sum(ty1, ty_side')
          };
        Some((ZExp.Deeper(NotInHole, ZExp.InjZ(side, ze1')), ty', u_gen'));
      };
    | _ => None /* should never happen */
    }
  | (_, ZExp.Deeper(err, ZExp.OpSeqZ(_, ze0, surround))) =>
    let i = OperatorSeq.surround_prefix_length(surround);
    switch (ZExp.erase(ze)) {
    | UHExp.Tm(_, UHExp.OpSeq(skel, seq)) =>
      switch (Statics.syn_skel(ctx, skel, seq, Some(i))) {
      | Some((ty, Some(mode))) =>
        switch (mode) {
        | Statics.AnalyzedAgainst(ty0) =>
          switch (perform_ana(u_gen, ctx, a, ze0, ty0)) {
          | None => None
          | Some((ze0', u_gen)) =>
            let ze0'' = ZExp.bidelimit(ze0');
            Some((
              ZExp.Deeper(err, ZExp.OpSeqZ(skel, ze0'', surround)),
              ty,
              u_gen,
            ));
          }
        | Statics.Synthesized(ty0) =>
          switch (perform_syn(ctx, a, (ze0, ty0, u_gen))) {
          | None => None
          | Some((ze0', ty0', u_gen)) =>
            let ze0'' = ZExp.bidelimit(ze0');
            make_and_syn_OpSeqZ(ctx, u_gen, ze0'', surround);
          }
        }
      | Some(_) => None /* should never happen */
      | None => None /* should never happen */
      }
    | _ => None /* should never happen */
    };
  | (
      _,
      ZExp.Deeper(_, ZExp.ApPaletteZ(name, serialized_model, z_hole_data)),
    ) =>
    None
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
         ZExp.Deeper(
           NotInHole,
           ZExp.ApPaletteZ(name, serialized_model, z_hole_data'),
         ),
         ty,
         u_gen',
       ));
     }; */
  | (_, ZExp.Deeper(_, ZExp.CaseZE(_, _))) => None
  | (_, ZExp.Deeper(_, ZExp.CaseZR(_, _))) => None
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
  | (_, ZExp.Deeper(InHole(TypeInconsistent, u) as err, ze1')) =>
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
  | (Backspace, ZExp.CursorE(After, e)) =>
    switch (e) {
    | UHExp.Tm(_, UHExp.EmptyHole(_)) =>
      Some((ZExp.CursorE(Before, e), u_gen))
    | _ =>
      let (e', u_gen) = UHExp.new_EmptyHole(u_gen);
      Some((ZExp.CursorE(Before, e'), u_gen));
    }
  | (Backspace, ZExp.CursorE(Before, e)) => None
  | (Delete, ZExp.CursorE(Before, e)) =>
    switch (e) {
    | UHExp.Tm(_, UHExp.EmptyHole(_)) =>
      Some((ZExp.CursorE(After, e), u_gen))
    | _ =>
      let (e', u_gen) = UHExp.new_EmptyHole(u_gen);
      Some((ZExp.CursorE(Before, e'), u_gen));
    }
  | (Delete, ZExp.CursorE(After, e)) => None
  | (Backspace, ZExp.CursorE(In(_), e))
  | (Delete, ZExp.CursorE(In(_), e)) =>
    let (e', u_gen) = UHExp.new_EmptyHole(u_gen);
    let ze' = ZExp.CursorE(Before, e');
    Some((ze', u_gen));
  | (Backspace, ZExp.Deeper(_, ZExp.AscZ2(e1, ZTyp.CursorT(Before, uty1)))) =>
    let ze' = ZExp.CursorE(After, e1);
    zexp_ana_fix_holes(ctx, u_gen, ze', ty);
  | (
      Backspace,
      ZExp.Deeper(
        _,
        ZExp.AscZ2(
          e1,
          ZTyp.OpSeqZ(
            _,
            ZTyp.CursorT(Before, _),
            OperatorSeq.EmptyPrefix(_),
          ),
        ),
      ),
    ) =>
    let ze' = ZExp.CursorE(After, e1);
    zexp_ana_fix_holes(ctx, u_gen, ze', ty);
  | (Delete, ZExp.Deeper(_, ZExp.AscZ1(ZExp.CursorE(After, e1), _))) =>
    switch (Statics.ana_fix_holes(ctx, u_gen, e1, ty)) {
    | Some((e1', u_gen)) =>
      let ze' = ZExp.CursorE(After, e1');
      Some((ze', u_gen));
    | None => None
    }
  | (
      Backspace,
      ZExp.Deeper(
        err_status,
        ZExp.LineItemZE(
          li,
          ZExp.Deeper(
            _,
            ZExp.LineItemZL(ZExp.CursorL(_, UHExp.EmptyLine), e2),
          ),
        ),
      ),
    ) =>
    let zli = ZExp.place_After_line_item(li);
    let ze = ZExp.prepend_zline(~err_status, zli, e2);
    Some((ze, u_gen));
  | (
      Delete,
      ZExp.Deeper(_, ZExp.LineItemZL(ZExp.CursorL(_, UHExp.EmptyLine), e1)),
    ) =>
    let ze = ZExp.place_Before(e1);
    Some((ze, u_gen));
  | (
      Backspace,
      ZExp.Deeper(_, ZExp.LineItemZL(ZExp.CursorL(Before, _), e2)),
    ) =>
    None
  | (
      Backspace,
      ZExp.Deeper(err_status, ZExp.LineItemZL(ZExp.CursorL(After, _), e2)),
    )
  | (
      Backspace,
      ZExp.Deeper(err_status, ZExp.LineItemZL(ZExp.CursorL(In(_), _), e2)),
    ) =>
    switch (Statics.ana_fix_holes(ctx, u_gen, e2, ty)) {
    | None => None
    | Some((e2, u_gen)) =>
      let ze =
        ZExp.(
          prepend_zline(~err_status, CursorL(After, UHExp.EmptyLine), e2)
        );
      Some((ze, u_gen));
    }
  | (Delete, ZExp.Deeper(_, ZExp.LineItemZL(ZExp.CursorL(After, _), e2))) =>
    None
  | (
      Delete,
      ZExp.Deeper(err_status, ZExp.LineItemZL(ZExp.CursorL(Before, _), e2)),
    )
  | (
      Delete,
      ZExp.Deeper(err_status, ZExp.LineItemZL(ZExp.CursorL(In(_), _), e2)),
    ) =>
    switch (Statics.ana_fix_holes(ctx, u_gen, e2, ty)) {
    | None => None
    | Some((e2, u_gen)) =>
      let ze =
        ZExp.(
          prepend_zline(~err_status, CursorL(Before, UHExp.EmptyLine), e2)
        );
      Some((ze, u_gen));
    }
  | (Backspace, ZExp.Deeper(_, ZExp.LineItemZE(UHExp.EmptyLine, ze1)))
      when ZExp.cursor_at_start(ze1) =>
    Some((ze1, u_gen))
  | (
      Delete,
      ZExp.Deeper(
        err_status,
        ZExp.LineItemZL(
          zli,
          UHExp.Tm(_, UHExp.LineItem(UHExp.EmptyLine, e2)),
        ),
      ),
    )
      when ZExp.cursor_at_end_line_item(zli) =>
    let ze = ZExp.prepend_zline(~err_status, zli, e2);
    Some((ze, u_gen));
  | (
      Backspace,
      ZExp.Deeper(
        _,
        ZExp.LineItemZE(
          UHExp.ExpLine(e1),
          ZExp.CursorE(Before, UHExp.Tm(_, UHExp.EmptyHole(_))),
        ),
      ),
    ) =>
    switch (Statics.ana_fix_holes(ctx, u_gen, e1, ty)) {
    | None => None
    | Some((e, u_gen)) =>
      let ze = ZExp.place_After(e);
      Some((ze, u_gen));
    }
  | (
      Delete,
      ZExp.Deeper(
        _,
        ZExp.LineItemZL(
          ZExp.DeeperL(ZExp.ExpLineZ(ze1)),
          UHExp.Tm(_, UHExp.EmptyHole(_)),
        ),
      ),
    )
      when ZExp.cursor_at_end(ze1) =>
    zexp_ana_fix_holes(ctx, u_gen, ze1, ty)
  | (
      Backspace,
      ZExp.Deeper(
        _,
        ZExp.LineItemZL(ZExp.DeeperL(ZExp.LetLineZA(p, zty, e1)), e2),
      ),
    )
      when ZTyp.cursor_at_start(zty) =>
    switch (Statics.syn_fix_holes(ctx, u_gen, e1)) {
    | None => None
    | Some((e1, ty1, u_gen)) =>
      switch (Statics.ana_pat_fix_holes(ctx, u_gen, false, p, ty1)) {
      | None => None
      | Some((p, ctx, u_gen)) =>
        switch (Statics.ana_fix_holes(ctx, u_gen, e2, ty)) {
        | None => None
        | Some((e2, u_gen)) =>
          let ze =
            ZExp.(
              prepend_zline(
                DeeperL(LetLineZP(ZPat.place_After(p), None, e1)),
                e2,
              )
            );
          Some((ze, u_gen));
        }
      }
    }
  | (
      Delete,
      ZExp.Deeper(
        _,
        ZExp.LineItemZL(ZExp.DeeperL(ZExp.LetLineZP(zp, Some(_), e1)), e2),
      ),
    )
      when ZPat.cursor_at_end(zp) =>
    switch (Statics.syn_fix_holes(ctx, u_gen, e1)) {
    | None => None
    | Some((e1, ty1, u_gen)) =>
      switch (ana_zpat_fix_holes(ctx, u_gen, zp, ty1)) {
      | None => None
      | Some((zp, ctx, u_gen)) =>
        switch (Statics.ana_fix_holes(ctx, u_gen, e2, ty)) {
        | None => None
        | Some((e2, u_gen)) =>
          let ze =
            ZExp.(prepend_zline(DeeperL(LetLineZP(zp, None, e1)), e2));
          Some((ze, u_gen));
        }
      }
    }
  | (Backspace, ZExp.Deeper(_, ZExp.LamZA(p, zty, e1)))
      when ZTyp.cursor_at_start(zty) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => None
    | Some((ty1, ty2)) =>
      switch (Statics.ana_pat_fix_holes(ctx, u_gen, false, p, ty1)) {
      | None => None
      | Some((p, ctx, u_gen)) =>
        switch (Statics.ana_fix_holes(ctx, u_gen, e1, ty2)) {
        | None => None
        | Some((e1, u_gen)) =>
          let zp = ZPat.place_After(p);
          let ze = ZExp.Deeper(NotInHole, ZExp.LamZP(zp, None, e1));
          Some((ze, u_gen));
        }
      }
    }
  | (Delete, ZExp.Deeper(_, ZExp.LamZP(zp, Some(_), e1)))
      when ZPat.cursor_at_end(zp) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => None
    | Some((ty1, ty2)) =>
      switch (ana_zpat_fix_holes(ctx, u_gen, zp, ty1)) {
      | None => None
      | Some((zp, ctx, u_gen)) =>
        switch (Statics.ana_fix_holes(ctx, u_gen, e1, ty2)) {
        | None => None
        | Some((e1, u_gen)) =>
          let ze = ZExp.Deeper(NotInHole, ZExp.LamZP(zp, None, e1));
          Some((ze, u_gen));
        }
      }
    }
  | (
      Backspace,
      ZExp.Deeper(
        _,
        ZExp.CaseZR(e1, (prefix, ZExp.RuleZP(zp, _), suffix)),
      ),
    )
      when ZPat.cursor_at_start(zp) =>
    switch (suffix) {
    | [] =>
      switch (prefix) {
      | [] =>
        let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
        let ze =
          ZExp.Deeper(NotInHole, ZExp.CaseZR(e1, (prefix, zrule, suffix)));
        Some((ze, u_gen));
      | [_, ..._] =>
        switch (List.rev(prefix)) {
        | [] => None
        | [UHExp.Rule(p2, e2), ...rev_prefix'] =>
          let prefix' = List.rev(rev_prefix');
          let zrule = ZExp.RuleZP(ZPat.place_Before(p2), e2);
          let ze =
            ZExp.Deeper(
              NotInHole,
              ZExp.CaseZR(e1, (prefix', zrule, suffix)),
            );
          Some((ze, u_gen));
        }
      }
    | [UHExp.Rule(p2, e2), ...suffix'] =>
      let zrule = ZExp.RuleZP(ZPat.place_Before(p2), e2);
      let ze =
        ZExp.Deeper(NotInHole, ZExp.CaseZR(e1, (prefix, zrule, suffix')));
      Some((ze, u_gen));
    }
  | (
      Backspace,
      ZExp.Deeper(
        _,
        ZExp.OpSeqZ(
          _,
          ZExp.CursorE(Before, e0) as ze0,
          OperatorSeq.EmptySuffix(_) as surround,
        ),
      ),
    )
  | (
      Backspace,
      ZExp.Deeper(
        _,
        ZExp.OpSeqZ(
          _,
          ZExp.CursorE(Before, e0) as ze0,
          OperatorSeq.BothNonEmpty(_, _) as surround,
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
      (side, e) => ZExp.CursorE(side, e),
      ctx,
      u_gen,
      e0,
      ze0,
      surround,
    )
  | (
      Delete,
      ZExp.Deeper(
        _,
        ZExp.OpSeqZ(
          _,
          ZExp.CursorE(After, e0) as ze0,
          OperatorSeq.EmptyPrefix(_) as surround,
        ),
      ),
    )
  | (
      Delete,
      ZExp.Deeper(
        _,
        ZExp.OpSeqZ(
          _,
          ZExp.CursorE(After, e0) as ze0,
          OperatorSeq.BothNonEmpty(_, _) as surround,
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
      (side, e) => ZExp.CursorE(side, e),
      ctx,
      u_gen,
      e0,
      ze0,
      surround,
    )
  /* Construction */
  | (Construct(SParenthesized), ZExp.CursorE(_, e)) =>
    Some((ZExp.ParenthesizedZ(ze), u_gen))
  | (Construct(SAsc), ZExp.CursorE(_, e)) =>
    let e' = UHExp.bidelimit(e);
    let uty = UHTyp.contract(ty);
    Some((
      ZExp.Deeper(NotInHole, ZExp.AscZ2(e', ZTyp.place_Before(uty))),
      u_gen,
    ));
  | (
      Construct(SAsc),
      ZExp.Deeper(
        err_status,
        ZExp.LineItemZL(ZExp.DeeperL(ZExp.LetLineZP(zp, None, e1)), e2),
      ),
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
              LetLineZA(ZPat.erase(zp), ZTyp.place_Before(uty1), e1),
            ),
            e2,
          )
        );
      Some((ze, u_gen));
    }
  | (Construct(SAsc), ZExp.Deeper(err_status, ZExp.LamZP(zp, None, e1))) =>
    let ze =
      ZExp.Deeper(
        err_status,
        ZExp.LamZA(ZPat.erase(zp), ZTyp.CursorT(Before, UHTyp.Hole), e1),
      );
    Some((ze, u_gen));
  | (
      Construct(SAsc),
      ZExp.Deeper(
        err_status,
        ZExp.LineItemZL(
          ZExp.DeeperL(ZExp.LetLineZP(zp, Some(uty1), e1)),
          e2,
        ),
      ),
    ) =>
    /* just move the cursor over if there is already an ascription */
    let ze =
      ZExp.(
        prepend_zline(
          ~err_status,
          DeeperL(LetLineZA(ZPat.erase(zp), ZTyp.place_Before(uty1), e1)),
          e2,
        )
      );
    Some((ze, u_gen));
  | (
      Construct(SAsc),
      ZExp.Deeper(err_status, ZExp.LamZP(zp, Some(uty1), e1)),
    ) =>
    /* just move the cursor over if there is already an ascription */
    let ze =
      ZExp.Deeper(
        err_status,
        ZExp.LamZA(ZPat.erase(zp), ZTyp.place_Before(uty1), e1),
      );
    Some((ze, u_gen));
  | (
      Construct(SLine),
      ZExp.Deeper(
        _,
        ZExp.CaseZR(
          e1,
          (prefix, ZExp.RuleZP(ZPat.CursorP(Before, p), re), suffix),
        ),
      ),
    ) =>
    let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
    let prev_rule = UHExp.Rule(p, re);
    let suffix = [prev_rule, ...suffix];
    let ze =
      ZExp.Deeper(NotInHole, ZExp.CaseZR(e1, (prefix, zrule, suffix)));
    Some((ze, u_gen));
  | (
      Construct(SLine),
      ZExp.Deeper(
        _,
        ZExp.CaseZR(e1, (prefix, ZExp.RuleZE(_, ze) as zrule, suffix)),
      ),
    )
      when ZExp.cursor_at_end(ze) =>
    let prev_rule = ZExp.erase_rule(zrule);
    let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
    let prefix = prefix @ [prev_rule];
    let ze =
      ZExp.Deeper(NotInHole, ZExp.CaseZR(e1, (prefix, zrule, suffix)));
    Some((ze, u_gen));
  | (
      Construct(SLine),
      ZExp.Deeper(
        _,
        ZExp.CaseZR(e1, (prefix, ZExp.RuleZP(zp, _) as zrule, suffix)),
      ),
    )
      when ZPat.cursor_at_end(zp) =>
    let prev_rule = ZExp.erase_rule(zrule);
    let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
    let prefix = prefix @ [prev_rule];
    let ze =
      ZExp.Deeper(NotInHole, ZExp.CaseZR(e1, (prefix, zrule, suffix)));
    Some((ze, u_gen));
  | (Construct(SLine), ZExp.Deeper(err_status, ZExp.LineItemZL(zli, e2)))
      when ZExp.cursor_at_end_line_item(zli) =>
    let li = ZExp.erase_line_item(zli);
    let ze =
      ZExp.(
        prepend_line(
          ~err_status,
          li,
          prepend_zline(CursorL(Before, UHExp.EmptyLine), e2),
        )
      );
    Some((ze, u_gen));
  | (Construct(SLine), ze1) when ZExp.cursor_at_start(ze1) =>
    let ze = ZExp.prepend_line(UHExp.EmptyLine, ze1);
    Some((ze, u_gen));
  | (Construct(SLine), ze1) when ZExp.cursor_at_end(ze1) =>
    let e1 = ZExp.erase(ze1);
    let (ze2, u_gen) = ZExp.new_EmptyHole(u_gen);
    let ze = ZExp.prune_and_prepend_lines(e1, ze2);
    zexp_ana_fix_holes(ctx, u_gen, ze, ty);
  | (Construct(SLet), ZExp.CursorE(_, _) as ze1)
  | (
      Construct(SLet),
      ZExp.Deeper(
        _,
        ZExp.OpSeqZ(_, ZExp.CursorE(Before, _), OperatorSeq.EmptyPrefix(_)),
      ) as ze1,
    ) =>
    let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
    let e1 = ZExp.erase(ze1);
    let (e2, u_gen) = UHExp.new_EmptyHole(u_gen);
    switch (Statics.syn_fix_holes(ctx, u_gen, e1)) {
    | Some((e1, ty1, u_gen)) =>
      let ze = ZExp.(prepend_zline(DeeperL(LetLineZP(zp, None, e1)), e2));
      Some((ze, u_gen));
    | None =>
      let ann = Some(UHTyp.contract(ty));
      let ze = ZExp.(prepend_zline(DeeperL(LetLineZP(zp, ann, e1)), e2));
      Some((ze, u_gen));
    };
  | (
      Construct(SLet),
      ZExp.Deeper(
        err_status,
        ZExp.LineItemZL(ZExp.DeeperL(ZExp.ExpLineZ(ze1)), e2),
      ),
    )
      when ZExp.cursor_at_start(ze1) =>
    let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
    let e1 = ZExp.erase(ze1);
    let ze =
      ZExp.(
        prepend_zline(~err_status, DeeperL(LetLineZP(zp, None, e1)), e2)
      );
    Some((ze, u_gen));
  | (
      Construct(_) as a,
      ZExp.Deeper(
        err_status,
        ZExp.LineItemZL(ZExp.CursorL(side, UHExp.ExpLine(e1)), e2),
      ),
    ) =>
    switch (side) {
    | In(_) => None
    | Before =>
      let ze1 = ZExp.place_Before(e1);
      let ze =
        ZExp.(
          prune_and_prepend_zline(~err_status, DeeperL(ExpLineZ(ze1)), e2)
        );
      perform_ana(u_gen, ctx, a, ze, ty);
    | After =>
      let ze1 = ZExp.place_After(e1);
      let ze =
        ZExp.(
          prune_and_prepend_zline(~err_status, DeeperL(ExpLineZ(ze1)), e2)
        );
      perform_ana(u_gen, ctx, a, ze, ty);
    }
  | (
      Construct(_) as a,
      ZExp.Deeper(
        _,
        ZExp.LineItemZL(
          ZExp.CursorL(side, UHExp.LetLine(_, _, _) as li),
          e2,
        ),
      ),
    ) =>
    switch (Statics.ana_fix_holes(ctx, u_gen, e2, ty)) {
    | None => None
    | Some((e2, u_gen)) =>
      let (hole, u_gen) = UHExp.new_EmptyHole(u_gen);
      let ze1 = ZExp.CursorE(side, UHExp.prepend_line(li, hole));
      let ze = ZExp.(prepend_zline(DeeperL(ExpLineZ(ze1)), e2));
      perform_ana(u_gen, ctx, a, ze, ty);
    }
  | (Construct(SLam), ZExp.CursorE(_, e)) =>
    switch (HTyp.matched_arrow(ty)) {
    | Some((_, ty2)) =>
      switch (Statics.ana_fix_holes(ctx, u_gen, e, ty2)) {
      | None => None
      | Some((e, u_gen)) =>
        let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
        let ze = ZExp.Deeper(NotInHole, ZExp.LamZP(zp, None, e));
        Some((ze, u_gen));
      }
    | None =>
      switch (Statics.syn_fix_holes(ctx, u_gen, e)) {
      | Some((e, _, u_gen)) =>
        let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
        let (u, u_gen) = MetaVarGen.next(u_gen);
        let ze =
          ZExp.Deeper(InHole(TypeInconsistent, u), ZExp.LamZP(zp, None, e));
        Some((ze, u_gen));
      | None =>
        let e = UHExp.Tm(NotInHole, UHExp.Asc(e, UHTyp.contract(ty)));
        let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
        let (u, u_gen) = MetaVarGen.next(u_gen);
        let ze =
          ZExp.Deeper(InHole(TypeInconsistent, u), ZExp.LamZP(zp, None, e));
        Some((ze, u_gen));
      }
    }
  | (Construct(SInj(side)), ZExp.CursorE(cursor_side, e1)) =>
    switch (HTyp.matched_sum(ty)) {
    | Some((tyL, tyR)) =>
      let ty1 = pick_side(side, tyL, tyR);
      switch (Statics.ana_fix_holes(ctx, u_gen, e1, ty1)) {
      | None => None
      | Some((e1, u_gen)) =>
        let ze =
          ZExp.Deeper(
            NotInHole,
            ZExp.InjZ(side, ZExp.CursorE(cursor_side, e1)),
          );
        Some((ze, u_gen));
      };
    | None =>
      switch (Statics.syn_fix_holes(ctx, u_gen, e1)) {
      | Some((e1, _, u_gen)) =>
        let (u, u_gen) = MetaVarGen.next(u_gen);
        let ze =
          ZExp.Deeper(
            InHole(TypeInconsistent, u),
            ZExp.InjZ(side, ZExp.CursorE(cursor_side, e1)),
          );
        Some((ze, u_gen));
      | None =>
        let e1 = UHExp.Tm(NotInHole, UHExp.Asc(e1, UHTyp.contract(ty)));
        let (u, u_gen) = MetaVarGen.next(u_gen);
        let ze =
          ZExp.Deeper(
            InHole(TypeInconsistent, u),
            ZExp.InjZ(side, ZExp.CursorE(cursor_side, e1)),
          );
        Some((ze, u_gen));
      }
    }
  | (Construct(SCase), ZExp.CursorE(_, e1)) =>
    switch (e1) {
    | UHExp.Tm(_, UHExp.EmptyHole(_)) =>
      let (rule, u_gen) = UHExp.empty_rule(u_gen);
      let rules = [rule];
      let ze = ZExp.Deeper(NotInHole, ZExp.CaseZE(ze, rules));
      Some((ze, u_gen));
    | _ =>
      let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
      let zrules = ZList.singleton(zrule);
      switch (Statics.syn_fix_holes(ctx, u_gen, e1)) {
      | Some((e1, _, u_gen)) =>
        let ze = ZExp.Deeper(NotInHole, ZExp.CaseZR(e1, zrules));
        Some((ze, u_gen));
      | None =>
        let e1 = UHExp.Tm(NotInHole, UHExp.Asc(e1, UHTyp.contract(ty)));
        let ze = ZExp.Deeper(NotInHole, ZExp.CaseZR(e1, zrules));
        Some((ze, u_gen));
      };
    }
  | (
      Construct(SOp(os)),
      ZExp.Deeper(_, ZExp.OpSeqZ(_, ZExp.CursorE(In(_), e), surround)),
    )
  | (
      Construct(SOp(os)),
      ZExp.Deeper(_, ZExp.OpSeqZ(_, ZExp.CursorE(After, e), surround)),
    ) =>
    switch (exp_op_of(os)) {
    | None => None
    | Some(op) =>
      abs_perform_Construct_SOp_After_surround(
        ZExp.new_EmptyHole,
        (ctx, u_gen, ze, surround) =>
          make_and_ana_OpSeqZ(ctx, u_gen, ze, surround, ty),
        UHExp.is_Space,
        UHExp.Space,
        (side, e) => ZExp.CursorE(side, e),
        ctx,
        u_gen,
        e,
        op,
        surround,
      )
    }
  | (
      Construct(SOp(os)),
      ZExp.Deeper(
        _,
        ZExp.OpSeqZ(_, ZExp.CursorE(Before, _) as ze0, surround),
      ),
    ) =>
    switch (exp_op_of(os)) {
    | None => None
    | Some(op) =>
      abs_perform_Construct_SOp_Before_surround(
        ZExp.erase,
        ZExp.new_EmptyHole,
        (ctx, u_gen, ze, surround) =>
          make_and_ana_OpSeqZ(ctx, u_gen, ze, surround, ty),
        UHExp.is_Space,
        UHExp.Space,
        (side, e) => ZExp.CursorE(side, e),
        ctx,
        u_gen,
        ze0,
        op,
        surround,
      )
    }
  | (Construct(SOp(os)), ZExp.CursorE(In(_), e))
  | (Construct(SOp(os)), ZExp.CursorE(After, e)) =>
    switch (exp_op_of(os)) {
    | None => None
    | Some(op) =>
      abs_perform_Construct_SOp_After(
        UHExp.bidelimit,
        ZExp.new_EmptyHole,
        (ctx, u_gen, ze, surround) =>
          make_and_ana_OpSeqZ(ctx, u_gen, ze, surround, ty),
        ctx,
        u_gen,
        e,
        op,
      )
    }
  | (Construct(SOp(os)), ZExp.CursorE(Before, e)) =>
    switch (exp_op_of(os)) {
    | None => None
    | Some(op) =>
      abs_perform_Construct_SOp_Before(
        UHExp.bidelimit,
        ZExp.new_EmptyHole,
        (ctx, u_gen, ze, surround) =>
          make_and_ana_OpSeqZ(ctx, u_gen, ze, surround, ty),
        ctx,
        u_gen,
        e,
        op,
      )
    }
  /* Zipper Cases */
  | (_, ZExp.ParenthesizedZ(ze1)) =>
    switch (perform_ana(u_gen, ctx, a, ze1, ty)) {
    | Some((ze1', u_gen')) => Some((ZExp.ParenthesizedZ(ze1'), u_gen'))
    | None => None
    }
  | (
      _,
      ZExp.Deeper(
        err_status,
        ZExp.LineItemZL(ZExp.CursorL(_, UHExp.EmptyLine), e2),
      ),
    ) =>
    let (ze1, u_gen) = ZExp.new_EmptyHole(u_gen);
    let ze = ZExp.(prepend_zline(~err_status, DeeperL(ExpLineZ(ze1)), e2));
    perform_ana(u_gen, ctx, a, ze, ty);
  | (
      _,
      ZExp.Deeper(
        err_status,
        ZExp.LineItemZL(ZExp.DeeperL(ZExp.ExpLineZ(ze1)), e2),
      ),
    ) =>
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
  | (
      _,
      ZExp.Deeper(
        _,
        ZExp.LineItemZL(ZExp.DeeperL(ZExp.LetLineZP(zp, ann, e1)), e2),
      ),
    ) =>
    switch (ann) {
    | Some(uty1) =>
      let ty1 = UHTyp.expand(uty1);
      switch (perform_ana_pat(ctx, u_gen, a, zp, ty1)) {
      | None => None
      | Some((zp, ctx2, u_gen)) =>
        let p = ZPat.erase(zp);
        let ctx1 = Statics.ctx_for_let(ctx, p, ty1, e1);
        switch (Statics.ana_fix_holes(ctx1, u_gen, e1, ty1)) {
        | None => None
        | Some((e1, u_gen)) =>
          switch (Statics.ana_fix_holes(ctx2, u_gen, e2, ty)) {
          | None => None
          | Some((e2, u_gen)) =>
            let ze =
              ZExp.(prepend_zline(DeeperL(LetLineZP(zp, ann, e1)), e2));
            Some((ze, u_gen));
          }
        };
      };
    | None =>
      switch (Statics.syn(ctx, e1)) {
      | None => None
      | Some(ty1) =>
        switch (perform_ana_pat(ctx, u_gen, a, zp, ty1)) {
        | None => None
        | Some((zp, ctx2, u_gen)) =>
          switch (Statics.ana_fix_holes(ctx2, u_gen, e2, ty)) {
          | None => None
          | Some((e2, u_gen)) =>
            let ze =
              ZExp.(prepend_zline(DeeperL(LetLineZP(zp, ann, e1)), e2));
            Some((ze, u_gen));
          }
        }
      }
    }
  | (
      _,
      ZExp.Deeper(
        _,
        ZExp.LineItemZL(ZExp.DeeperL(ZExp.LetLineZA(p, zann, e1)), e2),
      ),
    ) =>
    /* (ctx) let p (ctx2) : ty = (ctx1) e1 in (ctx2) e2 */
    switch (perform_ty(a, zann)) {
    | None => None
    | Some(zann) =>
      let ty1 = UHTyp.expand(ZTyp.erase(zann));
      switch (Statics.ana_pat_fix_holes(ctx, u_gen, false, p, ty1)) {
      | None => None
      | Some((p, ctx2, u_gen)) =>
        let ctx1 = Statics.ctx_for_let(ctx, p, ty1, e1);
        switch (Statics.ana_fix_holes(ctx1, u_gen, e1, ty1)) {
        | None => None
        | Some((e1, u_gen)) =>
          switch (Statics.ana_fix_holes(ctx2, u_gen, e2, ty)) {
          | None => None
          | Some((e2, u_gen)) =>
            let ze =
              ZExp.(prepend_zline(DeeperL(LetLineZA(p, zann, e1)), e2));
            Some((ze, u_gen));
          }
        };
      };
    }
  | (
      _,
      ZExp.Deeper(
        _,
        ZExp.LineItemZL(ZExp.DeeperL(ZExp.LetLineZE(p, ann, ze1)), e2),
      ),
    ) =>
    switch (ann) {
    | Some(ann_ty) =>
      let ty1 = UHTyp.expand(ann_ty);
      let ctx1 = Statics.ctx_for_let(ctx, p, ty1, ZExp.erase(ze1));
      switch (perform_ana(u_gen, ctx1, a, ze1, ty1)) {
      | None => None
      | Some((ze1, u_gen)) =>
        let ze =
          ZExp.(prepend_zline(DeeperL(ZExp.LetLineZE(p, ann, ze1)), e2));
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
          switch (Statics.ana_pat_fix_holes(ctx, u_gen, false, p, ty1)) {
          | None => None
          | Some((p, ctx2, u_gen)) =>
            switch (Statics.ana_fix_holes(ctx2, u_gen, e2, ty)) {
            | None => None
            | Some((e2, u_gen)) =>
              let ze =
                ZExp.(
                  prepend_zline(DeeperL(ZExp.LetLineZE(p, ann, ze1)), e2)
                );
              Some((ze, u_gen));
            }
          }
        }
      };
    }
  | (_, ZExp.Deeper(_, ZExp.LineItemZE(UHExp.EmptyLine as li, ze2)))
  | (_, ZExp.Deeper(_, ZExp.LineItemZE(UHExp.ExpLine(_) as li, ze2))) =>
    switch (perform_ana(u_gen, ctx, a, ze2, ty)) {
    | None => None
    | Some((ze2, u_gen)) =>
      let ze = ZExp.prepend_line(li, ze2);
      Some((ze, u_gen));
    }
  | (_, ZExp.Deeper(_, ZExp.LineItemZE(UHExp.LetLine(p, ann, e1), ze2))) =>
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
          let ze = ZExp.prepend_line(UHExp.LetLine(p, ann, e1), ze2);
          Some((ze, u_gen));
        }
      }
    };
  | (_, ZExp.Deeper(err, ZExp.LamZP(zp, ann, e1))) =>
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
        switch (Statics.ana_fix_holes(ctx, u_gen, e1, ty2)) {
        | None => None
        | Some((e1, u_gen)) =>
          let ze = ZExp.Deeper(err, ZExp.LamZP(zp, ann, e1));
          Some((ze, u_gen));
        }
      };
    }
  | (_, ZExp.Deeper(_, ZExp.LamZA(p, zann, e1))) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => None
    | Some((ty1_given, ty2)) =>
      switch (perform_ty(a, zann)) {
      | None => None
      | Some(zann) =>
        let ty1 = UHTyp.expand(ZTyp.erase(zann));
        HTyp.consistent(ty1, ty1_given)
          ? switch (Statics.ana_pat_fix_holes(ctx, u_gen, false, p, ty1)) {
            | None => None
            | Some((p, ctx, u_gen)) =>
              switch (Statics.ana_fix_holes(ctx, u_gen, e1, ty2)) {
              | None => None
              | Some((e1, u_gen)) =>
                let ze = ZExp.Deeper(NotInHole, ZExp.LamZA(p, zann, e1));
                Some((ze, u_gen));
              }
            }
          : (
            switch (Statics.ana_pat_fix_holes(ctx, u_gen, false, p, ty1)) {
            | None => None
            | Some((p, ctx, u_gen)) =>
              switch (Statics.syn_fix_holes(ctx, u_gen, e1)) {
              | None => None
              | Some((e1, _, u_gen)) =>
                let (u, u_gen) = MetaVarGen.next(u_gen);
                let ze =
                  ZExp.Deeper(
                    InHole(TypeInconsistent, u),
                    ZExp.LamZA(p, zann, e1),
                  );
                Some((ze, u_gen));
              }
            }
          );
      }
    }
  | (_, ZExp.Deeper(err, ZExp.LamZE(p, ann, ze1))) =>
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
          let ze = ZExp.Deeper(err, ZExp.LamZE(p, ann, ze1));
          Some((ze, u_gen));
        }
      };
    }
  | (_, ZExp.Deeper(err, ZExp.InjZ(side, ze))) =>
    switch (HTyp.matched_sum(ty)) {
    | Some((ty1, ty2)) =>
      let picked = pick_side(side, ty1, ty2);
      switch (perform_ana(u_gen, ctx, a, ze, picked)) {
      | Some((ze', u_gen)) =>
        Some((ZExp.Deeper(err, ZExp.InjZ(side, ze')), u_gen))
      | None => None
      };
    | None => None
    }
  | (_, ZExp.Deeper(_, ZExp.CaseZE(ze1, rules))) =>
    switch (Statics.syn(ctx, ZExp.erase(ze1))) {
    | None => None
    | Some(ty1) =>
      switch (perform_syn(ctx, a, (ze1, ty1, u_gen))) {
      | None => None
      | Some((ze1, ty1, u_gen)) =>
        switch (
          Statics.ana_rules_fix_holes(ctx, u_gen, false, rules, ty1, ty)
        ) {
        | None => None
        | Some((rules, u_gen)) =>
          let ze = ZExp.Deeper(NotInHole, ZExp.CaseZE(ze1, rules));
          Some((ze, u_gen));
        }
      }
    }
  | (_, ZExp.Deeper(_, ZExp.CaseZR(e1, zrules))) =>
    switch (Statics.syn(ctx, e1)) {
    | None => None
    | Some(ty1) =>
      switch (ZList.prj_z(zrules)) {
      | ZExp.RuleZP(zp, e) =>
        switch (perform_ana_pat(ctx, u_gen, a, zp, ty1)) {
        | None => None
        | Some((zp, ctx, u_gen)) =>
          switch (Statics.ana_fix_holes(ctx, u_gen, e, ty)) {
          | None => None
          | Some((e, u_gen)) =>
            let zrule = ZExp.RuleZP(zp, e);
            let ze =
              ZExp.Deeper(
                NotInHole,
                ZExp.CaseZR(e1, ZList.replace_z(zrules, zrule)),
              );
            Some((ze, u_gen));
          }
        }
      | ZExp.RuleZE(p, ze) =>
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
                ZExp.CaseZR(e1, ZList.replace_z(zrules, zrule)),
              );
            Some((ze, u_gen));
          }
        }
      }
    }
  | (_, ZExp.Deeper(err, ZExp.OpSeqZ(_, ze0, surround))) =>
    let i = OperatorSeq.surround_prefix_length(surround);
    switch (ZExp.erase(ze)) {
    | UHExp.Tm(_, UHExp.OpSeq(skel, seq)) =>
      switch (Statics.ana_skel(ctx, skel, seq, ty, Some(i))) {
      | Some(Some(mode)) =>
        switch (mode) {
        | Statics.AnalyzedAgainst(ty0) =>
          switch (perform_ana(u_gen, ctx, a, ze0, ty0)) {
          | None => None
          | Some((ze0', u_gen)) =>
            let ze0'' = ZExp.bidelimit(ze0');
            Some((
              ZExp.Deeper(err, ZExp.OpSeqZ(skel, ze0'', surround)),
              u_gen,
            ));
          }
        | Statics.Synthesized(ty0) =>
          switch (perform_syn(ctx, a, (ze0, ty0, u_gen))) {
          | None => None
          | Some((ze0', ty0', u_gen)) =>
            let ze0'' = ZExp.bidelimit(ze0');
            make_and_ana_OpSeqZ(ctx, u_gen, ze0'', surround, ty);
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
  | (_, ZExp.Deeper(_, ZExp.AscZ1(_, _)))
  | (_, ZExp.Deeper(_, ZExp.AscZ2(_, _)))
  | (_, ZExp.Deeper(_, ZExp.ApPaletteZ(_, _, _))) =>
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
  | Construct(SAsc) =>
    switch (ci.sort) {
    | CursorInfo.IsLineItem(_) => true
    | CursorInfo.IsExpr(_) => true
    | CursorInfo.IsPat(_) => true
    | CursorInfo.IsType => false
    }
  | Construct(SLine)
  | Construct(SLet)
  | Construct(SCase) =>
    switch (ci.sort) {
    | CursorInfo.IsLineItem(_) => true
    | CursorInfo.IsExpr(_) => true
    | CursorInfo.IsPat(_) => false
    | CursorInfo.IsType => false
    }
  | Construct(SInj(_)) =>
    switch (ci.sort) {
    | CursorInfo.IsLineItem(_) => true
    | CursorInfo.IsExpr(_) => true
    | CursorInfo.IsPat(_) => true
    | CursorInfo.IsType => false
    }
  | Construct(SListNil) =>
    switch (ci.sort) {
    | CursorInfo.IsLineItem(UHExp.EmptyLine) => true
    | CursorInfo.IsLineItem(UHExp.ExpLine(UHExp.Tm(_, UHExp.EmptyHole(_)))) =>
      true
    | CursorInfo.IsLineItem(_) => false
    | CursorInfo.IsExpr(UHExp.Tm(_, UHExp.EmptyHole(_))) => true
    | CursorInfo.IsExpr(_) => false
    | CursorInfo.IsPat(UHPat.Pat(_, UHPat.EmptyHole(_))) => true
    | CursorInfo.IsPat(_) => false
    | CursorInfo.IsType => false
    }
  | Construct(SOp(SArrow))
  | Construct(SOp(SVBar))
  | Construct(SList) =>
    switch (ci.sort) {
    | CursorInfo.IsType => true
    | CursorInfo.IsLineItem(_)
    | CursorInfo.IsExpr(_)
    | CursorInfo.IsPat(_) => false
    }
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
  | CursorInfo.IsLineItem(UHExp.EmptyLine)
  | CursorInfo.IsLineItem(UHExp.ExpLine(UHExp.Tm(_, UHExp.EmptyHole(_))))
  | CursorInfo.IsExpr(UHExp.Tm(_, UHExp.Var(_, _)))
  | CursorInfo.IsExpr(UHExp.Tm(_, UHExp.EmptyHole(_)))
  | CursorInfo.IsExpr(UHExp.Tm(_, UHExp.BoolLit(_)))
  | CursorInfo.IsPat(UHPat.Pat(_, UHPat.Var(_)))
  | CursorInfo.IsPat(UHPat.Pat(_, UHPat.EmptyHole(_)))
  | CursorInfo.IsPat(UHPat.Pat(_, UHPat.BoolLit(_))) => true
  | CursorInfo.IsExpr(UHExp.Tm(_, UHExp.NumLit(_)))
  | CursorInfo.IsPat(UHPat.Pat(_, UHPat.NumLit(_))) =>
    switch (ci.side) {
    | Before => true
    | In(_)
    | After => false
    }
  | CursorInfo.IsLineItem(_)
  | CursorInfo.IsExpr(_)
  | CursorInfo.IsPat(_)
  | CursorInfo.IsType => false
  };

let can_enter_numeral = (ci: CursorInfo.t): bool =>
  switch (ci.sort) {
  | CursorInfo.IsLineItem(UHExp.EmptyLine)
  | CursorInfo.IsLineItem(UHExp.ExpLine(UHExp.Tm(_, UHExp.EmptyHole(_))))
  | CursorInfo.IsExpr(UHExp.Tm(_, UHExp.NumLit(_)))
  | CursorInfo.IsExpr(UHExp.Tm(_, UHExp.EmptyHole(_)))
  | CursorInfo.IsPat(UHPat.Pat(_, UHPat.NumLit(_)))
  | CursorInfo.IsPat(UHPat.Pat(_, UHPat.EmptyHole(_))) => true
  | CursorInfo.IsLineItem(_)
  | CursorInfo.IsExpr(_)
  | CursorInfo.IsPat(_)
  | CursorInfo.IsType => false
  };

let can_construct_palette = (ci: CursorInfo.t): bool =>
  switch (ci.sort) {
  | CursorInfo.IsLineItem(UHExp.EmptyLine)
  | CursorInfo.IsLineItem(UHExp.ExpLine(UHExp.Tm(_, UHExp.EmptyHole(_))))
  | CursorInfo.IsExpr(UHExp.Tm(_, UHExp.EmptyHole(_))) => true
  | _ => false
  };
