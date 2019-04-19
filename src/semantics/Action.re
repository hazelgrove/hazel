let _TEST_PERFORM = false;
open SemanticsCommon;
open GeneralUtil;

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
  | SVar(Var.t, outer_cursor)
  | SLam
  | SNumLit(int, outer_cursor)
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

type result('success) =
  | Succeeded('success)
  | CursorEscaped(side)
  | Failed;

let make_ty_OpSeqZ = (zty0: ZTyp.t, surround: ZTyp.opseq_surround): ZTyp.t => {
  let uty0 = ZTyp.erase(zty0);
  let seq = OperatorSeq.opseq_of_exp_and_surround(uty0, surround);
  let skel = Associator.associate_ty(seq);
  OpSeqZ(skel, zty0, surround);
};

let move_to_prev_node_pos_typ = (zty: ZTyp.t): result(ZTyp.t) =>
  switch (Path.steps_of_prev_node_pos_ty(zty)) {
  | None => CursorEscaped(Before)
  | Some(steps) =>
    switch (Path.follow_ty_and_place_after(steps, ZTyp.erase(zty))) {
    | None => Failed
    | Some(zty) => Succeeded(zty)
    }
  };

let move_to_next_node_pos_typ = (zty: ZTyp.t): result(ZTyp.t) =>
  switch (Path.steps_of_next_node_pos_ty(zty)) {
  | None => CursorEscaped(After)
  | Some(steps) =>
    switch (Path.follow_ty_and_place_before(steps, ZTyp.erase(zty))) {
    | None => Failed
    | Some(zty) => Succeeded(zty)
    }
  };

let rec perform_ty = (a: t, zty: ZTyp.t): result(ZTyp.t) =>
  switch (a, zty) {
  /* Movement */
  | (MoveTo(path), _) =>
    switch (Path.follow_ty(path, ZTyp.erase(zty))) {
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
  /* Backspace and Delete */
  | (Backspace, _) when ZTyp.is_before(zty) => CursorEscaped(Before)
  | (Delete, _) when ZTyp.is_after(zty) => CursorEscaped(After)
  | (
      Backspace,
      CursorTO(Char(_) as outer_cursor, (Hole | Unit | Num | Bool) as utyo),
    )
      when
        !ZTyp.is_before(zty)
        && ZTyp.is_valid_outer_cursor(outer_cursor, utyo) =>
    Succeeded(ZTyp.place_after(TO(Hole)))
  | (
      Delete,
      CursorTO(Char(_) as outer_cursor, (Hole | Unit | Num | Bool) as utyo),
    )
      when
        !ZTyp.is_after(zty) && ZTyp.is_valid_outer_cursor(outer_cursor, utyo) =>
    Succeeded(ZTyp.place_before(TO(Hole)))
  | (Backspace | Delete, CursorTO(_, _)) => Failed
  /* (<| _ )  ==>  |_ */
  | (
      Backspace,
      CursorTI(BeforeChild(0, After), Parenthesized(uty1) | List(uty1)),
    )
  /* |>( _ )  ==>  |_ */
  | (
      Delete,
      CursorTI(BeforeChild(0, Before), Parenthesized(uty1) | List(uty1)),
    ) =>
    Succeeded(ZTyp.place_before(uty1))
  /* ( _ )<|  ==>  _| */
  | (
      Backspace,
      CursorTI(ClosingDelimiter(After), Parenthesized(uty1) | List(uty1)),
    )
  /* ( _ |>)  ==>  _| */
  | (
      Delete,
      CursorTI(ClosingDelimiter(Before), Parenthesized(uty1) | List(uty1)),
    ) =>
    Succeeded(ZTyp.place_after(uty1))
  /* <|( _ )  ==>  |( _ ) */
  | (
      Backspace,
      CursorTI(BeforeChild(0, Before), Parenthesized(_) | List(_)),
    ) =>
    CursorEscaped(Before)
  /* ( _ )|>  ==>  ( _ )| */
  | (Delete, CursorTI(ClosingDelimiter(After), Parenthesized(_) | List(_))) =>
    CursorEscaped(After)
  /* ( _ <|)   ==>   ( _| ) */
  | (Backspace, CursorTI(ClosingDelimiter(Before), Parenthesized(uty1))) =>
    Succeeded(ParenthesizedZ(ZTyp.place_after(uty1)))
  | (Backspace, CursorTI(ClosingDelimiter(Before), List(uty1))) =>
    Succeeded(ListZ(ZTyp.place_after(uty1)))
  /* (|> _ )   ==>   ( |_ ) */
  | (Delete, CursorTI(BeforeChild(0, After), Parenthesized(uty1))) =>
    Succeeded(ParenthesizedZ(ZTyp.place_before(uty1)))
  | (Delete, CursorTI(BeforeChild(0, After), List(uty1))) =>
    Succeeded(ListZ(ZTyp.place_before(uty1)))
  /* invalid cursor position */
  | (
      Backspace | Delete,
      CursorTI(BeforeChild(_, _), Parenthesized(_) | List(_)),
    ) =>
    Failed
  /* ... + [k-2] + [k-1] +<| [k] + ...   ==>   ... + [k-2] +| [k] + ... */
  | (
      Backspace,
      CursorTI(
        BeforeChild(k, After) as inner_cursor,
        OpSeq(_, seq) as utyi,
      ),
    )
      when ZTyp.is_valid_inner_cursor(inner_cursor, utyi) =>
    switch (OperatorSeq.split(k - 1, seq)) {
    | None => Failed /* invalid cursor position */
    | Some((_, surround)) =>
      switch (surround) {
      | EmptySuffix(_) => Failed /* should never happen */
      | EmptyPrefix(suffix) =>
        switch (suffix) {
        | ExpSuffix(_, ty) => Succeeded(ZTyp.place_before(ty))
        | SeqSuffix(_, seq) =>
          let skel = Associator.associate_ty(seq);
          Succeeded(ZTyp.place_before(TI(OpSeq(skel, seq))));
        }
      | BothNonEmpty(prefix, suffix) =>
        let seq =
          switch (suffix) {
          | ExpSuffix(_, ty) =>
            OperatorSeq.opseq_of_prefix_and_exp(prefix, ty)
          | SeqSuffix(_, seq) =>
            OperatorSeq.opseq_of_prefix_and_seq(prefix, seq)
          };
        let skel = Associator.associate_ty(seq);
        Succeeded(CursorTI(BeforeChild(k - 1, After), OpSeq(skel, seq)));
      }
    }
  /* ... + [k-1] |>+ [k] + [k+1] + ...   ==>   ... + [k-1] |+ [k+1] + ... */
  | (
      Delete,
      CursorTI(
        BeforeChild(k, Before) as inner_cursor,
        OpSeq(_, seq) as utyi,
      ),
    )
      when ZTyp.is_valid_inner_cursor(inner_cursor, utyi) =>
    switch (OperatorSeq.split(k, seq)) {
    | None => Failed /* should never happen */
    | Some((_, surround)) =>
      switch (surround) {
      | EmptyPrefix(_) => Failed /* should never happen */
      | EmptySuffix(prefix) =>
        switch (prefix) {
        | ExpPrefix(ty, _) => Succeeded(ZTyp.place_after(ty))
        | SeqPrefix(seq, _) =>
          let skel = Associator.associate_ty(seq);
          Succeeded(ZTyp.place_after(TI(OpSeq(skel, seq))));
        }
      | BothNonEmpty(prefix, suffix) =>
        let seq =
          switch (prefix) {
          | ExpPrefix(ty, _) =>
            OperatorSeq.opseq_of_exp_and_suffix(ty, suffix)
          | SeqPrefix(seq, _) =>
            OperatorSeq.opseq_of_seq_and_suffix(seq, suffix)
          };
        let skel = Associator.associate_ty(seq);
        Succeeded(CursorTI(BeforeChild(k, Before), OpSeq(skel, seq)));
      }
    }
  /* ... + [k-2] + [k-1] <|+ [k] + ...   ==>   ... + [k-2] + [k-1]| + [k] + ... */
  | (
      Backspace,
      CursorTI(
        BeforeChild(k, Before) as inner_cursor,
        OpSeq(skel, seq) as utyi,
      ),
    )
      when ZTyp.is_valid_inner_cursor(inner_cursor, utyi) =>
    switch (OperatorSeq.split(k - 1, seq)) {
    | None => Failed
    | Some((uty0, surround)) =>
      Succeeded(OpSeqZ(skel, ZTyp.place_after(uty0), surround))
    }
  /* ... + [k-1] +|> [k] + [k+1] + ...   ==>   ... + [k-1] + |[k] + [k+1] + ... */
  | (
      Delete,
      CursorTI(
        BeforeChild(k, After) as inner_cursor,
        OpSeq(skel, seq) as utyi,
      ),
    )
      when ZTyp.is_valid_inner_cursor(inner_cursor, utyi) =>
    switch (OperatorSeq.split(k, seq)) {
    | None => Failed
    | Some((uty0, surround)) =>
      Succeeded(OpSeqZ(skel, ZTyp.place_after(uty0), surround))
    }
  /* invalid cursor position */
  | (
      Backspace | Delete,
      CursorTI(BeforeChild(_, _) | ClosingDelimiter(_), OpSeq(_, _)),
    ) =>
    Failed
  /* Construction */
  | (Construct(SParenthesized), CursorTO(_, _) | CursorTI(_, _)) =>
    Succeeded(ParenthesizedZ(zty))
  | (Construct(SNum), CursorTO(_, Hole)) =>
    Succeeded(ZTyp.place_after(TO(Num)))
  | (Construct(SNum), CursorTO(_, _) | CursorTI(_, _)) => Failed
  | (Construct(SBool), CursorTO(_, Hole)) =>
    Succeeded(ZTyp.place_after(TO(Bool)))
  | (Construct(SBool), CursorTO(_, _) | CursorTI(_, _)) => Failed
  | (Construct(SList), CursorTO(_, _) | CursorTI(_, _)) =>
    Succeeded(ListZ(zty))
  | (Construct(SOp(os)), CursorTO(_, _) | CursorTI(_, _)) =>
    let uty = ZTyp.erase(zty);
    if (ZTyp.is_before(zty)) {
      switch (ty_op_of(os)) {
      | None => Failed
      | Some(op) =>
        let surround = OperatorSeq.EmptyPrefix(ExpSuffix(op, uty));
        let zty0 = ZTyp.place_before(TO(Hole));
        Succeeded(make_ty_OpSeqZ(zty0, surround));
      };
    } else {
      switch (ty_op_of(os)) {
      | None => Failed
      | Some(op) =>
        let surround = OperatorSeq.EmptySuffix(ExpPrefix(uty, op));
        let zty0 = ZTyp.place_before(TO(Hole));
        Succeeded(make_ty_OpSeqZ(zty0, surround));
      };
    };
  | (
      Construct(SOp(os)),
      OpSeqZ(_, (CursorTO(_, _) | CursorTI(_, _)) as zty, surround),
    ) =>
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
          let zty0' = ZTyp.place_before(TO(Hole));
          Succeeded(make_ty_OpSeqZ(zty0', surround'));
        | EmptySuffix(prefix) =>
          /* prefix |zty0 -> prefix |_ op uty0 */
          let suffix' = OperatorSeq.ExpSuffix(op, uty);
          let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
          let zty0' = ZTyp.place_before(TO(Hole));
          Succeeded(make_ty_OpSeqZ(zty0', surround'));
        | BothNonEmpty(prefix, suffix) =>
          /* prefix |zty0 suffix -> prefix |_ op uty0 suffix */
          let suffix' = OperatorSeq.suffix_prepend_exp(suffix, op, uty);
          let surround' = OperatorSeq.BothNonEmpty(prefix, suffix');
          let zty0' = ZTyp.place_before(TO(Hole));
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
          let zty0' = ZTyp.place_before(TO(Hole));
          Succeeded(make_ty_OpSeqZ(zty0', surround'));
        | EmptySuffix(prefix) =>
          /* prefix zty0| -> prefix uty0 op |_ */
          let prefix' = OperatorSeq.prefix_append_exp(prefix, uty, op);
          let surround' = OperatorSeq.EmptySuffix(prefix');
          let zty0' = ZTyp.place_before(TO(Hole));
          Succeeded(make_ty_OpSeqZ(zty0', surround'));
        | BothNonEmpty(prefix, suffix) =>
          /* prefix zty0| suffix -> prefix uty0 op |_ suffix */
          let prefix' = OperatorSeq.prefix_append_exp(prefix, uty, op);
          let surround' = OperatorSeq.BothNonEmpty(prefix', suffix);
          let zty0' = ZTyp.place_before(TO(Hole));
          Succeeded(make_ty_OpSeqZ(zty0', surround'));
        }
      };
    };
  /* Zipper Cases */
  | (a, ParenthesizedZ(zty1)) =>
    switch (perform_ty(a, zty1)) {
    | Failed => Failed
    | CursorEscaped(Before) => move_to_prev_node_pos_typ(zty)
    | CursorEscaped(After) => move_to_next_node_pos_typ(zty)
    | Succeeded(zty1') => Succeeded(ParenthesizedZ(zty1'))
    }
  | (a, ListZ(zty1)) =>
    switch (perform_ty(a, zty1)) {
    | Failed => Failed
    | CursorEscaped(Before) => move_to_prev_node_pos_typ(zty)
    | CursorEscaped(After) => move_to_next_node_pos_typ(zty)
    | Succeeded(zty1) => Succeeded(ListZ(zty1))
    }
  | (a, OpSeqZ(skel, zty0, surround)) =>
    switch (perform_ty(a, zty0)) {
    | Failed => Failed
    | CursorEscaped(Before) => move_to_prev_node_pos_typ(zty)
    | CursorEscaped(After) => move_to_next_node_pos_typ(zty)
    | Succeeded(zty0') => Succeeded(OpSeqZ(skel, zty0', surround))
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
              let ze0 = place_before(e');
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
              let ze0 = place_before(e0');
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
              let ze0 = place_before(e');
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
              let ze0' = place_before(e0');
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
  let p = UHPat.PI(OpSeq(skel, seq));
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
  let p = UHPat.PI(OpSeq(skel, seq));
  let zp = Path.follow_pat_or_fail(path0, p);
  (zp, ctx, u_gen);
};

let check_valid = (x: Var.t, result: result('a)): result('a) =>
  if (Var.is_valid(x)) {
    result;
  } else {
    Failed;
  };

let move_to_prev_node_pos_pat =
    (zp: ZPat.t, result: ZPat.t => result('a)): result('a) =>
  switch (Path.steps_of_prev_node_pos_pat(zp)) {
  | None => CursorEscaped(Before)
  | Some(steps) =>
    switch (Path.follow_pat_and_place_after(steps, ZPat.erase(zp))) {
    | None => Failed
    | Some(zp) => result(zp)
    }
  };

let move_to_next_node_pos_pat =
    (zp: ZPat.t, result: ZPat.t => result('a)): result('a) =>
  switch (Path.steps_of_next_node_pos_pat(zp)) {
  | None => CursorEscaped(After)
  | Some(steps) =>
    switch (Path.follow_pat_and_place_before(steps, ZPat.erase(zp))) {
    | None => Failed
    | Some(zp) => result(zp)
    }
  };

let rec syn_perform_pat =
        (ctx: Contexts.t, u_gen: MetaVarGen.t, a: t, zp: ZPat.t)
        : result((ZPat.t, HTyp.t, Contexts.t, MetaVarGen.t)) =>
  switch (a, zp) {
  /* Movement */
  /* NOTE: we don't need to handle movement actions here for the purposes of the UI,
   * since it's handled at the top (expression) level, but for the sake of API completeness
   * we include it */
  | (MoveTo(path), _) =>
    let p = ZPat.erase(zp);
    switch (Statics.syn_pat(ctx, p)) {
    | None => Failed
    | Some((ty, _)) =>
      switch (Path.follow_pat(path, p)) {
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
  /* Backspace and Delete */
  | (Backspace, _) when ZPat.is_before(zp) => CursorEscaped(Before)
  | (Delete, _) when ZPat.is_after(zp) => CursorEscaped(After)
  | (
      Backspace,
      CursorPO(
        Char(_) as outer_cursor,
        (
          EmptyHole(_) | Var(_, _, _) | Wild(_) | NumLit(_, _) |
          BoolLit(_, _) |
          ListNil(_)
        ) as po,
      ),
    )
      when
        !ZPat.is_before(zp) && ZPat.is_valid_outer_cursor(outer_cursor, po) =>
    let (zp, u_gen) =
      switch (po) {
      | EmptyHole(_) => (ZPat.place_before(PO(po)), u_gen)
      | _ => ZPat.new_EmptyHole(u_gen)
      };
    Succeeded((zp, Hole, ctx, u_gen));
  | (
      Delete,
      CursorPO(
        Char(_) as outer_cursor,
        (
          EmptyHole(_) | Var(_, _, _) | Wild(_) | NumLit(_, _) |
          BoolLit(_, _) |
          ListNil(_)
        ) as po,
      ),
    )
      when !ZPat.is_after(zp) && ZPat.is_valid_outer_cursor(outer_cursor, po) =>
    let (zp, u_gen) =
      switch (po) {
      | EmptyHole(_) => (ZPat.place_before(PO(po)), u_gen)
      | _ => ZPat.new_EmptyHole(u_gen)
      };
    Succeeded((zp, Hole, ctx, u_gen));
  | (Backspace | Delete, CursorPO(_, _)) => Failed
  /* ... + [k-1] <|+ [k] + ...   ==>   ... + [k-1]| + [k] + ... */
  | (
      Backspace,
      CursorPI(
        (BeforeChild(_, Before) | ClosingDelimiter(Before)) as inner_cursor,
        pi,
      ),
    )
      when
        ZPat.is_valid_inner_cursor(inner_cursor, pi) && !ZPat.is_before(zp) =>
    move_to_prev_node_pos_pat(zp, zp =>
      switch (Statics.syn_pat(ctx, ZPat.erase(zp))) {
      | None => Failed
      | Some((ty, ctx)) => Succeeded((zp, ty, ctx, u_gen))
      }
    )
  /* ... + [k-1] +|> [k] + ...   ==>   ... + [k-1] + |[k] + ... */
  | (Delete, CursorPI(BeforeChild(_, After) as inner_cursor, pi))
      when ZPat.is_valid_inner_cursor(inner_cursor, pi) && !ZPat.is_after(zp) =>
    move_to_next_node_pos_pat(zp, zp =>
      switch (Statics.syn_pat(ctx, ZPat.erase(zp))) {
      | None => Failed
      | Some((ty, ctx)) => Succeeded((zp, ty, ctx, u_gen))
      }
    )
  | (
      Backspace,
      CursorPI(
        (BeforeChild(_, After) | ClosingDelimiter(After)) as inner_cursor,
        (Parenthesized(_) | Inj(_)) as pi,
      ),
    )
      when ZPat.is_valid_inner_cursor(inner_cursor, pi) =>
    switch (ZPat.split_pat_children_across_cursor(inner_cursor, pi)) {
    /* invalid cursor */
    | None => Failed
    /* inner nodes have children */
    | Some(([], [])) => Failed
    /* ( _ )<|   ==>   _| */
    | Some(([p0, ...rev_prefix], suffix)) =>
      let zp =
        PatUtil.mk_Space_separated_zpat(
          List.rev(rev_prefix),
          ZPat.place_after(p0),
          suffix,
        );
      Succeeded(Statics.syn_fix_holes_zpat(ctx, u_gen, zp));
    /* (<| _ )   ==>   |_ */
    | Some(([], [p0, ...suffix])) =>
      let zp =
        PatUtil.mk_Space_separated_zpat([], ZPat.place_before(p0), suffix);
      Succeeded(Statics.syn_fix_holes_zpat(ctx, u_gen, zp));
    }
  | (
      Delete,
      CursorPI(
        (BeforeChild(_, Before) | ClosingDelimiter(After)) as inner_cursor,
        (Parenthesized(_) | Inj(_)) as pi,
      ),
    )
      when ZPat.is_valid_inner_cursor(inner_cursor, pi) =>
    switch (ZPat.split_pat_children_across_cursor(inner_cursor, pi)) {
    /* invalid cursor */
    | None => Failed
    /* inner nodes have children */
    | Some(([], [])) => Failed
    /* |>( _ )   ==>   |_ */
    | Some((rev_prefix, [p0, ...suffix])) =>
      let zp =
        PatUtil.mk_Space_separated_zpat(
          List.rev(rev_prefix),
          ZPat.place_before(p0),
          suffix,
        );
      Succeeded(Statics.syn_fix_holes_zpat(ctx, u_gen, zp));
    /* ( _ |>)   ==>   _| */
    | Some(([p0, ...rev_prefix], [])) =>
      let zp =
        PatUtil.mk_Space_separated_zpat(
          List.rev(rev_prefix),
          ZPat.place_after(p0),
          [],
        );
      Succeeded(Statics.syn_fix_holes_zpat(ctx, u_gen, zp));
    }
  /* invalid cursor positions */
  | (
      Backspace | Delete,
      CursorPI(
        BeforeChild(_, _) | ClosingDelimiter(_),
        Parenthesized(_) | Inj(_, _, _),
      ),
    ) =>
    Failed
  | (Backspace, OpSeqZ(_, CursorPO(_, EmptyHole(_)) as zp0, surround))
      when ZPat.opseqz_preceded_by_Space(zp0, surround) =>
    switch (surround) {
    /* should never happen given guard */
    | EmptyPrefix(_) => Failed
    /* ... + [k-1]   _<|   ==>   ... + [k-1]| */
    | EmptySuffix(prefix) =>
      let p =
        switch (prefix) {
        | ExpPrefix(p1, _space) => p1
        | SeqPrefix(seq, _space) => UHPat.PI(PatUtil.mk_OpSeq(seq))
        };
      Succeeded(
        Statics.syn_fix_holes_zpat(ctx, u_gen, ZPat.place_after(p)),
      );
    /* ... + [k-1]   _<| + [k+1] + ...   ==>   ... + [k-1]| + [k+1] + ... */
    | BothNonEmpty(prefix, suffix) =>
      switch (prefix) {
      | ExpPrefix(p1, _space) =>
        let zp1 = ZPat.place_after(p1);
        let zp = PatUtil.mk_OpSeqZ(zp1, EmptyPrefix(suffix));
        Succeeded(Statics.syn_fix_holes_zpat(ctx, u_gen, zp));
      | SeqPrefix(seq, _space) =>
        let (prefix: ZPat.opseq_prefix, p0) =
          switch (seq) {
          | ExpOpExp(p1, op, p2) => (ExpPrefix(p1, op), p2)
          | SeqOpExp(seq, op, p1) => (SeqPrefix(seq, op), p1)
          };
        let zp0 = ZPat.place_after(p0);
        let zp = PatUtil.mk_OpSeqZ(zp0, BothNonEmpty(prefix, suffix));
        Succeeded(Statics.syn_fix_holes_zpat(ctx, u_gen, zp));
      }
    }
  | (Delete, OpSeqZ(_, CursorPO(_, EmptyHole(_)) as zp0, surround))
      when ZPat.opseqz_followed_by_Space(zp0, surround) =>
    switch (surround) {
    /* should never happen given guard */
    | EmptySuffix(_) => Failed
    /* |>_   [1] + ...   ==>   |[1] + ... */
    | EmptyPrefix(suffix) =>
      let p =
        switch (suffix) {
        | ExpSuffix(_space, p1) => p1
        | SeqSuffix(_space, seq) => UHPat.PI(PatUtil.mk_OpSeq(seq))
        };
      Succeeded(
        Statics.syn_fix_holes_zpat(ctx, u_gen, ZPat.place_before(p)),
      );
    /* ... + [k-1] + |>_   [k+1] + ...   ==>   ... + [k-1] + |[k+1] + ... */
    | BothNonEmpty(prefix, suffix) =>
      switch (suffix) {
      | ExpSuffix(_space, p1) =>
        let zp1 = ZPat.place_before(p1);
        let zp = PatUtil.mk_OpSeqZ(zp1, EmptySuffix(prefix));
        Succeeded(Statics.syn_fix_holes_zpat(ctx, u_gen, zp));
      | SeqSuffix(_space, seq) =>
        let (p0, suffix: ZPat.opseq_suffix) =
          switch (seq) {
          | ExpOpExp(p1, op, p2) => (p1, ExpSuffix(op, p2))
          | SeqOpExp(seq, op, p1) => (p1, SeqSuffix(op, seq))
          };
        let zp0 = ZPat.place_before(p0);
        let zp = PatUtil.mk_OpSeqZ(zp0, BothNonEmpty(prefix, suffix));
        Succeeded(Statics.syn_fix_holes_zpat(ctx, u_gen, zp));
      }
    }
  | (Backspace, OpSeqZ(_, CursorPO(_, EmptyHole(_)) as zp0, surround))
      when ZPat.opseqz_followed_by_Space(zp0, surround) =>
    switch (surround) {
    /* should never happen given guard */
    | EmptySuffix(_) => Failed
    /* _<|   [1] + ...   ==>   |[1] + ... */
    | EmptyPrefix(suffix) =>
      let p =
        switch (suffix) {
        | ExpSuffix(_space, p1) => p1
        | SeqSuffix(_space, seq) => UHPat.PI(PatUtil.mk_OpSeq(seq))
        };
      Succeeded(
        Statics.syn_fix_holes_zpat(ctx, u_gen, ZPat.place_before(p)),
      );
    /* ... + [k-1] + _<|   [k+1] + ...   ==>   ... + [k-1] +| [k+1] + ... */
    | BothNonEmpty(prefix, suffix) =>
      let seq =
        switch (suffix) {
        | ExpSuffix(_space, p1) =>
          OperatorSeq.opseq_of_prefix_and_exp(prefix, p1)
        | SeqSuffix(_space, seq) =>
          OperatorSeq.opseq_of_prefix_and_seq(prefix, seq)
        };
      let pi = PatUtil.mk_OpSeq(seq);
      let k = OperatorSeq.prefix_length(prefix);
      let zp = ZPat.CursorPI(BeforeChild(k, After), pi);
      Succeeded(Statics.syn_fix_holes_zpat(ctx, u_gen, zp));
    }
  | (Delete, OpSeqZ(_, CursorPO(_, EmptyHole(_)) as zp0, surround))
      when ZPat.opseqz_preceded_by_Space(zp0, surround) =>
    switch (surround) {
    /* should never happen given guard */
    | EmptyPrefix(_) => Failed
    /* ... + [k-1]   |>_   ==>   ... + [k-1]| */
    | EmptySuffix(prefix) =>
      let p =
        switch (prefix) {
        | ExpPrefix(p1, _space) => p1
        | SeqPrefix(seq, _space) => UHPat.PI(PatUtil.mk_OpSeq(seq))
        };
      Succeeded(
        Statics.syn_fix_holes_zpat(ctx, u_gen, ZPat.place_after(p)),
      );
    /* ... + [k-1]   |>_ + [k+1] + ...   ==>   ... + [k-1] |+ [k+1] + ... */
    | BothNonEmpty(prefix, suffix) =>
      let seq =
        switch (prefix) {
        | ExpPrefix(p1, _space) =>
          OperatorSeq.opseq_of_exp_and_suffix(p1, suffix)
        | SeqPrefix(seq, _space) =>
          OperatorSeq.opseq_of_seq_and_suffix(seq, suffix)
        };
      let pi = PatUtil.mk_OpSeq(seq);
      let k = OperatorSeq.prefix_length(prefix);
      let zp = ZPat.CursorPI(BeforeChild(k, After), pi);
      Succeeded(Statics.syn_fix_holes_zpat(ctx, u_gen, zp));
    }
  /* ... + [k-1] +<| [k] + ...   ==>   ... + [k-1]| [k] */
  | (
      Backspace,
      CursorPI(BeforeChild(k, After) as inner_cursor, OpSeq(_, seq) as pi),
    )
      when ZPat.is_valid_inner_cursor(inner_cursor, pi) =>
    switch (OperatorSeq.split(k - 1, seq)) {
    | None => Failed /* should never happen */
    | Some((p0, surround)) =>
      switch (OperatorSeq.replace_following_op(surround, UHPat.Space)) {
      | None => Failed /* should never happen */
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
  /* ... + [k-1] |>+ [k] + ...   ==>   ... + [k-1] |[k] */
  | (
      Delete,
      CursorPI(BeforeChild(k, Before) as inner_cursor, OpSeq(_, seq) as pi),
    )
      when ZPat.is_valid_inner_cursor(inner_cursor, pi) =>
    switch (OperatorSeq.split(k, seq)) {
    | None => Failed /* should never happen */
    | Some((p0, surround)) =>
      switch (OperatorSeq.replace_preceding_op(surround, UHPat.Space)) {
      | None => Failed /* should never happen */
      | Some(surround) =>
        Succeeded(
          make_and_syn_OpSeqZ_pat(
            ctx,
            u_gen,
            ZPat.place_before(p0),
            surround,
          ),
        )
      }
    }
  /* invalid cursor position */
  | (
      Backspace | Delete,
      CursorPI(BeforeChild(_, _) | ClosingDelimiter(_), OpSeq(_, _)),
    ) =>
    Failed
  /* Construct */
  | (Construct(SParenthesized), CursorPO(_, _) | CursorPI(_, _)) =>
    switch (Statics.syn_pat(ctx, ZPat.erase(zp))) {
    | None => Failed
    | Some((ty, ctx)) => Succeeded((ParenthesizedZ(zp), ty, ctx, u_gen))
    }
  | (Construct(SVar(x, outer_cursor)), CursorPO(_, EmptyHole(_)))
  | (Construct(SVar(x, outer_cursor)), CursorPO(_, Wild(_)))
  | (Construct(SVar(x, outer_cursor)), CursorPO(_, Var(_, _, _)))
  | (Construct(SVar(x, outer_cursor)), CursorPO(_, NumLit(_, _)))
  | (Construct(SVar(x, outer_cursor)), CursorPO(_, BoolLit(_, _))) =>
    if (Var.is_true(x)) {
      Succeeded((
        CursorPO(outer_cursor, BoolLit(NotInHole, true)),
        Bool,
        ctx,
        u_gen,
      ));
    } else if (Var.is_false(x)) {
      Succeeded((
        CursorPO(outer_cursor, BoolLit(NotInHole, false)),
        Bool,
        ctx,
        u_gen,
      ));
    } else if (Var.is_let(x)) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      Succeeded((
        CursorPO(
          outer_cursor,
          Var(NotInHole, InVHole(Keyword(Let), u), x),
        ),
        Hole,
        ctx,
        u_gen,
      ));
    } else if (Var.is_case(x)) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      Succeeded((
        CursorPO(
          outer_cursor,
          Var(NotInHole, InVHole(Keyword(Case), u), x),
        ),
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
            ZPat.CursorPO(outer_cursor, Var(NotInHole, NotInVHole, x)),
            HTyp.Hole,
            ctx,
            u_gen,
          ));
        },
      );
    }
  | (Construct(SVar(_, _)), CursorPO(_, _) | CursorPI(_, _)) => Failed
  | (Construct(SWild), CursorPO(_, EmptyHole(_)))
  | (Construct(SWild), CursorPO(_, Wild(_)))
  | (Construct(SWild), CursorPO(_, Var(_, _, _)))
  | (Construct(SWild), CursorPO(_, NumLit(_, _)))
  | (Construct(SWild), CursorPO(_, BoolLit(_, _))) =>
    Succeeded((ZPat.place_after(PO(Wild(NotInHole))), Hole, ctx, u_gen))
  | (Construct(SWild), CursorPO(_, _) | CursorPI(_, _)) => Failed
  | (Construct(SNumLit(n, outer_cursor)), CursorPO(_, EmptyHole(_)))
  | (Construct(SNumLit(n, outer_cursor)), CursorPO(_, Wild(_)))
  | (Construct(SNumLit(n, outer_cursor)), CursorPO(_, Var(_, _, _)))
  | (Construct(SNumLit(n, outer_cursor)), CursorPO(_, NumLit(_, _)))
  | (Construct(SNumLit(n, outer_cursor)), CursorPO(_, BoolLit(_, _))) =>
    Succeeded((
      CursorPO(outer_cursor, NumLit(NotInHole, n)),
      Num,
      ctx,
      u_gen,
    ))
  | (Construct(SNumLit(_, _)), CursorPO(_, _) | CursorPI(_, _)) => Failed
  | (Construct(SInj(side)), CursorPO(_, _) | CursorPI(_, _)) =>
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
  | (Construct(SListNil), CursorPO(_, EmptyHole(_))) =>
    let zp = ZPat.place_after(PO(ListNil(NotInHole)));
    Succeeded((zp, List(Hole), ctx, u_gen));
  | (Construct(SListNil), CursorPO(_, _) | CursorPI(_, _)) => Failed
  | (Construct(SOp(os)), OpSeqZ(_, zp0, surround)) when ZPat.is_after(zp0) =>
    switch (pat_op_of(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded(
        abs_perform_Construct_SOp_After_surround(
          ZPat.new_EmptyHole,
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
          ZPat.erase,
          ZPat.new_EmptyHole,
          make_and_syn_OpSeqZ_pat,
          UHPat.is_Space,
          UHPat.Space,
          ctx,
          u_gen,
          zp0,
          op,
          surround,
        ),
      )
    }
  | (Construct(SOp(os)), _) when ZPat.is_after(zp) =>
    switch (pat_op_of(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded(
        abs_perform_Construct_SOp_After(
          UHPat.bidelimit,
          ZPat.new_EmptyHole,
          make_and_syn_OpSeqZ_pat,
          ctx,
          u_gen,
          ZPat.erase(zp),
          op,
        ),
      )
    }
  | (Construct(SOp(os)), _) when ZPat.is_before(zp) =>
    switch (pat_op_of(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded(
        abs_perform_Construct_SOp_Before(
          UHPat.bidelimit,
          ZPat.new_EmptyHole,
          make_and_syn_OpSeqZ_pat,
          ctx,
          u_gen,
          ZPat.erase(zp),
          op,
        ),
      )
    }
  | (Construct(SOp(_)), CursorPO(_, _) | CursorPI(_, _)) => Failed
  /* Zipper */
  | (_, ParenthesizedZ(zp1)) =>
    switch (syn_perform_pat(ctx, u_gen, a, zp1)) {
    | Failed => Failed
    | CursorEscaped(Before) =>
      move_to_prev_node_pos_pat(zp, zp =>
        switch (Statics.syn_pat(ctx, ZPat.erase(zp))) {
        | None => Failed
        | Some((ty, ctx)) => Succeeded((zp, ty, ctx, u_gen))
        }
      )
    | CursorEscaped(After) =>
      move_to_prev_node_pos_pat(zp, zp =>
        switch (Statics.syn_pat(ctx, ZPat.erase(zp))) {
        | None => Failed
        | Some((ty, ctx)) => Succeeded((zp, ty, ctx, u_gen))
        }
      )
    | Succeeded((zp1, ty, ctx, u_gen)) =>
      Succeeded((ParenthesizedZ(zp1), ty, ctx, u_gen))
    }
  | (_, InjZ(_, side, zp1)) =>
    switch (syn_perform_pat(ctx, u_gen, a, zp1)) {
    | Failed => Failed
    | CursorEscaped(Before) =>
      move_to_prev_node_pos_pat(zp, zp =>
        switch (Statics.syn_pat(ctx, ZPat.erase(zp))) {
        | None => Failed
        | Some((ty, ctx)) => Succeeded((zp, ty, ctx, u_gen))
        }
      )
    | CursorEscaped(After) =>
      move_to_prev_node_pos_pat(zp, zp =>
        switch (Statics.syn_pat(ctx, ZPat.erase(zp))) {
        | None => Failed
        | Some((ty, ctx)) => Succeeded((zp, ty, ctx, u_gen))
        }
      )
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
    | PI(OpSeq(skel, seq)) =>
      switch (Statics.syn_skel_pat(ctx, skel, seq, Some(i))) {
      | Some((_ty, ctx, Some(mode))) =>
        switch (mode) {
        | Statics.AnalyzedAgainst(ty0) =>
          switch (ana_perform_pat(ctx, u_gen, a, zp0, ty0)) {
          | Failed => Failed
          | CursorEscaped(Before) =>
            move_to_prev_node_pos_pat(zp, zp =>
              switch (Statics.syn_pat(ctx, ZPat.erase(zp))) {
              | None => Failed
              | Some((ty, ctx)) => Succeeded((zp, ty, ctx, u_gen))
              }
            )
          | CursorEscaped(After) =>
            move_to_prev_node_pos_pat(zp, zp =>
              switch (Statics.syn_pat(ctx, ZPat.erase(zp))) {
              | None => Failed
              | Some((ty, ctx)) => Succeeded((zp, ty, ctx, u_gen))
              }
            )
          | Succeeded((zp0, ctx, u_gen)) =>
            let zp0 = ZPat.bidelimit(zp0);
            Succeeded(make_and_syn_OpSeqZ_pat(ctx, u_gen, zp0, surround));
          }
        | Statics.Synthesized(_) =>
          switch (syn_perform_pat(ctx, u_gen, a, zp0)) {
          | Failed => Failed
          | CursorEscaped(Before) =>
            move_to_prev_node_pos_pat(zp, zp =>
              switch (Statics.syn_pat(ctx, ZPat.erase(zp))) {
              | None => Failed
              | Some((ty, ctx)) => Succeeded((zp, ty, ctx, u_gen))
              }
            )
          | CursorEscaped(After) =>
            move_to_prev_node_pos_pat(zp, zp =>
              switch (Statics.syn_pat(ctx, ZPat.erase(zp))) {
              | None => Failed
              | Some((ty, ctx)) => Succeeded((zp, ty, ctx, u_gen))
              }
            )
          | Succeeded((zp0, _ty0, ctx, u_gen)) =>
            let zp0 = ZPat.bidelimit(zp0);
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
  }
and ana_perform_pat =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, a: t, zp: ZPat.t, ty: HTyp.t)
    : result((ZPat.t, Contexts.t, MetaVarGen.t)) =>
  switch (a, zp) {
  /* switch to synthesis if in a hole */
  | (_, _) when ZPat.is_inconsistent(zp) =>
    let err = zp |> ZPat.erase |> UHPat.get_err_status_t;
    let zp_not_in_hole = ZPat.set_err_status_t(NotInHole, zp);
    let p = ZPat.erase(zp_not_in_hole);
    switch (Statics.syn_pat(ctx, p)) {
    | None => Failed
    | Some((_, _)) =>
      switch (syn_perform_pat(ctx, u_gen, a, zp_not_in_hole)) {
      | (Failed | CursorEscaped(_)) as err => err
      | Succeeded((zp1, ty', ctx, u_gen)) =>
        if (HTyp.consistent(ty, ty')) {
          Succeeded((zp1, ctx, u_gen));
        } else {
          Succeeded((ZPat.set_err_status_t(err, zp1), ctx, u_gen));
        }
      }
    };
  /* Movement */
  /* NOTE: we don't need to handle movement actions here for the purposes of the UI,
   * since it's handled at the top (expression) level, but for the sake of API completeness
   * we include it */
  | (MoveTo(path), _) =>
    let p = ZPat.erase(zp);
    switch (Path.follow_pat(path, p)) {
    | Some(zp) => Succeeded((zp, ctx, u_gen))
    | None => Failed
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
  /* Backspace and Delete */
  | (Backspace, _) when ZPat.is_before(zp) => CursorEscaped(Before)
  | (Delete, _) when ZPat.is_after(zp) => CursorEscaped(After)
  | (
      Backspace,
      CursorPO(
        Char(_) as outer_cursor,
        (
          EmptyHole(_) | Var(_, _, _) | Wild(_) | NumLit(_, _) |
          BoolLit(_, _) |
          ListNil(_)
        ) as po,
      ),
    )
      when
        !ZPat.is_before(zp) && ZPat.is_valid_outer_cursor(outer_cursor, po) =>
    let (zp, u_gen) =
      switch (po) {
      | EmptyHole(_) => (ZPat.place_before(PO(po)), u_gen)
      | _ => ZPat.new_EmptyHole(u_gen)
      };
    Succeeded((zp, ctx, u_gen));
  | (
      Delete,
      CursorPO(
        Char(_) as outer_cursor,
        (
          EmptyHole(_) | Var(_, _, _) | Wild(_) | NumLit(_, _) |
          BoolLit(_, _) |
          ListNil(_)
        ) as po,
      ),
    )
      when !ZPat.is_after(zp) && ZPat.is_valid_outer_cursor(outer_cursor, po) =>
    let (zp, u_gen) =
      switch (po) {
      | EmptyHole(_) => (ZPat.place_before(PO(po)), u_gen)
      | _ => ZPat.new_EmptyHole(u_gen)
      };
    Succeeded((zp, ctx, u_gen));
  | (Backspace | Delete, CursorPO(_, _)) => Failed
  /* ... + [k-1] <|+ [k] + ...   ==>   ... + [k-1]| + [k] + ... */
  | (
      Backspace,
      CursorPI(
        (BeforeChild(_, Before) | ClosingDelimiter(Before)) as inner_cursor,
        pi,
      ),
    )
      when
        ZPat.is_valid_inner_cursor(inner_cursor, pi) && !ZPat.is_before(zp) =>
    move_to_prev_node_pos_pat(zp, zp =>
      switch (Statics.ana_pat(ctx, PI(pi), ty)) {
      | None => Failed
      | Some(ctx) => Succeeded((zp, ctx, u_gen))
      }
    )
  /* ... + [k-1] +|> [k] + ...   ==>   ... + [k-1] + |[k] + ... */
  | (Delete, CursorPI(BeforeChild(_, After) as inner_cursor, pi))
      when ZPat.is_valid_inner_cursor(inner_cursor, pi) && !ZPat.is_after(zp) =>
    move_to_next_node_pos_pat(zp, zp =>
      switch (Statics.ana_pat(ctx, PI(pi), ty)) {
      | None => Failed
      | Some(ctx) => Succeeded((zp, ctx, u_gen))
      }
    )
  | (
      Backspace,
      CursorPI(
        (BeforeChild(_, After) | ClosingDelimiter(After)) as inner_cursor,
        (Parenthesized(_) | Inj(_)) as pi,
      ),
    )
      when ZPat.is_valid_inner_cursor(inner_cursor, pi) =>
    switch (ZPat.split_pat_children_across_cursor(inner_cursor, pi)) {
    /* invalid cursor */
    | None => Failed
    /* inner nodes have children */
    | Some(([], [])) => Failed
    /* ( _ )<|   ==>   _| */
    | Some(([p0, ...rev_prefix], suffix)) =>
      let zp =
        PatUtil.mk_Space_separated_zpat(
          List.rev(rev_prefix),
          ZPat.place_after(p0),
          suffix,
        );
      Succeeded(Statics.ana_fix_holes_zpat(ctx, u_gen, zp, ty));
    /* (<| _ )   ==>   |_ */
    | Some(([], [p0, ...suffix])) =>
      let zp =
        PatUtil.mk_Space_separated_zpat([], ZPat.place_before(p0), suffix);
      Succeeded(Statics.ana_fix_holes_zpat(ctx, u_gen, zp, ty));
    }
  | (
      Delete,
      CursorPI(
        (BeforeChild(_, Before) | ClosingDelimiter(After)) as inner_cursor,
        (Parenthesized(_) | Inj(_)) as pi,
      ),
    )
      when ZPat.is_valid_inner_cursor(inner_cursor, pi) =>
    switch (ZPat.split_pat_children_across_cursor(inner_cursor, pi)) {
    /* invalid cursor */
    | None => Failed
    /* inner nodes have children */
    | Some(([], [])) => Failed
    /* |>( _ )   ==>   |_ */
    | Some((rev_prefix, [p0, ...suffix])) =>
      let zp =
        PatUtil.mk_Space_separated_zpat(
          List.rev(rev_prefix),
          ZPat.place_before(p0),
          suffix,
        );
      Succeeded(Statics.ana_fix_holes_zpat(ctx, u_gen, zp, ty));
    /* ( _ |>)   ==>   _| */
    | Some(([p0, ...rev_prefix], [])) =>
      let zp =
        PatUtil.mk_Space_separated_zpat(
          List.rev(rev_prefix),
          ZPat.place_after(p0),
          [],
        );
      Succeeded(Statics.ana_fix_holes_zpat(ctx, u_gen, zp, ty));
    }
  /* invalid cursor positions */
  | (
      Backspace | Delete,
      CursorPI(
        BeforeChild(_, _) | ClosingDelimiter(_),
        Parenthesized(_) | Inj(_, _, _),
      ),
    ) =>
    Failed
  | (Backspace, OpSeqZ(_, CursorPO(_, EmptyHole(_)) as zp0, surround))
      when ZPat.opseqz_preceded_by_Space(zp0, surround) =>
    switch (surround) {
    /* should never happen given guard */
    | EmptyPrefix(_) => Failed
    /* ... + [k-1]   _<|   ==>   ... + [k-1]| */
    | EmptySuffix(prefix) =>
      let p =
        switch (prefix) {
        | ExpPrefix(p1, _space) => p1
        | SeqPrefix(seq, _space) => UHPat.PI(PatUtil.mk_OpSeq(seq))
        };
      Succeeded(
        Statics.ana_fix_holes_zpat(ctx, u_gen, ZPat.place_after(p), ty),
      );
    /* ... + [k-1]   _<| + [k+1] + ...   ==>   ... + [k-1]| + [k+1] + ... */
    | BothNonEmpty(prefix, suffix) =>
      switch (prefix) {
      | ExpPrefix(p1, _space) =>
        let zp1 = ZPat.place_after(p1);
        let zp = PatUtil.mk_OpSeqZ(zp1, EmptyPrefix(suffix));
        Succeeded(Statics.ana_fix_holes_zpat(ctx, u_gen, zp, ty));
      | SeqPrefix(seq, _space) =>
        let (prefix: ZPat.opseq_prefix, p0) =
          switch (seq) {
          | ExpOpExp(p1, op, p2) => (ExpPrefix(p1, op), p2)
          | SeqOpExp(seq, op, p1) => (SeqPrefix(seq, op), p1)
          };
        let zp0 = ZPat.place_after(p0);
        let zp = PatUtil.mk_OpSeqZ(zp0, BothNonEmpty(prefix, suffix));
        Succeeded(Statics.ana_fix_holes_zpat(ctx, u_gen, zp, ty));
      }
    }
  | (Delete, OpSeqZ(_, CursorPO(_, EmptyHole(_)) as zp0, surround))
      when ZPat.opseqz_followed_by_Space(zp0, surround) =>
    switch (surround) {
    /* should never happen given guard */
    | EmptySuffix(_) => Failed
    /* |>_   [1] + ...   ==>   |[1] + ... */
    | EmptyPrefix(suffix) =>
      let p =
        switch (suffix) {
        | ExpSuffix(_space, p1) => p1
        | SeqSuffix(_space, seq) => UHPat.PI(PatUtil.mk_OpSeq(seq))
        };
      Succeeded(
        Statics.ana_fix_holes_zpat(ctx, u_gen, ZPat.place_before(p), ty),
      );
    /* ... + [k-1] + |>_   [k+1] + ...   ==>   ... + [k-1] + |[k+1] + ... */
    | BothNonEmpty(prefix, suffix) =>
      switch (suffix) {
      | ExpSuffix(_space, p1) =>
        let zp1 = ZPat.place_before(p1);
        let zp = PatUtil.mk_OpSeqZ(zp1, EmptySuffix(prefix));
        Succeeded(Statics.ana_fix_holes_zpat(ctx, u_gen, zp, ty));
      | SeqSuffix(_space, seq) =>
        let (p0, suffix: ZPat.opseq_suffix) =
          switch (seq) {
          | ExpOpExp(p1, op, p2) => (p1, ExpSuffix(op, p2))
          | SeqOpExp(seq, op, p1) => (p1, SeqSuffix(op, seq))
          };
        let zp0 = ZPat.place_before(p0);
        let zp = PatUtil.mk_OpSeqZ(zp0, BothNonEmpty(prefix, suffix));
        Succeeded(Statics.ana_fix_holes_zpat(ctx, u_gen, zp, ty));
      }
    }
  | (Backspace, OpSeqZ(_, CursorPO(_, EmptyHole(_)) as zp0, surround))
      when ZPat.opseqz_followed_by_Space(zp0, surround) =>
    switch (surround) {
    /* should never happen given guard */
    | EmptySuffix(_) => Failed
    /* _<|   [1] + ...   ==>   |[1] + ... */
    | EmptyPrefix(suffix) =>
      let p =
        switch (suffix) {
        | ExpSuffix(_space, p1) => p1
        | SeqSuffix(_space, seq) => UHPat.PI(PatUtil.mk_OpSeq(seq))
        };
      Succeeded(
        Statics.ana_fix_holes_zpat(ctx, u_gen, ZPat.place_before(p), ty),
      );
    /* ... + [k-1] + _<|   [k+1] + ...   ==>   ... + [k-1] +| [k+1] + ... */
    | BothNonEmpty(prefix, suffix) =>
      let seq =
        switch (suffix) {
        | ExpSuffix(_space, p1) =>
          OperatorSeq.opseq_of_prefix_and_exp(prefix, p1)
        | SeqSuffix(_space, seq) =>
          OperatorSeq.opseq_of_prefix_and_seq(prefix, seq)
        };
      let pi = PatUtil.mk_OpSeq(seq);
      let k = OperatorSeq.prefix_length(prefix);
      let zp = ZPat.CursorPI(BeforeChild(k, After), pi);
      Succeeded(Statics.ana_fix_holes_zpat(ctx, u_gen, zp, ty));
    }
  | (Delete, OpSeqZ(_, CursorPO(_, EmptyHole(_)) as zp0, surround))
      when ZPat.opseqz_preceded_by_Space(zp0, surround) =>
    switch (surround) {
    /* should never happen given guard */
    | EmptyPrefix(_) => Failed
    /* ... + [k-1]   |>_   ==>   ... + [k-1]| */
    | EmptySuffix(prefix) =>
      let p =
        switch (prefix) {
        | ExpPrefix(p1, _space) => p1
        | SeqPrefix(seq, _space) => UHPat.PI(PatUtil.mk_OpSeq(seq))
        };
      Succeeded(
        Statics.ana_fix_holes_zpat(ctx, u_gen, ZPat.place_after(p), ty),
      );
    /* ... + [k-1]   |>_ + [k+1] + ...   ==>   ... + [k-1] |+ [k+1] + ... */
    | BothNonEmpty(prefix, suffix) =>
      let seq =
        switch (prefix) {
        | ExpPrefix(p1, _space) =>
          OperatorSeq.opseq_of_exp_and_suffix(p1, suffix)
        | SeqPrefix(seq, _space) =>
          OperatorSeq.opseq_of_seq_and_suffix(seq, suffix)
        };
      let pi = PatUtil.mk_OpSeq(seq);
      let k = OperatorSeq.prefix_length(prefix);
      let zp = ZPat.CursorPI(BeforeChild(k, After), pi);
      Succeeded(Statics.ana_fix_holes_zpat(ctx, u_gen, zp, ty));
    }
  /* ... + [k-1] +<| [k] + ...   ==>   ... + [k-1]| [k] */
  | (
      Backspace,
      CursorPI(BeforeChild(k, After) as inner_cursor, OpSeq(_, seq) as pi),
    )
      when ZPat.is_valid_inner_cursor(inner_cursor, pi) =>
    switch (OperatorSeq.split(k - 1, seq)) {
    | None => Failed /* should never happen */
    | Some((p0, surround)) =>
      switch (OperatorSeq.replace_following_op(surround, UHPat.Space)) {
      | None => Failed /* should never happen */
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
  /* ... + [k-1] |>+ [k] + ...   ==>   ... + [k-1] |[k] */
  | (
      Delete,
      CursorPI(BeforeChild(k, Before) as inner_cursor, OpSeq(_, seq) as pi),
    )
      when ZPat.is_valid_inner_cursor(inner_cursor, pi) =>
    switch (OperatorSeq.split(k, seq)) {
    | None => Failed /* should never happen */
    | Some((p0, surround)) =>
      switch (OperatorSeq.replace_preceding_op(surround, UHPat.Space)) {
      | None => Failed /* should never happen */
      | Some(surround) =>
        Succeeded(
          make_and_ana_OpSeqZ_pat(
            ctx,
            u_gen,
            ZPat.place_before(p0),
            surround,
            ty,
          ),
        )
      }
    }
  /* invalid cursor position */
  | (
      Backspace | Delete,
      CursorPI(BeforeChild(_, _) | ClosingDelimiter(_), OpSeq(_, _)),
    ) =>
    Failed
  /* Construct */
  | (Construct(SParenthesized), CursorPO(_, _) | CursorPI(_, _)) =>
    switch (Statics.ana_pat(ctx, ZPat.erase(zp), ty)) {
    | None => Failed
    | Some(ctx) => Succeeded((ParenthesizedZ(zp), ctx, u_gen))
    }
  | (Construct(SVar("true", _)), _)
  | (Construct(SVar("false", _)), _) =>
    switch (syn_perform_pat(ctx, u_gen, a, zp)) {
    | (Failed | CursorEscaped(_)) as err => err
    | Succeeded((zp, ty', ctx, u_gen)) =>
      if (HTyp.consistent(ty, ty')) {
        Succeeded((zp, ctx, u_gen));
      } else {
        let (zp, u_gen) = ZPat.make_t_inconsistent(u_gen, zp);
        Succeeded((zp, ctx, u_gen));
      }
    }
  | (Construct(SVar(x, outer_cursor)), CursorPO(_, EmptyHole(_)))
  | (Construct(SVar(x, outer_cursor)), CursorPO(_, Wild(_)))
  | (Construct(SVar(x, outer_cursor)), CursorPO(_, Var(_, _, _)))
  | (Construct(SVar(x, outer_cursor)), CursorPO(_, NumLit(_, _)))
  | (Construct(SVar(x, outer_cursor)), CursorPO(_, BoolLit(_, _))) =>
    if (Var.is_let(x)) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      Succeeded((
        CursorPO(
          outer_cursor,
          Var(NotInHole, InVHole(Keyword(Let), u), x),
        ),
        ctx,
        u_gen,
      ));
    } else if (Var.is_case(x)) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      Succeeded((
        CursorPO(
          outer_cursor,
          Var(NotInHole, InVHole(Keyword(Case), u), x),
        ),
        ctx,
        u_gen,
      ));
    } else {
      check_valid(
        x,
        {
          let ctx = Contexts.extend_gamma(ctx, (x, ty));
          Succeeded((
            ZPat.CursorPO(outer_cursor, Var(NotInHole, NotInVHole, x)),
            ctx,
            u_gen,
          ));
        },
      );
    }
  | (Construct(SVar(_, _)), CursorPO(_, _) | CursorPI(_, _)) => Failed
  | (Construct(SWild), CursorPO(_, EmptyHole(_)))
  | (Construct(SWild), CursorPO(_, Wild(_)))
  | (Construct(SWild), CursorPO(_, Var(_, _, _)))
  | (Construct(SWild), CursorPO(_, NumLit(_, _)))
  | (Construct(SWild), CursorPO(_, BoolLit(_, _))) =>
    Succeeded((ZPat.place_after(PO(Wild(NotInHole))), ctx, u_gen))
  | (Construct(SWild), CursorPO(_, _) | CursorPI(_, _)) => Failed
  | (Construct(SInj(side)), (CursorPO(_, _) | CursorPI(_, _)) as zp1) =>
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
  | (Construct(SOp(os)), OpSeqZ(_, zp0, surround)) when ZPat.is_after(zp0) =>
    switch (pat_op_of(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded(
        abs_perform_Construct_SOp_After_surround(
          ZPat.new_EmptyHole,
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
          ZPat.erase,
          ZPat.new_EmptyHole,
          (ctx, u_gen, zp, surround) =>
            make_and_ana_OpSeqZ_pat(ctx, u_gen, zp, surround, ty),
          UHPat.is_Space,
          UHPat.Space,
          ctx,
          u_gen,
          zp0,
          op,
          surround,
        ),
      )
    }
  | (Construct(SOp(os)), _) when ZPat.is_after(zp) =>
    switch (pat_op_of(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded(
        abs_perform_Construct_SOp_After(
          UHPat.bidelimit,
          ZPat.new_EmptyHole,
          (ctx, u_gen, zp, surround) =>
            make_and_ana_OpSeqZ_pat(ctx, u_gen, zp, surround, ty),
          ctx,
          u_gen,
          ZPat.erase(zp),
          op,
        ),
      )
    }
  | (Construct(SOp(os)), _) when ZPat.is_before(zp) =>
    switch (pat_op_of(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded(
        abs_perform_Construct_SOp_Before(
          UHPat.bidelimit,
          ZPat.new_EmptyHole,
          (ctx, u_gen, zp, surround) =>
            make_and_ana_OpSeqZ_pat(ctx, u_gen, zp, surround, ty),
          ctx,
          u_gen,
          ZPat.erase(zp),
          op,
        ),
      )
    }
  | (Construct(SOp(_)), CursorPO(_, _) | CursorPI(_, _)) => Failed
  /* Zipper */
  | (_, ParenthesizedZ(zp1)) =>
    switch (ana_perform_pat(ctx, u_gen, a, zp1, ty)) {
    | Failed => Failed
    | CursorEscaped(Before) =>
      move_to_prev_node_pos_pat(zp, zp =>
        switch (Statics.ana_pat(ctx, ZPat.erase(zp), ty)) {
        | None => Failed
        | Some(ctx) => Succeeded((zp, ctx, u_gen))
        }
      )
    | CursorEscaped(After) =>
      move_to_prev_node_pos_pat(zp, zp =>
        switch (Statics.ana_pat(ctx, ZPat.erase(zp), ty)) {
        | None => Failed
        | Some(ctx) => Succeeded((zp, ctx, u_gen))
        }
      )
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
      | CursorEscaped(Before) =>
        move_to_prev_node_pos_pat(zp, zp =>
          switch (Statics.ana_pat(ctx, ZPat.erase(zp), ty)) {
          | None => Failed
          | Some(ctx) => Succeeded((zp, ctx, u_gen))
          }
        )
      | CursorEscaped(After) =>
        move_to_prev_node_pos_pat(zp, zp =>
          switch (Statics.ana_pat(ctx, ZPat.erase(zp), ty)) {
          | None => Failed
          | Some(ctx) => Succeeded((zp, ctx, u_gen))
          }
        )
      | Succeeded((zp1, ctx, u_gen)) =>
        let zp = ZPat.InjZ(NotInHole, side, zp1);
        Succeeded((zp, ctx, u_gen));
      };
    }
  | (_, OpSeqZ(_, zp0, surround)) =>
    let i = OperatorSeq.surround_prefix_length(surround);
    switch (ZPat.erase(zp)) {
    | PI(OpSeq(skel, seq)) =>
      switch (Statics.ana_skel_pat(ctx, skel, seq, ty, Some(i))) {
      | Some((_, Some(mode))) =>
        switch (mode) {
        | Statics.AnalyzedAgainst(ty0) =>
          switch (ana_perform_pat(ctx, u_gen, a, zp0, ty0)) {
          | Failed => Failed
          | CursorEscaped(Before) =>
            move_to_prev_node_pos_pat(zp, zp =>
              switch (Statics.ana_pat(ctx, ZPat.erase(zp), ty)) {
              | None => Failed
              | Some(ctx) => Succeeded((zp, ctx, u_gen))
              }
            )
          | CursorEscaped(After) =>
            move_to_prev_node_pos_pat(zp, zp =>
              switch (Statics.ana_pat(ctx, ZPat.erase(zp), ty)) {
              | None => Failed
              | Some(ctx) => Succeeded((zp, ctx, u_gen))
              }
            )
          | Succeeded((zp0, _, u_gen)) =>
            let zp0 = ZPat.bidelimit(zp0);
            Succeeded(
              make_and_ana_OpSeqZ_pat(ctx, u_gen, zp0, surround, ty),
            );
          }
        | Statics.Synthesized(_) =>
          switch (syn_perform_pat(ctx, u_gen, a, zp0)) {
          | Failed => Failed
          | CursorEscaped(Before) =>
            move_to_prev_node_pos_pat(zp, zp =>
              switch (Statics.ana_pat(ctx, ZPat.erase(zp), ty)) {
              | None => Failed
              | Some(ctx) => Succeeded((zp, ctx, u_gen))
              }
            )
          | CursorEscaped(After) =>
            move_to_prev_node_pos_pat(zp, zp =>
              switch (Statics.ana_pat(ctx, ZPat.erase(zp), ty)) {
              | None => Failed
              | Some(ctx) => Succeeded((zp, ctx, u_gen))
              }
            )
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
    | (Failed | CursorEscaped(_)) as err => err
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
  let e = UHExp.EI(OpSeq(skel, seq));
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
    let e = UHExp.EI(OpSeq(skel, seq));
    let ze = Path.follow_e_or_fail(path0, e);
    (ze, u_gen);
  };
};

let combine_for_Backspace_Space = (e1: UHExp.t, ze0: ZExp.t): ZExp.t =>
  switch (e1, ze0) {
  | (_, CursorEO(_, EmptyHole(_))) =>
    /* e1 |_ --> e1| */
    ZExp.place_after_exp(e1)
  | _ => ze0
  };

let combine_for_Delete_Space = (ze0: ZExp.t, e: UHExp.t): ZExp.t =>
  switch (ze0, e) {
  | (CursorEO(_, EmptyHole(_)), EO(EmptyHole(_)))
      when ZExp.is_after_exp(ze0) =>
    /* _| _ --> _| */
    ze0
  | (CursorEO(_, EmptyHole(_)), _) when ZExp.is_after_exp(ze0) =>
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
      EI(OpSeq(Associator.associate_exp(seq), seq)),
      u_gen,
    )
  | ExpSuffix(_, _)
  | SeqSuffix(_, _) =>
    let (hole, u_gen) = UHExp.new_EmptyHole(u_gen);
    let opseq = OperatorSeq.opseq_of_exp_and_suffix(hole, suffix);
    let skel = Associator.associate_exp(opseq);
    (EI(OpSeq(skel, opseq)), u_gen);
  };

let keyword_action = (k: keyword): t =>
  switch (k) {
  | Let => Construct(SLet)
  | Case => Construct(SCase)
  };

let move_to_prev_node_pos_line =
    (zline: ZExp.zline, result: ZExp.zline => result('a)): result('a) =>
  switch (Path.steps_of_prev_node_pos_line(zline)) {
  | None => CursorEscaped(Before)
  | Some(steps) =>
    switch (Path.follow_line_and_place_after(steps, ZExp.erase_line(zline))) {
    | None => Failed
    | Some(zline) => result(zline)
    }
  };

let move_to_next_node_pos_line =
    (zline: ZExp.zline, result: ZExp.zline => result('a)): result('a) =>
  switch (Path.steps_of_next_node_pos_line(zline)) {
  | None => CursorEscaped(After)
  | Some(steps) =>
    switch (Path.follow_line_and_place_before(steps, ZExp.erase_line(zline))) {
    | None => Failed
    | Some(zline) => result(zline)
    }
  };

let move_to_prev_node_pos_exp =
    (ze: ZExp.t, result: ZExp.t => result('a)): result('a) =>
  switch (Path.steps_of_prev_node_pos_exp(ze)) {
  | None => CursorEscaped(Before)
  | Some(steps) =>
    switch (Path.follow_exp_and_place_after(steps, ZExp.erase(ze))) {
    | None => Failed
    | Some(ze) => result(ze)
    }
  };

let move_to_next_node_pos_exp =
    (ze: ZExp.t, result: ZExp.t => result('a)): result('a) =>
  switch (Path.steps_of_next_node_pos_exp(ze)) {
  | None => CursorEscaped(After)
  | Some(steps) =>
    switch (Path.follow_exp_and_place_before(steps, ZExp.erase(ze))) {
    | None => Failed
    | Some(ze) => result(ze)
    }
  };

let rec syn_perform_block =
        (
          ctx: Contexts.t,
          a: t,
          (zblock, ty, u_gen): (ZExp.zblock, HTyp.t, MetaVarGen.t),
        )
        : result((ZExp.zblock, HTyp.t, MetaVarGen.t)) =>
  switch (a, zblock) {
  /* Movement */
  | (MoveTo(path), _) =>
    let block = ZExp.erase_block(zblock);
    switch (Path.follow_block(path, block)) {
    | None => Failed
    | Some(zblock) => Succeeded((zblock, ty, u_gen))
    };
  | (MoveToPrevHole, _) =>
    switch (Path.prev_hole_path_zblock(zblock)) {
    | None => Failed
    | Some(path) =>
      syn_perform_block(ctx, MoveTo(path), (zblock, ty, u_gen))
    }
  | (MoveToNextHole, _) =>
    switch (Path.next_hole_path_zblock(zblock)) {
    | None => Failed
    | Some(path) =>
      syn_perform_block(ctx, MoveTo(path), (zblock, ty, u_gen))
    }
  /* Backspace & Delete */
  | (Backspace, _) when ZExp.is_before_block(zblock) =>
    CursorEscaped(Before)
  | (Delete, _) when ZExp.is_after_block(zblock) => CursorEscaped(After)
  | (Delete, BlockZL((prefix, CursorLO(_, EmptyLine), []), e)) =>
    let ze = ZExp.place_before_exp(e);
    let zblock = ZExp.BlockZE(prefix, ze);
    Succeeded((zblock, ty, u_gen));
  | (Backspace, BlockZE(lines, ze)) when ZExp.is_before_exp(ze) =>
    switch (split_last(lines)) {
    | None => Failed
    | Some((lines, li)) =>
      switch (li) {
      | ExpLine(e1) =>
        switch (ZExp.erase(ze)) {
        | EO(EmptyHole(_)) =>
          let ze1 = ZExp.place_after_exp(e1);
          let zblock = ZExp.BlockZE(lines, ze1);
          Succeeded((zblock, ty, u_gen));
        | _ => Failed
        }
      | LI(LetLine(_, _, _)) => Failed
      | LO(EmptyLine) =>
        let zblock = ZExp.BlockZE(lines, ze);
        Succeeded((zblock, ty, u_gen));
      }
    }
  | (Delete, BlockZL((prefix, ExpLineZ(ze), []), EO(EmptyHole(_))))
      when ZExp.is_after_exp(ze) =>
    switch (Statics.syn_exp(ctx, ZExp.erase(ze))) {
    | None => Failed
    | Some(ty) =>
      let zblock = ZExp.BlockZE(prefix, ze);
      Succeeded((zblock, ty, u_gen));
    }
  /* Construction */
  | (Construct(SLine), BlockZE(lines, ze)) when ZExp.is_before_exp(ze) =>
    let zblock = ZExp.BlockZE(lines @ [LO(EmptyLine)], ze);
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
        (prefix, (CursorLO(_, EmptyLine) | ExpLineZ(_)) as zline, suffix),
        e2,
      ),
    )
      when ZExp.is_before_line(zline) =>
    let (e1, u_gen) =
      switch (zline) {
      | ExpLineZ(ze1) => (ZExp.erase(ze1), u_gen)
      | _ =>
        let (u, u_gen) = MetaVarGen.next(u_gen);
        (EO(EmptyHole(u)), u_gen);
      };
    let rule_block = UHExp.Block(suffix, e2);
    let (ze, u_gen) =
      switch (e1) {
      | EO(EmptyHole(_)) =>
        let (p, u_gen) = UHPat.new_EmptyHole(u_gen);
        let rule = UHExp.Rule(p, rule_block);
        let scrut_zblock = ZExp.BlockZE([], ZExp.place_before_exp(e1));
        (
          ZExp.CaseZE(NotInHole, scrut_zblock, [rule], Some(TO(Hole))),
          u_gen,
        );
      | _ =>
        let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
        let zrule = ZExp.RuleZP(zp, rule_block);
        let zrules = ZList.singleton(zrule);
        let scrut_block = UHExp.wrap_in_block(e1);
        (
          ZExp.CaseZR(NotInHole, scrut_block, zrules, Some(TO(Hole))),
          u_gen,
        );
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
              CursorEO(_, Var(_, InVHole(Keyword(k), _), _)) as ze0,
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
    syn_perform_block(ctx, keyword_action(k), (zblock, ty, u_gen));
  | (
      Construct(SOp(SSpace)),
      BlockZL(
        (
          prefix,
          ExpLineZ(CursorEO(_, Var(_, InVHole(Keyword(k), _), _)) as ze0),
          suffix,
        ),
        e2,
      ),
    )
      when ZExp.is_after_exp(ze0) =>
    let zlines = (prefix, ZExp.place_before_line(LO(EmptyLine)), suffix);
    let zblock = ZExp.BlockZL(zlines, e2);
    syn_perform_block(ctx, keyword_action(k), (zblock, ty, u_gen));
  | (
      Construct(SOp(SSpace)),
      BlockZE(
        lines,
        OpSeqZ(
          _,
          CursorEO(_, Var(_, InVHole(Keyword(k), _), _)) as ze0,
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
      syn_perform_block(ctx, keyword_action(k), (zblock, ty, u_gen));
    };
  | (
      Construct(SOp(SSpace)),
      BlockZE(
        lines,
        CursorEO(_, Var(_, InVHole(Keyword(k), _), _)) as ze0,
      ),
    )
      when ZExp.is_after_exp(ze0) =>
    let (ze, u_gen) = ZExp.new_EmptyHole(u_gen);
    let zblock = ZExp.BlockZE(lines, ze);
    syn_perform_block(ctx, keyword_action(k), (zblock, Hole, u_gen));
  /* Zipper Cases */
  | (_, BlockZL(zlines, e)) =>
    switch (syn_perform_lines(ctx, a, (zlines, u_gen))) {
    | Failed => Failed
    | CursorEscaped(Before) => CursorEscaped(Before)
    | CursorEscaped(After) =>
      Succeeded((
        BlockZE(ZExp.erase_lines(zlines), ZExp.place_before_exp(e)),
        ty,
        u_gen,
      ))
    | Succeeded((zlines, ctx, u_gen)) =>
      let (e, ty, u_gen) = Statics.syn_fix_holes_exp(ctx, u_gen, e);
      let zblock = ZExp.BlockZL(zlines, e);
      Succeeded((zblock, ty, u_gen));
    }
  | (_, BlockZE(lines, ze)) =>
    switch (Statics.syn_lines(ctx, lines)) {
    | None => Failed
    | Some(ctx) =>
      switch (syn_perform_exp(ctx, a, (ze, ty, u_gen))) {
      | Failed => Failed
      | CursorEscaped(Before) =>
        switch (ZExp.place_after_lines(lines)) {
        | None => CursorEscaped(Before)
        | Some(zlines) =>
          Succeeded((BlockZL(zlines, ZExp.erase(ze)), ty, u_gen))
        }
      | CursorEscaped(After) => CursorEscaped(After)
      | Succeeded((ze, ty, u_gen)) =>
        Succeeded((BlockZE(lines, ze), ty, u_gen))
      }
    }
  }
and syn_perform_lines =
    (ctx: Contexts.t, a: t, (zlines, u_gen): (ZExp.zlines, MetaVarGen.t))
    : result((ZExp.zlines, Contexts.t, MetaVarGen.t)) =>
  switch (a, zlines) {
  /* Movement */
  | (MoveTo(_), _)
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
  | (Delete, (prefix, CursorLO(_, EmptyLine), suffix)) =>
    switch (suffix) {
    | [] => Failed
    | [line, ...suffix] =>
      let zlines = (prefix, ZExp.place_before_line(line), suffix);
      switch (Statics.syn_zlines(ctx, zlines)) {
      | None => Failed
      | Some(ctx) => Succeeded((zlines, ctx, u_gen))
      };
    }
  | (Backspace, (prefix, CursorLO(_, EmptyLine), suffix)) =>
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
      | LI(LetLine(_, _, _)) => Failed
      | LO(EmptyLine) =>
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
      | LI(LetLine(_, _, _)) => Failed
      | LO(EmptyLine) =>
        let zlines = (prefix, zline2, suffix);
        switch (Statics.syn_zlines(ctx, zlines)) {
        | None => Failed
        | Some(ctx) => Succeeded((zlines, ctx, u_gen))
        };
      }
    }
  /* Construction */
  | (Construct(SLine), (prefix, zline, suffix))
      when ZExp.is_before_line(zline) =>
    let zlines = (prefix @ [LO(EmptyLine)], zline, suffix);
    switch (Statics.syn_zlines(ctx, zlines)) {
    | None => Failed
    | Some(ctx) => Succeeded((zlines, ctx, u_gen))
    };
  | (Construct(SLine), (prefix, zline, suffix))
      when ZExp.is_after_line(zline) =>
    let line = ZExp.erase_line(zline);
    let zlines = (
      prefix @ [line],
      ZExp.place_before_line(LO(EmptyLine)),
      suffix,
    );
    switch (Statics.syn_zlines(ctx, zlines)) {
    | None => Failed
    | Some(ctx) => Succeeded((zlines, ctx, u_gen))
    };
  | (Construct(_), (_, CursorLI(_, LetLine(_, _, _)), _)) => Failed
  | (Construct(_), (prefix, CursorLO(_, EmptyLine), suffix)) =>
    let (e, u_gen) = UHExp.new_EmptyHole(u_gen);
    let ze = ZExp.place_before_exp(e);
    syn_perform_lines(ctx, a, ((prefix, ExpLineZ(ze), suffix), u_gen));
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
        | LO(EmptyLine)
        | LI(LetLine(_, _, _)) =>
          let (e2, u_gen) = UHExp.new_EmptyHole(u_gen);
          (UHExp.Block(suffix, e2), u_gen);
        | ExpLine(e2) => (UHExp.Block(lines, e2), u_gen)
        }
      };
    let (ze, u_gen) =
      switch (e1) {
      | EO(EmptyHole(_)) =>
        let (p, u_gen) = UHPat.new_EmptyHole(u_gen);
        let rule = UHExp.Rule(p, rule_block);
        let scrut_zblock = ZExp.BlockZE([], ze1);
        (
          ZExp.CaseZE(NotInHole, scrut_zblock, [rule], Some(TO(Hole))),
          u_gen,
        );
      | _ =>
        let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
        let zrule = ZExp.RuleZP(zp, rule_block);
        let zrules = ZList.singleton(zrule);
        let scrut_block = UHExp.wrap_in_block(e1);
        (
          ZExp.CaseZR(NotInHole, scrut_block, zrules, Some(TO(Hole))),
          u_gen,
        );
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
            CursorEO(_, Var(_, InVHole(Keyword(k), _), _)) as ze0,
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
    syn_perform_lines(ctx, keyword_action(k), (zlines, u_gen));
  | (
      Construct(SOp(SSpace)),
      (
        prefix,
        ExpLineZ(CursorEO(_, Var(_, InVHole(Keyword(k), _), _)) as ze0),
        suffix,
      ),
    )
      when ZExp.is_after_exp(ze0) =>
    let zlines = (prefix, ZExp.place_before_line(LO(EmptyLine)), suffix);
    syn_perform_lines(ctx, keyword_action(k), (zlines, u_gen));
  /* Zipper Cases */
  | (_, (prefix, zline, suffix)) =>
    switch (Statics.syn_lines(ctx, prefix)) {
    | None => Failed
    | Some(ctx) =>
      switch (syn_perform_line(ctx, a, (zline, u_gen))) {
      | Failed => Failed
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
      | Succeeded((zline, ctx, u_gen)) =>
        let (suffix, ctx, u_gen) =
          Statics.syn_fix_holes_lines(ctx, u_gen, suffix);
        let zlines = (prefix, zline, suffix);
        Succeeded((zlines, ctx, u_gen));
      }
    }
  }
and syn_perform_line =
    (ctx: Contexts.t, a: t, (zline, u_gen): (ZExp.zline, MetaVarGen.t))
    : result((ZExp.zline, Contexts.t, MetaVarGen.t)) =>
  switch (a, zline) {
  /* Movement */
  | (MoveTo(_), _)
  | (MoveToPrevHole, _)
  | (MoveToNextHole, _) =>
    /* handled at block or lines level */
    Failed
  /* Backspace & Delete */
  | (Backspace, _) when ZExp.is_before_line(zline) => CursorEscaped(Before)
  | (Delete, _) when ZExp.is_after_line(zline) => CursorEscaped(After)
  | (Backspace | Delete, CursorLO(_, EmptyLine)) => Failed /* should never happen */
  | (
      Backspace,
      CursorLI(
        (BeforeChild(_, Before) | ClosingDelimiter(Before)) as inner_cursor,
        li,
      ),
    )
      when
        ZExp.is_valid_inner_cursor_line(inner_cursor, li)
        && !ZExp.is_before_line(zline) =>
    move_to_prev_node_pos_line(zline, zline =>
      switch (Statics.syn_line(ctx, LI(li))) {
      | None => Failed
      | Some(ctx) => Succeeded((zline, ctx, u_gen))
      }
    )
  | (Delete, CursorLI(BeforeChild(_, After) as inner_cursor, li))
      when
        ZExp.is_valid_inner_cursor_line(inner_cursor, li)
        && !ZExp.is_after_line(zline) =>
    move_to_next_node_pos_line(zline, zline =>
      switch (Statics.syn_line(ctx, LI(li))) {
      | None => Failed
      | Some(ctx) => Succeeded((zline, ctx, u_gen))
      }
    )
  | (
      Backspace,
      CursorLI(BeforeChild(1, After), LetLine(p, Some(_), block)),
    ) =>
    let (block, ty, u_gen) = Statics.syn_fix_holes_block(ctx, u_gen, block);
    let (p, ctx, u_gen) = Statics.ana_fix_holes_pat(ctx, u_gen, p, ty);
    let zp = ZPat.place_after(p);
    let zline = ZExp.LetLineZP(zp, None, block);
    Succeeded((zline, ctx, u_gen));
  | (Delete, CursorLI(BeforeChild(1, Before), LetLine(p, Some(_), block))) =>
    let (block, ty, u_gen) = Statics.syn_fix_holes_block(ctx, u_gen, block);
    let (p, ctx, u_gen) = Statics.ana_fix_holes_pat(ctx, u_gen, p, ty);
    let zline = ZExp.LetLineZP(ZPat.place_after(p), None, block);
    Succeeded((zline, ctx, u_gen));
  /* invalid cursor positions */
  | (
      Backspace | Delete,
      CursorLI(BeforeChild(_, _) | ClosingDelimiter(_), LetLine(_, _, _)),
    ) =>
    Failed
  /* Construction */
  | (Construct(_), CursorLO(_, _) | CursorLI(_, _)) =>
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
        Succeeded((zline, ctx, u_gen));
      };
    }
  | (Construct(SAsc), LetLineZP(zp, Some(uty), block)) =>
    /* just move the cursor over if there is already an ascription */
    let p = ZPat.erase(zp);
    switch (Statics.ana_pat(ctx, p, UHTyp.expand(uty))) {
    | None => Failed
    | Some(ctx) =>
      let zty = ZTyp.place_before(uty);
      let zline = ZExp.LetLineZA(p, zty, block);
      Succeeded((zline, ctx, u_gen));
    };
  /* Zipper Cases */
  | (_, ExpLineZ(ze)) =>
    switch (Statics.syn_exp(ctx, ZExp.erase(ze))) {
    | None => Failed
    | Some(ty) =>
      switch (syn_perform_exp(ctx, a, (ze, ty, u_gen))) {
      | (Failed | CursorEscaped(_)) as err => err
      | Succeeded((ze, _, u_gen)) =>
        let zline = ZExp.prune_empty_hole_line(ExpLineZ(ze));
        Succeeded((zline, ctx, u_gen));
      }
    }
  | (_, LetLineZP(zp, ann, block)) =>
    switch (ann) {
    | Some(uty) =>
      let ty = UHTyp.expand(uty);
      switch (ana_perform_pat(ctx, u_gen, a, zp, ty)) {
      | Failed => Failed
      | CursorEscaped(Before) =>
        let zline =
          ZExp.CursorLI(
            BeforeChild(0, After),
            LetLine(ZPat.erase(zp), ann, block),
          );
        Succeeded((zline, ctx, u_gen));
      | CursorEscaped(After) =>
        let zline =
          ZExp.CursorLI(
            BeforeChild(2, Before),
            LetLine(ZPat.erase(zp), ann, block),
          );
        Succeeded((zline, ctx, u_gen));
      | Succeeded((zp, ctx_after, u_gen)) =>
        let p = ZPat.erase(zp);
        let ctx_block = Statics.ctx_for_let(ctx, p, ty, block);
        let (block, u_gen) =
          Statics.ana_fix_holes_block(ctx_block, u_gen, block, ty);
        let zline = ZExp.LetLineZP(zp, ann, block);
        Succeeded((zline, ctx_after, u_gen));
      };
    | None =>
      switch (Statics.syn_block(ctx, block)) {
      | None => Failed
      | Some(ty) =>
        switch (ana_perform_pat(ctx, u_gen, a, zp, ty)) {
        | Failed => Failed
        | CursorEscaped(Before) =>
          let zline =
            ZExp.CursorLI(
              BeforeChild(0, After),
              LetLine(ZPat.erase(zp), ann, block),
            );
          Succeeded((zline, ctx, u_gen));
        | CursorEscaped(After) =>
          let zline =
            ZExp.CursorLI(
              BeforeChild(1, Before),
              LetLine(ZPat.erase(zp), ann, block),
            );
          Succeeded((zline, ctx, u_gen));
        | Succeeded((zp, ctx, u_gen)) =>
          let (block, _, u_gen) =
            Statics.syn_fix_holes_block(ctx, u_gen, block);
          let zline = ZExp.LetLineZP(zp, ann, block);
          Succeeded((zline, ctx, u_gen));
        }
      }
    }
  | (_, LetLineZA(p, zann, block)) =>
    switch (perform_ty(a, zann)) {
    | Failed => Failed
    | CursorEscaped(Before) =>
      let zline =
        ZExp.CursorLI(
          BeforeChild(1, After),
          LetLine(p, Some(ZTyp.erase(zann)), block),
        );
      Succeeded((zline, ctx, u_gen));
    | CursorEscaped(After) =>
      switch (Statics.ana_pat(ctx, p, UHTyp.expand(ZTyp.erase(zann)))) {
      | None => Failed
      | Some(ctx) =>
        let zline =
          ZExp.CursorLI(
            BeforeChild(2, Before),
            LetLine(p, Some(ZTyp.erase(zann)), block),
          );
        Succeeded((zline, ctx, u_gen));
      }
    | Succeeded(zann) =>
      let ty = UHTyp.expand(ZTyp.erase(zann));
      let (p, ctx_after, u_gen) =
        Statics.ana_fix_holes_pat(ctx, u_gen, p, ty);
      let ctx_block = Statics.ctx_for_let(ctx, p, ty, block);
      let (block, u_gen) =
        Statics.ana_fix_holes_block(ctx_block, u_gen, block, ty);
      let zline = ZExp.LetLineZA(p, zann, block);
      Succeeded((zline, ctx_after, u_gen));
    }
  | (_, LetLineZE(p, ann, zblock)) =>
    switch (ann) {
    | Some(uty) =>
      let ty = UHTyp.expand(uty);
      let ctx_block =
        Statics.ctx_for_let(ctx, p, ty, ZExp.erase_block(zblock));
      switch (ana_perform_block(ctx_block, a, (zblock, u_gen), ty)) {
      | Failed => Failed
      | CursorEscaped(Before) =>
        switch (Statics.ana_pat(ctx, p, ty)) {
        | None => Failed
        | Some(ctx) =>
          let zline =
            ZExp.CursorLI(
              BeforeChild(2, After),
              LetLine(p, ann, ZExp.erase_block(zblock)),
            );
          Succeeded((zline, ctx, u_gen));
        }
      | CursorEscaped(After) => CursorEscaped(After)
      | Succeeded((zblock, u_gen)) =>
        switch (Statics.ana_pat(ctx, p, ty)) {
        | None => Failed
        | Some(ctx) =>
          let zline = ZExp.LetLineZE(p, ann, zblock);
          Succeeded((zline, ctx, u_gen));
        }
      };
    | None =>
      let block = ZExp.erase_block(zblock);
      switch (Statics.syn_block(ctx, block)) {
      | None => Failed
      | Some(ty) =>
        switch (syn_perform_block(ctx, a, (zblock, ty, u_gen))) {
        | Failed => Failed
        | CursorEscaped(Before) =>
          switch (Statics.ana_pat(ctx, p, ty)) {
          | None => Failed
          | Some(ctx) =>
            let zline =
              ZExp.CursorLI(
                BeforeChild(2, After),
                LetLine(p, ann, ZExp.erase_block(zblock)),
              );
            Succeeded((zline, ctx, u_gen));
          }
        | CursorEscaped(After) => CursorEscaped(After)
        | Succeeded((zblock, ty, u_gen)) =>
          let (p, ctx, u_gen) = Statics.ana_fix_holes_pat(ctx, u_gen, p, ty);
          let zline = ZExp.LetLineZE(p, ann, zblock);
          Succeeded((zline, ctx, u_gen));
        }
      };
    }
  | (UpdateApPalette(_), _) => Failed
  }
and syn_perform_exp =
    (ctx: Contexts.t, a: t, (ze, ty, u_gen): (ZExp.t, HTyp.t, MetaVarGen.t))
    : result((ZExp.t, HTyp.t, MetaVarGen.t)) =>
  switch (a, ze) {
  /* Movement */
  | (MoveTo(path), _) =>
    let e = ZExp.erase(ze);
    switch (Path.follow_exp(path, e)) {
    | None => Failed
    | Some(ze) => Succeeded((ze, ty, u_gen))
    };
  | (MoveToPrevHole, _) =>
    let holes = Path.holes_ze(ze, []);
    switch (Path.prev_hole_path(holes)) {
    | None => Failed
    | Some(path) => syn_perform_exp(ctx, MoveTo(path), (ze, ty, u_gen))
    };
  | (MoveToNextHole, _) =>
    let holes = Path.holes_ze(ze, []);
    switch (Path.next_hole_path(holes)) {
    | None => Failed
    | Some(path) => syn_perform_exp(ctx, MoveTo(path), (ze, ty, u_gen))
    };
  /* Backspace & Deletion */
  | (Backspace, _) when ZExp.is_before_exp(ze) => CursorEscaped(Before)
  | (Delete, _) when ZExp.is_after_exp(ze) => CursorEscaped(After)
  | (
      Backspace,
      CursorEO(
        Char(_) as outer_cursor,
        (
          EmptyHole(_) | Var(_, _, _) | NumLit(_, _) | BoolLit(_, _) |
          ListNil(_)
        ) as eo,
      ),
    )
      when
        ZExp.is_valid_outer_cursor_exp(outer_cursor, eo)
        && !ZExp.is_before_exp(ze) =>
    let (ze, u_gen) =
      switch (eo) {
      | EmptyHole(_) => (ZExp.place_before_exp(EO(eo)), u_gen)
      | _ => ZExp.new_EmptyHole(u_gen)
      };
    Succeeded((ze, Hole, u_gen));
  | (
      Delete,
      CursorEO(
        Char(_) as outer_cursor,
        (
          EmptyHole(_) | Var(_, _, _) | NumLit(_, _) | BoolLit(_, _) |
          ListNil(_)
        ) as eo,
      ),
    )
      when
        ZExp.is_valid_outer_cursor_exp(outer_cursor, eo)
        && !ZExp.is_after_exp(ze) =>
    let (ze, u_gen) =
      switch (eo) {
      | EmptyHole(_) => (ZExp.place_after_exp(EO(eo)), u_gen)
      | _ => ZExp.new_EmptyHole(u_gen)
      };
    Succeeded((ze, Hole, u_gen));
  | (
      Backspace | Delete,
      CursorEO(
        Char(_),
        EmptyHole(_) | Var(_, _, _) | NumLit(_, _) | BoolLit(_, _) |
        ListNil(_),
      ),
    ) =>
    /* invalid cursor position */
    Failed
  | (Backspace, CursorEI(BeforeChild(1, After), Lam(_, p, Some(_), block)))
  | (Delete, CursorEI(BeforeChild(1, Before), Lam(_, p, Some(_), block))) =>
    let (p, ctx, u_gen) = Statics.ana_fix_holes_pat(ctx, u_gen, p, Hole);
    let (block, ty2, u_gen) = Statics.syn_fix_holes_block(ctx, u_gen, block);
    let ze = ZExp.LamZP(NotInHole, ZPat.place_after(p), None, block);
    Succeeded((ze, Arrow(Hole, ty2), u_gen));
  /* TODO consider deletion of type ascription on case */
  | (
      Backspace,
      CursorEI(
        (BeforeChild(_, Before) | ClosingDelimiter(Before)) as inner_cursor,
        ei,
      ),
    )
      when
        ZExp.is_valid_inner_cursor_exp(inner_cursor, ei)
        && !ZExp.is_before_exp(ze) =>
    move_to_prev_node_pos_exp(ze, ze =>
      switch (Statics.syn_exp(ctx, ZExp.erase(ze))) {
      | None => Failed
      | Some(ty) => Succeeded((ze, ty, u_gen))
      }
    )
  | (Delete, CursorEI(BeforeChild(_, After) as inner_cursor, ei))
      when
        ZExp.is_valid_inner_cursor_exp(inner_cursor, ei)
        && !ZExp.is_after_exp(ze) =>
    move_to_next_node_pos_exp(ze, ze =>
      switch (Statics.syn_exp(ctx, ZExp.erase(ze))) {
      | None => Failed
      | Some(ty) => Succeeded((ze, ty, u_gen))
      }
    )
  | (
      Backspace,
      CursorEI(
        (BeforeChild(_, After) | ClosingDelimiter(After)) as inner_cursor,
        (
          Lam(_, _, _, _) | Inj(_, _, _) | Case(_, _, _, _) |
          Parenthesized(_) |
          ApPalette(_, _, _, _)
        ) as ei,
      ),
    )
  | (
      Delete,
      CursorEI(
        (BeforeChild(_, Before) | ClosingDelimiter(Before)) as inner_cursor,
        (
          Lam(_, _, _, _) | Inj(_, _, _) | Case(_, _, _, _) |
          Parenthesized(_) |
          ApPalette(_, _, _, _)
        ) as ei,
      ),
    )
      when ZExp.is_valid_inner_cursor_exp(inner_cursor, ei) =>
    let (ze, u_gen) = ZExp.new_EmptyHole(u_gen);
    Succeeded((ze, Hole, u_gen));
  | (
      Backspace | Delete,
      CursorEI(
        BeforeChild(_, _) | ClosingDelimiter(_),
        Lam(_, _, _, _) | Inj(_, _, _) | Case(_, _, _, _) | Parenthesized(_) |
        ApPalette(_, _, _, _),
      ),
    ) =>
    Failed
  | (
      Backspace,
      CaseZR(
        _,
        e1,
        (prefix, CursorR(BeforeChild(0 | 1, After), _), suffix),
        ann,
      ),
    ) =>
    switch (split_last(prefix)) {
    | Some((prefix, last_of_prefix)) =>
      let zrule = ZExp.place_after_rule(last_of_prefix);
      let ze = ZExp.CaseZR(NotInHole, e1, (prefix, zrule, suffix), ann);
      Succeeded((ze, ty, u_gen));
    | None =>
      switch (suffix) {
      | [first_of_suffix, ...suffix] =>
        let zrule = ZExp.place_before_rule(first_of_suffix);
        let ze = ZExp.CaseZR(NotInHole, e1, (prefix, zrule, suffix), ann);
        Succeeded((ze, ty, u_gen));
      | [] =>
        let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
        Succeeded((CaseZR(NotInHole, e1, ([], zrule, []), ann), ty, u_gen));
      }
    }
  | (
      Delete,
      CaseZR(
        _,
        e1,
        (prefix, CursorR(BeforeChild(0 | 1, Before), _), suffix),
        ann,
      ),
    ) =>
    switch (suffix) {
    | [first_of_suffix, ...suffix] =>
      let zrule = ZExp.place_before_rule(first_of_suffix);
      let ze = ZExp.CaseZR(NotInHole, e1, (prefix, zrule, suffix), ann);
      Succeeded((ze, ty, u_gen));
    | [] =>
      switch (split_last(prefix)) {
      | Some((prefix, last_of_prefix)) =>
        let zrule = ZExp.place_after_rule(last_of_prefix);
        let ze = ZExp.CaseZR(NotInHole, e1, (prefix, zrule, suffix), ann);
        Succeeded((ze, ty, u_gen));
      | None =>
        let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
        Succeeded((CaseZR(NotInHole, e1, ([], zrule, []), ann), ty, u_gen));
      }
    }
  | (
      Backspace,
      CaseZR(
        _,
        e1,
        (prefix, CursorR(BeforeChild(0, Before), rule), suffix),
        ann,
      ),
    ) =>
    switch (split_last(prefix)) {
    | None =>
      let rules = prefix @ [rule] @ suffix;
      let ze1 = ZExp.place_after_block(e1);
      let ze = ZExp.CaseZE(NotInHole, ze1, rules, ann);
      Succeeded((ze, ty, u_gen));
    | Some((prefix, last_of_prefix)) =>
      let zrule = ZExp.place_after_rule(last_of_prefix);
      let zrules = (prefix, zrule, [rule, ...suffix]);
      let ze = ZExp.CaseZR(NotInHole, e1, zrules, ann);
      Succeeded((ze, ty, u_gen));
    }
  | (
      Backspace,
      CaseZR(
        _,
        e1,
        (prefix, CursorR(BeforeChild(1, Before), Rule(p, clause)), suffix),
        ann,
      ),
    ) =>
    let zrule = ZExp.RuleZP(ZPat.place_after(p), clause);
    let ze = ZExp.CaseZR(NotInHole, e1, (prefix, zrule, suffix), ann);
    Succeeded((ze, ty, u_gen));
  | (
      Delete,
      CaseZR(
        _,
        e1,
        (prefix, CursorR(BeforeChild(0, After), Rule(p, clause)), suffix),
        ann,
      ),
    ) =>
    let zrule = ZExp.RuleZP(ZPat.place_before(p), clause);
    let ze = ZExp.CaseZR(NotInHole, e1, (prefix, zrule, suffix), ann);
    Succeeded((ze, ty, u_gen));
  | (
      Delete,
      CaseZR(
        _,
        e1,
        (prefix, CursorR(BeforeChild(1, After), Rule(p, clause)), suffix),
        ann,
      ),
    ) =>
    let zrule = ZExp.RuleZE(p, ZExp.place_before_block(clause));
    let ze = ZExp.CaseZR(NotInHole, e1, (prefix, zrule, suffix), ann);
    Succeeded((ze, ty, u_gen));
  | (
      Backspace | Delete,
      CaseZR(_, _, (_, CursorR(BeforeChild(_, _), _), _), _),
    ) =>
    Failed
  | (Backspace, OpSeqZ(_, CursorEO(_, EmptyHole(_)) as ze0, surround))
      when ZExp.opseqz_preceded_by_Space(ze0, surround) =>
    switch (surround) {
    /* should never happen given guard */
    | EmptyPrefix(_) => Failed
    /* ... + [k-1]   _<|   ==>   ... + [k-1]| */
    | EmptySuffix(prefix) =>
      let e =
        switch (prefix) {
        | ExpPrefix(e1, _space) => e1
        | SeqPrefix(seq, _space) => UHExp.EI(ExpUtil.mk_OpSeq(seq))
        };
      Succeeded(
        Statics.syn_fix_holes_zexp(ctx, u_gen, ZExp.place_after_exp(e)),
      );
    /* ... + [k-1]   _<| + [k+1] + ...   ==>   ... + [k-1]| + [k+1] + ... */
    | BothNonEmpty(prefix, suffix) =>
      switch (prefix) {
      | ExpPrefix(e1, _space) =>
        let ze1 = ZExp.place_after_exp(e1);
        let ze = ExpUtil.mk_OpSeqZ(ze1, EmptyPrefix(suffix));
        Succeeded(Statics.syn_fix_holes_zexp(ctx, u_gen, ze));
      | SeqPrefix(seq, _space) =>
        let (prefix: ZExp.opseq_prefix, e0) =
          switch (seq) {
          | ExpOpExp(e1, op, e2) => (ExpPrefix(e1, op), e2)
          | SeqOpExp(seq, op, e1) => (SeqPrefix(seq, op), e1)
          };
        let ze0 = ZExp.place_after_exp(e0);
        let ze = ExpUtil.mk_OpSeqZ(ze0, BothNonEmpty(prefix, suffix));
        Succeeded(Statics.syn_fix_holes_zexp(ctx, u_gen, ze));
      }
    }
  | (Delete, OpSeqZ(_, CursorEO(_, EmptyHole(_)) as ze0, surround))
      when ZExp.opseqz_followed_by_Space(ze0, surround) =>
    switch (surround) {
    /* should never happen given guard */
    | EmptySuffix(_) => Failed
    /* |>_   [1] + ...   ==>   |[1] + ... */
    | EmptyPrefix(suffix) =>
      let e =
        switch (suffix) {
        | ExpSuffix(_space, e1) => e1
        | SeqSuffix(_space, seq) => UHExp.EI(ExpUtil.mk_OpSeq(seq))
        };
      Succeeded(
        Statics.syn_fix_holes_zexp(ctx, u_gen, ZExp.place_before_exp(e)),
      );
    /* ... + [k-1] + |>_   [k+1] + ...   ==>   ... + [k-1] + |[k+1] + ... */
    | BothNonEmpty(prefix, suffix) =>
      switch (suffix) {
      | ExpSuffix(_space, e1) =>
        let ze1 = ZExp.place_before_exp(e1);
        let ze = ExpUtil.mk_OpSeqZ(ze1, EmptySuffix(prefix));
        Succeeded(Statics.syn_fix_holes_zexp(ctx, u_gen, ze));
      | SeqSuffix(_space, seq) =>
        let (e0, suffix: ZExp.opseq_suffix) =
          switch (seq) {
          | ExpOpExp(e1, op, e2) => (e1, ExpSuffix(op, e2))
          | SeqOpExp(seq, op, e1) => (e1, SeqSuffix(op, seq))
          };
        let ze0 = ZExp.place_before_exp(e0);
        let ze = ExpUtil.mk_OpSeqZ(ze0, BothNonEmpty(prefix, suffix));
        Succeeded(Statics.syn_fix_holes_zexp(ctx, u_gen, ze));
      }
    }
  | (Backspace, OpSeqZ(_, CursorEO(_, EmptyHole(_)) as ze0, surround))
      when ZExp.opseqz_followed_by_Space(ze0, surround) =>
    switch (surround) {
    /* should never happen given guard */
    | EmptySuffix(_) => Failed
    /* _<|   [1] + ...   ==>   |[1] + ... */
    | EmptyPrefix(suffix) =>
      let e =
        switch (suffix) {
        | ExpSuffix(_space, e1) => e1
        | SeqSuffix(_space, seq) => UHExp.EI(ExpUtil.mk_OpSeq(seq))
        };
      Succeeded(
        Statics.syn_fix_holes_zexp(ctx, u_gen, ZExp.place_before_exp(e)),
      );
    /* ... + [k-1] + _<|   [k+1] + ...   ==>   ... + [k-1] +| [k+1] + ... */
    | BothNonEmpty(prefix, suffix) =>
      let seq =
        switch (suffix) {
        | ExpSuffix(_space, e1) =>
          OperatorSeq.opseq_of_prefix_and_exp(prefix, e1)
        | SeqSuffix(_space, seq) =>
          OperatorSeq.opseq_of_prefix_and_seq(prefix, seq)
        };
      let ei = ExpUtil.mk_OpSeq(seq);
      let k = OperatorSeq.prefix_length(prefix);
      let ze = ZExp.CursorEI(BeforeChild(k, After), ei);
      Succeeded(Statics.syn_fix_holes_zexp(ctx, u_gen, ze));
    }
  | (Delete, OpSeqZ(_, CursorEO(_, EmptyHole(_)) as ze0, surround))
      when ZExp.opseqz_preceded_by_Space(ze0, surround) =>
    switch (surround) {
    /* should never happen given guard */
    | EmptyPrefix(_) => Failed
    /* ... + [k-1]   |>_   ==>   ... + [k-1]| */
    | EmptySuffix(prefix) =>
      let e =
        switch (prefix) {
        | ExpPrefix(e1, _space) => e1
        | SeqPrefix(seq, _space) => UHExp.EI(ExpUtil.mk_OpSeq(seq))
        };
      Succeeded(
        Statics.syn_fix_holes_zexp(ctx, u_gen, ZExp.place_after_exp(e)),
      );
    /* ... + [k-1]   |>_ + [k+1] + ...   ==>   ... + [k-1] |+ [k+1] + ... */
    | BothNonEmpty(prefix, suffix) =>
      let seq =
        switch (prefix) {
        | ExpPrefix(e1, _space) =>
          OperatorSeq.opseq_of_exp_and_suffix(e1, suffix)
        | SeqPrefix(seq, _space) =>
          OperatorSeq.opseq_of_seq_and_suffix(seq, suffix)
        };
      let ei = ExpUtil.mk_OpSeq(seq);
      let k = OperatorSeq.prefix_length(prefix);
      let ze = ZExp.CursorEI(BeforeChild(k, After), ei);
      Succeeded(Statics.syn_fix_holes_zexp(ctx, u_gen, ze));
    }
  /* ... + [k-1] +<| [k] + ...   ==>   ... + [k-1]| [k] */
  | (
      Backspace,
      CursorEI(BeforeChild(k, After) as inner_cursor, OpSeq(_, seq) as ei),
    )
      when ZExp.is_valid_inner_cursor_exp(inner_cursor, ei) =>
    switch (OperatorSeq.split(k - 1, seq)) {
    | None => Failed /* should never happen */
    | Some((e0, surround)) =>
      switch (OperatorSeq.replace_following_op(surround, UHExp.Space)) {
      | None => Failed /* should never happen */
      | Some(surround) =>
        Succeeded(
          make_and_syn_OpSeqZ(
            ctx,
            u_gen,
            ZExp.place_after_exp(e0),
            surround,
          ),
        )
      }
    }
  /* ... + [k-1] |>+ [k] + ...   ==>   ... + [k-1] |[k] */
  | (
      Delete,
      CursorEI(BeforeChild(k, Before) as inner_cursor, OpSeq(_, seq) as ei),
    )
      when ZExp.is_valid_inner_cursor_exp(inner_cursor, ei) =>
    switch (OperatorSeq.split(k, seq)) {
    | None => Failed /* should never happen */
    | Some((e0, surround)) =>
      switch (OperatorSeq.replace_preceding_op(surround, UHExp.Space)) {
      | None => Failed /* should never happen */
      | Some(surround) =>
        Succeeded(
          make_and_syn_OpSeqZ(
            ctx,
            u_gen,
            ZExp.place_before_exp(e0),
            surround,
          ),
        )
      }
    }
  /* invalid cursor position */
  | (
      Backspace | Delete,
      CursorEI(BeforeChild(_, _) | ClosingDelimiter(_), OpSeq(_, _)),
    ) =>
    Failed
  /* Construction */
  | (Construct(SLine), CursorEO(_, _) | CursorEI(_, _))
  | (Construct(SLet), CursorEO(_, _) | CursorEI(_, _)) =>
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
    Succeeded((ze, ty, u_gen));
  | (
      Construct(SLine),
      CaseZR(_, e1, (prefix, RuleZE(_, ze) as zrule, suffix), ann),
    )
      when ZExp.is_after_block(ze) =>
    let prev_rule = ZExp.erase_rule(zrule);
    let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
    let prefix = prefix @ [prev_rule];
    let ze = ZExp.CaseZR(NotInHole, e1, (prefix, zrule, suffix), ann);
    Succeeded((ze, ty, u_gen));
  | (
      Construct(SLine),
      CaseZR(_, e1, (prefix, RuleZP(zp, _) as zrule, suffix), ann),
    )
      when ZPat.is_after(zp) =>
    let prev_rule = ZExp.erase_rule(zrule);
    let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
    let prefix = prefix @ [prev_rule];
    let ze = ZExp.CaseZR(NotInHole, e1, (prefix, zrule, suffix), ann);
    Succeeded((ze, ty, u_gen));
  | (Construct(SCase), ze1) when ZExp.is_before_exp(ze1) =>
    let e1 = ZExp.erase(ze1);
    let (ze, u_gen) =
      switch (e1) {
      | EO(EmptyHole(_)) =>
        let (rule, u_gen) = UHExp.empty_rule(u_gen);
        (
          ZExp.CaseZE(
            NotInHole,
            ZExp.wrap_in_block(ze1),
            [rule],
            Some(TO(Hole)),
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
            Some(TO(Hole)),
          ),
          u_gen,
        );
      };
    Succeeded((ze, Hole, u_gen));
  | (Construct(SCase), CursorEO(_, _) | CursorEI(_, _)) => Failed
  | (Construct(SParenthesized), CursorEO(_, _) | CursorEI(_, _)) =>
    let zblock = ZExp.BlockZE([], ze);
    Succeeded((ParenthesizedZ(zblock), ty, u_gen));
  | (Construct(SAsc), LamZP(err_status, zp, None, e1)) =>
    let ze =
      ZExp.LamZA(
        err_status,
        ZPat.erase(zp),
        ZTyp.place_before(TO(Hole)),
        e1,
      );
    Succeeded((ze, ty, u_gen));
  | (Construct(SAsc), LamZP(err_status, zp, Some(uty1), e1)) =>
    /* just move the cursor over if there is already an ascription */
    let ze =
      ZExp.LamZA(err_status, ZPat.erase(zp), ZTyp.place_before(uty1), e1);
    Succeeded((ze, ty, u_gen));
  | (Construct(SAsc), CursorEI(_, Case(_, e1, rules, Some(uty)))) =>
    /* just move the cursor over if there is already an ascription */
    let ze = ZExp.CaseZA(NotInHole, e1, rules, ZTyp.place_before(uty));
    Succeeded((ze, ty, u_gen));
  | (Construct(SAsc), CursorEO(_, _) | CursorEI(_, _)) => Failed
  | (Construct(SVar(x, outer_cursor)), CursorEO(_, EmptyHole(_)))
  | (Construct(SVar(x, outer_cursor)), CursorEO(_, Var(_, _, _)))
  | (Construct(SVar(x, outer_cursor)), CursorEO(_, NumLit(_, _)))
  | (Construct(SVar(x, outer_cursor)), CursorEO(_, BoolLit(_, _))) =>
    if (String.equal(x, "true")) {
      Succeeded((
        CursorEO(outer_cursor, BoolLit(NotInHole, true)),
        Bool,
        u_gen,
      ));
    } else if (String.equal(x, "false")) {
      Succeeded((
        CursorEO(outer_cursor, BoolLit(NotInHole, false)),
        Bool,
        u_gen,
      ));
    } else if (Var.is_let(x)) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      Succeeded((
        CursorEO(
          outer_cursor,
          Var(NotInHole, InVHole(Keyword(Let), u), x),
        ),
        Hole,
        u_gen,
      ));
    } else if (Var.is_case(x)) {
      let (u, u_gen) = MetaVarGen.next(u_gen);
      Succeeded((
        CursorEO(
          outer_cursor,
          Var(NotInHole, InVHole(Keyword(Case), u), x),
        ),
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
              ZExp.CursorEO(outer_cursor, Var(NotInHole, NotInVHole, x)),
              xty,
              u_gen,
            ))
          | None =>
            let (u, u_gen) = MetaVarGen.next(u_gen);
            Succeeded((
              ZExp.CursorEO(
                outer_cursor,
                Var(NotInHole, InVHole(Free, u), x),
              ),
              HTyp.Hole,
              u_gen,
            ));
          };
        },
      );
    }
  | (Construct(SVar(_, _)), CursorEO(_, _) | CursorEI(_, _)) => Failed
  | (Construct(SLam), (CursorEO(_, _) | CursorEI(_, _)) as ze1) =>
    let e1 = ZExp.erase(ze1);
    let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
    let block = UHExp.wrap_in_block(e1);
    let ze = ZExp.LamZP(NotInHole, zp, Some(TO(Hole)), block);
    let ty' = HTyp.Arrow(Hole, ty);
    Succeeded((ze, ty', u_gen));
  | (Construct(SNumLit(n, outer_cursor)), CursorEO(_, EmptyHole(_)))
  | (Construct(SNumLit(n, outer_cursor)), CursorEO(_, NumLit(_, _)))
  | (Construct(SNumLit(n, outer_cursor)), CursorEO(_, BoolLit(_, _)))
  | (Construct(SNumLit(n, outer_cursor)), CursorEO(_, Var(_, _, _))) =>
    Succeeded((CursorEO(outer_cursor, NumLit(NotInHole, n)), Num, u_gen))
  | (Construct(SNumLit(_, _)), CursorEO(_, _) | CursorEI(_, _)) => Failed
  | (Construct(SInj(side)), CursorEO(_, _) | CursorEI(_, _)) =>
    let zblock = ZExp.BlockZE([], ze);
    let ze' = ZExp.InjZ(NotInHole, side, zblock);
    let ty' =
      switch (side) {
      | L => HTyp.Sum(ty, Hole)
      | R => HTyp.Sum(Hole, ty)
      };
    Succeeded((ze', ty', u_gen));
  | (Construct(SListNil), CursorEO(_, EmptyHole(_))) =>
    let ze = ZExp.place_after_exp(EO(ListNil(NotInHole)));
    let ty = HTyp.List(Hole);
    Succeeded((ze, ty, u_gen));
  | (Construct(SListNil), CursorEO(_, _) | CursorEI(_, _)) => Failed
  | (Construct(SOp(os)), OpSeqZ(_, ze0, surround))
      when ZExp.is_after_exp(ze0) =>
    switch (exp_op_of(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded(
        abs_perform_Construct_SOp_After_surround(
          ZExp.new_EmptyHole,
          make_and_syn_OpSeqZ,
          UHExp.is_Space,
          UHExp.Space,
          ZExp.place_before_exp,
          ctx,
          u_gen,
          ZExp.erase(ze0),
          op,
          surround,
        ),
      )
    }
  | (Construct(SOp(os)), OpSeqZ(_, ze0, surround))
      when ZExp.is_before_exp(ze0) =>
    switch (exp_op_of(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded(
        abs_perform_Construct_SOp_Before_surround(
          ZExp.erase,
          ZExp.new_EmptyHole,
          make_and_syn_OpSeqZ,
          UHExp.is_Space,
          UHExp.Space,
          ctx,
          u_gen,
          ze0,
          op,
          surround,
        ),
      )
    }
  | (Construct(SOp(os)), _) when ZExp.is_after_exp(ze) =>
    switch (exp_op_of(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded(
        abs_perform_Construct_SOp_After(
          UHExp.bidelimit,
          ZExp.new_EmptyHole,
          make_and_syn_OpSeqZ,
          ctx,
          u_gen,
          ZExp.erase(ze),
          op,
        ),
      )
    }
  | (Construct(SOp(os)), _) when ZExp.is_before_exp(ze) =>
    switch (exp_op_of(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded(
        abs_perform_Construct_SOp_Before(
          UHExp.bidelimit,
          ZExp.new_EmptyHole,
          make_and_syn_OpSeqZ,
          ctx,
          u_gen,
          ZExp.erase(ze),
          op,
        ),
      )
    }
  | (Construct(SOp(_)), _) => Failed
  | (Construct(SApPalette(name)), CursorEO(_, EmptyHole(_))) =>
    let palette_ctx = Contexts.palette_ctx(ctx);
    switch (PaletteCtx.lookup(palette_ctx, name)) {
    | None => Failed
    | Some(palette_defn) =>
      let init_model_cmd = palette_defn.init_model;
      let (init_model, init_splice_info, u_gen) =
        SpliceGenMonad.exec(init_model_cmd, SpliceInfo.empty, u_gen);
      switch (Statics.ana_splice_map(ctx, init_splice_info.splice_map)) {
      | None => Failed
      | Some(splice_ctx) =>
        let expansion_ty = palette_defn.expansion_ty;
        let expand = palette_defn.expand;
        let expansion = expand(init_model);
        switch (Statics.ana_block(splice_ctx, expansion, expansion_ty)) {
        | None => Failed
        | Some(_) =>
          Succeeded((
            ZExp.place_before_exp(
              EI(ApPalette(NotInHole, name, init_model, init_splice_info)),
            ),
            expansion_ty,
            u_gen,
          ))
        };
      };
    };
  | (Construct(SApPalette(_)), CursorEO(_, _) | CursorEI(_, _)) => Failed
  | (UpdateApPalette(_), CursorEI(_, ApPalette(_, _name, _, _hole_data))) =>
    Failed
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
  | (UpdateApPalette(_), CursorEO(_, _) | CursorEI(_, _)) => Failed
  /* Zipper Cases */
  | (_, ParenthesizedZ(zblock)) =>
    switch (syn_perform_block(ctx, a, (zblock, ty, u_gen))) {
    | Failed => Failed
    | CursorEscaped(Before) =>
      move_to_prev_node_pos_exp(ze, ze => Succeeded((ze, ty, u_gen)))
    | CursorEscaped(After) =>
      move_to_next_node_pos_exp(ze, ze => Succeeded((ze, ty, u_gen)))
    | Succeeded((ze1', ty', u_gen')) =>
      Succeeded((ParenthesizedZ(ze1'), ty', u_gen'))
    }
  | (_, LamZP(_, zp, ann, block)) =>
    let ty1 =
      switch (ann) {
      | Some(uty1) => UHTyp.expand(uty1)
      | None => HTyp.Hole
      };
    switch (ana_perform_pat(ctx, u_gen, a, zp, ty1)) {
    | Failed => Failed
    | CursorEscaped(Before) =>
      move_to_prev_node_pos_exp(ze, ze => Succeeded((ze, ty, u_gen)))
    | CursorEscaped(After) =>
      move_to_next_node_pos_exp(ze, ze => Succeeded((ze, ty, u_gen)))
    | Succeeded((zp, ctx, u_gen)) =>
      let (block, ty2, u_gen) =
        Statics.syn_fix_holes_block(ctx, u_gen, block);
      let ty = HTyp.Arrow(ty1, ty2);
      let ze = ZExp.LamZP(NotInHole, zp, ann, block);
      Succeeded((ze, ty, u_gen));
    };
  | (_, LamZA(_, p, zann, block)) =>
    switch (perform_ty(a, zann)) {
    | Failed => Failed
    | CursorEscaped(Before) =>
      move_to_prev_node_pos_exp(ze, ze => Succeeded((ze, ty, u_gen)))
    | CursorEscaped(After) =>
      move_to_next_node_pos_exp(ze, ze => Succeeded((ze, ty, u_gen)))
    | Succeeded(zann) =>
      let ty1 = UHTyp.expand(ZTyp.erase(zann));
      let (p, ctx, u_gen) = Statics.ana_fix_holes_pat(ctx, u_gen, p, ty1);
      let (block, ty2, u_gen) =
        Statics.syn_fix_holes_block(ctx, u_gen, block);
      let ze = ZExp.LamZA(NotInHole, p, zann, block);
      Succeeded((ze, Arrow(ty1, ty2), u_gen));
    }
  | (_, LamZE(_, p, ann, zblock)) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => Failed
    | Some((_, ty2)) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => HTyp.Hole
        };
      switch (Statics.ana_pat(ctx, p, ty1)) {
      | None => Failed
      | Some(ctx) =>
        switch (syn_perform_block(ctx, a, (zblock, ty2, u_gen))) {
        | Failed => Failed
        | CursorEscaped(Before) =>
          move_to_prev_node_pos_exp(ze, ze => Succeeded((ze, ty, u_gen)))
        | CursorEscaped(After) =>
          move_to_next_node_pos_exp(ze, ze => Succeeded((ze, ty, u_gen)))
        | Succeeded((zblock, ty2, u_gen)) =>
          let ze = ZExp.LamZE(NotInHole, p, ann, zblock);
          Succeeded((ze, Arrow(ty1, ty2), u_gen));
        }
      };
    }
  | (_, InjZ(_, side, zblock)) =>
    switch (ty) {
    | Sum(ty1, ty2) =>
      let ty_side = pick_side(side, ty1, ty2);
      switch (syn_perform_block(ctx, a, (zblock, ty_side, u_gen))) {
      | Failed => Failed
      | CursorEscaped(Before) =>
        move_to_prev_node_pos_exp(ze, ze => Succeeded((ze, ty, u_gen)))
      | CursorEscaped(After) =>
        move_to_next_node_pos_exp(ze, ze => Succeeded((ze, ty, u_gen)))
      | Succeeded((zblock, ty_side', u_gen)) =>
        let ty' =
          switch (side) {
          | L => HTyp.Sum(ty_side', ty2)
          | R => HTyp.Sum(ty1, ty_side')
          };
        Succeeded((InjZ(NotInHole, side, zblock), ty', u_gen));
      };
    | _ => Failed /* should never happen */
    }
  | (_, OpSeqZ(_, ze0, surround)) =>
    let i = OperatorSeq.surround_prefix_length(surround);
    switch (ZExp.erase(ze)) {
    | EI(OpSeq(skel, seq)) =>
      switch (Statics.syn_skel(ctx, skel, seq, Some(i))) {
      | Some((ty, Some(mode))) =>
        switch (mode) {
        | Statics.AnalyzedAgainst(ty0) =>
          switch (ana_perform_exp(ctx, a, (ze0, u_gen), ty0)) {
          | Failed => Failed
          | CursorEscaped(Before) =>
            move_to_prev_node_pos_exp(ze, ze => Succeeded((ze, ty, u_gen)))
          | CursorEscaped(After) =>
            move_to_next_node_pos_exp(ze, ze => Succeeded((ze, ty, u_gen)))
          | Succeeded((ze0', u_gen)) =>
            let ze0'' = ZExp.bidelimit(ze0');
            Succeeded((OpSeqZ(skel, ze0'', surround), ty, u_gen));
          }
        | Statics.Synthesized(ty0) =>
          switch (syn_perform_exp(ctx, a, (ze0, ty0, u_gen))) {
          | Failed => Failed
          | CursorEscaped(Before) =>
            move_to_prev_node_pos_exp(ze, ze => Succeeded((ze, ty, u_gen)))
          | CursorEscaped(After) =>
            move_to_next_node_pos_exp(ze, ze => Succeeded((ze, ty, u_gen)))
          | Succeeded((ze0', _ty0', u_gen)) =>
            let ze0'' = ZExp.bidelimit(ze0');
            Succeeded(make_and_syn_OpSeqZ(ctx, u_gen, ze0'', surround));
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
     switch (ana_perform_exp(ctx, a, (cell_ze, u_gen), cell_ty)) {
     | Failed => Failed
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
      switch (syn_perform_block(ctx, a, (zblock, ty1, u_gen))) {
      | Failed => Failed
      | CursorEscaped(Before) =>
        move_to_prev_node_pos_exp(ze, ze => Succeeded((ze, ty, u_gen)))
      | CursorEscaped(After) =>
        move_to_next_node_pos_exp(ze, ze => Succeeded((ze, ty, u_gen)))
      | Succeeded((zblock, ty1, u_gen)) =>
        let ty = UHTyp.expand(uty);
        let (rules, u_gen) =
          Statics.ana_fix_holes_rules(ctx, u_gen, rules, ty1, ty);
        let ze = ZExp.CaseZE(NotInHole, zblock, rules, ann);
        Succeeded((ze, ty, u_gen));
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
        | CursorEscaped(Before) =>
          move_to_prev_node_pos_exp(ze, ze => Succeeded((ze, ty, u_gen)))
        | CursorEscaped(After) =>
          move_to_next_node_pos_exp(ze, ze => Succeeded((ze, ty, u_gen)))
        | Succeeded((zp, ctx, u_gen)) =>
          let ty = UHTyp.expand(uty);
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
          Succeeded((ze, ty, u_gen));
        }
      | RuleZE(p, zclause) =>
        switch (Statics.ana_pat(ctx, p, ty1)) {
        | None => Failed
        | Some(ctx) =>
          let ty = UHTyp.expand(uty);
          switch (ana_perform_block(ctx, a, (zclause, u_gen), ty)) {
          | Failed => Failed
          | CursorEscaped(Before) =>
            move_to_prev_node_pos_exp(ze, ze => Succeeded((ze, ty, u_gen)))
          | CursorEscaped(After) =>
            move_to_next_node_pos_exp(ze, ze => Succeeded((ze, ty, u_gen)))
          | Succeeded((zclause, u_gen)) =>
            let zrule = ZExp.RuleZE(p, zclause);
            let ze =
              ZExp.CaseZR(
                NotInHole,
                block,
                ZList.replace_z(zrules, zrule),
                ann,
              );
            Succeeded((ze, ty, u_gen));
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
      | CursorEscaped(Before) =>
        move_to_prev_node_pos_exp(ze, ze => Succeeded((ze, ty, u_gen)))
      | CursorEscaped(After) =>
        move_to_next_node_pos_exp(ze, ze => Succeeded((ze, ty, u_gen)))
      | Succeeded(zann) =>
        let ty = UHTyp.expand(ZTyp.erase(zann));
        let (rules, u_gen) =
          Statics.ana_fix_holes_rules(ctx, u_gen, rules, ty1, ty);
        let ze = ZExp.CaseZA(NotInHole, block, rules, zann);
        Succeeded((ze, ty, u_gen));
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
      ctx: Contexts.t,
      a: t,
      (zblock, u_gen): (ZExp.zblock, MetaVarGen.t),
      ty: HTyp.t,
    )
    : result((ZExp.zblock, MetaVarGen.t)) =>
  switch (a, zblock) {
  /* Movement */
  | (MoveTo(path), _) =>
    let block = ZExp.erase_block(zblock);
    switch (Path.follow_block(path, block)) {
    | None => Failed
    | Some(zblock) => Succeeded((zblock, u_gen))
    };
  | (MoveToPrevHole, _) =>
    switch (Path.prev_hole_path_zblock(zblock)) {
    | None => Failed
    | Some(path) =>
      ana_perform_block(ctx, MoveTo(path), (zblock, u_gen), ty)
    }
  | (MoveToNextHole, _) =>
    switch (Path.next_hole_path_zblock(zblock)) {
    | None => Failed
    | Some(path) =>
      ana_perform_block(ctx, MoveTo(path), (zblock, u_gen), ty)
    }
  /* Backspace & Delete */
  | (Backspace, _) when ZExp.is_before_block(zblock) =>
    CursorEscaped(Before)
  | (Delete, _) when ZExp.is_after_block(zblock) => CursorEscaped(After)
  | (Delete, BlockZL((prefix, CursorLO(_, EmptyLine), []), e)) =>
    let ze = ZExp.place_before_exp(e);
    let zblock = ZExp.BlockZE(prefix, ze);
    Succeeded((zblock, u_gen));
  | (Backspace, BlockZE(lines, ze)) when ZExp.is_before_exp(ze) =>
    switch (split_last(lines)) {
    | None => Failed
    | Some((lines, li)) =>
      switch (li) {
      | ExpLine(e1) =>
        switch (ZExp.erase(ze)) {
        | EO(EmptyHole(_)) =>
          let ze1 = ZExp.place_after_exp(e1);
          let zblock = ZExp.BlockZE(lines, ze1);
          Succeeded(Statics.ana_fix_holes_zblock(ctx, u_gen, zblock, ty));
        | _ => Failed
        }
      | LI(LetLine(_, _, _)) => Failed
      | LO(EmptyLine) =>
        let zblock = ZExp.BlockZE(lines, ze);
        Succeeded((zblock, u_gen));
      }
    }
  | (Delete, BlockZL((prefix, ExpLineZ(ze), []), EO(EmptyHole(_))))
      when ZExp.is_after_exp(ze) =>
    let zblock = ZExp.BlockZE(prefix, ze);
    Succeeded(Statics.ana_fix_holes_zblock(ctx, u_gen, zblock, ty));
  /* Construction */
  | (Construct(SLine), BlockZE(lines, ze)) when ZExp.is_before_exp(ze) =>
    let zblock = ZExp.BlockZE(lines @ [LO(EmptyLine)], ze);
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
        (prefix, (CursorLO(_, EmptyLine) | ExpLineZ(_)) as zline, suffix),
        e2,
      ),
    )
      when ZExp.is_before_line(zline) =>
    let (e1, u_gen) =
      switch (zline) {
      | ExpLineZ(ze1) => (ZExp.erase(ze1), u_gen)
      | _ =>
        let (u, u_gen) = MetaVarGen.next(u_gen);
        (EO(EmptyHole(u)), u_gen);
      };
    let clause = UHExp.Block(suffix, e2);
    let (ze, u_gen) =
      switch (e1) {
      | EO(EmptyHole(_)) =>
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
              CursorEO(_, Var(_, InVHole(Keyword(k), _), _)) as ze0,
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
    ana_perform_block(ctx, keyword_action(k), (zblock, u_gen), ty);
  | (
      Construct(SOp(SSpace)),
      BlockZL(
        (
          prefix,
          ExpLineZ(CursorEO(_, Var(_, InVHole(Keyword(k), _), _)) as ze0),
          suffix,
        ),
        e2,
      ),
    )
      when ZExp.is_after_exp(ze0) =>
    let zlines = (prefix, ZExp.place_before_line(LO(EmptyLine)), suffix);
    let zblock = ZExp.BlockZL(zlines, e2);
    ana_perform_block(ctx, keyword_action(k), (zblock, u_gen), ty);
  | (
      Construct(SOp(SSpace)),
      BlockZE(
        lines,
        OpSeqZ(
          _,
          CursorEO(_, Var(_, InVHole(Keyword(k), _), _)) as ze0,
          EmptyPrefix(suffix),
        ),
      ),
    )
      when ZExp.is_after_exp(ze0) =>
    let (e, u_gen) = keyword_suffix_to_exp(suffix, u_gen);
    let ze = ZExp.place_before_exp(e);
    let zblock = ZExp.BlockZE(lines, ze);
    ana_perform_block(ctx, keyword_action(k), (zblock, u_gen), ty);
  | (
      Construct(SOp(SSpace)),
      BlockZE(
        lines,
        CursorEO(_, Var(_, InVHole(Keyword(k), _), _)) as ze0,
      ),
    )
      when ZExp.is_after_exp(ze0) =>
    let (ze, u_gen) = ZExp.new_EmptyHole(u_gen);
    let zblock = ZExp.BlockZE(lines, ze);
    ana_perform_block(ctx, keyword_action(k), (zblock, u_gen), ty);
  /* Zipper Cases */
  | (_, BlockZL(zlines, e)) =>
    switch (syn_perform_lines(ctx, a, (zlines, u_gen))) {
    | Failed => Failed
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
  | (_, BlockZE(lines, ze)) =>
    switch (Statics.syn_lines(ctx, lines)) {
    | None => Failed
    | Some(ctx1) =>
      switch (ana_perform_exp(ctx1, a, (ze, u_gen), ty)) {
      | Failed => Failed
      | CursorEscaped(Before) =>
        switch (ZExp.place_after_lines(lines)) {
        | None => CursorEscaped(Before)
        | Some(zlines) =>
          Succeeded((BlockZL(zlines, ZExp.erase(ze)), u_gen))
        }
      | CursorEscaped(After) => CursorEscaped(After)
      | Succeeded((ze, u_gen)) =>
        let zblock = ZExp.BlockZE(lines, ze);
        Succeeded(Statics.ana_fix_holes_zblock(ctx, u_gen, zblock, ty));
      }
    }
  }
and ana_perform_exp =
    (ctx: Contexts.t, a: t, (ze, u_gen): (ZExp.t, MetaVarGen.t), ty: HTyp.t)
    : result((ZExp.t, MetaVarGen.t)) =>
  switch (a, ze) {
  | (_, CursorEO(_, Var(InHole(TypeInconsistent, _) as err, _, _)))
  | (_, CursorEO(_, NumLit(InHole(TypeInconsistent, _) as err, _)))
  | (_, CursorEO(_, BoolLit(InHole(TypeInconsistent, _) as err, _)))
  | (_, CursorEO(_, ListNil(InHole(TypeInconsistent, _) as err)))
  | (_, CursorEI(_, Lam(InHole(TypeInconsistent, _) as err, _, _, _)))
  | (_, CursorEI(_, Inj(InHole(TypeInconsistent, _) as err, _, _)))
  | (_, CursorEI(_, Case(InHole(TypeInconsistent, _) as err, _, _, _)))
  | (
      _,
      CursorEI(_, ApPalette(InHole(TypeInconsistent, _) as err, _, _, _)),
    ) =>
    let ze' = ZExp.set_err_status_t(NotInHole, ze);
    let e' = ZExp.erase(ze');
    switch (Statics.syn_exp(ctx, e')) {
    | None => Failed
    | Some(ty1) =>
      switch (syn_perform_exp(ctx, a, (ze', ty1, u_gen))) {
      | (Failed | CursorEscaped(_)) as result => result
      | Succeeded((ze', ty1', u_gen')) =>
        if (HTyp.consistent(ty1', ty)) {
          Succeeded((ze', u_gen'));
        } else {
          Succeeded((ZExp.set_err_status_t(err, ze'), u_gen'));
        }
      }
    };
  /* Movement */
  | (MoveTo(path), _) =>
    let e = ZExp.erase(ze);
    switch (Path.follow_exp(path, e)) {
    | Some(ze') => Succeeded((ze', u_gen))
    | None => Failed
    };
  | (MoveToPrevHole, _) =>
    switch (Path.prev_hole_path(Path.holes_ze(ze, []))) {
    | None => Failed
    | Some(path) => ana_perform_exp(ctx, MoveTo(path), (ze, u_gen), ty)
    }
  | (MoveToNextHole, _) =>
    switch (Path.next_hole_path(Path.holes_ze(ze, []))) {
    | None => Failed
    | Some(path) => ana_perform_exp(ctx, MoveTo(path), (ze, u_gen), ty)
    }
  /* Backspace & Delete */
  | (Backspace, _) when ZExp.is_before_exp(ze) => CursorEscaped(Before)
  | (Delete, _) when ZExp.is_after_exp(ze) => CursorEscaped(After)
  | (
      Backspace,
      CursorEO(
        Char(_) as outer_cursor,
        (
          EmptyHole(_) | Var(_, _, _) | NumLit(_, _) | BoolLit(_, _) |
          ListNil(_)
        ) as eo,
      ),
    )
      when
        ZExp.is_valid_outer_cursor_exp(outer_cursor, eo)
        && !ZExp.is_before_exp(ze) =>
    let (ze, u_gen) =
      switch (eo) {
      | EmptyHole(_) => (ZExp.place_before_exp(EO(eo)), u_gen)
      | _ => ZExp.new_EmptyHole(u_gen)
      };
    Succeeded((ze, u_gen));
  | (
      Delete,
      CursorEO(
        Char(_) as outer_cursor,
        (
          EmptyHole(_) | Var(_, _, _) | NumLit(_, _) | BoolLit(_, _) |
          ListNil(_)
        ) as eo,
      ),
    )
      when
        ZExp.is_valid_outer_cursor_exp(outer_cursor, eo)
        && !ZExp.is_after_exp(ze) =>
    let (ze, u_gen) =
      switch (eo) {
      | EmptyHole(_) => (ZExp.place_after_exp(EO(eo)), u_gen)
      | _ => ZExp.new_EmptyHole(u_gen)
      };
    Succeeded((ze, u_gen));
  | (
      Backspace | Delete,
      CursorEO(
        Char(_),
        EmptyHole(_) | Var(_, _, _) | NumLit(_, _) | BoolLit(_, _) |
        ListNil(_),
      ),
    ) =>
    /* invalid cursor position */
    Failed
  | (Backspace, CursorEI(BeforeChild(1, After), Lam(_, p, Some(_), block)))
  | (Delete, CursorEI(BeforeChild(1, Before), Lam(_, p, Some(_), block))) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => Failed
    | Some((ty1, ty2)) =>
      let (p, ctx, u_gen) = Statics.ana_fix_holes_pat(ctx, u_gen, p, ty1);
      let (block, u_gen) =
        Statics.ana_fix_holes_block(ctx, u_gen, block, ty2);
      let ze = ZExp.LamZP(NotInHole, ZPat.place_after(p), None, block);
      Succeeded((ze, u_gen));
    }
  /* TODO consider deletion of type ascription on case */
  | (
      Backspace,
      CursorEI(
        (BeforeChild(_, Before) | ClosingDelimiter(Before)) as inner_cursor,
        ei,
      ),
    )
      when
        ZExp.is_valid_inner_cursor_exp(inner_cursor, ei)
        && !ZExp.is_before_exp(ze) =>
    move_to_prev_node_pos_exp(ze, ze =>
      Succeeded(Statics.ana_fix_holes_zexp(ctx, u_gen, ze, ty))
    )
  | (Delete, CursorEI(BeforeChild(_, After) as inner_cursor, ei))
      when
        ZExp.is_valid_inner_cursor_exp(inner_cursor, ei)
        && !ZExp.is_after_exp(ze) =>
    move_to_next_node_pos_exp(ze, ze =>
      Succeeded(Statics.ana_fix_holes_zexp(ctx, u_gen, ze, ty))
    )
  | (
      Backspace,
      CursorEI(
        (BeforeChild(_, After) | ClosingDelimiter(After)) as inner_cursor,
        (
          Lam(_, _, _, _) | Inj(_, _, _) | Case(_, _, _, _) |
          Parenthesized(_) |
          ApPalette(_, _, _, _)
        ) as ei,
      ),
    )
  | (
      Delete,
      CursorEI(
        (BeforeChild(_, Before) | ClosingDelimiter(Before)) as inner_cursor,
        (
          Lam(_, _, _, _) | Inj(_, _, _) | Case(_, _, _, _) |
          Parenthesized(_) |
          ApPalette(_, _, _, _)
        ) as ei,
      ),
    )
      when ZExp.is_valid_inner_cursor_exp(inner_cursor, ei) =>
    let (ze, u_gen) = ZExp.new_EmptyHole(u_gen);
    Succeeded((ze, u_gen));
  | (
      Backspace | Delete,
      CursorEI(
        BeforeChild(_, _) | ClosingDelimiter(_),
        Lam(_, _, _, _) | Inj(_, _, _) | Case(_, _, _, _) | Parenthesized(_) |
        ApPalette(_, _, _, _),
      ),
    ) =>
    Failed
  | (
      Backspace,
      CaseZR(
        err,
        e1,
        (prefix, CursorR(BeforeChild(0 | 1, After), _), suffix),
        ann,
      ),
    ) =>
    switch (split_last(prefix)) {
    | Some((prefix, last_of_prefix)) =>
      let zrule = ZExp.place_after_rule(last_of_prefix);
      let ze = ZExp.CaseZR(err, e1, (prefix, zrule, suffix), ann);
      Succeeded((ze, u_gen));
    | None =>
      switch (suffix) {
      | [first_of_suffix, ...suffix] =>
        let zrule = ZExp.place_before_rule(first_of_suffix);
        let ze = ZExp.CaseZR(err, e1, (prefix, zrule, suffix), ann);
        Succeeded((ze, u_gen));
      | [] =>
        let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
        Succeeded((CaseZR(err, e1, ([], zrule, []), ann), u_gen));
      }
    }
  | (
      Delete,
      CaseZR(
        err,
        e1,
        (prefix, CursorR(BeforeChild(0 | 1, Before), _), suffix),
        ann,
      ),
    ) =>
    switch (suffix) {
    | [first_of_suffix, ...suffix] =>
      let zrule = ZExp.place_before_rule(first_of_suffix);
      let ze = ZExp.CaseZR(err, e1, (prefix, zrule, suffix), ann);
      Succeeded((ze, u_gen));
    | [] =>
      switch (split_last(prefix)) {
      | Some((prefix, last_of_prefix)) =>
        let zrule = ZExp.place_after_rule(last_of_prefix);
        let ze = ZExp.CaseZR(err, e1, (prefix, zrule, suffix), ann);
        Succeeded((ze, u_gen));
      | None =>
        let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
        Succeeded((CaseZR(err, e1, ([], zrule, []), ann), u_gen));
      }
    }
  | (
      Backspace,
      CaseZR(
        err,
        e1,
        (prefix, CursorR(BeforeChild(0, Before), rule), suffix),
        ann,
      ),
    ) =>
    switch (split_last(prefix)) {
    | None =>
      let rules = prefix @ [rule] @ suffix;
      let ze1 = ZExp.place_after_block(e1);
      let ze = ZExp.CaseZE(err, ze1, rules, ann);
      Succeeded((ze, u_gen));
    | Some((prefix, last_of_prefix)) =>
      let zrule = ZExp.place_after_rule(last_of_prefix);
      let zrules = (prefix, zrule, [rule, ...suffix]);
      let ze = ZExp.CaseZR(err, e1, zrules, ann);
      Succeeded((ze, u_gen));
    }
  | (
      Backspace,
      CaseZR(
        err,
        e1,
        (prefix, CursorR(BeforeChild(1, Before), Rule(p, clause)), suffix),
        ann,
      ),
    ) =>
    let zrule = ZExp.RuleZP(ZPat.place_after(p), clause);
    let ze = ZExp.CaseZR(err, e1, (prefix, zrule, suffix), ann);
    Succeeded((ze, u_gen));
  | (
      Delete,
      CaseZR(
        err,
        e1,
        (prefix, CursorR(BeforeChild(0, After), Rule(p, clause)), suffix),
        ann,
      ),
    ) =>
    let zrule = ZExp.RuleZP(ZPat.place_before(p), clause);
    let ze = ZExp.CaseZR(err, e1, (prefix, zrule, suffix), ann);
    Succeeded((ze, u_gen));
  | (
      Delete,
      CaseZR(
        err,
        e1,
        (prefix, CursorR(BeforeChild(1, After), Rule(p, clause)), suffix),
        ann,
      ),
    ) =>
    let zrule = ZExp.RuleZE(p, ZExp.place_before_block(clause));
    let ze = ZExp.CaseZR(err, e1, (prefix, zrule, suffix), ann);
    Succeeded((ze, u_gen));
  | (
      Backspace | Delete,
      CaseZR(_, _, (_, CursorR(BeforeChild(_, _), _), _), _),
    ) =>
    Failed
  | (Backspace, OpSeqZ(_, CursorEO(_, EmptyHole(_)) as ze0, surround))
      when ZExp.opseqz_preceded_by_Space(ze0, surround) =>
    switch (surround) {
    /* should never happen given guard */
    | EmptyPrefix(_) => Failed
    /* ... + [k-1]   _<|   ==>   ... + [k-1]| */
    | EmptySuffix(prefix) =>
      let e =
        switch (prefix) {
        | ExpPrefix(e1, _space) => e1
        | SeqPrefix(seq, _space) => UHExp.EI(ExpUtil.mk_OpSeq(seq))
        };
      Succeeded(
        Statics.ana_fix_holes_zexp(ctx, u_gen, ZExp.place_after_exp(e), ty),
      );
    /* ... + [k-1]   _<| + [k+1] + ...   ==>   ... + [k-1]| + [k+1] + ... */
    | BothNonEmpty(prefix, suffix) =>
      switch (prefix) {
      | ExpPrefix(e1, _space) =>
        let ze1 = ZExp.place_after_exp(e1);
        let ze = ExpUtil.mk_OpSeqZ(ze1, EmptyPrefix(suffix));
        Succeeded(Statics.ana_fix_holes_zexp(ctx, u_gen, ze, ty));
      | SeqPrefix(seq, _space) =>
        let (prefix: ZExp.opseq_prefix, e0) =
          switch (seq) {
          | ExpOpExp(e1, op, e2) => (ExpPrefix(e1, op), e2)
          | SeqOpExp(seq, op, e1) => (SeqPrefix(seq, op), e1)
          };
        let ze0 = ZExp.place_after_exp(e0);
        let ze = ExpUtil.mk_OpSeqZ(ze0, BothNonEmpty(prefix, suffix));
        Succeeded(Statics.ana_fix_holes_zexp(ctx, u_gen, ze, ty));
      }
    }
  | (Delete, OpSeqZ(_, CursorEO(_, EmptyHole(_)) as ze0, surround))
      when ZExp.opseqz_followed_by_Space(ze0, surround) =>
    switch (surround) {
    /* should never happen given guard */
    | EmptySuffix(_) => Failed
    /* |>_   [1] + ...   ==>   |[1] + ... */
    | EmptyPrefix(suffix) =>
      let e =
        switch (suffix) {
        | ExpSuffix(_space, e1) => e1
        | SeqSuffix(_space, seq) => UHExp.EI(ExpUtil.mk_OpSeq(seq))
        };
      Succeeded(
        Statics.ana_fix_holes_zexp(ctx, u_gen, ZExp.place_before_exp(e), ty),
      );
    /* ... + [k-1] + |>_   [k+1] + ...   ==>   ... + [k-1] + |[k+1] + ... */
    | BothNonEmpty(prefix, suffix) =>
      switch (suffix) {
      | ExpSuffix(_space, e1) =>
        let ze1 = ZExp.place_before_exp(e1);
        let ze = ExpUtil.mk_OpSeqZ(ze1, EmptySuffix(prefix));
        Succeeded(Statics.ana_fix_holes_zexp(ctx, u_gen, ze, ty));
      | SeqSuffix(_space, seq) =>
        let (e0, suffix: ZExp.opseq_suffix) =
          switch (seq) {
          | ExpOpExp(e1, op, e2) => (e1, ExpSuffix(op, e2))
          | SeqOpExp(seq, op, e1) => (e1, SeqSuffix(op, seq))
          };
        let ze0 = ZExp.place_before_exp(e0);
        let ze = ExpUtil.mk_OpSeqZ(ze0, BothNonEmpty(prefix, suffix));
        Succeeded(Statics.ana_fix_holes_zexp(ctx, u_gen, ze, ty));
      }
    }
  | (Backspace, OpSeqZ(_, CursorEO(_, EmptyHole(_)) as ze0, surround))
      when ZExp.opseqz_followed_by_Space(ze0, surround) =>
    switch (surround) {
    /* should never happen given guard */
    | EmptySuffix(_) => Failed
    /* _<|   [1] + ...   ==>   |[1] + ... */
    | EmptyPrefix(suffix) =>
      let e =
        switch (suffix) {
        | ExpSuffix(_space, e1) => e1
        | SeqSuffix(_space, seq) => UHExp.EI(ExpUtil.mk_OpSeq(seq))
        };
      Succeeded(
        Statics.ana_fix_holes_zexp(ctx, u_gen, ZExp.place_before_exp(e), ty),
      );
    /* ... + [k-1] + _<|   [k+1] + ...   ==>   ... + [k-1] +| [k+1] + ... */
    | BothNonEmpty(prefix, suffix) =>
      let seq =
        switch (suffix) {
        | ExpSuffix(_space, e1) =>
          OperatorSeq.opseq_of_prefix_and_exp(prefix, e1)
        | SeqSuffix(_space, seq) =>
          OperatorSeq.opseq_of_prefix_and_seq(prefix, seq)
        };
      let ei = ExpUtil.mk_OpSeq(seq);
      let k = OperatorSeq.prefix_length(prefix);
      let ze = ZExp.CursorEI(BeforeChild(k, After), ei);
      Succeeded(Statics.ana_fix_holes_zexp(ctx, u_gen, ze, ty));
    }
  | (Delete, OpSeqZ(_, CursorEO(_, EmptyHole(_)) as ze0, surround))
      when ZExp.opseqz_preceded_by_Space(ze0, surround) =>
    switch (surround) {
    /* should never happen given guard */
    | EmptyPrefix(_) => Failed
    /* ... + [k-1]   |>_   ==>   ... + [k-1]| */
    | EmptySuffix(prefix) =>
      let e =
        switch (prefix) {
        | ExpPrefix(e1, _space) => e1
        | SeqPrefix(seq, _space) => UHExp.EI(ExpUtil.mk_OpSeq(seq))
        };
      Succeeded(
        Statics.ana_fix_holes_zexp(ctx, u_gen, ZExp.place_after_exp(e), ty),
      );
    /* ... + [k-1]   |>_ + [k+1] + ...   ==>   ... + [k-1] |+ [k+1] + ... */
    | BothNonEmpty(prefix, suffix) =>
      let seq =
        switch (prefix) {
        | ExpPrefix(e1, _space) =>
          OperatorSeq.opseq_of_exp_and_suffix(e1, suffix)
        | SeqPrefix(seq, _space) =>
          OperatorSeq.opseq_of_seq_and_suffix(seq, suffix)
        };
      let ei = ExpUtil.mk_OpSeq(seq);
      let k = OperatorSeq.prefix_length(prefix);
      let ze = ZExp.CursorEI(BeforeChild(k, After), ei);
      Succeeded(Statics.ana_fix_holes_zexp(ctx, u_gen, ze, ty));
    }
  /* ... + [k-1] +<| [k] + ...   ==>   ... + [k-1]| [k] */
  | (
      Backspace,
      CursorEI(BeforeChild(k, After) as inner_cursor, OpSeq(_, seq) as ei),
    )
      when ZExp.is_valid_inner_cursor_exp(inner_cursor, ei) =>
    switch (OperatorSeq.split(k - 1, seq)) {
    | None => Failed /* should never happen */
    | Some((e0, surround)) =>
      switch (OperatorSeq.replace_following_op(surround, UHExp.Space)) {
      | None => Failed /* should never happen */
      | Some(surround) =>
        Succeeded(
          make_and_ana_OpSeqZ(
            ctx,
            u_gen,
            ZExp.place_after_exp(e0),
            surround,
            ty,
          ),
        )
      }
    }
  /* ... + [k-1] |>+ [k] + ...   ==>   ... + [k-1] |[k] */
  | (
      Delete,
      CursorEI(BeforeChild(k, Before) as inner_cursor, OpSeq(_, seq) as ei),
    )
      when ZExp.is_valid_inner_cursor_exp(inner_cursor, ei) =>
    switch (OperatorSeq.split(k, seq)) {
    | None => Failed /* should never happen */
    | Some((e0, surround)) =>
      switch (OperatorSeq.replace_preceding_op(surround, UHExp.Space)) {
      | None => Failed /* should never happen */
      | Some(surround) =>
        Succeeded(
          make_and_ana_OpSeqZ(
            ctx,
            u_gen,
            ZExp.place_before_exp(e0),
            surround,
            ty,
          ),
        )
      }
    }
  /* invalid cursor position */
  | (
      Backspace | Delete,
      CursorEI(BeforeChild(_, _) | ClosingDelimiter(_), OpSeq(_, _)),
    ) =>
    Failed
  /* Construction */
  | (Construct(SLine), CursorEO(_, _) | CursorEI(_, _))
  | (Construct(SLet), CursorEO(_, _) | CursorEI(_, _)) =>
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
    Succeeded((ze, u_gen));
  | (
      Construct(SLine),
      CaseZR(err, e1, (prefix, RuleZE(_, ze) as zrule, suffix), ann),
    )
      when ZExp.is_after_block(ze) =>
    let prev_rule = ZExp.erase_rule(zrule);
    let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
    let prefix = prefix @ [prev_rule];
    let ze = ZExp.CaseZR(err, e1, (prefix, zrule, suffix), ann);
    Succeeded((ze, u_gen));
  | (
      Construct(SLine),
      CaseZR(err, e1, (prefix, RuleZP(zp, _) as zrule, suffix), ann),
    )
      when ZPat.is_after(zp) =>
    let prev_rule = ZExp.erase_rule(zrule);
    let (zrule, u_gen) = ZExp.empty_zrule(u_gen);
    let prefix = prefix @ [prev_rule];
    let ze = ZExp.CaseZR(err, e1, (prefix, zrule, suffix), ann);
    Succeeded((ze, u_gen));
  | (Construct(SCase), ze1) when ZExp.is_before_exp(ze1) =>
    let e1 = ZExp.erase(ze1);
    let (ze, u_gen) =
      switch (e1) {
      | EO(EmptyHole(_)) =>
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
    Succeeded((ze, u_gen));
  | (Construct(SCase), CursorEO(_, _) | CursorEI(_, _)) => Failed
  | (Construct(SParenthesized), CursorEO(_, _) | CursorEI(_, _)) =>
    Succeeded((ParenthesizedZ(ZExp.wrap_in_block(ze)), u_gen))
  | (Construct(SAsc), LamZP(err_status, zp, None, e1)) =>
    let ze =
      ZExp.LamZA(
        err_status,
        ZPat.erase(zp),
        ZTyp.place_before(TO(Hole)),
        e1,
      );
    Succeeded((ze, u_gen));
  | (Construct(SAsc), LamZP(err_status, zp, Some(uty1), e1)) =>
    /* just move the cursor over if there is already an ascription */
    let ze =
      ZExp.LamZA(err_status, ZPat.erase(zp), ZTyp.place_before(uty1), e1);
    Succeeded((ze, u_gen));
  | (Construct(SAsc), CursorEI(_, Case(_, e1, rules, None))) =>
    let ze = ZExp.CaseZA(NotInHole, e1, rules, ZTyp.place_before(TO(Hole)));
    Succeeded((ze, u_gen));
  | (Construct(SAsc), CursorEI(_, Case(_, e1, rules, Some(uty)))) =>
    /* just move the cursor over if there is already an ascription */
    let ze = ZExp.CaseZA(NotInHole, e1, rules, ZTyp.place_before(uty));
    Succeeded((ze, u_gen));
  | (Construct(SAsc), CursorEO(_, _) | CursorEI(_, _)) => Failed
  | (Construct(SLam), CursorEO(_, _) | CursorEI(_, _)) =>
    let e = ZExp.erase(ze);
    switch (HTyp.matched_arrow(ty)) {
    | Some((_, ty2)) =>
      let (e, u_gen) = Statics.ana_fix_holes_exp(ctx, u_gen, e, ty2);
      let (zp, u_gen) = ZPat.new_EmptyHole(u_gen);
      let ze = ZExp.LamZP(NotInHole, zp, None, UHExp.wrap_in_block(e));
      Succeeded((ze, u_gen));
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
      Succeeded((ze, u_gen));
    };
  | (Construct(SInj(side)), (CursorEO(_, _) | CursorEI(_, _)) as ze1) =>
    switch (HTyp.matched_sum(ty)) {
    | Some((tyL, tyR)) =>
      let ty1 = pick_side(side, tyL, tyR);
      let (ze1, u_gen) = Statics.ana_fix_holes_zexp(ctx, u_gen, ze1, ty1);
      let ze = ZExp.InjZ(NotInHole, side, ZExp.wrap_in_block(ze1));
      Succeeded((ze, u_gen));
    | None =>
      let (ze1, _, u_gen) = Statics.syn_fix_holes_zexp(ctx, u_gen, ze1);
      let (u, u_gen) = MetaVarGen.next(u_gen);
      let ze =
        ZExp.InjZ(
          InHole(TypeInconsistent, u),
          side,
          ZExp.wrap_in_block(ze1),
        );
      Succeeded((ze, u_gen));
    }
  | (Construct(SOp(os)), OpSeqZ(_, ze0, surround))
      when ZExp.is_after_exp(ze0) =>
    switch (exp_op_of(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded(
        abs_perform_Construct_SOp_After_surround(
          ZExp.new_EmptyHole,
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
        ),
      )
    }
  | (Construct(SOp(os)), OpSeqZ(_, ze0, surround))
      when ZExp.is_before_exp(ze0) =>
    switch (exp_op_of(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded(
        abs_perform_Construct_SOp_Before_surround(
          ZExp.erase,
          ZExp.new_EmptyHole,
          (ctx, u_gen, ze, surround) =>
            make_and_ana_OpSeqZ(ctx, u_gen, ze, surround, ty),
          UHExp.is_Space,
          UHExp.Space,
          ctx,
          u_gen,
          ze0,
          op,
          surround,
        ),
      )
    }
  | (Construct(SOp(os)), _) when ZExp.is_after_exp(ze) =>
    switch (exp_op_of(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded(
        abs_perform_Construct_SOp_After(
          UHExp.bidelimit,
          ZExp.new_EmptyHole,
          (ctx, u_gen, ze, surround) =>
            make_and_ana_OpSeqZ(ctx, u_gen, ze, surround, ty),
          ctx,
          u_gen,
          ZExp.erase(ze),
          op,
        ),
      )
    }
  | (Construct(SOp(os)), _) when ZExp.is_before_exp(ze) =>
    switch (exp_op_of(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded(
        abs_perform_Construct_SOp_Before(
          UHExp.bidelimit,
          ZExp.new_EmptyHole,
          (ctx, u_gen, ze, surround) =>
            make_and_ana_OpSeqZ(ctx, u_gen, ze, surround, ty),
          ctx,
          u_gen,
          ZExp.erase(ze),
          op,
        ),
      )
    }
  | (Construct(SOp(_)), _) => Failed
  /* Zipper Cases */
  | (_, ParenthesizedZ(zblock)) =>
    switch (ana_perform_block(ctx, a, (zblock, u_gen), ty)) {
    | Failed => Failed
    | CursorEscaped(Before) =>
      move_to_prev_node_pos_exp(ze, ze => Succeeded((ze, u_gen)))
    | CursorEscaped(After) =>
      move_to_next_node_pos_exp(ze, ze => Succeeded((ze, u_gen)))
    | Succeeded((zblock, u_gen)) =>
      Succeeded((ParenthesizedZ(zblock), u_gen))
    }
  | (_, LamZP(err, zp, ann, block)) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => Failed
    | Some((ty1_given, ty2)) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => ty1_given
        };
      switch (ana_perform_pat(ctx, u_gen, a, zp, ty1)) {
      | Failed => Failed
      | CursorEscaped(Before) =>
        move_to_prev_node_pos_exp(ze, ze => Succeeded((ze, u_gen)))
      | CursorEscaped(After) =>
        move_to_next_node_pos_exp(ze, ze => Succeeded((ze, u_gen)))
      | Succeeded((zp, ctx, u_gen)) =>
        let (block, u_gen) =
          Statics.ana_fix_holes_block(ctx, u_gen, block, ty2);
        let ze = ZExp.LamZP(err, zp, ann, block);
        Succeeded((ze, u_gen));
      };
    }
  | (_, LamZA(_, p, zann, block)) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => Failed
    | Some((ty1_given, ty2)) =>
      switch (perform_ty(a, zann)) {
      | Failed => Failed
      | CursorEscaped(Before) =>
        move_to_prev_node_pos_exp(ze, ze => Succeeded((ze, u_gen)))
      | CursorEscaped(After) =>
        move_to_next_node_pos_exp(ze, ze => Succeeded((ze, u_gen)))
      | Succeeded(zann) =>
        let ty1 = UHTyp.expand(ZTyp.erase(zann));
        HTyp.consistent(ty1, ty1_given)
          ? {
            let (p, ctx, u_gen) =
              Statics.ana_fix_holes_pat(ctx, u_gen, p, ty1);
            let (block, u_gen) =
              Statics.ana_fix_holes_block(ctx, u_gen, block, ty2);
            let ze = ZExp.LamZA(NotInHole, p, zann, block);
            Succeeded((ze, u_gen));
          }
          : {
            let (p, ctx, u_gen) =
              Statics.ana_fix_holes_pat(ctx, u_gen, p, ty1);
            let (block, _, u_gen) =
              Statics.syn_fix_holes_block(ctx, u_gen, block);
            let (u, u_gen) = MetaVarGen.next(u_gen);
            let ze = ZExp.LamZA(InHole(TypeInconsistent, u), p, zann, block);
            Succeeded((ze, u_gen));
          };
      }
    }
  | (_, LamZE(err, p, ann, zblock)) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => Failed
    | Some((ty1_given, ty2)) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => ty1_given
        };
      switch (Statics.ana_pat(ctx, p, ty1)) {
      | None => Failed
      | Some(ctx) =>
        switch (ana_perform_block(ctx, a, (zblock, u_gen), ty2)) {
        | Failed => Failed
        | CursorEscaped(Before) =>
          move_to_prev_node_pos_exp(ze, ze => Succeeded((ze, u_gen)))
        | CursorEscaped(After) =>
          move_to_next_node_pos_exp(ze, ze => Succeeded((ze, u_gen)))
        | Succeeded((zblock, u_gen)) =>
          let ze = ZExp.LamZE(err, p, ann, zblock);
          Succeeded((ze, u_gen));
        }
      };
    }
  | (_, InjZ(err, side, zblock)) =>
    switch (HTyp.matched_sum(ty)) {
    | None => Failed
    | Some((ty1, ty2)) =>
      let picked = pick_side(side, ty1, ty2);
      switch (ana_perform_block(ctx, a, (zblock, u_gen), picked)) {
      | Failed => Failed
      | CursorEscaped(Before) =>
        move_to_prev_node_pos_exp(ze, ze => Succeeded((ze, u_gen)))
      | CursorEscaped(After) =>
        move_to_next_node_pos_exp(ze, ze => Succeeded((ze, u_gen)))
      | Succeeded((zblock, u_gen)) =>
        Succeeded((InjZ(err, side, zblock), u_gen))
      };
    }
  | (_, CaseZE(_, zblock, rules, ann)) =>
    switch (Statics.syn_block(ctx, ZExp.erase_block(zblock))) {
    | None => Failed
    | Some(ty1) =>
      switch (syn_perform_block(ctx, a, (zblock, ty1, u_gen))) {
      | Failed => Failed
      | CursorEscaped(Before) =>
        move_to_prev_node_pos_exp(ze, ze => Succeeded((ze, u_gen)))
      | CursorEscaped(After) =>
        move_to_next_node_pos_exp(ze, ze => Succeeded((ze, u_gen)))
      | Succeeded((zblock, ty1, u_gen)) =>
        let (rules, u_gen) =
          Statics.ana_fix_holes_rules(ctx, u_gen, rules, ty1, ty);
        let ze = ZExp.CaseZE(NotInHole, zblock, rules, ann);
        Succeeded((ze, u_gen));
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
        | CursorEscaped(Before) =>
          move_to_prev_node_pos_exp(ze, ze => Succeeded((ze, u_gen)))
        | CursorEscaped(After) =>
          move_to_next_node_pos_exp(ze, ze => Succeeded((ze, u_gen)))
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
          Succeeded((ze, u_gen));
        }
      | RuleZE(p, zclause) =>
        switch (Statics.ana_pat(ctx, p, ty1)) {
        | None => Failed
        | Some(ctx) =>
          switch (ana_perform_block(ctx, a, (zclause, u_gen), ty)) {
          | Failed => Failed
          | CursorEscaped(Before) =>
            move_to_prev_node_pos_exp(ze, ze => Succeeded((ze, u_gen)))
          | CursorEscaped(After) =>
            move_to_next_node_pos_exp(ze, ze => Succeeded((ze, u_gen)))
          | Succeeded((zclause, u_gen)) =>
            let zrule = ZExp.RuleZE(p, zclause);
            let ze =
              ZExp.CaseZR(
                NotInHole,
                block,
                ZList.replace_z(zrules, zrule),
                ann,
              );
            Succeeded((ze, u_gen));
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
      | CursorEscaped(Before) =>
        move_to_prev_node_pos_exp(ze, ze => Succeeded((ze, u_gen)))
      | CursorEscaped(After) =>
        move_to_next_node_pos_exp(ze, ze => Succeeded((ze, u_gen)))
      | Succeeded(zann) =>
        let ty2 = UHTyp.expand(ZTyp.erase(zann));
        let (rules, u_gen) =
          Statics.ana_fix_holes_rules(ctx, u_gen, rules, ty1, ty2);
        let ze = ZExp.CaseZA(NotInHole, block, rules, zann);
        Succeeded(Statics.ana_fix_holes_zexp(ctx, u_gen, ze, ty));
      }
    }
  | (_, OpSeqZ(_, ze0, surround)) =>
    let i = OperatorSeq.surround_prefix_length(surround);
    switch (ZExp.erase(ze)) {
    | EI(OpSeq(skel, seq)) =>
      switch (Statics.ana_skel(ctx, skel, seq, ty, Some(i))) {
      | Some(Some(mode)) =>
        switch (mode) {
        | Statics.AnalyzedAgainst(ty0) =>
          switch (ana_perform_exp(ctx, a, (ze0, u_gen), ty0)) {
          | Failed => Failed
          | CursorEscaped(Before) =>
            move_to_prev_node_pos_exp(ze, ze => Succeeded((ze, u_gen)))
          | CursorEscaped(After) =>
            move_to_next_node_pos_exp(ze, ze => Succeeded((ze, u_gen)))
          | Succeeded((ze0', u_gen)) =>
            let ze0'' = ZExp.bidelimit(ze0');
            Succeeded((ZExp.OpSeqZ(skel, ze0'', surround), u_gen));
          }
        | Statics.Synthesized(ty0) =>
          switch (syn_perform_exp(ctx, a, (ze0, ty0, u_gen))) {
          | Failed => Failed
          | CursorEscaped(Before) =>
            move_to_prev_node_pos_exp(ze, ze => Succeeded((ze, u_gen)))
          | CursorEscaped(After) =>
            move_to_next_node_pos_exp(ze, ze => Succeeded((ze, u_gen)))
          | Succeeded((ze0', _ty0', u_gen)) =>
            let ze0'' = ZExp.bidelimit(ze0');
            Succeeded(make_and_ana_OpSeqZ(ctx, u_gen, ze0'', surround, ty));
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
    ana_perform_exp_subsume(ctx, a, (ze, u_gen), ty)
  /* Invalid actions at expression level */
  | (Construct(SNum), _)
  | (Construct(SBool), _)
  | (Construct(SList), _)
  | (Construct(SWild), _) => Failed
  }
and ana_perform_exp_subsume =
    (ctx: Contexts.t, a: t, (ze, u_gen): (ZExp.t, MetaVarGen.t), ty: HTyp.t)
    : result((ZExp.t, MetaVarGen.t)) =>
  switch (Statics.syn_exp(ctx, ZExp.erase(ze))) {
  | None => Failed
  | Some(ty1) =>
    switch (syn_perform_exp(ctx, a, (ze, ty1, u_gen))) {
    | (Failed | CursorEscaped(_)) as err => err
    | Succeeded((ze', ty1', u_gen')) =>
      if (HTyp.consistent(ty, ty1')) {
        Succeeded((ze', u_gen'));
      } else {
        let (ze'', u_gen'') = ZExp.make_t_inconsistent(u_gen', ze');
        Succeeded((ze'', u_gen''));
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
    switch (ci.sort) {
    | IsLine(_) => true
    | IsExpr(_) => true
    | IsPat(_) => false
    | IsType => false
    | IsBlock(_) => false
    }
  | Construct(SInj(_)) =>
    switch (ci.sort) {
    | IsLine(_) => true
    | IsExpr(_) => true
    | IsPat(_) => true
    | IsType => false
    | IsBlock(_) => false
    }
  | Construct(SListNil) =>
    switch (ci.sort) {
    | IsLine(LO(EmptyLine)) => true
    | IsLine(ExpLine(EO(EmptyHole(_)))) => true
    | IsLine(_) => false
    | IsExpr(EO(EmptyHole(_))) => true
    | IsExpr(_) => false
    | IsPat(PO(EmptyHole(_))) => true
    | IsPat(_) => false
    | IsType => false
    | IsBlock(_) => false
    }
  | Construct(SOp(SArrow))
  | Construct(SOp(SVBar))
  | Construct(SList) =>
    switch (ci.sort) {
    | IsType => true
    | IsLine(_)
    | IsExpr(_)
    | IsPat(_) => false
    | IsBlock(_) => false
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
      ? switch (syn_perform_block(ctx, a, edit_state)) {
        | Succeeded(_)
        | CursorEscaped(_) => true
        | Failed => false
        }
      : true
  };

let can_enter_varchar = (ci: CursorInfo.t): bool =>
  switch (ci.sort) {
  | IsLine(LO(EmptyLine))
  | IsLine(ExpLine(EO(EmptyHole(_))))
  | IsExpr(EO(Var(_, _, _)))
  | IsExpr(EO(EmptyHole(_)))
  | IsExpr(EO(BoolLit(_, _)))
  | IsPat(PO(Var(_, _, _)))
  | IsPat(PO(EmptyHole(_)))
  | IsPat(PO(BoolLit(_, _))) => true
  | IsExpr(EO(NumLit(_, _)))
  | IsPat(PO(NumLit(_, _))) =>
    switch (ci.side) {
    | O(Char(0)) => true
    | _ => false
    }
  | IsBlock(_)
  | IsLine(_)
  | IsExpr(_)
  | IsPat(_)
  | IsType => false
  };

let can_enter_numeral = (ci: CursorInfo.t): bool =>
  switch (ci.sort) {
  | IsLine(LO(EmptyLine))
  | IsLine(ExpLine(EO(EmptyHole(_))))
  | IsExpr(EO(NumLit(_, _)))
  | IsExpr(EO(EmptyHole(_)))
  | IsPat(PO(NumLit(_, _)))
  | IsPat(PO(EmptyHole(_))) => true
  | IsBlock(_)
  | IsLine(_)
  | IsExpr(_)
  | IsPat(_)
  | IsType => false
  };

let can_construct_palette = (ci: CursorInfo.t): bool =>
  switch (ci.sort) {
  | IsLine(LO(EmptyLine))
  | IsLine(ExpLine(EO(EmptyHole(_))))
  | IsExpr(EO(EmptyHole(_))) => true
  | _ => false
  };
