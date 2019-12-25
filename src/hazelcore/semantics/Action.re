let _TEST_PERFORM = false;
open GeneralUtil;
open Sexplib.Std;

[@deriving sexp]
type operator_shape =
  | SMinus
  | SPlus
  | STimes
  | SLessThan
  | SGreaterThan
  | SEquals
  | SSpace
  | SComma
  | SArrow
  | SVBar
  | SCons
  | SAnd
  | SOr;

[@deriving sexp]
type shape =
  | SParenthesized
  | SChar(string)
  | SAsc
  | SLam
  | SListNil
  | SInj(InjSide.t)
  | SLet
  | SLine
  | SCase
  | SOp(operator_shape)
  | SApPalette(PaletteName.t);

[@deriving sexp]
type t =
  | MoveTo(CursorPath.t)
  | MoveToBefore(CursorPath.steps)
  | MoveLeft
  | MoveRight
  | MoveToNextHole
  | MoveToPrevHole
  | UpdateApPalette(SpliceGenMonad.t(SerializedModel.t))
  | Delete
  | Backspace
  | Construct(shape);

type result('success) =
  | Succeeded('success)
  | CursorEscaped(Side.t)
  | Failed;

let escape: Side.t => t =
  fun
  | Before => MoveLeft
  | After => MoveRight;

module Typ = {
  let operator_of_shape = (os: operator_shape): option(UHTyp.operator) =>
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
    | SGreaterThan
    | SEquals
    | SSpace
    | SCons => None
    };

  let shape_of_operator = (op: UHTyp.operator): operator_shape =>
    switch (op) {
    | Arrow => SArrow
    | Prod => SComma
    | Sum => SVBar
    };

  let mk_ZOpSeq =
    ZOpSeq.mk(
      ~associate=Associator.Typ.associate,
      ~erase_zoperand=ZTyp.erase_zoperand,
      ~erase_zoperator=ZTyp.erase_zoperator,
    );

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
    mk_ZOpSeq(ZOperand(zoperand, surround));
  };

  let rec perform = (a: t, zty: ZTyp.t): result(ZTyp.t) =>
    switch (a) {
    /* Movement */
    | MoveTo(path) =>
      switch (CursorPath.Typ.follow(path, zty |> ZTyp.erase)) {
      | None => Failed
      | Some(zty) => Succeeded(zty)
      }
    | MoveToBefore(steps) =>
      switch (
        CursorPath.Typ.follow_steps(~side=Before, steps, zty |> ZTyp.erase)
      ) {
      | None => Failed
      | Some(zty) => Succeeded(zty)
      }
    | MoveToPrevHole =>
      switch (CursorPath.(prev_hole_steps(Typ.holes_z(zty, [])))) {
      | None => Failed
      | Some(steps) => perform(MoveToBefore(steps), zty)
      }
    | MoveToNextHole =>
      switch (CursorPath.(next_hole_steps(Typ.holes_z(zty, [])))) {
      | None => Failed
      | Some(steps) => perform(MoveToBefore(steps), zty)
      }
    | MoveLeft =>
      zty
      |> ZTyp.move_cursor_left
      |> Opt.map_default(~default=CursorEscaped(Before), z => Succeeded(z))
    | MoveRight =>
      zty
      |> ZTyp.move_cursor_right
      |> Opt.map_default(~default=CursorEscaped(After), z => Succeeded(z))
    | _ =>
      switch (zty) {
      | ZT1(zty1) => perform_opseq(a, zty1)
      | ZT0(zty0) => perform_operand(a, zty0)
      }
    }
  and perform_opseq =
      (a: t, ZOpSeq(skel, zseq) as zopseq: ZTyp.zopseq): result(ZTyp.t) =>
    switch (a, zseq) {
    /* Invalid actions at the type level */
    | (
        UpdateApPalette(_) |
        Construct(
          SAsc | SLet | SLine | SLam | SListNil | SInj(_) | SCase |
          SApPalette(_),
        ),
        _,
      )
    /* Invalid cursor positions */
    | (_, ZOperator((OnText(_) | OnDelim(_), _), _)) => Failed

    /* Movement handled at top level */
    | (
        MoveTo(_) | MoveToBefore(_) | MoveToPrevHole | MoveToNextHole | MoveLeft |
        MoveRight,
        _,
      ) =>
      Failed

    /* Deletion */

    | (Delete, ZOperator((OnOp(After as side), _), _))
    | (Backspace, ZOperator((OnOp(Before as side), _), _)) =>
      perform(escape(side), ZT1(zopseq))

    /* Delete before operator == Backspace after operator */
    | (Delete, ZOperator((OnOp(Before), op), surround)) =>
      perform_opseq(
        Backspace,
        ZOpSeq(skel, ZOperator((OnOp(After), op), surround)),
      )
    /* ... + [k-2] + [k-1] +<| [k] + ...   ==>   ... + [k-2] + [k-1]| + ...
     * (for now until we have proper type constructors) */
    | (Backspace, ZOperator((OnOp(After), _), (prefix, suffix))) =>
      let S(prefix_hd, new_prefix) = prefix;
      let zoperand = prefix_hd |> ZTyp.place_after_operand;
      let S(_, new_suffix) = suffix;
      Succeeded(
        ZT1(mk_ZOpSeq(ZOperand(zoperand, (new_prefix, new_suffix)))),
      );

    /* Construction */
    /* construction on operators becomes movement... */
    | (Construct(SOp(SSpace)), ZOperator((OnOp(After), _), _)) =>
      perform(MoveRight, ZT1(zopseq))
    /* ...or construction after movement */
    | (Construct(_) as a, ZOperator((OnOp(side), _), _)) =>
      switch (perform(escape(side), ZT1(zopseq))) {
      | Failed
      | CursorEscaped(_) => Failed
      | Succeeded(zty) => perform(a, zty)
      }

    /* Space becomes movement until we have proper type constructors */
    | (Construct(SOp(SSpace)), ZOperand(zoperand, _))
        when ZTyp.is_after_zoperand(zoperand) =>
      perform(MoveRight, ZT1(zopseq))

    | (Construct(SOp(os)), ZOperand(CursorT(_) as zoperand, surround)) =>
      switch (operator_of_shape(os)) {
      | None => Failed
      | Some(op) =>
        Succeeded(ZT1(construct_operator(op, zoperand, surround)))
      }

    /* Zipper */
    | (_, ZOperand(zoperand, (prefix, suffix) as surround)) =>
      switch (perform_operand(a, zoperand)) {
      | Failed => Failed
      | CursorEscaped(side) => perform(escape(side), ZT1(zopseq))
      | Succeeded(ZT0(zoperand)) =>
        Succeeded(ZT1(mk_ZOpSeq(ZOperand(zoperand, surround))))
      | Succeeded(ZT1(ZOpSeq(_, zseq))) =>
        switch (zseq) {
        | ZOperand(zoperand, (inner_prefix, inner_suffix)) =>
          let new_prefix = Seq.affix_affix(inner_prefix, prefix);
          let new_suffix = Seq.affix_affix(inner_suffix, suffix);
          Succeeded(
            ZT1(mk_ZOpSeq(ZOperand(zoperand, (new_prefix, new_suffix)))),
          );
        | ZOperator(zoperator, (inner_prefix, inner_suffix)) =>
          let new_prefix = Seq.seq_affix(inner_prefix, prefix);
          let new_suffix = Seq.seq_affix(inner_suffix, suffix);
          Succeeded(
            ZT1(mk_ZOpSeq(ZOperator(zoperator, (new_prefix, new_suffix)))),
          );
        }
      }
    }
  and perform_operand = (a: t, zoperand: ZTyp.zoperand): result(ZTyp.t) =>
    switch (a, zoperand) {
    /* Invalid actions at the type level */
    | (
        UpdateApPalette(_) |
        Construct(
          SAsc | SLet | SLine | SLam | SListNil | SInj(_) | SCase |
          SApPalette(_),
        ),
        _,
      ) =>
      Failed

    /* Invalid cursor positions */
    | (_, CursorT(OnText(_) | OnOp(_), _)) => Failed
    | (_, CursorT(cursor, operand))
        when !ZTyp.is_valid_cursor_operand(cursor, operand) =>
      Failed

    /* Movement handled at top level */
    | (
        MoveTo(_) | MoveToBefore(_) | MoveToPrevHole | MoveToNextHole | MoveLeft |
        MoveRight,
        _,
      ) =>
      Failed

    /* Backspace and Delete */

    /* ( _ <|)   ==>   ( _| ) */
    | (Backspace, CursorT(OnDelim(_, Before), _)) =>
      let zty = ZTyp.ZT0(zoperand);
      zty |> ZTyp.is_before ? CursorEscaped(Before) : perform(MoveLeft, zty);
    /* (|> _ )   ==>   ( |_ ) */
    | (Delete, CursorT(OnDelim(_, After), _)) =>
      let zty = ZTyp.ZT0(zoperand);
      zty |> ZTyp.is_after ? CursorEscaped(After) : perform(MoveRight, zty);

    /* Delete before delimiter == Backspace after delimiter */
    | (Delete, CursorT(OnDelim(k, Before), operand)) =>
      perform_operand(Backspace, CursorT(OnDelim(k, After), operand))

    | (Backspace, CursorT(OnDelim(_, After), Hole)) =>
      Succeeded(ZT0(ZTyp.place_before_operand(Hole)))

    | (Backspace, CursorT(OnDelim(_, After), Unit | Num | Bool)) =>
      Succeeded(ZT0(ZTyp.place_before_operand(Hole)))

    /* ( _ )<|  ==>  _| */
    /* (<| _ )  ==>  |_ */
    | (
        Backspace,
        CursorT(OnDelim(k, After), Parenthesized(body) | List(body)),
      ) =>
      let place_cursor = k == 0 ? ZTyp.place_before : ZTyp.place_after;
      Succeeded(body |> place_cursor);

    /* Construction */

    | (Construct(SOp(SSpace)), CursorT(OnDelim(_, After), _)) =>
      perform(MoveRight, ZT0(zoperand))
    | (Construct(_) as a, CursorT(OnDelim(_, side), _))
        when
          !ZTyp.is_before_zoperand(zoperand)
          && !ZTyp.is_after_zoperand(zoperand) =>
      switch (perform(escape(side), ZT0(zoperand))) {
      | (Failed | CursorEscaped(_)) as err => err
      | Succeeded(zty) => perform(a, zty)
      }

    | (Construct(SChar("N")), CursorT(_, Hole)) =>
      Succeeded(UHTyp.T0(Num) |> ZTyp.place_after)
    | (Construct(SChar("B")), CursorT(_, Hole)) =>
      Succeeded(UHTyp.T0(Bool) |> ZTyp.place_after)
    | (Construct(SChar("L")), CursorT(_)) =>
      Succeeded(ZT0(ListZ(ZT0(zoperand))))
    | (Construct(SChar(_)), CursorT(_)) => Failed

    | (Construct(SParenthesized), CursorT(_)) =>
      Succeeded(ZT0(ParenthesizedZ(ZT0(zoperand))))

    | (Construct(SOp(os)), CursorT(_)) =>
      switch (operator_of_shape(os)) {
      | None => Failed
      | Some(op) =>
        Succeeded(ZT1(construct_operator(op, zoperand, (E, E))))
      }

    /* Zipper Cases */
    | (_, ParenthesizedZ(zbody)) =>
      switch (perform(a, zbody)) {
      | Failed => Failed
      | CursorEscaped(side) => perform(escape(side), ZT0(zoperand))
      | Succeeded(zbody) => Succeeded(ZT0(ParenthesizedZ(zbody)))
      }
    | (_, ListZ(zbody)) =>
      switch (perform(a, zbody)) {
      | Failed => Failed
      | CursorEscaped(side) => perform(escape(side), ZT0(zoperand))
      | Succeeded(zbody) => Succeeded(ZT0(ListZ(zbody)))
      }
    };
};

let check_valid = (x: Var.t, result: result('a)): result('a) =>
  if (Var.is_valid(x)) {
    result;
  } else {
    Failed;
  };

let _syn_insert_text =
    (
      ~mk_syn_text:
         (Contexts.t, MetaVarGen.t, int, string) => result('success),
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      (caret_index: int, insert_text: string),
      text: string,
    )
    : result('success) =>
  mk_syn_text(
    ctx,
    u_gen,
    caret_index + String.length(insert_text),
    text |> insert_string(caret_index, insert_text),
  );
let _ana_insert_text =
    (
      ~mk_ana_text:
         (Contexts.t, MetaVarGen.t, int, string, HTyp.t) => result('success),
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      (caret_index: int, insert_text: string),
      text: string,
      ty: HTyp.t,
    )
    : result('success) =>
  mk_ana_text(
    ctx,
    u_gen,
    caret_index + String.length(insert_text),
    text |> insert_string(caret_index, insert_text),
    ty,
  );

let _syn_backspace_text =
    (
      ~mk_syn_text:
         (Contexts.t, MetaVarGen.t, int, string) => result('success),
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      caret_index: int,
      text: string,
    )
    : result('success) =>
  if (caret_index == 0) {
    CursorEscaped(Before);
  } else {
    let new_text = text |> backspace_string(caret_index);
    mk_syn_text(ctx, u_gen, caret_index - 1, new_text);
  };
let _ana_backspace_text =
    (
      ~mk_ana_text:
         (Contexts.t, MetaVarGen.t, int, string, HTyp.t) => result('success),
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      caret_index: int,
      text: string,
      ty: HTyp.t,
    )
    : result('success) =>
  if (caret_index == 0) {
    CursorEscaped(Before);
  } else {
    let new_text = text |> backspace_string(caret_index);
    mk_ana_text(ctx, u_gen, caret_index - 1, new_text, ty);
  };

let _syn_delete_text =
    (
      ~mk_syn_text:
         (Contexts.t, MetaVarGen.t, int, string) => result('success),
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      caret_index: int,
      text: string,
    )
    : result('success) =>
  if (caret_index == String.length(text)) {
    CursorEscaped(After);
  } else {
    let new_text = text |> delete_string(caret_index);
    mk_syn_text(ctx, u_gen, caret_index, new_text);
  };
let _ana_delete_text =
    (
      ~mk_ana_text:
         (Contexts.t, MetaVarGen.t, int, string, HTyp.t) => result('success),
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      caret_index: int,
      text: string,
      ty: HTyp.t,
    )
    : result('success) =>
  if (caret_index == String.length(text)) {
    CursorEscaped(After);
  } else {
    let new_text = text |> delete_string(caret_index);
    mk_ana_text(ctx, u_gen, caret_index, new_text, ty);
  };

let _construct_operator_after_zoperand =
    (
      ~is_Space: 'operator => bool,
      ~new_EmptyHole: MetaVarGen.t => ('operand, MetaVarGen.t),
      ~erase_zoperand: 'zoperand => 'operand,
      ~place_before_operand: 'operand => 'zoperand,
      ~place_after_operator: 'operator => option('zoperator),
      u_gen: MetaVarGen.t,
      operator: 'operator,
      zoperand: 'zoperand,
      (prefix, suffix): Seq.operand_surround('operand, 'operator),
    )
    : (ZSeq.t('operand, 'operator, 'zoperand, 'zoperator), MetaVarGen.t) => {
  let operand = zoperand |> erase_zoperand;
  switch (operator |> place_after_operator) {
  | None =>
    // operator == Space
    // ... + [k]| + [k+1] + ...   ==>   ... + [k]  |_ + [k+1] + ...
    let (hole, u_gen) = u_gen |> new_EmptyHole;
    let new_prefix = Seq.A(operator, S(operand, prefix));
    let new_zoperand = hole |> place_before_operand;
    (ZOperand(new_zoperand, (new_prefix, suffix)), u_gen);
  | Some(zoperator) =>
    let new_prefix = Seq.S(operand, prefix);
    let (new_suffix, u_gen) =
      switch (suffix) {
      | A(op, new_suffix) when op |> is_Space =>
        // zoperator overwrites Space
        // ... + [k]|  [k+1] + ...   ==>   ... + [k] *| [k+1] + ...
        (new_suffix, u_gen)
      | _ =>
        // ... + [k]| + [k+1] + ...   ==>   ... + [k] *| _ + [k+1] + ...
        let (hole, u_gen) = u_gen |> new_EmptyHole;
        (Seq.S(hole, suffix), u_gen);
      };
    (ZOperator(zoperator, (new_prefix, new_suffix)), u_gen);
  };
};
let _construct_operator_before_zoperand =
    (
      ~is_Space: 'operator => bool,
      ~new_EmptyHole: MetaVarGen.t => ('operand, MetaVarGen.t),
      ~erase_zoperand: 'zoperand => 'operand,
      ~place_before_operand: 'operand => 'zoperand,
      ~place_after_operator: 'operator => option('zoperator),
      u_gen: MetaVarGen.t,
      operator: 'operator,
      zoperand: 'zoperand,
      (prefix, suffix): Seq.operand_surround('operand, 'operator),
    )
    : (ZSeq.t('operand, 'operator, 'zoperand, 'zoperator), MetaVarGen.t) => {
  // symmetric to construct_operator_after_zoperand
  let mirror_surround = (suffix, prefix);
  let (mirror_zseq, u_gen) =
    _construct_operator_after_zoperand(
      ~is_Space,
      ~new_EmptyHole,
      ~erase_zoperand,
      ~place_before_operand,
      ~place_after_operator,
      u_gen,
      operator,
      zoperand,
      mirror_surround,
    );
  let zseq: ZSeq.t(_) =
    switch (mirror_zseq) {
    | ZOperator(z, (suffix, prefix)) => ZOperator(z, (prefix, suffix))
    | ZOperand(z, (suffix, prefix)) => ZOperand(z, (prefix, suffix))
    };
  (zseq, u_gen);
};

let _delete_operator =
    (
      ~space: 'operator,
      ~is_EmptyHole: 'operand => bool,
      ~place_before_operand: 'operand => 'zoperand,
      ~place_after_operand: 'operand => 'zoperand,
      ~place_after_operator: 'operator => option('zoperator),
      (prefix, suffix): Seq.operator_surround('operand, 'operator),
    )
    : ZSeq.t('operand, 'operator, 'zoperand, 'zoperator) =>
  switch (prefix, suffix) {
  /* _ +<| [1] + ...   ==>   |[1] + ... */
  | (S(operand, E as prefix), S(suffix_hd, new_suffix))
      when operand |> is_EmptyHole =>
    let zoperand = suffix_hd |> place_before_operand;
    ZOperand(zoperand, (prefix, new_suffix));

  | (S(operand, A(operator, prefix_tl) as prefix), suffix)
      when operand |> is_EmptyHole =>
    switch (operator |> place_after_operator) {
    /* ... + [k-2]  _ +<| [k] + ...   ==>  ... + [k-2] |[k] + ... */
    | None =>
      let S(suffix_hd, new_suffix) = suffix;
      let zoperand = suffix_hd |> place_before_operand;
      ZOperand(zoperand, (prefix, new_suffix));
    /* ... + [k-2] + _ +<| [k] + ...   ==>   ... + [k-2] +| [k] + ... */
    | Some(zoperator) =>
      let new_prefix = prefix_tl;
      ZOperator(zoperator, (new_prefix, suffix));
    }

  /* ... + [k-1] +<|  _ + ...   ==>   ... + [k-1]| + ... */
  | (S(prefix_hd, new_prefix), S(operand, new_suffix))
      when operand |> is_EmptyHole =>
    let zoperand = prefix_hd |> place_after_operand;
    ZOperand(zoperand, (new_prefix, new_suffix));

  /* ... + [k-1] +<| [k] + ...   ==>   ... + [k-1]| [k] + ... */
  | (S(prefix_hd, new_prefix), _) =>
    let zoperand = prefix_hd |> place_after_operand;
    let new_suffix = Seq.A(space, suffix);
    ZOperand(zoperand, (new_prefix, new_suffix));
  };

module Pat = {
  let operator_of_shape: operator_shape => option(UHPat.operator) =
    fun
    | SComma => Some(Comma)
    | SSpace => Some(Space)
    | SCons => Some(Cons)
    | SAnd
    | SOr
    | SMinus
    | SPlus
    | STimes
    | SLessThan
    | SGreaterThan
    | SEquals
    | SArrow
    | SVBar => None;

  let shape_of_operator: UHPat.operator => operator_shape =
    fun
    | Comma => SComma
    | Space => SSpace
    | Cons => SCons;

  let mk_ZOpSeq =
    ZOpSeq.mk(
      ~associate=Associator.Pat.associate,
      ~erase_zoperand=ZPat.erase_zoperand,
      ~erase_zoperator=ZPat.erase_zoperator,
    );

  let mk_and_syn_fix_ZOpSeq =
      (ctx: Contexts.t, u_gen: MetaVarGen.t, zseq: ZPat.zseq)
      : (ZPat.t, HTyp.t, Contexts.t, MetaVarGen.t) => {
    let zopseq = mk_ZOpSeq(zseq);
    Statics.Pat.syn_fix_holes_z(ctx, u_gen, ZP1(zopseq));
  };
  let mk_and_ana_fix_ZOpSeq =
      (ctx: Contexts.t, u_gen: MetaVarGen.t, zseq: ZPat.zseq, ty: HTyp.t)
      : (ZPat.t, Contexts.t, MetaVarGen.t) => {
    let zopseq = mk_ZOpSeq(zseq);
    Statics.Pat.ana_fix_holes_z(ctx, u_gen, ZP1(zopseq), ty);
  };

  let mk_syn_result = (ctx: Contexts.t, u_gen: MetaVarGen.t, zp: ZPat.t) =>
    switch (Statics.Pat.syn(ctx, zp |> ZPat.erase)) {
    | None => Failed
    | Some((ty, ctx)) => Succeeded((zp, ty, ctx, u_gen))
    };
  let mk_ana_result =
      (ctx: Contexts.t, u_gen: MetaVarGen.t, zp: ZPat.t, ty: HTyp.t) =>
    switch (Statics.Pat.ana(ctx, zp |> ZPat.erase, ty)) {
    | None => Failed
    | Some(ctx) => Succeeded((zp, ctx, u_gen))
    };

  let mk_syn_text =
      (ctx: Contexts.t, u_gen: MetaVarGen.t, caret_index: int, text: string) => {
    let text_cursor = CursorPosition.OnText(caret_index);
    switch (TextShape.of_text(text)) {
    | None =>
      if (text |> is_empty_string) {
        let (zhole, u_gen) = u_gen |> ZPat.new_EmptyHole;
        Succeeded((ZPat.ZP0(zhole), HTyp.Hole, ctx, u_gen));
      } else {
        Failed;
      }
    | Some(Underscore) =>
      let zp = ZPat.ZP0(CursorP(OnDelim(0, After), UHPat.wild()));
      Succeeded((zp, HTyp.Hole, ctx, u_gen));
    | Some(NumLit(n)) =>
      let zp = ZPat.ZP0(CursorP(text_cursor, UHPat.numlit(n)));
      Succeeded((zp, HTyp.Num, ctx, u_gen));
    | Some(BoolLit(b)) =>
      let zp = ZPat.ZP0(CursorP(text_cursor, UHPat.boollit(b)));
      Succeeded((zp, HTyp.Bool, ctx, u_gen));
    | Some(Keyword(k)) =>
      let (u, u_gen) = u_gen |> MetaVarGen.next;
      let var =
        UHPat.var(
          ~var_err=InVarHole(Keyword(k), u),
          k |> Keyword.to_string,
        );
      let zp = ZPat.ZP0(CursorP(text_cursor, var));
      Succeeded((zp, HTyp.Hole, ctx, u_gen));
    | Some(Var(x)) =>
      let ctx = Contexts.extend_gamma(ctx, (x, Hole));
      let zp = ZPat.ZP0(CursorP(text_cursor, UHPat.var(x)));
      Succeeded((zp, HTyp.Hole, ctx, u_gen));
    };
  };

  let mk_ana_text =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        caret_index: int,
        text: string,
        ty: HTyp.t,
      ) => {
    let text_cursor = CursorPosition.OnText(caret_index);
    switch (TextShape.of_text(text)) {
    | None =>
      if (text |> is_empty_string) {
        let (zhole, u_gen) = u_gen |> ZPat.new_EmptyHole;
        Succeeded((ZPat.ZP0(zhole), ctx, u_gen));
      } else {
        Failed;
      }
    | Some(Underscore) =>
      let zp = ZPat.ZP0(CursorP(OnDelim(0, After), UHPat.wild()));
      Succeeded((zp, ctx, u_gen));
    | Some(NumLit(_))
    | Some(BoolLit(_)) =>
      switch (mk_syn_text(ctx, u_gen, caret_index, text)) {
      | (Failed | CursorEscaped(_)) as err => err
      | Succeeded((zp, ty', ctx, u_gen)) =>
        if (HTyp.consistent(ty, ty')) {
          Succeeded((zp, ctx, u_gen));
        } else {
          let (zp, u_gen) = zp |> ZPat.make_inconsistent(u_gen);
          Succeeded((zp, ctx, u_gen));
        }
      }
    | Some(Keyword(k)) =>
      let (u, u_gen) = u_gen |> MetaVarGen.next;
      let var = UHPat.var(~var_err=InVarHole(Keyword(k), u), text);
      let zp = ZPat.ZP0(CursorP(text_cursor, var));
      Succeeded((zp, ctx, u_gen));
    | Some(Var(x)) =>
      let ctx = Contexts.extend_gamma(ctx, (x, ty));
      let zp = ZPat.ZP0(CursorP(text_cursor, UHPat.var(x)));
      Succeeded((zp, ctx, u_gen));
    };
  };

  let syn_insert_text = _syn_insert_text(~mk_syn_text);
  let ana_insert_text = _ana_insert_text(~mk_ana_text);
  let syn_backspace_text = _syn_backspace_text(~mk_syn_text);
  let ana_backspace_text = _ana_backspace_text(~mk_ana_text);
  let syn_delete_text = _syn_delete_text(~mk_syn_text);
  let ana_delete_text = _ana_delete_text(~mk_ana_text);

  let delete_operator =
    _delete_operator(
      ~space=UHPat.Space,
      ~is_EmptyHole=UHPat.is_EmptyHole,
      ~place_before_operand=ZPat.place_before_operand,
      ~place_after_operand=ZPat.place_after_operand,
      ~place_after_operator=ZPat.place_after_operator,
    );

  let construct_operator_before_zoperand =
    _construct_operator_before_zoperand(
      ~is_Space=UHPat.is_Space,
      ~new_EmptyHole=UHPat.new_EmptyHole,
      ~erase_zoperand=ZPat.erase_zoperand,
      ~place_before_operand=ZPat.place_before_operand,
      ~place_after_operator=ZPat.place_after_operator,
    );
  let construct_operator_after_zoperand =
    _construct_operator_after_zoperand(
      ~is_Space=UHPat.is_Space,
      ~new_EmptyHole=UHPat.new_EmptyHole,
      ~erase_zoperand=ZPat.erase_zoperand,
      ~place_before_operand=ZPat.place_before_operand,
      ~place_after_operator=ZPat.place_after_operator,
    );

  let resurround_z =
      (zp: ZPat.t, (prefix, suffix) as surround: ZPat.operand_surround)
      : ZPat.zseq =>
    switch (zp) {
    | ZP0(zoperand) => ZOperand(zoperand, surround)
    | ZP1(ZOpSeq(_, ZOperand(zoperand, (inner_prefix, inner_suffix)))) =>
      let new_prefix = Seq.affix_affix(inner_prefix, prefix);
      let new_suffix = Seq.affix_affix(inner_suffix, suffix);
      ZOperand(zoperand, (new_prefix, new_suffix));
    | ZP1(ZOpSeq(_, ZOperator(zoperator, (inner_prefix, inner_suffix)))) =>
      let new_prefix = Seq.seq_affix(inner_prefix, prefix);
      let new_suffix = Seq.seq_affix(inner_suffix, suffix);
      ZOperator(zoperator, (new_prefix, new_suffix));
    };

  let rec syn_perform =
          (ctx: Contexts.t, u_gen: MetaVarGen.t, a: t, zp: ZPat.t)
          : result((ZPat.t, HTyp.t, Contexts.t, MetaVarGen.t)) =>
    switch (a) {
    /* Movement */
    | MoveTo(path) =>
      switch (CursorPath.Pat.follow(path, zp |> ZPat.erase)) {
      | None => Failed
      | Some(zp) => mk_syn_result(ctx, u_gen, zp)
      }
    | MoveToBefore(steps) =>
      switch (
        CursorPath.Pat.follow_steps(~side=Before, steps, zp |> ZPat.erase)
      ) {
      | None => Failed
      | Some(zp) => mk_syn_result(ctx, u_gen, zp)
      }
    | MoveToPrevHole =>
      switch (CursorPath.(prev_hole_steps(Pat.holes_z(zp, [])))) {
      | None => Failed
      | Some(steps) => syn_perform(ctx, u_gen, MoveToBefore(steps), zp)
      }
    | MoveToNextHole =>
      switch (CursorPath.(next_hole_steps(Pat.holes_z(zp, [])))) {
      | None => Failed
      | Some(steps) => syn_perform(ctx, u_gen, MoveToBefore(steps), zp)
      }
    | MoveLeft =>
      switch (zp |> ZPat.move_cursor_left) {
      | None => CursorEscaped(Before)
      | Some(zp) => mk_syn_result(ctx, u_gen, zp)
      }
    | MoveRight =>
      switch (zp |> ZPat.move_cursor_right) {
      | None => CursorEscaped(After)
      | Some(zp) => mk_syn_result(ctx, u_gen, zp)
      }
    | _ =>
      switch (zp) {
      | ZP1(zp1) => syn_perform_opseq(ctx, u_gen, a, zp1)
      | ZP0(zp0) => syn_perform_operand(ctx, u_gen, a, zp0)
      }
    }
  and syn_perform_opseq =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        a: t,
        ZOpSeq(skel, zseq) as zopseq: ZPat.zopseq,
      )
      : result((ZPat.t, HTyp.t, Contexts.t, MetaVarGen.t)) =>
    switch (a, zseq) {
    /* Invalid cursor positions */
    | (_, ZOperator((OnText(_) | OnDelim(_), _), _)) => Failed

    /* Invalid actions */
    | (UpdateApPalette(_), ZOperator(_)) => Failed

    /* Movement handled at top level */
    | (
        MoveTo(_) | MoveToBefore(_) | MoveToPrevHole | MoveToNextHole | MoveLeft |
        MoveRight,
        _,
      ) =>
      Failed

    /* Deletion */

    | (Delete, ZOperator((OnOp(After as side), _), _))
    | (Backspace, ZOperator((OnOp(Before as side), _), _)) =>
      syn_perform(ctx, u_gen, escape(side), ZP1(zopseq))

    /* Delete before operator == Backspace after operator */
    | (Delete, ZOperator((OnOp(Before), op), surround)) =>
      let new_zp =
        ZPat.ZP1(ZOpSeq(skel, ZOperator((OnOp(After), op), surround)));
      syn_perform(ctx, u_gen, Backspace, new_zp);

    /* ... + [k-1] +<| [k] + ... */
    | (Backspace, ZOperator((OnOp(After), _), surround)) =>
      let new_zseq = delete_operator(surround);
      Succeeded(mk_and_syn_fix_ZOpSeq(ctx, u_gen, new_zseq));

    /* ... + [k-1]  <|_ + [k+1] + ...  ==>   ... + [k-1]| + [k+1] + ... */
    | (
        Backspace,
        ZOperand(
          CursorP(_, EmptyHole(_)) as zhole,
          (A(Space, prefix_tl), suffix),
        ),
      )
        when ZPat.is_before_zoperand(zhole) =>
      let S(operand, new_prefix) = prefix_tl;
      let zoperand = operand |> ZPat.place_after_operand;
      let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, suffix));
      Succeeded(mk_and_syn_fix_ZOpSeq(ctx, u_gen, new_zseq));

    /* ... + [k-1] + _|>  [k+1] + ...  ==>   ... + [k-1] + |[k+1] + ... */
    | (
        Delete,
        ZOperand(
          CursorP(_, EmptyHole(_)) as zhole,
          (prefix, A(Space, suffix_tl)),
        ),
      )
        when ZPat.is_after_zoperand(zhole) =>
      let S(operand, new_suffix) = suffix_tl;
      let zoperand = operand |> ZPat.place_before_operand;
      let new_zseq = ZSeq.ZOperand(zoperand, (prefix, new_suffix));
      Succeeded(mk_and_syn_fix_ZOpSeq(ctx, u_gen, new_zseq));

    /* Construction */

    /* construction on operators either becomes movement... */
    | (Construct(SOp(SSpace)), ZOperator(zoperator, _))
        when ZPat.is_after_zoperator(zoperator) =>
      syn_perform(ctx, u_gen, MoveRight, ZP1(zopseq))
    /* ...or construction after movement */
    | (Construct(_), ZOperator(zoperator, _)) =>
      let move_cursor =
        ZPat.is_before_zoperator(zoperator)
          ? ZPat.move_cursor_left : ZPat.move_cursor_right;
      switch (ZPat.ZP1(zopseq) |> move_cursor) {
      | None => Failed
      | Some(zp) => syn_perform(ctx, u_gen, a, zp)
      };

    /* Zipper */

    | (_, ZOperand(zoperand, surround)) =>
      switch (CursorInfo.Pat.syn_cursor_info(ctx, ZP1(zopseq))) {
      | None => Failed
      | Some(ci) =>
        switch (ci |> CursorInfo.type_mode) {
        | None => Failed
        | Some(Syn) =>
          switch (syn_perform(ctx, u_gen, a, ZP0(zoperand))) {
          | Failed => Failed
          | CursorEscaped(side) =>
            syn_perform(ctx, u_gen, escape(side), ZP1(zopseq))
          | Succeeded((zp, _, _, u_gen)) =>
            let zseq = resurround_z(zp, surround);
            Succeeded(mk_and_syn_fix_ZOpSeq(ctx, u_gen, zseq));
          }
        | Some(Ana(ty_zoperand)) =>
          switch (
            ana_perform(ctx, u_gen, a, ZPat.ZP0(zoperand), ty_zoperand)
          ) {
          | Failed => Failed
          | CursorEscaped(side) =>
            syn_perform(ctx, u_gen, escape(side), ZP1(zopseq))
          | Succeeded((zp, _, u_gen)) =>
            let new_zseq = resurround_z(zp, surround);
            Succeeded(mk_and_syn_fix_ZOpSeq(ctx, u_gen, new_zseq));
          }
        }
      }
    }
  and syn_perform_operand =
      (ctx: Contexts.t, u_gen: MetaVarGen.t, a: t, zoperand: ZPat.zoperand)
      : result((ZPat.t, HTyp.t, Contexts.t, MetaVarGen.t)) => {
    switch (a, zoperand) {
    /* Invalid cursor positions */
    | (
        _,
        CursorP(
          OnText(_),
          EmptyHole(_) | Wild(_) | ListNil(_) | Parenthesized(_) | Inj(_),
        ) |
        CursorP(OnDelim(_), Var(_) | NumLit(_) | BoolLit(_)) |
        CursorP(OnOp(_), _),
      ) =>
      Failed
    | (_, CursorP(cursor, operand))
        when !ZPat.is_valid_cursor_operand(cursor, operand) =>
      Failed

    /* Invalid actions */
    | (
        Construct(SApPalette(_) | SAsc | SLet | SLine | SLam | SCase) |
        UpdateApPalette(_),
        _,
      ) =>
      Failed

    /* Movement handled at top level */
    | (
        MoveTo(_) | MoveToBefore(_) | MoveToPrevHole | MoveToNextHole | MoveLeft |
        MoveRight,
        _,
      ) =>
      Failed

    /* Backspace and Delete */

    | (Backspace, _) when ZPat.is_before_zoperand(zoperand) =>
      CursorEscaped(Before)
    | (Delete, _) when ZPat.is_after_zoperand(zoperand) =>
      CursorEscaped(After)

    | (Backspace, CursorP(_, EmptyHole(_) as operand)) =>
      let zp = ZPat.(ZP0(place_before_operand(operand)));
      zp |> ZPat.is_after
        ? Succeeded((zp, Hole, ctx, u_gen)) : CursorEscaped(Before);
    | (Delete, CursorP(_, EmptyHole(_) as operand)) =>
      let zp = ZPat.(ZP0(place_after_operand(operand)));
      zp |> ZPat.is_before
        ? Succeeded((zp, Hole, ctx, u_gen)) : CursorEscaped(After);

    /* ( _ <|)   ==>   ( _| ) */
    | (Backspace, CursorP(OnDelim(_, Before), _)) =>
      syn_perform(ctx, u_gen, MoveLeft, ZP0(zoperand))
    /* (|> _ )   ==>   ( |_ ) */
    | (Delete, CursorP(OnDelim(_, After), _)) =>
      syn_perform(ctx, u_gen, MoveRight, ZP0(zoperand))

    /* Delete before delimiter == Backspace after delimiter */
    | (Delete, CursorP(OnDelim(k, Before), operand)) =>
      let new_zp = ZPat.ZP0(CursorP(OnDelim(k, After), operand));
      syn_perform(ctx, u_gen, Backspace, new_zp);

    | (Backspace, CursorP(OnDelim(_, After), ListNil(_) | Wild(_))) =>
      let (zhole, u_gen) = ZPat.new_EmptyHole(u_gen);
      let zp = ZPat.ZP0(zhole);
      Succeeded((zp, Hole, ctx, u_gen));

    | (Delete, CursorP(OnText(j), Var(_, _, x))) =>
      syn_delete_text(ctx, u_gen, j, x)
    | (Delete, CursorP(OnText(j), NumLit(_, n))) =>
      syn_delete_text(ctx, u_gen, j, string_of_int(n))
    | (Delete, CursorP(OnText(j), BoolLit(_, b))) =>
      syn_delete_text(ctx, u_gen, j, string_of_bool(b))

    | (Backspace, CursorP(OnText(j), Var(_, _, x))) =>
      syn_backspace_text(ctx, u_gen, j, x)
    | (Backspace, CursorP(OnText(j), NumLit(_, n))) =>
      syn_backspace_text(ctx, u_gen, j, string_of_int(n))
    | (Backspace, CursorP(OnText(j), BoolLit(_, b))) =>
      syn_backspace_text(ctx, u_gen, j, string_of_bool(b))

    /* ( _ )<|  ==>  _| */
    /* (<| _ )  ==>  |_ */
    | (
        Backspace,
        CursorP(OnDelim(k, After), Parenthesized(body) | Inj(_, _, body)),
      ) =>
      let place_cursor = k == 0 ? ZPat.place_before : ZPat.place_after;
      Succeeded(
        Statics.Pat.syn_fix_holes_z(ctx, u_gen, body |> place_cursor),
      );

    /* Construction */

    | (Construct(SOp(SSpace)), CursorP(OnDelim(_, After), _)) =>
      syn_perform(ctx, u_gen, MoveRight, ZP0(zoperand))
    | (Construct(_), CursorP(OnDelim(_, side), _))
        when
          !ZPat.is_before_zoperand(zoperand)
          && !ZPat.is_after_zoperand(zoperand) =>
      switch (syn_perform(ctx, u_gen, escape(side), ZP0(zoperand))) {
      | Failed
      | CursorEscaped(_) => Failed
      | Succeeded((zp, _, _, u_gen)) => syn_perform(ctx, u_gen, a, zp)
      }

    | (Construct(SChar(s)), CursorP(_, EmptyHole(_))) =>
      syn_insert_text(ctx, u_gen, (0, s), "")
    | (Construct(SChar(s)), CursorP(OnDelim(_, side), Wild(_))) =>
      let index =
        switch (side) {
        | Before => 0
        | After => 1
        };
      syn_insert_text(ctx, u_gen, (index, s), "_");
    | (Construct(SChar(s)), CursorP(OnText(j), Var(_, _, x))) =>
      syn_insert_text(ctx, u_gen, (j, s), x)
    | (Construct(SChar(s)), CursorP(OnText(j), NumLit(_, n))) =>
      syn_insert_text(ctx, u_gen, (j, s), string_of_int(n))
    | (Construct(SChar(s)), CursorP(OnText(j), BoolLit(_, b))) =>
      syn_insert_text(ctx, u_gen, (j, s), string_of_bool(b))
    | (Construct(SChar(_)), CursorP(_)) => Failed

    | (Construct(SListNil), CursorP(_, EmptyHole(_))) =>
      let zp = ZPat.(ZP0(place_after_operand(ListNil(NotInHole))));
      Succeeded((zp, List(Hole), ctx, u_gen));
    | (Construct(SListNil), CursorP(_, _)) => Failed

    | (Construct(SParenthesized), CursorP(_)) =>
      mk_syn_result(ctx, u_gen, ZPat.ZP0(ParenthesizedZ(ZP0(zoperand))))

    | (Construct(SInj(side)), CursorP(_) as zbody) =>
      let zp = ZPat.ZP0(InjZ(NotInHole, side, ZP0(zbody)));
      switch (Statics.Pat.syn(ctx, zp |> ZPat.erase)) {
      | None => Failed
      | Some((body_ty, ctx)) =>
        let ty =
          switch (side) {
          | L => HTyp.Sum(body_ty, Hole)
          | R => HTyp.Sum(Hole, body_ty)
          };
        Succeeded((zp, ty, ctx, u_gen));
      };

    | (Construct(SOp(os)), CursorP(_)) =>
      switch (operator_of_shape(os)) {
      | None => Failed
      | Some(operator) =>
        let construct_operator =
          ZPat.is_before_zoperand(zoperand)
            ? construct_operator_before_zoperand
            : construct_operator_after_zoperand;
        let (zseq, u_gen) =
          construct_operator(u_gen, operator, zoperand, (E, E));
        Succeeded(mk_and_syn_fix_ZOpSeq(ctx, u_gen, zseq));
      }

    /* Zipper */
    | (_, ParenthesizedZ(zbody)) =>
      switch (syn_perform(ctx, u_gen, a, zbody)) {
      | Failed => Failed
      | CursorEscaped(side) =>
        syn_perform(ctx, u_gen, escape(side), ZP0(zoperand))
      | Succeeded((zbody, ty, ctx, u_gen)) =>
        Succeeded((ZP0(ParenthesizedZ(zbody)), ty, ctx, u_gen))
      }
    | (_, InjZ(_, side, zbody)) =>
      switch (syn_perform(ctx, u_gen, a, zbody)) {
      | Failed => Failed
      | CursorEscaped(side) =>
        syn_perform(ctx, u_gen, escape(side), ZP0(zoperand))
      | Succeeded((zbody, ty1, ctx, u_gen)) =>
        let zp = ZPat.(ZP0(InjZ(NotInHole, side, zbody)));
        let ty =
          switch (side) {
          | L => HTyp.Sum(ty1, Hole)
          | R => HTyp.Sum(Hole, ty1)
          };
        Succeeded((zp, ty, ctx, u_gen));
      }
    };
  }
  and ana_perform =
      (ctx: Contexts.t, u_gen: MetaVarGen.t, a: t, zp: ZPat.t, ty: HTyp.t)
      : result((ZPat.t, Contexts.t, MetaVarGen.t)) =>
    switch (a) {
    /* Movement */
    | MoveTo(path) =>
      switch (CursorPath.Pat.follow(path, zp |> ZPat.erase)) {
      | None => Failed
      | Some(zp) => mk_ana_result(ctx, u_gen, zp, ty)
      }
    | MoveToBefore(steps) =>
      switch (
        CursorPath.Pat.follow_steps(~side=Before, steps, zp |> ZPat.erase)
      ) {
      | None => Failed
      | Some(zp) => mk_ana_result(ctx, u_gen, zp, ty)
      }
    | MoveToPrevHole =>
      switch (CursorPath.(prev_hole_steps(Pat.holes_z(zp, [])))) {
      | None => Failed
      | Some(steps) => ana_perform(ctx, u_gen, MoveToBefore(steps), zp, ty)
      }
    | MoveToNextHole =>
      switch (CursorPath.(next_hole_steps(Pat.holes_z(zp, [])))) {
      | None => Failed
      | Some(steps) => ana_perform(ctx, u_gen, MoveToBefore(steps), zp, ty)
      }
    | MoveLeft =>
      switch (zp |> ZPat.move_cursor_left) {
      | None => CursorEscaped(Before)
      | Some(zp) => mk_ana_result(ctx, u_gen, zp, ty)
      }
    | MoveRight =>
      switch (zp |> ZPat.move_cursor_right) {
      | None => CursorEscaped(After)
      | Some(zp) => mk_ana_result(ctx, u_gen, zp, ty)
      }
    | _ =>
      switch (zp) {
      | ZP1(zp1) => ana_perform_opseq(ctx, u_gen, a, zp1, ty)
      | ZP0(zp0) => ana_perform_operand(ctx, u_gen, a, zp0, ty)
      }
    }
  and ana_perform_opseq =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        a: t,
        ZOpSeq(skel, zseq) as zopseq: ZPat.zopseq,
        ty: HTyp.t,
      )
      : result((ZPat.t, Contexts.t, MetaVarGen.t)) =>
    switch (a, zseq) {
    /* Invalid cursor positions */
    | (_, ZOperator((OnText(_) | OnDelim(_), _), _)) => Failed

    /* Invalid actions */
    | (UpdateApPalette(_), ZOperator(_)) => Failed

    /* Movement handled at top level */
    | (
        MoveTo(_) | MoveToBefore(_) | MoveToPrevHole | MoveToNextHole | MoveLeft |
        MoveRight,
        _,
      ) =>
      Failed

    /* Deletion */

    | (Delete, ZOperator((OnOp(After as side), _), _))
    | (Backspace, ZOperator((OnOp(Before as side), _), _)) =>
      ana_perform(ctx, u_gen, escape(side), ZP1(zopseq), ty)

    /* Delete before operator == Backspace after operator */
    | (Delete, ZOperator((OnOp(Before), op), surround)) =>
      let new_zp =
        ZPat.ZP1(ZOpSeq(skel, ZOperator((OnOp(After), op), surround)));
      ana_perform(ctx, u_gen, Backspace, new_zp, ty);

    /* ... + [k-1] +<| [k] + ... */
    | (Backspace, ZOperator((OnOp(After), _), surround)) =>
      let new_zseq = delete_operator(surround);
      Succeeded(mk_and_ana_fix_ZOpSeq(ctx, u_gen, new_zseq, ty));

    /* ... + [k-1]  <|_ + [k+1] + ...  ==>   ... + [k-1]| + [k+1] + ... */
    | (
        Backspace,
        ZOperand(
          CursorP(_, EmptyHole(_)) as zhole,
          (A(Space, prefix_tl), suffix),
        ),
      )
        when ZPat.is_before_zoperand(zhole) =>
      let S(operand, new_prefix) = prefix_tl;
      let zoperand = operand |> ZPat.place_after_operand;
      let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, suffix));
      Succeeded(mk_and_ana_fix_ZOpSeq(ctx, u_gen, new_zseq, ty));

    /* ... + [k-1] + _|>  [k+1] + ...  ==>   ... + [k-1] + |[k+1] + ... */
    | (
        Delete,
        ZOperand(
          CursorP(_, EmptyHole(_)) as zhole,
          (prefix, A(Space, suffix_tl)),
        ),
      )
        when ZPat.is_after_zoperand(zhole) =>
      let S(operand, new_suffix) = suffix_tl;
      let zoperand = operand |> ZPat.place_before_operand;
      let new_zseq = ZSeq.ZOperand(zoperand, (prefix, new_suffix));
      Succeeded(mk_and_ana_fix_ZOpSeq(ctx, u_gen, new_zseq, ty));

    /* Construction */

    /* construction on operators either becomes movement... */
    | (Construct(SOp(SSpace)), ZOperator(zoperator, _))
        when ZPat.is_after_zoperator(zoperator) =>
      ana_perform(ctx, u_gen, MoveRight, ZP1(zopseq), ty)
    /* ...or construction after movement */
    | (Construct(_) as a, ZOperator(zoperator, _)) =>
      let move_cursor =
        ZPat.is_before_zoperator(zoperator)
          ? ZPat.move_cursor_left : ZPat.move_cursor_right;
      switch (ZPat.ZP1(zopseq) |> move_cursor) {
      | None => Failed
      | Some(zp) => ana_perform(ctx, u_gen, a, zp, ty)
      };

    | (Construct(SOp(os)), ZOperand(zoperand, surround))
        when
          ZPat.is_before_zoperand(zoperand)
          || ZPat.is_after_zoperand(zoperand) =>
      switch (operator_of_shape(os)) {
      | None => Failed
      | Some(operator) =>
        let construct_operator =
          ZPat.is_before_zoperand(zoperand)
            ? construct_operator_before_zoperand
            : construct_operator_after_zoperand;
        let (zseq, u_gen) =
          construct_operator(u_gen, operator, zoperand, surround);
        Succeeded(mk_and_ana_fix_ZOpSeq(ctx, u_gen, zseq, ty));
      }

    /* Zipper */
    | (_, ZOperand(zoperand, surround)) =>
      switch (CursorInfo.Pat.ana_cursor_info_zopseq(ctx, zopseq, ty)) {
      | None => Failed
      | Some(ci) =>
        switch (ci |> CursorInfo.type_mode) {
        | None => Failed
        | Some(Syn) =>
          switch (syn_perform_operand(ctx, u_gen, a, zoperand)) {
          | Failed => Failed
          | CursorEscaped(side) =>
            ana_perform(ctx, u_gen, escape(side), ZP1(zopseq), ty)
          | Succeeded((zp, _, _, u_gen)) =>
            let zseq = resurround_z(zp, surround);
            Succeeded(mk_and_ana_fix_ZOpSeq(ctx, u_gen, zseq, ty));
          }
        | Some(Ana(ty_zoperand)) =>
          switch (ana_perform_operand(ctx, u_gen, a, zoperand, ty_zoperand)) {
          | Failed => Failed
          | CursorEscaped(side) =>
            ana_perform(ctx, u_gen, escape(side), ZP1(zopseq), ty)
          | Succeeded((zp, _, u_gen)) =>
            let new_zseq = resurround_z(zp, surround);
            Succeeded(mk_and_ana_fix_ZOpSeq(ctx, u_gen, new_zseq, ty));
          }
        }
      }
    }
  and ana_perform_operand =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        a: t,
        zoperand: ZPat.zoperand,
        ty: HTyp.t,
      )
      : result((ZPat.t, Contexts.t, MetaVarGen.t)) =>
    switch (a, zoperand) {
    /* Invalid cursor positions */
    | (
        _,
        CursorP(
          OnText(_),
          EmptyHole(_) | Wild(_) | ListNil(_) | Parenthesized(_) | Inj(_),
        ) |
        CursorP(OnDelim(_), Var(_) | NumLit(_) | BoolLit(_)) |
        CursorP(OnOp(_), _),
      ) =>
      Failed
    | (_, CursorP(cursor, operand))
        when !ZPat.is_valid_cursor_operand(cursor, operand) =>
      Failed

    /* Invalid actions */
    | (
        Construct(SApPalette(_) | SAsc | SLet | SLine | SLam | SCase) |
        UpdateApPalette(_),
        _,
      ) =>
      Failed

    /* switch to synthesis if in a hole */
    | (_, _) when ZPat.is_inconsistent(ZP0(zoperand)) =>
      let zp = ZPat.ZP0(zoperand);
      let err = zp |> ZPat.erase |> UHPat.get_err_status;
      let zp' = zp |> ZPat.set_err_status(NotInHole);
      let p' = zp' |> ZPat.erase;
      switch (Statics.Pat.syn(ctx, p')) {
      | None => Failed
      | Some(_) =>
        switch (syn_perform(ctx, u_gen, a, zp')) {
        | (Failed | CursorEscaped(_)) as err => err
        | Succeeded((zp, ty', ctx, u_gen)) =>
          if (HTyp.consistent(ty, ty')) {
            Succeeded((zp, ctx, u_gen));
          } else {
            Succeeded((zp |> ZPat.set_err_status(err), ctx, u_gen));
          }
        }
      };

    /* Movement handled at top level */
    | (
        MoveTo(_) | MoveToBefore(_) | MoveToPrevHole | MoveToNextHole | MoveLeft |
        MoveRight,
        _,
      ) =>
      Failed

    /* Backspace and Delete */

    | (Backspace, _) when ZPat.is_before_zoperand(zoperand) =>
      CursorEscaped(Before)
    | (Delete, _) when ZPat.is_after_zoperand(zoperand) =>
      CursorEscaped(After)

    | (Backspace, CursorP(_, EmptyHole(_) as operand)) =>
      let zp = ZPat.(ZP0(place_before_operand(operand)));
      zp |> ZPat.is_after
        ? Succeeded((zp, ctx, u_gen)) : CursorEscaped(Before);
    | (Delete, CursorP(_, EmptyHole(_) as operand)) =>
      let zp = ZPat.(ZP0(place_after_operand(operand)));
      zp |> ZPat.is_before
        ? Succeeded((zp, ctx, u_gen)) : CursorEscaped(After);

    /* ( _ <|)   ==>   ( _| ) */
    | (Backspace, CursorP(OnDelim(_, Before), _)) =>
      ana_perform(ctx, u_gen, MoveLeft, ZP0(zoperand), ty)
    /* (|> _ )   ==>   ( |_ ) */
    | (Delete, CursorP(OnDelim(_, After), _)) =>
      ana_perform(ctx, u_gen, MoveRight, ZP0(zoperand), ty)

    /* Delete before delimiter == Backspace after delimiter */
    | (Delete, CursorP(OnDelim(k, Before), operand)) =>
      let new_zp = ZPat.ZP0(CursorP(OnDelim(k, After), operand));
      ana_perform(ctx, u_gen, Backspace, new_zp, ty);

    | (Backspace, CursorP(OnDelim(_, After), Wild(_) | ListNil(_))) =>
      let (zhole, u_gen) = ZPat.new_EmptyHole(u_gen);
      let zp = ZPat.ZP0(zhole);
      Succeeded((zp, ctx, u_gen));

    | (Delete, CursorP(OnText(j), Var(_, _, x))) =>
      ana_delete_text(ctx, u_gen, j, x, ty)
    | (Delete, CursorP(OnText(j), NumLit(_, n))) =>
      ana_delete_text(ctx, u_gen, j, string_of_int(n), ty)
    | (Delete, CursorP(OnText(j), BoolLit(_, b))) =>
      ana_delete_text(ctx, u_gen, j, string_of_bool(b), ty)

    | (Backspace, CursorP(OnText(j), Var(_, _, x))) =>
      ana_backspace_text(ctx, u_gen, j, x, ty)
    | (Backspace, CursorP(OnText(j), NumLit(_, n))) =>
      ana_backspace_text(ctx, u_gen, j, string_of_int(n), ty)
    | (Backspace, CursorP(OnText(j), BoolLit(_, b))) =>
      ana_backspace_text(ctx, u_gen, j, string_of_bool(b), ty)

    /* ( _ )<|  ==>  _| */
    /* (<| _ )  ==>  |_ */
    | (
        Backspace,
        CursorP(OnDelim(k, After), Parenthesized(body) | Inj(_, _, body)),
      ) =>
      let place_cursor = k == 0 ? ZPat.place_before : ZPat.place_after;
      Succeeded(
        Statics.Pat.ana_fix_holes_z(ctx, u_gen, body |> place_cursor, ty),
      );

    /* Construct */
    | (Construct(SOp(SSpace)), CursorP(OnDelim(_, After), _)) =>
      ana_perform(ctx, u_gen, MoveRight, ZP0(zoperand), ty)
    | (Construct(_) as a, CursorP(OnDelim(_, side), _))
        when
          !ZPat.is_before_zoperand(zoperand)
          && !ZPat.is_after_zoperand(zoperand) =>
      switch (ana_perform(ctx, u_gen, escape(side), ZP0(zoperand), ty)) {
      | (Failed | CursorEscaped(_)) as err => err
      | Succeeded((zp, _, u_gen)) => ana_perform(ctx, u_gen, a, zp, ty)
      }

    | (Construct(SChar(s)), CursorP(_, EmptyHole(_))) =>
      ana_insert_text(ctx, u_gen, (0, s), "", ty)
    | (Construct(SChar(s)), CursorP(OnDelim(_, side), Wild(_))) =>
      let index =
        switch (side) {
        | Before => 0
        | After => 1
        };
      ana_insert_text(ctx, u_gen, (index, s), "_", ty);
    | (Construct(SChar(s)), CursorP(OnText(j), Var(_, _, x))) =>
      ana_insert_text(ctx, u_gen, (j, s), x, ty)
    | (Construct(SChar(s)), CursorP(OnText(j), NumLit(_, n))) =>
      ana_insert_text(ctx, u_gen, (j, s), string_of_int(n), ty)
    | (Construct(SChar(s)), CursorP(OnText(j), BoolLit(_, b))) =>
      ana_insert_text(ctx, u_gen, (j, s), string_of_bool(b), ty)
    | (Construct(SChar(_)), CursorP(_)) => Failed

    | (Construct(SParenthesized), CursorP(_)) =>
      let new_zp = ZPat.ZP0(ParenthesizedZ(ZP0(zoperand)));
      mk_ana_result(ctx, u_gen, new_zp, ty);

    | (Construct(SInj(side)), CursorP(_)) =>
      switch (HTyp.matched_sum(ty)) {
      | Some((tyL, tyR)) =>
        let body_ty = InjSide.pick(side, tyL, tyR);
        let (zbody, ctx, u_gen) =
          Statics.Pat.ana_fix_holes_z(ctx, u_gen, ZP0(zoperand), body_ty);
        let zp = ZPat.ZP0(InjZ(NotInHole, side, zbody));
        Succeeded((zp, ctx, u_gen));
      | None =>
        let (zbody, _, ctx, u_gen) =
          Statics.Pat.syn_fix_holes_z(ctx, u_gen, ZP0(zoperand));
        let (u, u_gen) = u_gen |> MetaVarGen.next;
        let zp = ZPat.ZP0(InjZ(InHole(TypeInconsistent, u), side, zbody));
        Succeeded((zp, ctx, u_gen));
      }

    | (Construct(SOp(os)), CursorP(_)) =>
      switch (operator_of_shape(os)) {
      | None => Failed
      | Some(operator) =>
        let construct_operator =
          ZPat.is_before_zoperand(zoperand)
            ? construct_operator_before_zoperand
            : construct_operator_after_zoperand;
        let (zseq, u_gen) =
          construct_operator(u_gen, operator, zoperand, (E, E));
        Succeeded(mk_and_ana_fix_ZOpSeq(ctx, u_gen, zseq, ty));
      }

    /* Zipper */
    | (_, ParenthesizedZ(zbody)) =>
      switch (ana_perform(ctx, u_gen, a, zbody, ty)) {
      | Failed => Failed
      | CursorEscaped(side) =>
        ana_perform(ctx, u_gen, escape(side), ZP0(zoperand), ty)
      | Succeeded((zbody, ctx, u_gen)) =>
        let zp = ZPat.ZP0(ParenthesizedZ(zbody));
        Succeeded((zp, ctx, u_gen));
      }
    | (_, InjZ(_, side, zbody)) =>
      switch (HTyp.matched_sum(ty)) {
      | None => Failed
      | Some((tyL, tyR)) =>
        let body_ty = InjSide.pick(side, tyL, tyR);
        switch (ana_perform(ctx, u_gen, a, zbody, body_ty)) {
        | Failed => Failed
        | CursorEscaped(side) =>
          ana_perform(ctx, u_gen, escape(side), ZP0(zoperand), ty)
        | Succeeded((zbody, ctx, u_gen)) =>
          let zp = ZPat.ZP0(InjZ(NotInHole, side, zbody));
          Succeeded((zp, ctx, u_gen));
        };
      }

    /* Subsumption */
    | (Construct(SListNil), _) =>
      switch (syn_perform(ctx, u_gen, a, ZP0(zoperand))) {
      | (Failed | CursorEscaped(_)) as err => err
      | Succeeded((zp, ty', ctx, u_gen)) =>
        if (HTyp.consistent(ty, ty')) {
          Succeeded((zp, ctx, u_gen));
        } else {
          let (zp, u_gen) = zp |> ZPat.make_inconsistent(u_gen);
          Succeeded((zp, ctx, u_gen));
        }
      }
    };
};

module Exp = {
  let operator_of_shape = (os: operator_shape): option(UHExp.operator) =>
    switch (os) {
    | SPlus => Some(Plus)
    | SMinus => Some(Minus)
    | STimes => Some(Times)
    | SLessThan => Some(LessThan)
    | SGreaterThan => Some(GreaterThan)
    | SEquals => Some(Equals)
    | SSpace => Some(Space)
    | SComma => Some(Comma)
    | SCons => Some(Cons)
    | SAnd => Some(And)
    | SOr => Some(Or)
    | SArrow
    | SVBar => None
    };

  let shape_of_operator = (op: UHExp.operator): operator_shape =>
    switch (op) {
    | Minus => SMinus
    | Plus => SPlus
    | Times => STimes
    | LessThan => SLessThan
    | GreaterThan => SGreaterThan
    | Equals => SEquals
    | Space => SSpace
    | Comma => SComma
    | Cons => SCons
    | And => SAnd
    | Or => SOr
    };

  let mk_OpSeq = OpSeq.mk(~associate=Associator.Exp.associate);
  let mk_ZOpSeq =
    ZOpSeq.mk(
      ~associate=Associator.Exp.associate,
      ~erase_zoperand=ZExp.erase_zoperand,
      ~erase_zoperator=ZExp.erase_zoperator,
    );

  let mk_and_syn_fix_OpSeq =
      (ctx: Contexts.t, u_gen: MetaVarGen.t, seq: UHExp.seq)
      : (UHExp.opseq, HTyp.t, MetaVarGen.t) => {
    let opseq = mk_OpSeq(seq);
    Statics.Exp.syn_fix_holes_opseq(ctx, u_gen, opseq);
  };
  let mk_and_ana_fix_OpSeq =
      (ctx: Contexts.t, u_gen: MetaVarGen.t, seq: UHExp.seq, ty: HTyp.t)
      : (UHExp.opseq, MetaVarGen.t) => {
    let opseq = mk_OpSeq(seq);
    Statics.Exp.ana_fix_holes_opseq(ctx, u_gen, opseq, ty);
  };
  let mk_and_syn_fix_ZOpSeq =
      (ctx: Contexts.t, u_gen: MetaVarGen.t, zseq: ZExp.zseq)
      : (ZExp.t, HTyp.t, MetaVarGen.t) => {
    let zopseq = mk_ZOpSeq(zseq);
    Statics.Exp.syn_fix_holes_z(ctx, u_gen, ZE1(zopseq));
  };
  let mk_and_ana_fix_ZOpSeq =
      (ctx: Contexts.t, u_gen: MetaVarGen.t, zseq: ZExp.zseq, ty: HTyp.t)
      : (ZExp.t, MetaVarGen.t) => {
    let zopseq = mk_ZOpSeq(zseq);
    Statics.Exp.ana_fix_holes_z(ctx, u_gen, ZE1(zopseq), ty);
  };

  /**
   * Used to construct an expression from an opseq suffix that
   * follows a keyword when the user hits space after the keyword.
   * If the first operation is a space, then what follows the space
   * becomes the new expression. Otherwise, a new hole is generated,
   * prepended to the suffix, and the reuslting opseq becomes the
   * new expression.
   */
  let keyword_suffix_to_opseq =
      (suffix: Seq.affix(UHExp.operand, UHExp.operator), u_gen: MetaVarGen.t)
      : (UHExp.opseq, MetaVarGen.t) =>
    switch (suffix) {
    | A(Space, suffix_tl) => (mk_OpSeq(suffix_tl), u_gen)
    | _ =>
      let (hole, u_gen) = u_gen |> UHExp.new_EmptyHole;
      (mk_OpSeq(S(hole, suffix)), u_gen);
    };

  let keyword_action = (kw: ExpandingKeyword.t): t =>
    switch (kw) {
    | Let => Construct(SLet)
    | Case => Construct(SCase)
    };

  let delete_operator =
    _delete_operator(
      ~space=UHExp.Space,
      ~is_EmptyHole=UHExp.is_EmptyHole,
      ~place_before_operand=ZExp.place_before_operand,
      ~place_after_operand=ZExp.place_after_operand,
      ~place_after_operator=ZExp.place_after_operator,
    );

  let construct_operator_before_zoperand =
    _construct_operator_before_zoperand(
      ~is_Space=UHExp.is_Space,
      ~new_EmptyHole=UHExp.new_EmptyHole,
      ~erase_zoperand=ZExp.erase_zoperand,
      ~place_before_operand=ZExp.place_before_operand,
      ~place_after_operator=ZExp.place_after_operator,
    );
  let construct_operator_after_zoperand =
    _construct_operator_after_zoperand(
      ~is_Space=UHExp.is_Space,
      ~new_EmptyHole=UHExp.new_EmptyHole,
      ~erase_zoperand=ZExp.erase_zoperand,
      ~place_before_operand=ZExp.place_before_operand,
      ~place_after_operator=ZExp.place_after_operator,
    );

  let resurround =
      (
        u_gen: MetaVarGen.t,
        e: UHExp.t,
        (prefix, suffix): ZExp.operand_surround,
      )
      : (UHExp.t, MetaVarGen.t) =>
    switch (e) {
    | E0(operand) =>
      let new_seq =
        Seq.affix_seq(prefix, Seq.seq_affix(S(operand, E), suffix));
      (E1(mk_OpSeq(new_seq)), u_gen);
    | E1(OpSeq(_, seq)) =>
      let new_seq = Seq.affix_seq(prefix, Seq.seq_affix(seq, suffix));
      (E1(mk_OpSeq(new_seq)), u_gen);
    | E2(block) =>
      let (prefix_hole, u_gen) = u_gen |> UHExp.new_EmptyHole;
      let (suffix_hole, u_gen) = u_gen |> UHExp.new_EmptyHole;
      let new_prefix_line =
        UHExp.ExpLine(mk_OpSeq(Seq.affix_seq(prefix, S(prefix_hole, E))));
      let new_suffix_line =
        UHExp.ExpLine(mk_OpSeq(Seq.seq_affix(S(suffix_hole, E), suffix)));
      let new_block = [new_prefix_line, ...block] @ [new_suffix_line];
      (E2(new_block), u_gen);
    };
  let resurround_z =
      (
        u_gen: MetaVarGen.t,
        ze: ZExp.t,
        (prefix, suffix) as surround: ZExp.operand_surround,
      )
      : (ZExp.t, MetaVarGen.t) =>
    switch (ze) {
    | ZE0(zoperand) => (
        ZE1(mk_ZOpSeq(ZOperand(zoperand, surround))),
        u_gen,
      )
    | ZE1(ZOpSeq(_, ZOperand(zoperand, (inner_prefix, inner_suffix)))) =>
      let new_prefix = Seq.affix_affix(inner_prefix, prefix);
      let new_suffix = Seq.affix_affix(inner_suffix, suffix);
      (
        ZE1(mk_ZOpSeq(ZOperand(zoperand, (new_prefix, new_suffix)))),
        u_gen,
      );
    | ZE1(ZOpSeq(_, ZOperator(zoperator, (inner_prefix, inner_suffix)))) =>
      let new_prefix = Seq.seq_affix(inner_prefix, prefix);
      let new_suffix = Seq.seq_affix(inner_suffix, suffix);
      (
        ZE1(mk_ZOpSeq(ZOperator(zoperator, (new_prefix, new_suffix)))),
        u_gen,
      );
    | ZE2((prefix_lines, zline, suffix_lines)) =>
      let (prefix_hole, u_gen) = u_gen |> UHExp.new_EmptyHole;
      let (suffix_hole, u_gen) = u_gen |> UHExp.new_EmptyHole;
      let new_prefix_lines = [
        UHExp.ExpLine(mk_OpSeq(Seq.affix_seq(prefix, S(prefix_hole, E)))),
        ...prefix_lines,
      ];
      let new_suffix_lines =
        suffix_lines
        @ [
          UHExp.ExpLine(
            mk_OpSeq(Seq.seq_affix(S(suffix_hole, E), suffix)),
          ),
        ];
      (ZE2((new_prefix_lines, zline, new_suffix_lines)), u_gen);
    };

  // TODO refactor these types to incorporate ExpandingKeyword.t
  type line_success =
    | LineExpandsToLet({
        u_gen: MetaVarGen.t,
        prefix: list(UHExp.line),
        def: UHExp.t,
        suffix: list(UHExp.line),
      })
    | LineExpandsToCase({
        u_gen: MetaVarGen.t,
        prefix: list(UHExp.line),
        scrut: UHExp.t,
        suffix: list(UHExp.line),
      })
    | LineDone((ZExp.zblock, Contexts.t, MetaVarGen.t));

  type syn_done = (ZExp.t, HTyp.t, MetaVarGen.t);
  type syn_success =
    | SynExpandsToLet({
        u_gen: MetaVarGen.t,
        prefix: list(UHExp.line),
        def: UHExp.t,
        suffix: list(UHExp.line),
      })
    | SynExpandsToCase({
        u_gen: MetaVarGen.t,
        prefix: list(UHExp.line),
        scrut: UHExp.t,
        suffix: list(UHExp.line),
      })
    | SynDone(syn_done);
  let mk_SynExpandsToCase = (~u_gen, ~prefix=[], ~suffix=[], ~scrut, ()) =>
    SynExpandsToCase({u_gen, prefix, suffix, scrut});
  let mk_SynExpandsToLet = (~u_gen, ~prefix=[], ~suffix=[], ~def, ()) =>
    SynExpandsToLet({u_gen, prefix, suffix, def});
  let wrap_in_SynDone: result(syn_done) => result(syn_success) =
    fun
    | (Failed | CursorEscaped(_)) as err => err
    | Succeeded(syn_done) => Succeeded(SynDone(syn_done));

  type ana_done = (ZExp.t, MetaVarGen.t);
  type ana_success =
    | AnaExpandsToLet({
        u_gen: MetaVarGen.t,
        prefix: list(UHExp.line),
        def: UHExp.t,
        suffix: list(UHExp.line),
      })
    | AnaExpandsToCase({
        u_gen: MetaVarGen.t,
        prefix: list(UHExp.line),
        scrut: UHExp.t,
        suffix: list(UHExp.line),
      })
    | AnaDone(ana_done);
  let mk_AnaExpandsToCase = (~u_gen, ~prefix=[], ~suffix=[], ~scrut, ()) =>
    AnaExpandsToCase({u_gen, prefix, suffix, scrut});
  let mk_AnaExpandsToLet = (~u_gen, ~prefix=[], ~suffix=[], ~def, ()) =>
    AnaExpandsToLet({u_gen, prefix, suffix, def});
  let wrap_in_AnaDone: result(ana_done) => result(ana_success) =
    fun
    | (Failed | CursorEscaped(_)) as err => err
    | Succeeded(ana_done) => Succeeded(AnaDone(ana_done));

  let new_line_of_prefix =
      (ctx: Contexts.t, u_gen: MetaVarGen.t, prefix: UHExp.affix) => {
    let (hole, u_gen) = u_gen |> UHExp.new_EmptyHole;
    let seq = Seq.affix_seq(prefix, S(hole, E));
    let (opseq, _, u_gen) = mk_and_syn_fix_OpSeq(ctx, u_gen, seq);
    (UHExp.ExpLine(opseq), u_gen);
  };

  let append_suffix = (u_gen: MetaVarGen.t, e: UHExp.t, suffix: UHExp.affix) =>
    resurround(u_gen, e, (E, suffix));

  let zcase_of_scrut_and_suffix =
      (
        mode: Statics.type_mode,
        u_gen: MetaVarGen.t,
        scrut: UHExp.t,
        suffix: list(UHExp.line),
      )
      : (ZExp.zoperand, MetaVarGen.t) => {
    let ann =
      switch (mode) {
      | Syn => Some(UHTyp.T0(Hole))
      | Ana(_) => None
      };
    switch (scrut, suffix) {
    | (E0(EmptyHole(_)), []) =>
      let zscrut = scrut |> ZExp.place_before;
      let (rule, u_gen) = u_gen |> UHExp.empty_rule;
      (ZExp.CaseZE(NotInHole, zscrut, [rule], ann), u_gen);
    | (_, []) =>
      let (zrule, u_gen) = u_gen |> ZExp.empty_zrule;
      (ZExp.CaseZR(NotInHole, scrut, ([], zrule, []), ann), u_gen);
    | (E0(EmptyHole(_)), [_, ..._]) =>
      let zscrut = scrut |> ZExp.place_before;
      let (p_hole, u_gen) = u_gen |> UHPat.new_EmptyHole;
      let rule = UHExp.Rule(P0(p_hole), E2(suffix));
      (ZExp.CaseZE(NotInHole, zscrut, [rule], ann), u_gen);
    | (_, [_, ..._]) =>
      let (zp_hole, u_gen) = u_gen |> ZPat.new_EmptyHole;
      let zrule = ZExp.RuleZP(ZP0(zp_hole), E2(suffix));
      (ZExp.CaseZR(NotInHole, scrut, ([], zrule, []), ann), u_gen);
    };
  };

  let mk_syn_text =
      (ctx: Contexts.t, u_gen: MetaVarGen.t, caret_index: int, text: string) => {
    let text_cursor = CursorPosition.OnText(caret_index);
    switch (TextShape.of_text(text)) {
    | None =>
      if (text |> is_empty_string) {
        let (zhole, u_gen) = u_gen |> ZExp.new_EmptyHole;
        Succeeded(SynDone((ZExp.ZE0(zhole), HTyp.Hole, u_gen)));
      } else {
        Failed;
      }
    | Some(NumLit(n)) =>
      let ze = ZExp.ZE0(CursorE(text_cursor, UHExp.numlit(n)));
      Succeeded(SynDone((ze, HTyp.Num, u_gen)));
    | Some(BoolLit(b)) =>
      let ze = ZExp.ZE0(CursorE(text_cursor, UHExp.boollit(b)));
      Succeeded(SynDone((ze, HTyp.Bool, u_gen)));
    | Some(Keyword(k)) =>
      let (u, u_gen) = u_gen |> MetaVarGen.next;
      let var =
        UHExp.var(
          ~var_err=InVarHole(Keyword(k), u),
          k |> Keyword.to_string,
        );
      let ze = ZExp.ZE0(CursorE(text_cursor, var));
      Succeeded(SynDone((ze, HTyp.Hole, u_gen)));
    | Some((Underscore | Var(_)) as shape) =>
      let x =
        switch (shape) {
        | Var(x) => x
        | _ => "_"
        };
      switch (VarMap.lookup(ctx |> Contexts.gamma, x)) {
      | Some(ty) =>
        let ze = ZExp.ZE0(CursorE(text_cursor, UHExp.var(x)));
        Succeeded(SynDone((ze, ty, u_gen)));
      | None =>
        let (u, u_gen) = u_gen |> MetaVarGen.next;
        let var = UHExp.var(~var_err=InVarHole(Free, u), x);
        let new_ze = ZExp.ZE0(CursorE(text_cursor, var));
        Succeeded(SynDone((new_ze, Hole, u_gen)));
      };
    };
  };

  let mk_ana_text =
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        caret_index: int,
        text: string,
        ty: HTyp.t,
      ) => {
    let text_cursor = CursorPosition.OnText(caret_index);
    switch (TextShape.of_text(text)) {
    | None =>
      if (text |> is_empty_string) {
        let (zhole, u_gen) = u_gen |> ZExp.new_EmptyHole;
        Succeeded(AnaDone((ZExp.ZE0(zhole), u_gen)));
      } else {
        Failed;
      }
    | Some(Keyword(k)) =>
      let (u, u_gen) = u_gen |> MetaVarGen.next;
      let var =
        UHExp.var(
          ~var_err=InVarHole(Keyword(k), u),
          k |> Keyword.to_string,
        );
      let ze = ZExp.ZE0(CursorE(text_cursor, var));
      Succeeded(AnaDone((ze, u_gen)));
    | Some(NumLit(_) | BoolLit(_) | Underscore | Var(_)) =>
      // TODO: review whether subsumption correctly applied
      switch (mk_syn_text(ctx, u_gen, caret_index, text)) {
      | (Failed | CursorEscaped(_)) as err => err
      | Succeeded(SynExpandsToCase({u_gen, prefix, scrut, suffix})) =>
        Succeeded(AnaExpandsToCase({u_gen, prefix, scrut, suffix}))
      | Succeeded(SynExpandsToLet({u_gen, prefix, def, suffix})) =>
        Succeeded(AnaExpandsToLet({u_gen, prefix, def, suffix}))
      | Succeeded(SynDone((ze, ty', u_gen))) =>
        if (HTyp.consistent(ty, ty')) {
          Succeeded(AnaDone((ze, u_gen)));
        } else {
          let (ze, u_gen) = ze |> ZExp.make_inconsistent(u_gen);
          Succeeded(AnaDone((ze, u_gen)));
        }
      }
    };
  };

  let syn_insert_text = _syn_insert_text(~mk_syn_text);
  let ana_insert_text = _ana_insert_text(~mk_ana_text);
  let syn_backspace_text = _syn_backspace_text(~mk_syn_text);
  let ana_backspace_text = _ana_backspace_text(~mk_ana_text);
  let syn_delete_text = _syn_delete_text(~mk_syn_text);
  let ana_delete_text = _ana_delete_text(~mk_ana_text);

  let rec syn_perform =
          (
            ctx: Contexts.t,
            a: t,
            (ze: ZExp.t, ty: HTyp.t, u_gen: MetaVarGen.t),
          )
          : result(syn_done) =>
    switch (a) {
    /* Movement */
    | MoveTo(path) =>
      switch (CursorPath.Exp.follow(path, ze |> ZExp.erase)) {
      | None => Failed
      | Some(ze) => Succeeded((ze, ty, u_gen))
      }
    | MoveToBefore(steps) =>
      switch (CursorPath.Exp.follow_steps(steps, ze |> ZExp.erase)) {
      | None => Failed
      | Some(ze) => Succeeded((ze, ty, u_gen))
      }
    | MoveToPrevHole =>
      switch (CursorPath.Exp.prev_hole_steps_z(ze)) {
      | None => Failed
      | Some(steps) =>
        syn_perform(ctx, MoveToBefore(steps), (ze, ty, u_gen))
      }
    | MoveToNextHole =>
      switch (CursorPath.Exp.next_hole_steps_z(ze)) {
      | None => Failed
      | Some(steps) =>
        syn_perform(ctx, MoveToBefore(steps), (ze, ty, u_gen))
      }
    | MoveLeft =>
      ze
      |> ZExp.move_cursor_left
      |> Opt.map_default(~default=CursorEscaped(Before), ze =>
           Succeeded((ze, ty, u_gen))
         )
    | MoveRight =>
      ze
      |> ZExp.move_cursor_right
      |> Opt.map_default(~default=CursorEscaped(After), ze =>
           Succeeded((ze, ty, u_gen))
         )
    | _ =>
      let result =
        switch (ze) {
        | ZE2(ze2) => syn_perform_block(ctx, a, (ze2, ty, u_gen))
        | ZE1(ze1) => syn_perform_opseq(ctx, a, (ze1, ty, u_gen))
        | ZE0(ze0) => syn_perform_operand(ctx, a, (ze0, ty, u_gen))
        };
      switch (result) {
      | (Failed | CursorEscaped(_)) as err => err
      | Succeeded(SynDone(syn_done)) => Succeeded(syn_done)
      | Succeeded(SynExpandsToCase({prefix, scrut, suffix, u_gen})) =>
        let (zcase, u_gen) =
          zcase_of_scrut_and_suffix(Syn, u_gen, scrut, suffix);
        let new_ze = ZExp.ZE2((prefix, ExpLineZ(zcase |> ZOpSeq.wrap), []));
        Succeeded(Statics.Exp.syn_fix_holes_z(ctx, u_gen, new_ze));
      | Succeeded(SynExpandsToLet({prefix, def, suffix, u_gen})) =>
        let (zp_hole, u_gen) = u_gen |> ZPat.new_EmptyHole;
        let zlet = ZExp.LetLineZP(ZP0(zp_hole), None, def);
        let new_ze = ZExp.ZE2((prefix, zlet, suffix));
        Succeeded(Statics.Exp.syn_fix_holes_z(ctx, u_gen, new_ze));
      };
    }
  and syn_perform_block =
      (
        ctx: Contexts.t,
        a: t,
        (
          (prefix, zline, suffix) as zblock: ZExp.zblock,
          ty: HTyp.t,
          u_gen: MetaVarGen.t,
        ),
      )
      : result(syn_success) =>
    switch (a) {
    /* Movement handled at top level */
    | MoveTo(_)
    | MoveToBefore(_)
    | MoveToPrevHole
    | MoveToNextHole
    | MoveLeft
    | MoveRight => Failed

    /* Backspace & Delete */

    | Delete when ZExp.is_after_zline(zline) =>
      switch (zline |> ZExp.erase_zline, suffix) {
      | (_, []) => CursorEscaped(After)
      | (EmptyLine, [suffix_hd, ...new_suffix]) =>
        let new_zline = suffix_hd |> ZExp.place_before_line;
        let new_zblock = (prefix, new_zline, new_suffix);
        Succeeded(SynDone((ZE2(new_zblock), ty, u_gen)));
      | (_, [EmptyLine, ...new_suffix])
      | (
          ExpLine(_),
          [ExpLine(OpSeq(_, S(EmptyHole(_), E))), ...new_suffix],
        ) =>
        let new_ze = ZExp.ZE2((prefix, zline, new_suffix));
        Succeeded(SynDone(Statics.Exp.syn_fix_holes_z(ctx, u_gen, new_ze)));
      | _ =>
        syn_perform(ctx, MoveRight, (ZE2(zblock), ty, u_gen))
        |> wrap_in_SynDone
      }
    | Backspace when ZExp.is_before_zline(zline) =>
      switch (prefix |> split_last, zline |> ZExp.erase_zline) {
      | (None, _) => CursorEscaped(Before)
      | (Some((new_prefix, prefix_hd)), EmptyLine) =>
        let new_zline = prefix_hd |> ZExp.place_after_line;
        let new_ze = ZExp.ZE2((new_prefix, new_zline, suffix));
        Succeeded(SynDone((new_ze, ty, u_gen)));
      | (Some((new_prefix, EmptyLine)), _) =>
        let new_ze = ZExp.ZE2((new_prefix, zline, suffix));
        Succeeded(SynDone((new_ze, ty, u_gen)));
      | (
          Some((new_prefix, ExpLine(_) as prefix_hd)),
          ExpLine(OpSeq(_, S(EmptyHole(_), E))),
        ) =>
        let new_zline = prefix_hd |> ZExp.place_after_line;
        let new_ze = ZExp.ZE2((new_prefix, new_zline, suffix));
        Succeeded(SynDone(Statics.Exp.syn_fix_holes_z(ctx, u_gen, new_ze)));
      | _ =>
        syn_perform(ctx, MoveLeft, (ZE2(zblock), ty, u_gen))
        |> wrap_in_SynDone
      }

    /* No construction handled at block level */

    /* Zipper */
    | _ =>
      switch (Statics.Exp.syn_lines(ctx, prefix)) {
      | None => Failed
      | Some(ctx_zline) =>
        switch (syn_perform_line(ctx_zline, a, (zline, u_gen))) {
        | Failed => Failed
        | CursorEscaped(side) =>
          syn_perform(ctx, escape(side), (ZE2(zblock), ty, u_gen))
          |> wrap_in_SynDone
        | Succeeded(LineExpandsToCase(r)) =>
          Succeeded(
            SynExpandsToCase({
              u_gen: r.u_gen,
              prefix: prefix @ r.prefix,
              scrut: r.scrut,
              suffix: r.suffix @ suffix,
            }),
          )
        | Succeeded(LineExpandsToLet(r)) =>
          Succeeded(
            SynExpandsToLet({
              u_gen: r.u_gen,
              prefix: prefix @ r.prefix,
              def: r.def,
              suffix: r.suffix @ suffix,
            }),
          )
        | Succeeded(
            LineDone((
              (inner_prefix, new_zline, inner_suffix),
              ctx_suffix,
              u_gen,
            )),
          ) =>
          let (suffix, new_ty, u_gen) =
            Statics.Exp.syn_fix_holes_block(ctx_suffix, u_gen, suffix);
          let new_ze =
            ZExp.ZE2((
              prefix @ inner_prefix,
              new_zline,
              inner_suffix @ suffix,
            ));
          Succeeded(SynDone((new_ze, new_ty, u_gen)));
        }
      }
    }
  and syn_perform_line =
      (ctx: Contexts.t, a: t, (zline: ZExp.zline, u_gen: MetaVarGen.t))
      : result(line_success) => {
    let mk_result = (u_gen, zlines) => {
      let (zlines, ctx, u_gen) =
        Statics.Exp.syn_fix_holes_zlines(ctx, u_gen, zlines);
      Succeeded(LineDone((zlines, ctx, u_gen)));
    };
    let escape = (u_gen, side: Side.t) => {
      let move_cursor =
        switch (side) {
        | Before => ZExp.move_cursor_left_zline
        | After => ZExp.move_cursor_right_zline
        };
      zline
      |> move_cursor
      |> Opt.map_default(~default=CursorEscaped(side), new_zline =>
           mk_result(u_gen, ([], new_zline, []))
         );
    };

    switch (a, zline) {
    /* Invalid cursor positions */
    | (
        _,
        CursorL(OnDelim(_) | OnOp(_), EmptyLine) |
        CursorL(OnText(_) | OnOp(_), LetLine(_)) |
        CursorL(_, ExpLine(_)),
      ) =>
      Failed
    | (_, CursorL(cursor, line))
        when !ZExp.is_valid_cursor_line(cursor, line) =>
      Failed

    /* Movement handled at top level */
    | (
        MoveTo(_) | MoveToBefore(_) | MoveToPrevHole | MoveToNextHole | MoveLeft |
        MoveRight,
        _,
      ) =>
      Failed

    /* Backspace & Delete */

    /* Deletion of empty lines handled at block level */
    | (Backspace | Delete, CursorL(_, EmptyLine)) => Failed

    /* let x <|= 2   ==>   let x| = 2 */
    | (Backspace, CursorL(OnDelim(_, Before as side), _))
    /* let x =|> 2   ==>   let x = |2 */
    | (Delete, CursorL(OnDelim(_, After as side), _)) =>
      escape(u_gen, side)

    /* Delete before delimiter == Backspace after delimiter */
    | (Delete, CursorL(OnDelim(k, Before), line)) =>
      let new_zline = ZExp.CursorL(OnDelim(k, After), line);
      syn_perform_line(ctx, Backspace, (new_zline, u_gen));

    | (Backspace, CursorL(OnDelim(k, After), LetLine(p, _, def))) =>
      if (k == 1) {
        /* let x :<| Num = 2   ==>   let x| = 2 */
        let zp = p |> ZPat.place_after;
        let new_zblock = ([], ZExp.LetLineZP(zp, None, def), []);
        mk_result(u_gen, new_zblock);
      } else {
        let new_ze =
          k == 3 ? def |> ZExp.place_after : def |> ZExp.place_before;
        let new_zblock =
          switch (new_ze) {
          | ZE2(zblock) => zblock
          | ZE1(zopseq) => ([], ExpLineZ(zopseq), [])
          | ZE0(zoperand) => ([], ExpLineZ(zoperand |> ZOpSeq.wrap), [])
          };
        mk_result(u_gen, new_zblock);
      }

    /* Construction */

    | (Construct(_), CursorL(_, EmptyLine)) =>
      let (zhole, u_gen) = u_gen |> ZExp.new_EmptyHole;
      let new_zline = ZExp.ExpLineZ(zhole |> ZOpSeq.wrap);
      syn_perform_line(ctx, a, (new_zline, u_gen));

    | (Construct(SAsc), LetLineZP(zp, None, def)) =>
      switch (Statics.Exp.syn(ctx, def)) {
      | None => Failed
      | Some(ty) =>
        let zty = ty |> UHTyp.contract |> ZTyp.place_before;
        let new_zline = ZExp.LetLineZA(zp |> ZPat.erase, zty, def);
        mk_result(u_gen, ([], new_zline, []));
      }
    | (Construct(SAsc), LetLineZP(zp, Some(uty), def)) =>
      // just move the cursor over if there is already an ascription
      let zty = ZTyp.place_before(uty);
      let new_zline = ZExp.LetLineZA(zp |> ZPat.erase, zty, def);
      mk_result(u_gen, ([], new_zline, []));

    | (Construct(_) | UpdateApPalette(_), CursorL(OnDelim(_, side), _))
        when !ZExp.is_before_zline(zline) && !ZExp.is_after_zline(zline) =>
      let move_cursor =
        switch (side) {
        | Before => ZExp.move_cursor_left_zline
        | After => ZExp.move_cursor_right_zline
        };
      switch (zline |> move_cursor) {
      | None => Failed
      | Some(zline) => syn_perform_line(ctx, a, (zline, u_gen))
      };
    | (Construct(_) | UpdateApPalette(_), CursorL(_)) => Failed

    /* Zipper */

    | (_, ExpLineZ(zopseq)) =>
      let ze = ZExp.ZE1(zopseq);
      switch (Statics.Exp.syn(ctx, ze |> ZExp.erase)) {
      | None => Failed
      | Some(ty) =>
        switch (syn_perform(ctx, a, (ze, ty, u_gen))) {
        | (Failed | CursorEscaped(_)) as err => err
        | Succeeded((ze, _, u_gen)) =>
          let zblock =
            switch (ze) {
            | ZE0(zoperand) =>
              let new_zline =
                ZExp.(
                  ExpLineZ(ZOpSeq.wrap(zoperand)) |> prune_empty_hole_line
                );
              ([], new_zline, []);
            | ZE1(zopseq) => ([], ZExp.ExpLineZ(zopseq), [])
            | ZE2(zblock) => zblock
            };
          Succeeded(LineDone((zblock, ctx, u_gen)));
        }
      };

    | (_, LetLineZP(zp, None, def)) =>
      switch (Statics.Exp.syn(ctx, def)) {
      | None => Failed
      | Some(ty_def) =>
        switch (Pat.ana_perform(ctx, u_gen, a, zp, ty_def)) {
        | Failed => Failed
        | CursorEscaped(side) => escape(u_gen, side)
        | Succeeded((new_zp, new_ctx, u_gen)) =>
          let (new_def, _, u_gen) =
            Statics.Exp.syn_fix_holes(ctx, u_gen, def);
          let new_zline = ZExp.LetLineZP(new_zp, None, new_def);
          Succeeded(LineDone((([], new_zline, []), new_ctx, u_gen)));
        }
      }
    | (_, LetLineZP(zp, Some(ann), def)) =>
      let ty = ann |> UHTyp.expand;
      switch (Pat.ana_perform(ctx, u_gen, a, zp, ty)) {
      | Failed => Failed
      | CursorEscaped(side) => escape(u_gen, side)
      | Succeeded((new_zp, new_ctx, u_gen)) =>
        let ctx_def = Statics.Exp.ctx_for_let(ctx, zp |> ZPat.erase, ty, def);
        let (def, u_gen) =
          Statics.Exp.ana_fix_holes(ctx_def, u_gen, def, ty);
        let new_zline = ZExp.LetLineZP(new_zp, Some(ann), def);
        Succeeded(LineDone((([], new_zline, []), new_ctx, u_gen)));
      };

    | (_, LetLineZA(p, zann, def)) =>
      switch (Typ.perform(a, zann)) {
      | Failed => Failed
      | CursorEscaped(side) => escape(u_gen, side)
      | Succeeded(new_zann) =>
        let ty = new_zann |> ZTyp.erase |> UHTyp.expand;
        let (p, new_ctx, u_gen) =
          Statics.Pat.ana_fix_holes(ctx, u_gen, p, ty);
        let ctx_def = Statics.Exp.ctx_for_let(ctx, p, ty, def);
        let (def, u_gen) =
          Statics.Exp.ana_fix_holes(ctx_def, u_gen, def, ty);
        let new_zline = ZExp.LetLineZA(p, new_zann, def);
        Succeeded(LineDone((([], new_zline, []), new_ctx, u_gen)));
      }

    | (_, LetLineZE(p, None, zdef)) =>
      switch (Statics.Exp.syn(ctx, zdef |> ZExp.erase)) {
      | None => Failed
      | Some(def_ty) =>
        switch (syn_perform(ctx, a, (zdef, def_ty, u_gen))) {
        | Failed => Failed
        | CursorEscaped(side) => escape(u_gen, side)
        | Succeeded((new_zdef, new_def_ty, u_gen)) =>
          let (p, new_ctx, u_gen) =
            Statics.Pat.ana_fix_holes(ctx, u_gen, p, new_def_ty);
          let new_zline = ZExp.LetLineZE(p, None, new_zdef);
          Succeeded(LineDone((([], new_zline, []), new_ctx, u_gen)));
        }
      }
    | (_, LetLineZE(p, Some(ann), zdef)) =>
      let ty = ann |> UHTyp.expand;
      let ctx_def = Statics.Exp.ctx_for_let(ctx, p, ty, zdef |> ZExp.erase);
      switch (ana_perform(ctx_def, a, (zdef, u_gen), ty)) {
      | Failed => Failed
      | CursorEscaped(side) => escape(u_gen, side)
      | Succeeded((new_zdef, u_gen)) =>
        switch (Statics.Pat.ana(ctx, p, ty)) {
        | None => Failed
        | Some(new_ctx) =>
          let new_zline = ZExp.LetLineZE(p, Some(ann), new_zdef);
          Succeeded(LineDone((([], new_zline, []), new_ctx, u_gen)));
        }
      };
    };
  }
  and syn_perform_opseq =
      (
        ctx: Contexts.t,
        a: t,
        (
          ZOpSeq(skel, zseq) as zopseq: ZExp.zopseq,
          ty: HTyp.t,
          u_gen: MetaVarGen.t,
        ),
      )
      : result(syn_success) =>
    switch (a, zseq) {
    /* Invalid cursor positions */
    | (_, ZOperator((OnText(_) | OnDelim(_), _), _)) => Failed

    /* Invalid actions */
    | (UpdateApPalette(_), ZOperator(_)) => Failed

    /* Movement handled at top level */
    | (
        MoveTo(_) | MoveToBefore(_) | MoveToPrevHole | MoveToNextHole | MoveLeft |
        MoveRight,
        _,
      ) =>
      Failed

    /* Deletion */

    | (Delete, ZOperator((OnOp(After as side), _), _))
    | (Backspace, ZOperator((OnOp(Before as side), _), _)) =>
      syn_perform(ctx, escape(side), (ZE1(zopseq), ty, u_gen))
      |> wrap_in_SynDone

    /* Delete before operator == Backspace after operator */
    | (Delete, ZOperator((OnOp(Before), op), surround)) =>
      let new_ze =
        ZExp.ZE1(ZOpSeq(skel, ZOperator((OnOp(After), op), surround)));
      syn_perform(ctx, Backspace, (new_ze, ty, u_gen)) |> wrap_in_SynDone;

    /* ... + [k-1] +<| [k] + ... */
    | (Backspace, ZOperator((OnOp(After), _), surround)) =>
      let new_zseq = delete_operator(surround);
      Succeeded(SynDone(mk_and_syn_fix_ZOpSeq(ctx, u_gen, new_zseq)));

    /* ... + [k-1]  <|_ + [k+1] + ...  ==>   ... + [k-1]| + [k+1] + ... */
    | (
        Backspace,
        ZOperand(
          CursorE(_, EmptyHole(_)) as zhole,
          (A(Space, prefix_tl), suffix),
        ),
      )
        when ZExp.is_before_zoperand(zhole) =>
      let S(operand, new_prefix) = prefix_tl;
      let zoperand = operand |> ZExp.place_after_operand;
      let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, suffix));
      Succeeded(SynDone(mk_and_syn_fix_ZOpSeq(ctx, u_gen, new_zseq)));

    /* ... + [k-1] + _|>  [k+1] + ...  ==>   ... + [k-1] + |[k+1] + ... */
    | (
        Delete,
        ZOperand(
          CursorE(_, EmptyHole(_)) as zhole,
          (prefix, A(Space, suffix_tl)),
        ),
      )
        when ZExp.is_after_zoperand(zhole) =>
      let S(operand, new_suffix) = suffix_tl;
      let zoperand = operand |> ZExp.place_before_operand;
      let new_zseq = ZSeq.ZOperand(zoperand, (prefix, new_suffix));
      Succeeded(SynDone(mk_and_syn_fix_ZOpSeq(ctx, u_gen, new_zseq)));

    /* Construction */

    /* Space construction on operators becomes movement... */
    | (Construct(SOp(SSpace)), ZOperator(zoperator, _))
        when ZExp.is_after_zoperator(zoperator) =>
      syn_perform(ctx, MoveRight, (ZE1(zopseq), ty, u_gen))
      |> wrap_in_SynDone
    /* ...while other construction is applied after movement */
    | (Construct(_), ZOperator(zoperator, _)) =>
      let move_cursor =
        ZExp.is_before_zoperator(zoperator)
          ? ZExp.move_cursor_left : ZExp.move_cursor_right;
      switch (ZExp.ZE1(zopseq) |> move_cursor) {
      | None => Failed
      | Some(ze) => syn_perform(ctx, a, (ze, ty, u_gen)) |> wrap_in_SynDone
      };

    | (Construct(SOp(os)), ZOperand(zoperand, surround))
        when
          ZExp.is_before_zoperand(zoperand)
          || ZExp.is_after_zoperand(zoperand) =>
      switch (operator_of_shape(os)) {
      | None => Failed
      | Some(operator) =>
        let construct_operator =
          ZExp.is_before_zoperand(zoperand)
            ? construct_operator_before_zoperand
            : construct_operator_after_zoperand;
        let (zseq, u_gen) =
          construct_operator(u_gen, operator, zoperand, surround);
        Succeeded(SynDone(mk_and_syn_fix_ZOpSeq(ctx, u_gen, zseq)));
      }

    | (Construct(SLine), ZOperand(zoperand, (prefix, suffix)))
        when zoperand |> ZExp.is_after_zoperand =>
      let (new_line, u_gen) = {
        let operand = zoperand |> ZExp.erase_zoperand;
        let seq = Seq.affix_seq(prefix, S(operand, E));
        let (opseq, _, u_gen) = mk_and_syn_fix_OpSeq(ctx, u_gen, seq);
        (UHExp.ExpLine(opseq), u_gen);
      };
      let (new_zline, ty, u_gen) = {
        let (hole, u_gen) = u_gen |> UHExp.new_EmptyHole;
        let seq = Seq.seq_affix(S(hole, E), suffix);
        let (opseq, ty, u_gen) = mk_and_syn_fix_OpSeq(ctx, u_gen, seq);
        (ZExp.ExpLineZ(opseq |> ZExp.place_before_opseq), ty, u_gen);
      };
      let new_zblock = ([new_line], new_zline, []);
      Succeeded(SynDone((ZE2(new_zblock), ty, u_gen)));

    | (
        Construct(SLine),
        ZOperand(CursorE(_) as zoperand, (prefix, suffix)),
      ) =>
      let (new_line, u_gen) = {
        let (hole, u_gen) = u_gen |> UHExp.new_EmptyHole;
        let seq = Seq.affix_seq(prefix, S(hole, E));
        let (opseq, _, u_gen) = mk_and_syn_fix_OpSeq(ctx, u_gen, seq);
        (UHExp.ExpLine(opseq), u_gen);
      };
      let (new_zline, ty, u_gen) = {
        let zseq = ZSeq.ZOperand(zoperand, (E, suffix));
        let (zopseq, ty, u_gen) =
          switch (mk_and_syn_fix_ZOpSeq(ctx, u_gen, zseq)) {
          | (ZE2(_) | ZE0(_), _, _) => assert(false)
          | (ZE1(zopseq), ty, u_gen) => (zopseq, ty, u_gen)
          };
        (ZExp.ExpLineZ(zopseq), ty, u_gen);
      };
      let new_zblock = ([new_line], new_zline, []);
      Succeeded(SynDone((ZE2(new_zblock), ty, u_gen)));

    /* Zipper */

    | (_, ZOperand(zoperand, (prefix, suffix) as surround)) =>
      switch (CursorInfo.Exp.syn_cursor_info(ctx, ZE1(zopseq))) {
      | None => Failed
      | Some(ci) =>
        switch (ci |> CursorInfo.type_mode) {
        | None => Failed
        | Some(Syn) =>
          switch (syn_perform_operand(ctx, a, (zoperand, ty, u_gen))) {
          | Failed => Failed
          | CursorEscaped(side) =>
            syn_perform(ctx, escape(side), (ZE1(zopseq), ty, u_gen))
            |> wrap_in_SynDone
          | Succeeded(SynExpandsToCase({scrut, u_gen, _})) =>
            let (new_line, u_gen) = new_line_of_prefix(ctx, u_gen, prefix);
            let (new_scrut, u_gen) = append_suffix(u_gen, scrut, suffix);
            Succeeded(
              SynExpandsToCase({
                u_gen,
                prefix: [new_line],
                scrut: new_scrut,
                suffix: [],
              }),
            );
          | Succeeded(SynExpandsToLet({def, _})) =>
            let (new_line, u_gen) = new_line_of_prefix(ctx, u_gen, prefix);
            let (new_def, u_gen) = append_suffix(u_gen, def, suffix);
            Succeeded(
              SynExpandsToLet({
                u_gen,
                prefix: [new_line],
                def: new_def,
                suffix: [],
              }),
            );
          | Succeeded(SynDone((ze, _, u_gen))) =>
            let (new_ze, u_gen) = resurround_z(u_gen, ze, surround);
            Succeeded(
              SynDone(Statics.Exp.syn_fix_holes_z(ctx, u_gen, new_ze)),
            );
          }
        | Some(Ana(ty_zoperand)) =>
          switch (
            ana_perform_operand(ctx, a, (zoperand, u_gen), ty_zoperand)
          ) {
          | Failed => Failed
          | CursorEscaped(side) =>
            syn_perform(ctx, escape(side), (ZE1(zopseq), ty, u_gen))
            |> wrap_in_SynDone
          | Succeeded(AnaExpandsToCase({scrut, _})) =>
            let (new_line, u_gen) = new_line_of_prefix(ctx, u_gen, prefix);
            let (new_scrut, u_gen) = append_suffix(u_gen, scrut, suffix);
            Succeeded(
              SynExpandsToCase({
                u_gen,
                prefix: [new_line],
                scrut: new_scrut,
                suffix: [],
              }),
            );
          | Succeeded(AnaExpandsToLet({def, _})) =>
            let (new_line, u_gen) = new_line_of_prefix(ctx, u_gen, prefix);
            let (new_def, u_gen) = append_suffix(u_gen, def, suffix);
            Succeeded(
              SynExpandsToLet({
                u_gen,
                prefix: [new_line],
                def: new_def,
                suffix: [],
              }),
            );
          | Succeeded(AnaDone((ze, u_gen))) =>
            let (new_ze, u_gen) = resurround_z(u_gen, ze, surround);
            Succeeded(
              SynDone(Statics.Exp.syn_fix_holes_z(ctx, u_gen, new_ze)),
            );
          }
        }
      }
    }
  and syn_perform_operand =
      (
        ctx: Contexts.t,
        a: t,
        (zoperand: ZExp.zoperand, ty: HTyp.t, u_gen: MetaVarGen.t),
      )
      : result(syn_success) =>
    switch (a, zoperand) {
    /* Invalid cursor positions */
    | (
        _,
        CursorE(
          OnDelim(_) | OnOp(_),
          Var(_) | NumLit(_) | BoolLit(_) | ApPalette(_),
        ) |
        CursorE(
          OnText(_) | OnOp(_),
          EmptyHole(_) | ListNil(_) | Lam(_) | Inj(_) | Case(_) |
          Parenthesized(_) |
          ApPalette(_),
        ),
      ) =>
      Failed
    | (_, CursorE(cursor, operand))
        when !ZExp.is_valid_cursor_operand(cursor, operand) =>
      Failed

    /* Invalid actions at expression level */
    | (Construct(SLine), CursorE(OnText(_), _)) => Failed

    /* Movement handled at top level */
    | (
        MoveTo(_) | MoveToBefore(_) | MoveToPrevHole | MoveToNextHole | MoveLeft |
        MoveRight,
        _,
      ) =>
      Failed

    /* Backspace & Deletion */

    | (Backspace, _) when ZExp.is_before_zoperand(zoperand) =>
      CursorEscaped(Before)
    | (Delete, _) when ZExp.is_after_zoperand(zoperand) =>
      CursorEscaped(After)

    | (Backspace, CursorE(_, EmptyHole(_) as operand)) =>
      let ze = UHExp.E0(operand) |> ZExp.place_before;
      ze |> ZExp.is_after
        ? Succeeded(SynDone((ze, Hole, u_gen))) : CursorEscaped(Before);
    | (Delete, CursorE(_, EmptyHole(_) as operand)) =>
      let ze = UHExp.E0(operand) |> ZExp.place_after;
      ze |> ZExp.is_before
        ? Succeeded(SynDone((ze, Hole, u_gen))) : CursorEscaped(After);

    /* ( _ <|)   ==>   ( _| ) */
    | (Backspace, CursorE(OnDelim(_, Before), _)) =>
      syn_perform(ctx, MoveLeft, (ZE0(zoperand), ty, u_gen))
      |> wrap_in_SynDone
    /* (|> _ )   ==>   ( |_ ) */
    | (Delete, CursorE(OnDelim(_, After), _)) =>
      syn_perform(ctx, MoveRight, (ZE0(zoperand), ty, u_gen))
      |> wrap_in_SynDone

    /* Delete before delimiter == Backspace after delimiter */
    | (Delete, CursorE(OnDelim(k, Before), operand)) =>
      let new_ze = ZExp.ZE0(CursorE(OnDelim(k, After), operand));
      syn_perform(ctx, Backspace, (new_ze, ty, u_gen)) |> wrap_in_SynDone;

    | (Backspace, CursorE(OnDelim(_, After), ListNil(_))) =>
      let (zhole, u_gen) = u_gen |> ZExp.new_EmptyHole;
      let new_ze = ZExp.ZE0(zhole);
      Succeeded(SynDone((new_ze, Hole, u_gen)));

    | (Delete, CursorE(OnText(j), Var(_, _, x))) =>
      syn_delete_text(ctx, u_gen, j, x)
    | (Delete, CursorE(OnText(j), NumLit(_, n))) =>
      syn_delete_text(ctx, u_gen, j, string_of_int(n))
    | (Delete, CursorE(OnText(j), BoolLit(_, b))) =>
      syn_delete_text(ctx, u_gen, j, string_of_bool(b))

    | (Backspace, CursorE(OnText(j), Var(_, _, x))) =>
      syn_backspace_text(ctx, u_gen, j, x)
    | (Backspace, CursorE(OnText(j), NumLit(_, n))) =>
      syn_backspace_text(ctx, u_gen, j, string_of_int(n))
    | (Backspace, CursorE(OnText(j), BoolLit(_, b))) =>
      syn_backspace_text(ctx, u_gen, j, string_of_bool(b))

    /* \x :<| Num . x + 1   ==>   \x| . x + 1 */
    | (Backspace, CursorE(OnDelim(1, After), Lam(_, p, _, body))) =>
      let (p, body_ctx, u_gen) =
        Statics.Pat.ana_fix_holes(ctx, u_gen, p, Hole);
      let (body, body_ty, u_gen) =
        Statics.Exp.syn_fix_holes(body_ctx, u_gen, body);
      let new_ze =
        ZExp.ZE0(LamZP(NotInHole, ZPat.place_after(p), None, body));
      Succeeded((new_ze, HTyp.Arrow(Hole, body_ty), u_gen))
      |> wrap_in_SynDone;

    | (
        Backspace,
        CursorE(
          OnDelim(k, After),
          (
            Lam(_, _, _, e) | Inj(_, _, e) | Case(_, e, _, _) |
            Parenthesized(e)
          ) as operand,
        ),
      ) =>
      let place_cursor =
        switch (operand) {
        | Lam(_) =>
          switch (k) {
          | 0
          | 2 => ZExp.place_before
          | _three => ZExp.place_after
          }
        | _ =>
          switch (k) {
          | 0 => ZExp.place_before
          | _one => ZExp.place_after
          }
        };
      let new_ze = e |> place_cursor;
      Succeeded(SynDone(Statics.Exp.syn_fix_holes_z(ctx, u_gen, new_ze)));

    /* TODO consider deletion of type ascription on case */

    /* Construction */

    | (
        Construct(SOp(SSpace)),
        CursorE(_, Var(_, InVarHole(Keyword(k), _), _)),
      )
        when zoperand |> ZExp.is_after_zoperand =>
      let (zhole, u_gen) = u_gen |> ZExp.new_EmptyHole;
      syn_perform_operand(
        ctx,
        keyword_action(k),
        (zhole, HTyp.Hole, u_gen),
      );
    | (Construct(SCase), CursorE(_, operand)) =>
      Succeeded(mk_SynExpandsToCase(~u_gen, ~scrut=E0(operand), ()))
    | (Construct(SLet), CursorE(_, operand)) =>
      Succeeded(mk_SynExpandsToLet(~u_gen, ~def=E0(operand), ()))

    | (Construct(SAsc), LamZP(err, zp, None, body)) =>
      let new_zann = UHTyp.T0(Hole) |> ZTyp.place_before;
      let new_ze = ZExp.ZE0(LamZA(err, zp |> ZPat.erase, new_zann, body));
      Succeeded(SynDone((new_ze, ty, u_gen)));
    | (Construct(SAsc), LamZP(err, zp, Some(ann), body)) =>
      /* just move the cursor over if there is already an ascription */
      let new_zann = ann |> ZTyp.place_before;
      let new_ze = ZExp.ZE0(LamZA(err, zp |> ZPat.erase, new_zann, body));
      Succeeded(SynDone((new_ze, ty, u_gen)));
    | (Construct(SAsc), CursorE(_, Case(_, scrut, rules, Some(ann)))) =>
      /* just move the cursor over if there is already an ascription */
      let new_zann = ann |> ZTyp.place_before;
      let new_ze = ZExp.ZE0(CaseZA(NotInHole, scrut, rules, new_zann));
      Succeeded(SynDone((new_ze, ty, u_gen)));
    | (Construct(SAsc), CursorE(_)) => Failed

    | (Construct(SChar(s)), CursorE(_, EmptyHole(_))) =>
      syn_insert_text(ctx, u_gen, (0, s), "")
    | (Construct(SChar(s)), CursorE(OnText(j), Var(_, _, x))) =>
      syn_insert_text(ctx, u_gen, (j, s), x)
    | (Construct(SChar(s)), CursorE(OnText(j), NumLit(_, n))) =>
      syn_insert_text(ctx, u_gen, (j, s), string_of_int(n))
    | (Construct(SChar(s)), CursorE(OnText(j), BoolLit(_, b))) =>
      syn_insert_text(ctx, u_gen, (j, s), string_of_bool(b))
    | (Construct(SChar(_)), CursorE(_)) => Failed

    | (Construct(SListNil), CursorE(_, EmptyHole(_))) =>
      let new_ze = UHExp.(E0(listnil())) |> ZExp.place_after;
      let new_ty = HTyp.List(Hole);
      Succeeded(SynDone((new_ze, new_ty, u_gen)));
    | (Construct(SListNil), CursorE(_, _)) => Failed

    | (Construct(SParenthesized), CursorE(_)) =>
      let new_ze = ZExp.ZE0(ParenthesizedZ(ZE0(zoperand)));
      Succeeded(SynDone((new_ze, ty, u_gen)));

    | (Construct(SInj(side)), CursorE(_)) =>
      let new_ze = ZExp.ZE0(InjZ(NotInHole, side, ZE0(zoperand)));
      let new_ty =
        switch (side) {
        | L => HTyp.Sum(ty, Hole)
        | R => HTyp.Sum(Hole, ty)
        };
      Succeeded(SynDone((new_ze, new_ty, u_gen)));

    | (Construct(SLam), CursorE(_, operand)) =>
      let (zhole, u_gen) = u_gen |> ZPat.new_EmptyHole;
      let new_ze =
        ZExp.ZE0(
          LamZP(NotInHole, ZP0(zhole), Some(T0(Hole)), E0(operand)),
        );
      Succeeded(SynDone((new_ze, HTyp.Arrow(Hole, ty), u_gen)));

    | (Construct(SApPalette(name)), CursorE(_, EmptyHole(_))) =>
      let palette_ctx = Contexts.palette_ctx(ctx);
      switch (PaletteCtx.lookup(palette_ctx, name)) {
      | None => Failed
      | Some(palette_defn) =>
        let init_model_cmd = palette_defn.init_model;
        let (init_model, init_splice_info, u_gen) =
          SpliceGenMonad.exec(init_model_cmd, SpliceInfo.empty, u_gen);
        switch (Statics.Exp.ana_splice_map(ctx, init_splice_info.splice_map)) {
        | None => Failed
        | Some(splice_ctx) =>
          let expansion_ty = palette_defn.expansion_ty;
          let expand = palette_defn.expand;
          let expansion = expand(init_model);
          switch (Statics.Exp.ana(splice_ctx, expansion, expansion_ty)) {
          | None => Failed
          | Some(_) =>
            Succeeded(
              SynDone((
                ZExp.(
                  ZE0(
                    place_before_operand(
                      ApPalette(
                        NotInHole,
                        name,
                        init_model,
                        init_splice_info,
                      ),
                    ),
                  )
                ),
                expansion_ty,
                u_gen,
              )),
            )
          };
        };
      };
    | (Construct(SApPalette(_)), CursorE(_)) => Failed
    /* TODO
       | (UpdateApPalette(_), CursorE(_, ApPalette(_, _name, _, _hole_data))) =>
          let (_, palette_ctx) = ctx;
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
    | (UpdateApPalette(_), CursorE(_)) => Failed

    | (Construct(SOp(SSpace)), CursorE(OnDelim(_, After), _))
        when !ZExp.is_after_zoperand(zoperand) =>
      syn_perform(ctx, MoveRight, (ZE0(zoperand), ty, u_gen))
      |> wrap_in_SynDone

    | (Construct(SOp(os)), CursorE(_)) =>
      switch (operator_of_shape(os)) {
      | None => Failed
      | Some(operator) =>
        let construct_operator =
          ZExp.is_before_zoperand(zoperand)
            ? construct_operator_before_zoperand
            : construct_operator_after_zoperand;
        let (zseq, u_gen) =
          construct_operator(u_gen, operator, zoperand, (E, E));
        Succeeded(SynDone(mk_and_syn_fix_ZOpSeq(ctx, u_gen, zseq)));
      }

    | (Construct(_), CursorE(OnDelim(_, side), _))
        when
          !ZExp.is_before_zoperand(zoperand)
          && !ZExp.is_after_zoperand(zoperand) =>
      switch (syn_perform(ctx, escape(side), (ZE0(zoperand), ty, u_gen))) {
      | Failed
      | CursorEscaped(_) => Failed
      | Succeeded(new_edit_state) =>
        syn_perform(ctx, a, new_edit_state) |> wrap_in_SynDone
      }
    | (Construct(_), CursorE(OnDelim(_), _)) => Failed

    /* Zipper Cases */
    | (_, ParenthesizedZ(zbody)) =>
      switch (syn_perform(ctx, a, (zbody, ty, u_gen))) {
      | Failed => Failed
      | CursorEscaped(side) =>
        syn_perform(ctx, escape(side), (ZE0(zoperand), ty, u_gen))
        |> wrap_in_SynDone
      | Succeeded((new_zbody, new_ty, u_gen)) =>
        let new_ze = ZExp.ZE0(ParenthesizedZ(new_zbody));
        Succeeded(SynDone((new_ze, new_ty, u_gen)));
      }
    | (_, LamZP(_, zp, ann, body)) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => HTyp.Hole
        };
      switch (Pat.ana_perform(ctx, u_gen, a, zp, ty1)) {
      | Failed => Failed
      | CursorEscaped(side) =>
        syn_perform(ctx, escape(side), (ZE0(zoperand), ty, u_gen))
        |> wrap_in_SynDone
      | Succeeded((zp, ctx, u_gen)) =>
        let (body, ty2, u_gen) = Statics.Exp.syn_fix_holes(ctx, u_gen, body);
        let new_ty = HTyp.Arrow(ty1, ty2);
        let new_ze = ZExp.ZE0(LamZP(NotInHole, zp, ann, body));
        Succeeded(SynDone((new_ze, new_ty, u_gen)));
      };
    | (_, LamZA(_, p, zann, body)) =>
      switch (Typ.perform(a, zann)) {
      | Failed => Failed
      | CursorEscaped(side) =>
        syn_perform(ctx, escape(side), (ZE0(zoperand), ty, u_gen))
        |> wrap_in_SynDone
      | Succeeded(zann) =>
        let ty1 = UHTyp.expand(ZTyp.erase(zann));
        let (p, ctx, u_gen) = Statics.Pat.ana_fix_holes(ctx, u_gen, p, ty1);
        let (body, ty2, u_gen) = Statics.Exp.syn_fix_holes(ctx, u_gen, body);
        let new_ze = ZExp.ZE0(LamZA(NotInHole, p, zann, body));
        Succeeded(SynDone((new_ze, Arrow(ty1, ty2), u_gen)));
      }
    | (_, LamZE(_, p, ann, zbody)) =>
      switch (HTyp.matched_arrow(ty)) {
      | None => Failed
      | Some((_, ty2)) =>
        let ty1 =
          switch (ann) {
          | Some(uty1) => UHTyp.expand(uty1)
          | None => HTyp.Hole
          };
        switch (Statics.Pat.ana(ctx, p, ty1)) {
        | None => Failed
        | Some(ctx_body) =>
          switch (syn_perform(ctx_body, a, (zbody, ty2, u_gen))) {
          | Failed => Failed
          | CursorEscaped(side) =>
            syn_perform(ctx, escape(side), (ZE0(zoperand), ty, u_gen))
            |> wrap_in_SynDone
          | Succeeded((zbody, ty2, u_gen)) =>
            let new_ze = ZExp.ZE0(LamZE(NotInHole, p, ann, zbody));
            Succeeded(SynDone((new_ze, Arrow(ty1, ty2), u_gen)));
          }
        };
      }
    | (_, InjZ(_, side, zbody)) =>
      switch (ty) {
      | Sum(ty1, ty2) =>
        let ty_side = InjSide.pick(side, ty1, ty2);
        switch (syn_perform(ctx, a, (zbody, ty_side, u_gen))) {
        | Failed => Failed
        | CursorEscaped(side) =>
          syn_perform(ctx, escape(side), (ZE0(zoperand), ty, u_gen))
          |> wrap_in_SynDone
        | Succeeded((zbody, ty_side', u_gen)) =>
          let new_ty =
            switch (side) {
            | L => HTyp.Sum(ty_side', ty2)
            | R => HTyp.Sum(ty1, ty_side')
            };
          let new_ze = ZExp.ZE0(InjZ(NotInHole, side, zbody));
          Succeeded(SynDone((new_ze, new_ty, u_gen)));
        };
      | _ => Failed /* should never happen */
      }
    | (_, ApPaletteZ(_, _name, _serialized_model, _z_hole_data)) => Failed
    /* TODO let (next_lbl, z_nat_map) = z_hole_data;
       let (rest_map, z_data) = z_nat_map;
       let (cell_lbl, cell_data) = z_data;
       let (cell_ty, cell_ze) = cell_data;
       switch (ana_perform_operand(ctx, a, (cell_ze, u_gen), cell_ty)) {
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
    | (_, CaseZE(_, _, _, None) | CaseZR(_, _, _, None)) => Failed
    | (_, CaseZE(_, zscrut, rules, Some(uty) as ann)) =>
      switch (Statics.Exp.syn(ctx, ZExp.erase(zscrut))) {
      | None => Failed
      | Some(ty1) =>
        switch (syn_perform(ctx, a, (zscrut, ty1, u_gen))) {
        | Failed => Failed
        | CursorEscaped(side) =>
          syn_perform(ctx, escape(side), (ZE0(zoperand), ty, u_gen))
          |> wrap_in_SynDone
        | Succeeded((zscrut, ty1, u_gen)) =>
          let ty = UHTyp.expand(uty);
          let (rules, u_gen) =
            Statics.Exp.ana_fix_holes_rules(ctx, u_gen, rules, ty1, ty);
          let new_ze = ZExp.ZE0(CaseZE(NotInHole, zscrut, rules, ann));
          Succeeded(SynDone((new_ze, ty, u_gen)));
        }
      }
    | (_, CaseZR(_, scrut, zrules, Some(ann))) =>
      switch (Statics.Exp.syn(ctx, scrut)) {
      | None => Failed
      | Some(pat_ty) =>
        let clause_ty = ann |> UHTyp.expand;
        switch (
          ana_perform_rules(ctx, a, (zrules, u_gen), pat_ty, clause_ty)
        ) {
        | Failed => Failed
        | CursorEscaped(side) =>
          syn_perform(ctx, escape(side), (ZE0(zoperand), ty, u_gen))
          |> wrap_in_SynDone
        | Succeeded((new_zrules, u_gen)) =>
          let new_ze =
            ZExp.ZE0(CaseZR(NotInHole, scrut, new_zrules, Some(ann)));
          Succeeded(SynDone((new_ze, clause_ty, u_gen)));
        };
      }
    | (_, CaseZA(_, scrut, rules, zann)) =>
      switch (Statics.Exp.syn(ctx, scrut)) {
      | None => Failed
      | Some(ty1) =>
        switch (Typ.perform(a, zann)) {
        | Failed => Failed
        | CursorEscaped(side) =>
          syn_perform(ctx, escape(side), (ZE0(zoperand), ty, u_gen))
          |> wrap_in_SynDone
        | Succeeded(zann) =>
          let ty = UHTyp.expand(ZTyp.erase(zann));
          let (rules, u_gen) =
            Statics.Exp.ana_fix_holes_rules(ctx, u_gen, rules, ty1, ty);
          let new_ze = ZExp.ZE0(CaseZA(NotInHole, scrut, rules, zann));
          Succeeded(SynDone((new_ze, ty, u_gen)));
        }
      }
    }
  and ana_perform_rules =
      (
        ctx: Contexts.t,
        a: t,
        (
          (prefix, zrule, suffix) as zrules: ZExp.zrules,
          u_gen: MetaVarGen.t,
        ),
        pat_ty: HTyp.t,
        clause_ty: HTyp.t,
      )
      : result((ZExp.zrules, MetaVarGen.t)) => {
    let escape = (side: Side.t) => {
      let move_cursor =
        switch (side) {
        | Before => ZExp.move_cursor_left_zrules
        | After => ZExp.move_cursor_right_zrules
        };
      zrules
      |> move_cursor
      |> Opt.map_default(~default=CursorEscaped(side), new_zrules =>
           Succeeded((new_zrules, u_gen))
         );
    };

    switch (a, zrule) {
    /* Invalid cursor positions */
    | (_, CursorR(OnText(_) | OnOp(_), _)) => Failed

    /* Movement handled at top level */
    | (
        MoveTo(_) | MoveToBefore(_) | MoveToPrevHole | MoveToNextHole | MoveLeft |
        MoveRight,
        _,
      ) =>
      Failed

    /* Backspace & Delete */

    | (Backspace, CursorR(OnDelim(_, Before as side), _))
    | (Delete, CursorR(OnDelim(_, After as side), _)) => escape(side)

    // Delete before delim == Backspace after delim
    | (Delete, CursorR(OnDelim(k, Before), rule)) =>
      let new_zrules =
        zrules |> ZList.replace_z(ZExp.CursorR(OnDelim(k, After), rule));
      ana_perform_rules(
        ctx,
        Backspace,
        (new_zrules, u_gen),
        pat_ty,
        clause_ty,
      );
    | (Backspace, CursorR(OnDelim(_, After), _)) =>
      switch (prefix |> split_last, suffix) {
      | (None, []) =>
        let (new_zrule, u_gen) = u_gen |> ZExp.empty_zrule;
        let new_zrules = ([], new_zrule, []);
        Succeeded((new_zrules, u_gen));
      | (_, [suffix_hd, ...new_suffix]) =>
        let new_zrule = suffix_hd |> ZExp.place_before_rule;
        let new_zrules = (prefix, new_zrule, new_suffix);
        Succeeded((new_zrules, u_gen));
      | (Some((new_prefix, prefix_last)), _) =>
        let new_zrule = prefix_last |> ZExp.place_after_rule;
        let new_zrules = (new_prefix, new_zrule, suffix);
        Succeeded((new_zrules, u_gen));
      }

    /* Construction */

    | (Construct(SOp(SSpace)), CursorR(OnDelim(_, After), _))
        when !ZExp.is_after_zrule(zrule) =>
      escape(After)

    | (Construct(SLine), RuleZP(zp, _)) when zp |> ZPat.is_before =>
      let (new_zrule, u_gen) = u_gen |> ZExp.empty_zrule;
      let new_zrules = (
        prefix,
        new_zrule,
        [zrule |> ZExp.erase_zrule, ...suffix],
      );
      Succeeded((new_zrules, u_gen));
    | (Construct(SLine), RuleZP(zp, _)) when zp |> ZPat.is_after =>
      let (new_zrule, u_gen) = u_gen |> ZExp.empty_zrule;
      let new_zrules = (
        prefix @ [zrule |> ZExp.erase_zrule],
        new_zrule,
        suffix,
      );
      Succeeded((new_zrules, u_gen));
    | (Construct(SLine), RuleZE(_, zclause)) when zclause |> ZExp.is_after =>
      let (new_zrule, u_gen) = u_gen |> ZExp.empty_zrule;
      let new_zrules = (
        prefix @ [zrule |> ZExp.erase_zrule],
        new_zrule,
        suffix,
      );
      Succeeded((new_zrules, u_gen));

    | (Construct(_) | UpdateApPalette(_), CursorR(OnDelim(_, side), _))
        when !ZExp.is_before_zrule(zrule) && !ZExp.is_after_zrule(zrule) =>
      switch (escape(side)) {
      | Failed
      | CursorEscaped(_) => Failed
      | Succeeded((zrules, u_gen)) =>
        ana_perform_rules(ctx, a, (zrules, u_gen), pat_ty, clause_ty)
      }
    | (Construct(_) | UpdateApPalette(_), CursorR(OnDelim(_), _)) => Failed

    /* Zipper */
    | (_, RuleZP(zp, clause)) =>
      switch (Pat.ana_perform(ctx, u_gen, a, zp, pat_ty)) {
      | Failed => Failed
      | CursorEscaped(side) => escape(side)
      | Succeeded((new_zp, ctx, u_gen)) =>
        let (clause, u_gen) =
          Statics.Exp.ana_fix_holes(ctx, u_gen, clause, clause_ty);
        let new_zrules =
          zrules |> ZList.replace_z(ZExp.RuleZP(new_zp, clause));
        Succeeded((new_zrules, u_gen));
      }

    | (_, RuleZE(p, zclause)) =>
      switch (Statics.Pat.ana(ctx, p, pat_ty)) {
      | None => Failed
      | Some(ctx) =>
        switch (ana_perform(ctx, a, (zclause, u_gen), clause_ty)) {
        | Failed => Failed
        | CursorEscaped(side) => escape(side)
        | Succeeded((new_zclause, u_gen)) =>
          let new_zrules =
            zrules |> ZList.replace_z(ZExp.RuleZE(p, new_zclause));
          Succeeded((new_zrules, u_gen));
        }
      }
    };
  }
  and ana_perform =
      (
        ctx: Contexts.t,
        a: t,
        (ze, u_gen): (ZExp.t, MetaVarGen.t),
        ty: HTyp.t,
      )
      : result((ZExp.t, MetaVarGen.t)) =>
    switch (a) {
    /* Movement */
    | MoveTo(path) =>
      switch (CursorPath.Exp.follow(path, ze |> ZExp.erase)) {
      | None => Failed
      | Some(ze) => Succeeded((ze, u_gen))
      }
    | MoveToBefore(steps) =>
      switch (CursorPath.Exp.follow_steps(steps, ze |> ZExp.erase)) {
      | None => Failed
      | Some(ze) => Succeeded((ze, u_gen))
      }
    | MoveToPrevHole =>
      switch (CursorPath.Exp.prev_hole_steps_z(ze)) {
      | None => Failed
      | Some(steps) =>
        ana_perform(ctx, MoveToBefore(steps), (ze, u_gen), ty)
      }
    | MoveToNextHole =>
      switch (CursorPath.Exp.next_hole_steps_z(ze)) {
      | None => Failed
      | Some(steps) =>
        ana_perform(ctx, MoveToBefore(steps), (ze, u_gen), ty)
      }
    | MoveLeft =>
      ze
      |> ZExp.move_cursor_left
      |> Opt.map_default(~default=CursorEscaped(Before), ze =>
           Succeeded((ze, u_gen))
         )
    | MoveRight =>
      ze
      |> ZExp.move_cursor_right
      |> Opt.map_default(~default=CursorEscaped(After), ze =>
           Succeeded((ze, u_gen))
         )
    | _ =>
      let result =
        switch (ze) {
        | ZE2(ze2) => ana_perform_block(ctx, a, (ze2, u_gen), ty)
        | ZE1(ze1) => ana_perform_opseq(ctx, a, (ze1, u_gen), ty)
        | ZE0(ze0) => ana_perform_operand(ctx, a, (ze0, u_gen), ty)
        };
      switch (result) {
      | (Failed | CursorEscaped(_)) as err => err
      | Succeeded(AnaDone(ana_done)) => Succeeded(ana_done)
      | Succeeded(AnaExpandsToCase({prefix, scrut, suffix, u_gen})) =>
        let (zcase, u_gen) =
          zcase_of_scrut_and_suffix(Syn, u_gen, scrut, suffix);
        let new_ze = ZExp.ZE2((prefix, ExpLineZ(zcase |> ZOpSeq.wrap), []));
        Succeeded(Statics.Exp.ana_fix_holes_z(ctx, u_gen, new_ze, ty));
      | Succeeded(AnaExpandsToLet({prefix, def, suffix, u_gen})) =>
        let (zp_hole, u_gen) = u_gen |> ZPat.new_EmptyHole;
        let zlet = ZExp.LetLineZP(ZP0(zp_hole), None, def);
        let new_ze = ZExp.ZE2((prefix, zlet, suffix));
        Succeeded(Statics.Exp.ana_fix_holes_z(ctx, u_gen, new_ze, ty));
      };
    }
  and ana_perform_block =
      (
        ctx: Contexts.t,
        a: t,
        (
          (prefix, zline, suffix) as zblock: ZExp.zblock,
          u_gen: MetaVarGen.t,
        ),
        ty: HTyp.t,
      )
      : result(ana_success) =>
    switch (a, zline) {
    /* Movement handled at top level */
    | (
        MoveTo(_) | MoveToBefore(_) | MoveToPrevHole | MoveToNextHole | MoveLeft |
        MoveRight,
        _,
      ) =>
      Failed

    /* Backspace & Delete */

    | (Delete, _) when ZExp.is_after_zline(zline) =>
      switch (zline |> ZExp.erase_zline, suffix) {
      | (_, []) => CursorEscaped(After)
      | (EmptyLine, [suffix_hd, ...new_suffix]) =>
        let new_zline = suffix_hd |> ZExp.place_before_line;
        let new_ze = ZExp.ZE2((prefix, new_zline, new_suffix));
        Succeeded(AnaDone((new_ze, u_gen)));
      | (_, [EmptyLine, ...new_suffix])
      | (
          ExpLine(_),
          [ExpLine(OpSeq(_, S(EmptyHole(_), E))), ...new_suffix],
        ) =>
        let new_ze = ZExp.ZE2((prefix, zline, new_suffix));
        Succeeded(
          AnaDone(Statics.Exp.ana_fix_holes_z(ctx, u_gen, new_ze, ty)),
        );
      | _ =>
        ana_perform(ctx, MoveRight, (ZE2(zblock), u_gen), ty)
        |> wrap_in_AnaDone
      }
    | (Backspace, _) when ZExp.is_before_zline(zline) =>
      switch (prefix |> split_last, zline |> ZExp.erase_zline) {
      | (None, _) => CursorEscaped(Before)
      | (Some((new_prefix, prefix_hd)), EmptyLine) =>
        let new_zline = prefix_hd |> ZExp.place_after_line;
        let new_ze = ZExp.ZE2((new_prefix, new_zline, suffix));
        Succeeded(AnaDone((new_ze, u_gen)));
      | (Some((new_prefix, EmptyLine)), _) =>
        let new_ze = ZExp.ZE2((new_prefix, zline, suffix));
        Succeeded(AnaDone((new_ze, u_gen)));
      | (
          Some((new_prefix, ExpLine(_) as prefix_hd)),
          ExpLine(OpSeq(_, S(EmptyHole(_), E))),
        ) =>
        let new_zline = prefix_hd |> ZExp.place_after_line;
        let new_ze = ZExp.ZE2((new_prefix, new_zline, suffix));
        Succeeded(
          AnaDone(Statics.Exp.ana_fix_holes_z(ctx, u_gen, new_ze, ty)),
        );
      | _ =>
        ana_perform(ctx, MoveLeft, (ZE2(zblock), u_gen), ty)
        |> wrap_in_AnaDone
      }

    /* No construction handled at block level */

    /* Zipper */
    | _ =>
      switch (Statics.Exp.syn_lines(ctx, prefix)) {
      | None => Failed
      | Some(ctx_zline) =>
        switch (suffix) {
        | [] =>
          switch (zline) {
          | CursorL(_)
          | LetLineZP(_)
          | LetLineZA(_)
          | LetLineZE(_) => Failed
          | ExpLineZ(zopseq) =>
            switch (ana_perform(ctx_zline, a, (ZE1(zopseq), u_gen), ty)) {
            | Failed => Failed
            | CursorEscaped(side) =>
              ana_perform(ctx, escape(side), (ZE2(zblock), u_gen), ty)
              |> wrap_in_AnaDone
            | Succeeded((ZE0(zoperand), u_gen)) =>
              let new_ze =
                ZExp.ZE2((prefix, ExpLineZ(ZOpSeq.wrap(zoperand)), []));
              Succeeded(AnaDone((new_ze, u_gen)));
            | Succeeded((ZE1(zopseq), u_gen)) =>
              let new_ze = ZExp.ZE2((prefix, ExpLineZ(zopseq), []));
              Succeeded(AnaDone((new_ze, u_gen)));
            | Succeeded((ZE2((inner_prefix, zline, suffix)), u_gen)) =>
              let new_ze = ZExp.ZE2((prefix @ inner_prefix, zline, suffix));
              Succeeded(AnaDone((new_ze, u_gen)));
            }
          }
        | [_, ..._] =>
          switch (syn_perform_line(ctx_zline, a, (zline, u_gen))) {
          | Failed => Failed
          | CursorEscaped(side) =>
            ana_perform(ctx, escape(side), (ZE2(zblock), u_gen), ty)
            |> wrap_in_AnaDone
          | Succeeded(LineExpandsToCase(r)) =>
            Succeeded(
              AnaExpandsToCase({
                u_gen: r.u_gen,
                prefix: prefix @ r.prefix,
                scrut: r.scrut,
                suffix: r.suffix @ suffix,
              }),
            )
          | Succeeded(LineExpandsToLet(r)) =>
            Succeeded(
              AnaExpandsToLet({
                u_gen: r.u_gen,
                prefix: prefix @ r.prefix,
                def: r.def,
                suffix: r.suffix @ suffix,
              }),
            )
          | Succeeded(
              LineDone((
                (inner_prefix, new_zline, inner_suffix),
                ctx_suffix,
                u_gen,
              )),
            ) =>
            let (suffix, u_gen) =
              Statics.Exp.ana_fix_holes_block(ctx_suffix, u_gen, suffix, ty);
            let new_ze =
              ZExp.ZE2((
                prefix @ inner_prefix,
                new_zline,
                inner_suffix @ suffix,
              ));
            Succeeded(AnaDone((new_ze, u_gen)));
          }
        }
      }
    }
  and ana_perform_opseq =
      (
        ctx: Contexts.t,
        a: t,
        (ZOpSeq(skel, zseq) as zopseq: ZExp.zopseq, u_gen: MetaVarGen.t),
        ty: HTyp.t,
      )
      : result(ana_success) =>
    switch (a, zseq) {
    /* Invalid cursor positions */
    | (_, ZOperator((OnText(_) | OnDelim(_), _), _)) => Failed

    /* Invalid actions */
    | (UpdateApPalette(_), ZOperator(_)) => Failed

    /* Movement handled at top level */
    | (
        MoveTo(_) | MoveToBefore(_) | MoveToPrevHole | MoveToNextHole | MoveLeft |
        MoveRight,
        _,
      ) =>
      Failed

    /* Deletion */

    | (Delete, ZOperator((OnOp(After as side), _), _))
    | (Backspace, ZOperator((OnOp(Before as side), _), _)) =>
      ana_perform(ctx, escape(side), (ZE1(zopseq), u_gen), ty)
      |> wrap_in_AnaDone

    /* Delete before operator == Backspace after operator */
    | (Delete, ZOperator((OnOp(Before), op), surround)) =>
      let new_ze =
        ZExp.ZE1(ZOpSeq(skel, ZOperator((OnOp(After), op), surround)));
      ana_perform(ctx, Backspace, (new_ze, u_gen), ty) |> wrap_in_AnaDone;

    /* ... + [k-1] +<| [k] + ... */
    | (Backspace, ZOperator((OnOp(After), _), surround)) =>
      let new_zseq = delete_operator(surround);
      Succeeded(mk_and_ana_fix_ZOpSeq(ctx, u_gen, new_zseq, ty))
      |> wrap_in_AnaDone;

    /* ... + [k-1]  <|_ + [k+1] + ...  ==>   ... + [k-1]| + [k+1] + ... */
    | (
        Backspace,
        ZOperand(
          CursorE(_, EmptyHole(_)) as zhole,
          (A(Space, prefix_tl), suffix),
        ),
      )
        when ZExp.is_before_zoperand(zhole) =>
      let S(operand, new_prefix) = prefix_tl;
      let zoperand = operand |> ZExp.place_after_operand;
      let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, suffix));
      Succeeded(mk_and_ana_fix_ZOpSeq(ctx, u_gen, new_zseq, ty))
      |> wrap_in_AnaDone;

    /* ... + [k-1] + _|>  [k+1] + ...  ==>   ... + [k-1] + |[k+1] + ... */
    | (
        Delete,
        ZOperand(
          CursorE(_, EmptyHole(_)) as zhole,
          (prefix, A(Space, suffix_tl)),
        ),
      )
        when ZExp.is_after_zoperand(zhole) =>
      let S(operand, new_suffix) = suffix_tl;
      let zoperand = operand |> ZExp.place_before_operand;
      let new_zseq = ZSeq.ZOperand(zoperand, (prefix, new_suffix));
      Succeeded(mk_and_ana_fix_ZOpSeq(ctx, u_gen, new_zseq, ty))
      |> wrap_in_AnaDone;

    /* Construction */

    /* construction on operators either becomes movement... */
    | (Construct(SOp(SSpace)), ZOperator(zoperator, _))
        when ZExp.is_after_zoperator(zoperator) =>
      ana_perform(ctx, MoveRight, (ZE1(zopseq), u_gen), ty)
      |> wrap_in_AnaDone
    /* ...or construction after movement */
    | (Construct(_), ZOperator(zoperator, _)) =>
      let move_cursor =
        ZExp.is_before_zoperator(zoperator)
          ? ZExp.move_cursor_left : ZExp.move_cursor_right;
      switch (ZExp.ZE1(zopseq) |> move_cursor) {
      | None => Failed
      | Some(ze) => ana_perform(ctx, a, (ze, u_gen), ty) |> wrap_in_AnaDone
      };

    | (Construct(SOp(os)), ZOperand(zoperand, surround))
        when
          ZExp.is_before_zoperand(zoperand)
          || ZExp.is_after_zoperand(zoperand) =>
      switch (operator_of_shape(os)) {
      | None => Failed
      | Some(operator) =>
        let construct_operator =
          ZExp.is_before_zoperand(zoperand)
            ? construct_operator_before_zoperand
            : construct_operator_after_zoperand;
        let (zseq, u_gen) =
          construct_operator(u_gen, operator, zoperand, surround);
        Succeeded(mk_and_ana_fix_ZOpSeq(ctx, u_gen, zseq, ty))
        |> wrap_in_AnaDone;
      }

    | (Construct(SLine), ZOperand(zoperand, (prefix, suffix)))
        when zoperand |> ZExp.is_after_zoperand =>
      let (new_line, u_gen) = {
        let operand = zoperand |> ZExp.erase_zoperand;
        let seq = Seq.affix_seq(prefix, S(operand, E));
        let (opseq, u_gen) = mk_and_ana_fix_OpSeq(ctx, u_gen, seq, ty);
        (UHExp.ExpLine(opseq), u_gen);
      };
      let (new_zline, u_gen) = {
        let (hole, u_gen) = u_gen |> UHExp.new_EmptyHole;
        let seq = Seq.seq_affix(S(hole, E), suffix);
        let (opseq, u_gen) = mk_and_ana_fix_OpSeq(ctx, u_gen, seq, ty);
        (ZExp.ExpLineZ(opseq |> ZExp.place_before_opseq), u_gen);
      };
      let new_zblock = ([new_line], new_zline, []);
      Succeeded(AnaDone((ZE2(new_zblock), u_gen)));

    | (
        Construct(SLine),
        ZOperand(CursorE(_) as zoperand, (prefix, suffix)),
      ) =>
      let (new_line, u_gen) = {
        let (hole, u_gen) = u_gen |> UHExp.new_EmptyHole;
        let seq = Seq.affix_seq(prefix, S(hole, E));
        let (opseq, _, u_gen) = mk_and_syn_fix_OpSeq(ctx, u_gen, seq);
        (UHExp.ExpLine(opseq), u_gen);
      };
      let (new_zline, u_gen) = {
        let zseq = ZSeq.ZOperand(zoperand, (E, suffix));
        let (zopseq, u_gen) =
          switch (mk_and_ana_fix_ZOpSeq(ctx, u_gen, zseq, ty)) {
          | (ZE2(_) | ZE0(_), _) => assert(false)
          | (ZE1(zopseq), u_gen) => (zopseq, u_gen)
          };
        (ZExp.ExpLineZ(zopseq), u_gen);
      };
      let new_zblock = ([new_line], new_zline, []);
      Succeeded(AnaDone((ZE2(new_zblock), u_gen)));

    /* Zipper */

    | (_, ZOperand(zoperand, (prefix, suffix) as surround)) =>
      switch (CursorInfo.Exp.ana_cursor_info(ctx, ZE1(zopseq), ty)) {
      | None => Failed
      | Some(ci) =>
        switch (ci |> CursorInfo.type_mode) {
        | None => Failed
        | Some(Syn) =>
          switch (syn_perform_operand(ctx, a, (zoperand, ty, u_gen))) {
          | Failed => Failed
          | CursorEscaped(side) =>
            ana_perform(ctx, escape(side), (ZE1(zopseq), u_gen), ty)
            |> wrap_in_AnaDone
          | Succeeded(SynExpandsToCase({scrut, u_gen, _})) =>
            let (new_line, u_gen) = new_line_of_prefix(ctx, u_gen, prefix);
            let (new_scrut, u_gen) = append_suffix(u_gen, scrut, suffix);
            Succeeded(
              AnaExpandsToCase({
                u_gen,
                prefix: [new_line],
                scrut: new_scrut,
                suffix: [],
              }),
            );
          | Succeeded(SynExpandsToLet({def, _})) =>
            let (new_line, u_gen) = new_line_of_prefix(ctx, u_gen, prefix);
            let (new_def, u_gen) = append_suffix(u_gen, def, suffix);
            Succeeded(
              AnaExpandsToLet({
                u_gen,
                prefix: [new_line],
                def: new_def,
                suffix: [],
              }),
            );
          | Succeeded(SynDone((ze, _, u_gen))) =>
            let (new_ze, u_gen) = resurround_z(u_gen, ze, surround);
            Succeeded(
              AnaDone(Statics.Exp.ana_fix_holes_z(ctx, u_gen, new_ze, ty)),
            );
          }
        | Some(Ana(ty_zoperand)) =>
          switch (
            ana_perform_operand(ctx, a, (zoperand, u_gen), ty_zoperand)
          ) {
          | Failed => Failed
          | CursorEscaped(side) =>
            ana_perform(ctx, escape(side), (ZE1(zopseq), u_gen), ty)
            |> wrap_in_AnaDone
          | Succeeded(AnaExpandsToCase({scrut, _})) =>
            let (new_line, u_gen) = new_line_of_prefix(ctx, u_gen, prefix);
            let (new_scrut, u_gen) = append_suffix(u_gen, scrut, suffix);
            Succeeded(
              AnaExpandsToCase({
                u_gen,
                prefix: [new_line],
                scrut: new_scrut,
                suffix: [],
              }),
            );
          | Succeeded(AnaExpandsToLet({def, _})) =>
            let (new_line, u_gen) = new_line_of_prefix(ctx, u_gen, prefix);
            let (new_def, u_gen) = append_suffix(u_gen, def, suffix);
            Succeeded(
              AnaExpandsToLet({
                u_gen,
                prefix: [new_line],
                def: new_def,
                suffix: [],
              }),
            );
          | Succeeded(AnaDone((ze, u_gen))) =>
            let (new_ze, u_gen) = resurround_z(u_gen, ze, surround);
            Succeeded(
              AnaDone(Statics.Exp.ana_fix_holes_z(ctx, u_gen, new_ze, ty)),
            );
          }
        }
      }
    }
  and ana_perform_operand =
      (
        ctx: Contexts.t,
        a: t,
        (zoperand, u_gen): (ZExp.zoperand, MetaVarGen.t),
        ty: HTyp.t,
      )
      : result(ana_success) =>
    switch (a, zoperand) {
    /* Invalid cursor positions */
    | (
        _,
        CursorE(
          OnDelim(_) | OnOp(_),
          Var(_) | NumLit(_) | BoolLit(_) | ApPalette(_),
        ) |
        CursorE(
          OnText(_) | OnOp(_),
          EmptyHole(_) | ListNil(_) | Lam(_) | Inj(_) | Case(_) |
          Parenthesized(_) |
          ApPalette(_),
        ),
      ) =>
      Failed
    | (_, CursorE(cursor, operand))
        when !ZExp.is_valid_cursor_operand(cursor, operand) =>
      Failed

    /* Invalid actions at expression level */
    | (Construct(SLine), CursorE(OnText(_), _)) => Failed

    | _ when ZExp.is_inconsistent(zoperand) =>
      let ze = ZExp.ZE0(zoperand);
      let err = ze |> ZExp.erase |> UHExp.get_err_status;
      let ze' = ZExp.(ZE0(zoperand) |> set_err_status(NotInHole));
      let e' = ze' |> ZExp.erase;
      switch (Statics.Exp.syn(ctx, e')) {
      | None => Failed
      | Some(ty') =>
        switch (syn_perform(ctx, a, (ze', ty', u_gen))) {
        | (Failed | CursorEscaped(_)) as result => result
        | Succeeded((ze', ty', u_gen)) =>
          if (HTyp.consistent(ty', ty)) {
            Succeeded(AnaDone((ze', u_gen)));
          } else {
            let new_ze = ze' |> ZExp.set_err_status(err);
            Succeeded(AnaDone((new_ze, u_gen)));
          }
        }
      };

    /* Movement handled at top level */
    | (
        MoveTo(_) | MoveToBefore(_) | MoveToPrevHole | MoveToNextHole | MoveLeft |
        MoveRight,
        _,
      ) =>
      Failed

    /* Backspace & Delete */

    | (Backspace, CursorE(_, EmptyHole(_) as operand)) =>
      let ze = UHExp.E0(operand) |> ZExp.place_before;
      ze |> ZExp.is_after
        ? Succeeded(AnaDone((ze, u_gen))) : CursorEscaped(Before);
    | (Delete, CursorE(_, EmptyHole(_) as operand)) =>
      let ze = UHExp.E0(operand) |> ZExp.place_after;
      ze |> ZExp.is_before
        ? Succeeded(AnaDone((ze, u_gen))) : CursorEscaped(After);

    /* ( _ <|)   ==>   ( _| ) */
    | (Backspace, CursorE(OnDelim(_, Before), _)) =>
      ana_perform(ctx, MoveLeft, (ZE0(zoperand), u_gen), ty)
      |> wrap_in_AnaDone
    /* (|> _ )   ==>   ( |_ ) */
    | (Delete, CursorE(OnDelim(_, After), _)) =>
      ana_perform(ctx, MoveRight, (ZE0(zoperand), u_gen), ty)
      |> wrap_in_AnaDone

    /* Delete before delimiter == Backspace after delimiter */
    | (Delete, CursorE(OnDelim(k, Before), operand)) =>
      let new_ze = ZExp.ZE0(CursorE(OnDelim(k, After), operand));
      ana_perform(ctx, Backspace, (new_ze, u_gen), ty) |> wrap_in_AnaDone;

    | (Backspace, CursorE(OnDelim(_, After), ListNil(_))) =>
      let (zhole, u_gen) = u_gen |> ZExp.new_EmptyHole;
      Succeeded(AnaDone((ZE0(zhole), u_gen)));

    | (Delete, CursorE(OnText(j), Var(_, _, x))) =>
      ana_delete_text(ctx, u_gen, j, x, ty)
    | (Delete, CursorE(OnText(j), NumLit(_, n))) =>
      ana_delete_text(ctx, u_gen, j, string_of_int(n), ty)
    | (Delete, CursorE(OnText(j), BoolLit(_, b))) =>
      ana_delete_text(ctx, u_gen, j, string_of_bool(b), ty)

    | (Backspace, CursorE(OnText(j), Var(_, _, x))) =>
      ana_backspace_text(ctx, u_gen, j, x, ty)
    | (Backspace, CursorE(OnText(j), NumLit(_, n))) =>
      ana_backspace_text(ctx, u_gen, j, string_of_int(n), ty)
    | (Backspace, CursorE(OnText(j), BoolLit(_, b))) =>
      ana_backspace_text(ctx, u_gen, j, string_of_bool(b), ty)

    /* \x :<| Num . x + 1   ==>   \x| . x + 1 */
    | (Backspace, CursorE(OnDelim(1, After), Lam(_, p, _, body))) =>
      switch (HTyp.matched_arrow(ty)) {
      | None => Failed
      | Some((ty1, ty2)) =>
        let (p, body_ctx, u_gen) =
          Statics.Pat.ana_fix_holes(ctx, u_gen, p, ty1);
        let (body, u_gen) =
          Statics.Exp.ana_fix_holes(body_ctx, u_gen, body, ty2);
        let new_ze =
          ZExp.ZE0(LamZP(NotInHole, ZPat.place_after(p), None, body));
        Succeeded(AnaDone((new_ze, u_gen)));
      }
    | (
        Backspace,
        CursorE(
          OnDelim(k, After),
          (
            Lam(_, _, _, e) | Inj(_, _, e) | Case(_, e, _, _) |
            Parenthesized(e)
          ) as operand,
        ),
      ) =>
      let place_cursor =
        switch (operand) {
        | Lam(_) =>
          switch (k) {
          | 0
          | 2 => ZExp.place_before
          | _three => ZExp.place_after
          }
        | _ =>
          switch (k) {
          | 0 => ZExp.place_before
          | _ => ZExp.place_after
          }
        };
      let new_ze = e |> place_cursor;
      Succeeded(
        AnaDone(Statics.Exp.ana_fix_holes_z(ctx, u_gen, new_ze, ty)),
      );

    /* TODO consider deletion of type ascription on case */

    /* Construction */

    | (Construct(SChar(s)), CursorE(_, EmptyHole(_))) =>
      ana_insert_text(ctx, u_gen, (0, s), "", ty)
    | (Construct(SChar(s)), CursorE(OnText(j), Var(_, _, x))) =>
      ana_insert_text(ctx, u_gen, (j, s), x, ty)
    | (Construct(SChar(s)), CursorE(OnText(j), NumLit(_, n))) =>
      ana_insert_text(ctx, u_gen, (j, s), string_of_int(n), ty)
    | (Construct(SChar(s)), CursorE(OnText(j), BoolLit(_, b))) =>
      ana_insert_text(ctx, u_gen, (j, s), string_of_bool(b), ty)
    | (Construct(SChar(_)), CursorE(_)) => Failed

    | (
        Construct(SOp(SSpace)),
        CursorE(_, Var(_, InVarHole(Keyword(k), _), _)),
      )
        when zoperand |> ZExp.is_after_zoperand =>
      let (zhole, u_gen) = u_gen |> ZExp.new_EmptyHole;
      ana_perform_operand(ctx, keyword_action(k), (zhole, u_gen), ty);
    | (Construct(SCase), CursorE(_, operand)) =>
      Succeeded(mk_AnaExpandsToCase(~u_gen, ~scrut=E0(operand), ()))
    | (Construct(SLet), CursorE(_, operand)) =>
      Succeeded(mk_AnaExpandsToLet(~u_gen, ~def=E0(operand), ()))

    | (Construct(SAsc), LamZP(err, zp, None, body)) =>
      let new_zann = UHTyp.T0(Hole) |> ZTyp.place_before;
      let new_ze = ZExp.ZE0(LamZA(err, zp |> ZPat.erase, new_zann, body));
      Succeeded(AnaDone((new_ze, u_gen)));
    | (Construct(SAsc), LamZP(err, zp, Some(ann), body)) =>
      /* just move the cursor over if there is already an ascription */
      let new_zann = ann |> ZTyp.place_before;
      let new_ze = ZExp.ZE0(LamZA(err, zp |> ZPat.erase, new_zann, body));
      Succeeded(AnaDone((new_ze, u_gen)));
    | (Construct(SAsc), CursorE(_, Case(_, e1, rules, None))) =>
      let new_ze =
        ZExp.ZE0(
          CaseZA(NotInHole, e1, rules, UHTyp.T0(Hole) |> ZTyp.place_before),
        );
      Succeeded(AnaDone((new_ze, u_gen)));
    | (Construct(SAsc), CursorE(_, Case(_, scrut, rules, Some(ann)))) =>
      /* just move the cursor over if there is already an ascription */
      let new_zann = ann |> ZTyp.place_before;
      let new_ze = ZExp.ZE0(CaseZA(NotInHole, scrut, rules, new_zann));
      Succeeded(AnaDone((new_ze, u_gen)));
    | (Construct(SAsc), CursorE(_)) => Failed

    | (Construct(SParenthesized), CursorE(_)) =>
      let new_ze = ZExp.ZE0(ParenthesizedZ(ZE0(zoperand)));
      Succeeded(AnaDone((new_ze, u_gen)));

    | (Construct(SInj(side)), CursorE(_)) =>
      switch (HTyp.matched_sum(ty)) {
      | Some((tyL, tyR)) =>
        let ty1 = InjSide.pick(side, tyL, tyR);
        let (zbody, u_gen) =
          Statics.Exp.ana_fix_holes_z(ctx, u_gen, ZE0(zoperand), ty1);
        let new_ze = ZExp.ZE0(InjZ(NotInHole, side, zbody));
        Succeeded(AnaDone((new_ze, u_gen)));
      | None =>
        let (zbody, _, u_gen) =
          Statics.Exp.syn_fix_holes_z(ctx, u_gen, ZE0(zoperand));
        let (u, u_gen) = u_gen |> MetaVarGen.next;
        let new_ze =
          ZExp.ZE0(InjZ(InHole(TypeInconsistent, u), side, zbody));
        Succeeded(AnaDone((new_ze, u_gen)));
      }

    | (Construct(SLam), CursorE(_)) =>
      let body = ZExp.(ZE0(zoperand) |> erase);
      switch (HTyp.matched_arrow(ty)) {
      | Some((_, ty2)) =>
        let (body, u_gen) = Statics.Exp.ana_fix_holes(ctx, u_gen, body, ty2);
        let (zhole, u_gen) = u_gen |> ZPat.new_EmptyHole;
        let new_ze = ZExp.ZE0(LamZP(NotInHole, ZP0(zhole), None, body));
        Succeeded(AnaDone((new_ze, u_gen)));
      | None =>
        let (body, _, u_gen) = Statics.Exp.syn_fix_holes(ctx, u_gen, body);
        let (zhole, u_gen) = u_gen |> ZPat.new_EmptyHole;
        let (u, u_gen) = u_gen |> MetaVarGen.next;
        let new_ze =
          ZExp.ZE0(
            LamZP(InHole(TypeInconsistent, u), ZP0(zhole), None, body),
          );
        Succeeded(AnaDone((new_ze, u_gen)));
      };

    | (Construct(SOp(SSpace)), CursorE(OnDelim(_, After), _))
        when !ZExp.is_after_zoperand(zoperand) =>
      ana_perform(ctx, MoveRight, (ZE0(zoperand), u_gen), ty)
      |> wrap_in_AnaDone

    | (Construct(SOp(os)), CursorE(_)) =>
      switch (operator_of_shape(os)) {
      | None => Failed
      | Some(operator) =>
        let construct_operator =
          ZExp.is_before_zoperand(zoperand)
            ? construct_operator_before_zoperand
            : construct_operator_after_zoperand;
        let (zseq, u_gen) =
          construct_operator(u_gen, operator, zoperand, (E, E));
        Succeeded(mk_and_ana_fix_ZOpSeq(ctx, u_gen, zseq, ty))
        |> wrap_in_AnaDone;
      }

    // TODO: consider how this interacts with subsumption case below
    | (Construct(_), CursorE(OnDelim(_, side), _))
        when
          !ZExp.is_before_zoperand(zoperand)
          && !ZExp.is_after_zoperand(zoperand) =>
      switch (ana_perform(ctx, escape(side), (ZE0(zoperand), u_gen), ty)) {
      | Failed
      | CursorEscaped(_) => Failed
      | Succeeded(new_edit_state) =>
        ana_perform(ctx, a, new_edit_state, ty) |> wrap_in_AnaDone
      }
    | (Construct(_), CursorE(OnDelim(_), _)) => Failed

    /* Zipper Cases */
    | (_, ParenthesizedZ(zbody)) =>
      switch (ana_perform(ctx, a, (zbody, u_gen), ty)) {
      | Failed => Failed
      | CursorEscaped(side) =>
        ana_perform(ctx, escape(side), (ZE0(zoperand), u_gen), ty)
        |> wrap_in_AnaDone
      | Succeeded((zbody, u_gen)) =>
        let new_ze = ZExp.ZE0(ParenthesizedZ(zbody));
        Succeeded(AnaDone((new_ze, u_gen)));
      }
    | (_, LamZP(_, zp, ann, body)) =>
      switch (HTyp.matched_arrow(ty)) {
      | None => Failed
      | Some((ty1_given, ty2)) =>
        let ty1 =
          switch (ann) {
          | Some(uty1) => UHTyp.expand(uty1)
          | None => ty1_given
          };
        switch (Pat.ana_perform(ctx, u_gen, a, zp, ty1)) {
        | Failed => Failed
        | CursorEscaped(side) =>
          ana_perform(ctx, escape(side), (ZE0(zoperand), u_gen), ty)
          |> wrap_in_AnaDone
        | Succeeded((zp, ctx, u_gen)) =>
          let (body, u_gen) =
            Statics.Exp.ana_fix_holes(ctx, u_gen, body, ty2);
          let new_ze = ZExp.ZE0(LamZP(NotInHole, zp, ann, body));
          Succeeded(AnaDone((new_ze, u_gen)));
        };
      }
    | (_, LamZA(_, p, zann, body)) =>
      switch (HTyp.matched_arrow(ty)) {
      | None => Failed
      | Some((ty1_given, ty2)) =>
        switch (Typ.perform(a, zann)) {
        | Failed => Failed
        | CursorEscaped(side) =>
          ana_perform(ctx, escape(side), (ZE0(zoperand), u_gen), ty)
          |> wrap_in_AnaDone
        | Succeeded(zann) =>
          let ty1 = UHTyp.expand(ZTyp.erase(zann));
          HTyp.consistent(ty1, ty1_given)
            ? {
              let (p, ctx, u_gen) =
                Statics.Pat.ana_fix_holes(ctx, u_gen, p, ty1);
              let (body, u_gen) =
                Statics.Exp.ana_fix_holes(ctx, u_gen, body, ty2);
              let new_ze = ZExp.ZE0(LamZA(NotInHole, p, zann, body));
              Succeeded(AnaDone((new_ze, u_gen)));
            }
            : {
              let (p, ctx, u_gen) =
                Statics.Pat.ana_fix_holes(ctx, u_gen, p, ty1);
              let (body, _, u_gen) =
                Statics.Exp.syn_fix_holes(ctx, u_gen, body);
              let (u, u_gen) = u_gen |> MetaVarGen.next;
              let new_ze =
                ZExp.ZE0(LamZA(InHole(TypeInconsistent, u), p, zann, body));
              Succeeded(AnaDone((new_ze, u_gen)));
            };
        }
      }
    | (_, LamZE(_, p, ann, zbody)) =>
      switch (HTyp.matched_arrow(ty)) {
      | None => Failed
      | Some((ty1_given, ty2)) =>
        let ty1 =
          switch (ann) {
          | Some(uty1) => UHTyp.expand(uty1)
          | None => ty1_given
          };
        switch (Statics.Pat.ana(ctx, p, ty1)) {
        | None => Failed
        | Some(ctx_body) =>
          switch (ana_perform(ctx_body, a, (zbody, u_gen), ty2)) {
          | Failed => Failed
          | CursorEscaped(side) =>
            ana_perform(ctx, escape(side), (ZE0(zoperand), u_gen), ty)
            |> wrap_in_AnaDone
          | Succeeded((zbody, u_gen)) =>
            let new_ze = ZExp.ZE0(LamZE(NotInHole, p, ann, zbody));
            Succeeded(AnaDone((new_ze, u_gen)));
          }
        };
      }
    | (_, InjZ(_, side, zbody)) =>
      switch (HTyp.matched_sum(ty)) {
      | None => Failed
      | Some((ty1, ty2)) =>
        let picked = InjSide.pick(side, ty1, ty2);
        switch (ana_perform(ctx, a, (zbody, u_gen), picked)) {
        | Failed => Failed
        | CursorEscaped(side) =>
          ana_perform(ctx, escape(side), (ZE0(zoperand), u_gen), ty)
          |> wrap_in_AnaDone
        | Succeeded((zbody, u_gen)) =>
          let new_ze = ZExp.ZE0(InjZ(NotInHole, side, zbody));
          Succeeded(AnaDone((new_ze, u_gen)));
        };
      }
    | (_, CaseZE(_, zscrut, rules, ann)) =>
      // TODO: need to check consistency of ann with expected ty
      switch (Statics.Exp.syn(ctx, zscrut |> ZExp.erase)) {
      | None => Failed
      | Some(ty1) =>
        switch (syn_perform(ctx, a, (zscrut, ty1, u_gen))) {
        | Failed => Failed
        | CursorEscaped(side) =>
          ana_perform(ctx, escape(side), (ZE0(zoperand), u_gen), ty)
          |> wrap_in_AnaDone
        | Succeeded((zscrut, ty1, u_gen)) =>
          let (rules, u_gen) =
            Statics.Exp.ana_fix_holes_rules(ctx, u_gen, rules, ty1, ty);
          let new_ze = ZExp.ZE0(CaseZE(NotInHole, zscrut, rules, ann));
          Succeeded(AnaDone((new_ze, u_gen)));
        }
      }
    | (_, CaseZR(_, scrut, zrules, ann)) =>
      // TODO: need to check consistency of ann with expected ty
      switch (Statics.Exp.syn(ctx, scrut)) {
      | None => Failed
      | Some(pat_ty) =>
        switch (ana_perform_rules(ctx, a, (zrules, u_gen), pat_ty, ty)) {
        | Failed => Failed
        | CursorEscaped(side) =>
          ana_perform(ctx, escape(side), (ZE0(zoperand), u_gen), ty)
          |> wrap_in_AnaDone
        | Succeeded((new_zrules, u_gen)) =>
          let new_ze = ZExp.ZE0(CaseZR(NotInHole, scrut, new_zrules, ann));
          Succeeded(AnaDone((new_ze, u_gen)));
        }
      }
    | (_, CaseZA(_, scrut, rules, zann)) =>
      // TODO: need to check consistency of ann with expected ty
      switch (Statics.Exp.syn(ctx, scrut)) {
      | None => Failed
      | Some(ty1) =>
        switch (Typ.perform(a, zann)) {
        | Failed => Failed
        | CursorEscaped(side) =>
          ana_perform(ctx, escape(side), (ZE0(zoperand), u_gen), ty)
          |> wrap_in_AnaDone
        | Succeeded(zann) =>
          let ty2 = UHTyp.expand(ZTyp.erase(zann));
          let (rules, u_gen) =
            Statics.Exp.ana_fix_holes_rules(ctx, u_gen, rules, ty1, ty2);
          let new_ze = ZExp.ZE0(CaseZA(NotInHole, scrut, rules, zann));
          Succeeded(
            AnaDone(Statics.Exp.ana_fix_holes_z(ctx, u_gen, new_ze, ty)),
          );
        }
      }

    /* Subsumption */
    | (UpdateApPalette(_) | Construct(SApPalette(_) | SLine | SListNil), _)
    | (_, ApPaletteZ(_)) =>
      ana_perform_subsume(ctx, a, (zoperand, u_gen), ty)
    }
  and ana_perform_subsume =
      (
        ctx: Contexts.t,
        a: t,
        (zoperand: ZExp.zoperand, u_gen: MetaVarGen.t),
        ty: HTyp.t,
      )
      : result(ana_success) =>
    switch (Statics.Exp.syn(ctx, ZExp.(ZE0(zoperand) |> erase))) {
    | None => Failed
    | Some(ty1) =>
      // must call syn_perform_operand and not syn_perform
      // to pass along any keyword expansions
      switch (syn_perform_operand(ctx, a, (zoperand, ty1, u_gen))) {
      | Failed
      | CursorEscaped(_)
      | Succeeded(SynExpandsToCase(_) | SynExpandsToLet(_)) => Failed
      | Succeeded(SynDone((ze, ty1, u_gen))) =>
        if (HTyp.consistent(ty, ty1)) {
          Succeeded(AnaDone((ze, u_gen)));
        } else {
          let (ze, u_gen) = ze |> ZExp.make_inconsistent(u_gen);
          Succeeded(AnaDone((ze, u_gen)));
        }
      }
    };
};
