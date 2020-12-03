open Sexplib.Std;

[@deriving sexp]
type operator_shape =
  | SMinus
  | SPlus
  | STimes
  | SDivide
  | SLessThan
  | SGreaterThan
  | SEquals
  | SSpace
  | SComma
  | SArrow
  | SVBar
  | SCons
  | SAnd
  | SOr
  | SDot;

[@deriving sexp]
type shape =
  | SCommentLine
  | SList
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
  | MoveTo(CursorPath_common.t)
  | MoveLeft
  | MoveRight
  | MoveToNextHole
  | MoveToPrevHole
  | UpdateApPalette(SpliceGenMonad.t(SerializedModel.t))
  | Delete
  | Backspace
  | Construct(shape)
  | SwapLeft
  | SwapRight
  | SwapUp
  | SwapDown
  | Init;

let shape_to_string = (shape: shape): string => {
  switch (shape) {
  | SList => "list type"
  | SParenthesized => "parentheses"
  | SChar(str) => str
  | SAsc => "type annotation"
  | SLam => "function"
  | SListNil => "empty list"
  | SInj(side) =>
    switch (side) {
    | L => "left injection"
    | R => "right injection"
    }
  | SLet => "let binding"
  | SLine => "new line"
  | SCommentLine => "comment line"
  | SCase => "case expression"
  | SOp(operator_shape) =>
    switch (operator_shape) {
    | SMinus => "-"
    | SPlus => "+"
    | STimes => "*"
    | SDivide => "/"
    | SLessThan => "<"
    | SGreaterThan => ">"
    | SEquals => "=="
    | SSpace => "space"
    | SComma => ","
    | SArrow => UnicodeConstants.typeArrowSym
    | SVBar => "|"
    | SCons => "::"
    | SAnd => "&&"
    | SOr => "||"
    | SDot => "."
    }
  | SApPalette(_) => failwith("ApPalette not implemented")
  };
};

let escape: Side.t => t =
  fun
  | Before => MoveLeft
  | After => MoveRight;

let syn_insert_text_ =
    (
      ~mk_syn_text:
         (Contexts.t, MetaVarGen.t, int, string) => ActionOutcome.t('success),
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      (caret_index: int, insert_text: string),
      text: string,
    )
    : ActionOutcome.t('success) =>
  mk_syn_text(
    ctx,
    u_gen,
    caret_index + String.length(insert_text),
    text |> StringUtil.insert(caret_index, insert_text),
  );
let ana_insert_text_ =
    (
      ~mk_ana_text:
         (Contexts.t, MetaVarGen.t, int, string, HTyp.t) =>
         ActionOutcome.t('success),
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      (caret_index: int, insert_text: string),
      text: string,
      ty: HTyp.t,
    )
    : ActionOutcome.t('success) =>
  mk_ana_text(
    ctx,
    u_gen,
    caret_index + String.length(insert_text),
    text |> StringUtil.insert(caret_index, insert_text),
    ty,
  );

let syn_backspace_text_ =
    (
      ~mk_syn_text:
         (Contexts.t, MetaVarGen.t, int, string) => ActionOutcome.t('success),
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      caret_index: int,
      text: string,
    )
    : ActionOutcome.t('success) =>
  if (caret_index == 0) {
    CursorEscaped(Before);
  } else {
    let new_text = text |> StringUtil.backspace(caret_index);
    mk_syn_text(ctx, u_gen, caret_index - 1, new_text);
  };
let ana_backspace_text_ =
    (
      ~mk_ana_text:
         (Contexts.t, MetaVarGen.t, int, string, HTyp.t) =>
         ActionOutcome.t('success),
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      caret_index: int,
      text: string,
      ty: HTyp.t,
    )
    : ActionOutcome.t('success) =>
  if (caret_index == 0) {
    CursorEscaped(Before);
  } else {
    let new_text = text |> StringUtil.backspace(caret_index);
    mk_ana_text(ctx, u_gen, caret_index - 1, new_text, ty);
  };

let syn_delete_text_ =
    (
      ~mk_syn_text:
         (Contexts.t, MetaVarGen.t, int, string) => ActionOutcome.t('success),
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      caret_index: int,
      text: string,
    )
    : ActionOutcome.t('success) =>
  if (caret_index == String.length(text)) {
    CursorEscaped(After);
  } else {
    let new_text = text |> StringUtil.delete(caret_index);
    mk_syn_text(ctx, u_gen, caret_index, new_text);
  };
let ana_delete_text_ =
    (
      ~mk_ana_text:
         (Contexts.t, MetaVarGen.t, int, string, HTyp.t) =>
         ActionOutcome.t('success),
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      caret_index: int,
      text: string,
      ty: HTyp.t,
    )
    : ActionOutcome.t('success) =>
  if (caret_index == String.length(text)) {
    CursorEscaped(After);
  } else {
    let new_text = text |> StringUtil.delete(caret_index);
    mk_ana_text(ctx, u_gen, caret_index, new_text, ty);
  };

let construct_operator_after_zoperand_ =
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
let construct_operator_before_zoperand_ =
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
    construct_operator_after_zoperand_(
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

let delete_operator_ =
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

/**
 * Produce tuple completion upon entering comma after
 * opseq in analytic position against type ty.
 * Assumes no commas in input opseq and that ty is
 * an n-product where n >= 2.
 */
let complete_tuple_ =
    (
      ~mk_ZOpSeq:
         ZSeq.t('operand, 'operator, 'zoperand, 'zoperator) =>
         ZOpSeq.t('operand, 'operator, 'zoperand, 'zoperator),
      ~comma: 'operator,
      ~zcomma: 'zoperator,
      ~new_EmptyHole: MetaVarGen.t => ('operand, MetaVarGen.t),
      u_gen: MetaVarGen.t,
      OpSeq(_, seq): OpSeq.t('operand, 'operator),
      ty: HTyp.t,
    )
    : (ZOpSeq.t('operand, 'operator, 'zoperand, 'zoperator), MetaVarGen.t) => {
  let (new_suffix: Seq.t(_), u_gen) = {
    // guaranteed to construct at least one empty hole
    let (new_hole, u_gen) = u_gen |> new_EmptyHole;
    let (new_holes, u_gen) =
      ty
      |> HTyp.get_prod_elements
      // assuming ty has at least 2 elems
      |> List.tl
      |> List.tl
      // ensure that hole indices increase left to right
      |> List.fold_left(
           ((rev_holes, u_gen), _) => {
             let (new_hole, u_gen) = u_gen |> new_EmptyHole;
             ([new_hole, ...rev_holes], u_gen);
           },
           ([], u_gen),
         )
      |> (
        fun
        | (rev_holes, u_gen) => (rev_holes |> List.rev, u_gen)
      );
    (
      Seq.S(
        new_hole,
        List.fold_right(
          (new_hole, suffix: Seq.affix(_)) =>
            A(comma, S(new_hole, suffix)),
          new_holes,
          Seq.E,
        ),
      ),
      u_gen,
    );
  };
  let new_zopseq =
    mk_ZOpSeq(ZOperator(zcomma, (seq |> Seq.rev, new_suffix)));
  (new_zopseq, u_gen);
};
