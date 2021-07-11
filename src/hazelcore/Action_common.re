open Action;

let shape_to_string = (shape: shape): string => {
  switch (shape) {
  | SList => "list type"
  | SParenthesized => "parentheses"
  | SChar(str) => str
  | SAnn => "type annotation"
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
    | SArrow => Unicode.typeArrowSym
    | SVBar => "|"
    | SCons => "::"
    | SAnd => "&&"
    | SOr => "||"
    }
  | SApPalette(_) => failwith("ApPalette not implemented")
  };
};

type merge_class('a) =
  | Empty
  | Merge(string)
  | AbsorbLeft((string, MetaVarGen.t) => ('a, MetaVarGen.t))
  | Inert;

type delete_action =
  | Backspace
  | Delete;

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

let delete_action = (a: Action.t): delete_action =>
  switch (a) {
  | Backspace => Backspace
  | Delete => Delete
  | _ =>
    let action_str = Sexplib.Sexp.to_string_hum(Action.sexp_of_t(a));
    failwith(
      "Action_common.delete_action doesn't support action: " ++ action_str,
    );
  };

let spacebuster =
    (
      ~space: 'operator,
      ~merge_class: 'operand => merge_class('zoperand),
      ~before_: 'operand => 'zoperand,
      ~after_: 'operand => 'zoperand,
      ~at_index: ('operand, int) => 'zoperand,
      ~mk_operand_of_string:
         (MetaVarGen.t, string) => ('operand, MetaVarGen.t),
      opA: 'operand,
      opB: 'operand,
      prefix: Seq.affix('operand, 'operator),
      suffix: Seq.affix('operand, 'operator),
      a: Action.t,
      u_gen: MetaVarGen.t,
    )
    : (ZSeq.t('operand, 'operator, 'zoperand, _), MetaVarGen.t) => {
  /* This handles the logic for BACKSPACE and DELETE actions when
      the caret is _conceptually_ adjacent to a space operator.

      Precondition: in the surrounding context, the caret is
     'on the space' i.e. either after operandA or before operandB

      The cases below are intended to be total in this space. However,
      some cases should be handled either above or below the opseq level.
      If these cases get captured by this function, they may result in
      bad caret placement. They are denoted below as follows:

          *  : case should be handled at block level
          ** : case should be handled at operand level

      In addition, the following notation will be used:

          H    : empty hole
          A, B : non-empty-hole operands
          |    : caret
     */
  let a = delete_action(a);
  let mono = zop => (ZSeq.ZOperand(zop, (prefix, suffix)), u_gen);
  let bin_first = (zopA, opB) => (
    ZSeq.ZOperand(zopA, (prefix, A(space, S(opB, suffix)))),
    u_gen,
  );
  let bin_second = (opA, zopB) => (
    ZSeq.ZOperand(zopB, (A(space, S(opA, prefix)), suffix)),
    u_gen,
  );
  switch (merge_class(opA), a, merge_class(opB)) {
  | (Empty, Backspace, Empty) =>
    //  H| H   B=>   |H
    //  H |H   B=>   |H
    mono(before_(opB))
  | (Empty, Delete, Empty) =>
    //  H| H   D=>    H|
    //  H |H   D=>    H|
    mono(after_(opA))
  | (Empty, _, _) =>
    // |H  B   B=>   *    D=>   |B
    //  H| B   B=>   |B   D=>   |B
    //  H |B   B=>   |B   D=>   **
    mono(before_(opB))
  | (_, _, Empty) =>
    //  A| H   B=>   **   D=>   A|
    //  A |H   B=>   A|   D=>   A|
    //  A  H|  B=>   A|   D=>    *
    mono(after_(opA))
  | (Merge(sa), _, AbsorbLeft(absorb_op)) =>
    // This case exists to support type annotations in patterns
    let (zop, u_gen) = absorb_op(sa, u_gen);
    (ZSeq.ZOperand(zop, (prefix, suffix)), u_gen);
  | (Merge(sa), _, Merge(sb)) =>
    //  A| B   B=>  **    D=>   A|B
    //  A |B   B=>  A|B   D=>   **
    let (op, u_gen) = mk_operand_of_string(u_gen, sa ++ sb);
    let zop = at_index(op, String.length(sa));
    (ZSeq.ZOperand(zop, (prefix, suffix)), u_gen);
  // If we can't delete anything, we try to move in the relevant direction
  | (_, Backspace, _) =>
    //  A| B   B=>    **
    //  A |B   B=>    A| B
    bin_first(after_(opA), opB)
  | (_, Delete, _) =>
    //  A| B   D=>    A |B
    //  A |B   D=>    **
    bin_second(opA, before_(opB))
  };
};
