open Action;

let shape_to_string = (shape: shape): string => {
  switch (shape) {
  | SList => "list type"
  | SParenthesized => "parentheses"
  | SCloseParens => "closing parens"
  | SCloseBraces => "closing brace"
  | SCloseSquareBracket => "closing square bracket"
  | SChar(str) => str
  | SAnn => "type annotation"
  | SFun => "function"
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
  };
};

let escape: Side.t => t =
  fun
  | Before => MoveLeft
  | After => MoveRight;

let syn_insert_text_ =
    (
      ~mk_syn_text:
         (Contexts.t, IDGen.t, int, string) => ActionOutcome.t('success),
      ctx: Contexts.t,
      id_gen: IDGen.t,
      (caret_index: int, insert_text: string),
      text: string,
    )
    : ActionOutcome.t('success) =>
  mk_syn_text(
    ctx,
    id_gen,
    caret_index + String.length(insert_text),
    text |> StringUtil.insert(caret_index, insert_text),
  );
let ana_insert_text_ =
    (
      ~mk_ana_text:
         (Contexts.t, IDGen.t, int, string, HTyp.t) =>
         ActionOutcome.t('success),
      ctx: Contexts.t,
      id_gen: IDGen.t,
      (caret_index: int, insert_text: string),
      text: string,
      ty: HTyp.t,
    )
    : ActionOutcome.t('success) =>
  mk_ana_text(
    ctx,
    id_gen,
    caret_index + String.length(insert_text),
    text |> StringUtil.insert(caret_index, insert_text),
    ty,
  );

let syn_backspace_text_ =
    (
      ~mk_syn_text:
         (Contexts.t, IDGen.t, int, string) => ActionOutcome.t('success),
      ctx: Contexts.t,
      id_gen: IDGen.t,
      caret_index: int,
      text: string,
    )
    : ActionOutcome.t('success) =>
  if (caret_index == 0) {
    CursorEscaped(Before);
  } else {
    let new_text = text |> StringUtil.backspace(caret_index);
    mk_syn_text(ctx, id_gen, caret_index - 1, new_text);
  };
let ana_backspace_text_ =
    (
      ~mk_ana_text:
         (Contexts.t, IDGen.t, int, string, HTyp.t) =>
         ActionOutcome.t('success),
      ctx: Contexts.t,
      id_gen: IDGen.t,
      caret_index: int,
      text: string,
      ty: HTyp.t,
    )
    : ActionOutcome.t('success) =>
  if (caret_index == 0) {
    CursorEscaped(Before);
  } else {
    let new_text = text |> StringUtil.backspace(caret_index);
    mk_ana_text(ctx, id_gen, caret_index - 1, new_text, ty);
  };

let syn_delete_text_ =
    (
      ~mk_syn_text:
         (Contexts.t, IDGen.t, int, string) => ActionOutcome.t('success),
      ctx: Contexts.t,
      id_gen: IDGen.t,
      caret_index: int,
      text: string,
    )
    : ActionOutcome.t('success) =>
  if (caret_index == String.length(text)) {
    CursorEscaped(After);
  } else {
    let new_text = text |> StringUtil.delete(caret_index);
    mk_syn_text(ctx, id_gen, caret_index, new_text);
  };
let ana_delete_text_ =
    (
      ~mk_ana_text:
         (Contexts.t, IDGen.t, int, string, HTyp.t) =>
         ActionOutcome.t('success),
      ctx: Contexts.t,
      id_gen: IDGen.t,
      caret_index: int,
      text: string,
      ty: HTyp.t,
    )
    : ActionOutcome.t('success) =>
  if (caret_index == String.length(text)) {
    CursorEscaped(After);
  } else {
    let new_text = text |> StringUtil.delete(caret_index);
    mk_ana_text(ctx, id_gen, caret_index, new_text, ty);
  };

let construct_operator_after_zoperand_ =
    (
      ~is_Space: 'operator => bool,
      ~new_EmptyHole: IDGen.t => ('operand, IDGen.t),
      ~erase_zoperand: 'zoperand => 'operand,
      ~place_before_operand: 'operand => 'zoperand,
      ~place_after_operator: 'operator => option('zoperator),
      id_gen: IDGen.t,
      operator: 'operator,
      zoperand: 'zoperand,
      (prefix, suffix): Seq.operand_surround('operand, 'operator),
    )
    : (ZSeq.t('operand, 'operator, 'zoperand, 'zoperator), IDGen.t) => {
  let operand = zoperand |> erase_zoperand;
  switch (operator |> place_after_operator) {
  | None =>
    // operator == Space
    // ... + [k]| + [k+1] + ...   ==>   ... + [k]  |_ + [k+1] + ...
    let (hook, id_gen) = id_gen |> new_EmptyHole;
    let new_prefix = Seq.A(operator, S(operand, prefix));
    let new_zoperand = hook |> place_before_operand;
    (ZOperand(new_zoperand, (new_prefix, suffix)), id_gen);
  | Some(zoperator) =>
    let new_prefix = Seq.S(operand, prefix);
    let (new_suffix, id_gen) =
      switch (suffix) {
      | A(op, new_suffix) when op |> is_Space =>
        // zoperator overwrites Space
        // ... + [k]|  [k+1] + ...   ==>   ... + [k] *| [k+1] + ...
        (new_suffix, id_gen)
      | _ =>
        // ... + [k]| + [k+1] + ...   ==>   ... + [k] *| _ + [k+1] + ...
        let (hook, id_gen) = id_gen |> new_EmptyHole;
        (Seq.S(hook, suffix), id_gen);
      };
    (ZOperator(zoperator, (new_prefix, new_suffix)), id_gen);
  };
};
let construct_operator_before_zoperand_ =
    (
      ~is_Space: 'operator => bool,
      ~new_EmptyHole: IDGen.t => ('operand, IDGen.t),
      ~erase_zoperand: 'zoperand => 'operand,
      ~place_before_operand: 'operand => 'zoperand,
      ~place_after_operator: 'operator => option('zoperator),
      id_gen: IDGen.t,
      operator: 'operator,
      zoperand: 'zoperand,
      (prefix, suffix): Seq.operand_surround('operand, 'operator),
    )
    : (ZSeq.t('operand, 'operator, 'zoperand, 'zoperator), IDGen.t) => {
  // symmetric to construct_operator_after_zoperand
  let mirror_surround = (suffix, prefix);
  let (mirror_zseq, id_gen) =
    construct_operator_after_zoperand_(
      ~is_Space,
      ~new_EmptyHole,
      ~erase_zoperand,
      ~place_before_operand,
      ~place_after_operator,
      id_gen,
      operator,
      zoperand,
      mirror_surround,
    );
  let zseq: ZSeq.t(_) =
    switch (mirror_zseq) {
    | ZOperator(z, (suffix, prefix)) => ZOperator(z, (prefix, suffix))
    | ZOperand(z, (suffix, prefix)) => ZOperand(z, (prefix, suffix))
    };
  (zseq, id_gen);
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
      ~mk_OpSeq: Seq.t('operand, 'operator) => OpSeq.t('operand, 'operator),
      ~hooks_opseq:
         (
           CursorPath.rev_steps,
           OpSeq.t('operand, 'operator),
           CursorPath.hook_list
         ) =>
         CursorPath.hook_list,
      ~follow_opseq:
         (CursorPath.t, OpSeq.t('operand, 'operator)) =>
         option(ZOpSeq.t('operand, 'operator, 'zoperand, 'zoperator)),
      ~mk_ZOpSeq:
         ZSeq.t('operand, 'operator, 'zoperand, 'zoperator) =>
         ZOpSeq.t('operand, 'operator, 'zoperand, 'zoperator),
      ~place_before_opseq:
         OpSeq.t('operand, 'operator) =>
         ZOpSeq.t('operand, 'operator, 'zoperand, 'zoperator),
      ~place_before_operand: 'operand => 'zoperand,
      ~comma: 'operator,
      ~new_EmptyHole: IDGen.t => ('operand, IDGen.t),
      id_gen: IDGen.t,
      first_seq: Seq.t('operand, 'operator),
      ty: HTyp.t,
      ~triggered_by_paren: bool,
      ~is_after_zopseq: bool,
    ) // is_after_zopseq not needed when parenthesizing a tuple
    : (ZOpSeq.t('operand, 'operator, 'zoperand, 'zoperator), IDGen.t) => {
  // all top-level operators should be commas,
  // count the number of commas then add 1 to get the number of elements
  // in first_seq before or after the cursor
  let first_seq_length = List.length(Seq.operators(first_seq)) + 1;
  let (new_zopseq, id_gen) = {
    let (new_hooks, id_gen) =
      ty
      |> HTyp.get_prod_elements
      |> ListUtil.drop(first_seq_length)
      // ensure that hook indices increase left to right
      |> List.fold_left(
           ((rev_hooks, id_gen), _) => {
             let (new_hook, id_gen) = id_gen |> new_EmptyHole;
             ([new_hook, ...rev_hooks], id_gen);
           },
           ([], id_gen),
         )
      |> (
        fun
        | (rev_hooks, id_gen) => (rev_hooks |> List.rev, id_gen)
      );

    let new_zopseq =
      if (triggered_by_paren) {
        let new_suffix =
          List.fold_right(
            (new_hook, suffix: Seq.affix(_)) =>
              A(comma, S(new_hook, suffix)),
            new_hooks,
            Seq.E,
          );
        let opseq = mk_OpSeq(Seq.seq_suffix(first_seq, new_suffix));
        let hook_list = hooks_opseq([], opseq, []);
        // no hook detected, then place cursor at the beginning of opseq
        switch (hook_list) {
        | [] => place_before_opseq(opseq)
        | [first_hook_info, ..._tl] =>
          // has at least one hook, then place cursor at that hook
          let first_hook_steps = first_hook_info.steps;
          // place before the first hook in the completed tuple
          let cursor_position =
            switch (first_hook_info.sort) {
            | CursorPath.PatHole(_, CursorPath.Empty)
            | CursorPath.ExpHole(_, CursorPath.Empty) =>
              CursorPosition.OnDelim(0, Side.Before)
            | _ => CursorPosition.OnText(0)
            };
          Option.value(
            follow_opseq((first_hook_steps, cursor_position), opseq),
            ~default=place_before_opseq(opseq),
          );
        };
      } else {
        let first_new_hook = List.hd(new_hooks);
        let new_suffix =
          List.fold_right(
            (new_hook, suffix: Seq.affix(_)) =>
              A(comma, S(new_hook, suffix)),
            List.tl(new_hooks),
            Seq.E,
          );
        if (is_after_zopseq) {
          mk_ZOpSeq(
            ZOperand(
              place_before_operand(first_new_hook),
              (A(comma, Seq.rev(first_seq)), new_suffix),
            ),
          );
        } else {
          mk_ZOpSeq(
            ZOperand(
              place_before_operand(first_new_hook),
              (new_suffix, A(comma, first_seq)),
            ),
          );
        };
      };
    (new_zopseq, id_gen);
  };
  (new_zopseq, id_gen);
};
