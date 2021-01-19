[@deriving sexp]
type t('operand, 'operator, 'zoperand, 'zoperator) =
  | ZOperand('zoperand, Seq.operand_surround('operand, 'operator))
  | ZOperator('zoperator, Seq.operator_surround('operand, 'operator));

let wrap = zoperand => ZOperand(zoperand, (E, E));

let erase =
    (
      ~erase_zoperand: 'zoperand => 'operand,
      ~erase_zbinop: 'zoperator => 'operator,
      zseq: t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : Seq.t('operand, 'operator) =>
  switch (zseq) {
  | ZOperand(zoperand, (prefix, suffix)) =>
    let operand = zoperand |> erase_zoperand;
    Seq.prefix_seq(prefix, S(operand, suffix));
  | ZOperator(zoperator, (S(prefix_hd, prefix_tl), suffix)) =>
    let operator = zoperator |> erase_zbinop;
    Seq.prefix_seq(prefix_tl, S(prefix_hd, A(operator, suffix)));
  };

let is_before =
    (~is_before_zoperand: 'zoperand => bool, zseq: t(_, _, 'zoperand, _))
    : bool =>
  switch (zseq) {
  | ZOperator(_)
  | ZOperand(_, (A(_), _)) => false
  | ZOperand(zoperand, (E, _)) => is_before_zoperand(zoperand)
  };

let is_after =
    (~is_after_zoperand: 'zoperand => bool, zseq: t(_, _, 'zoperand, _))
    : bool =>
  switch (zseq) {
  | ZOperator(_)
  | ZOperand(_, (_, A(_))) => false
  | ZOperand(zoperand, (_, E)) => is_after_zoperand(zoperand)
  };

let move_cursor_left =
    (
      ~move_cursor_left_zoperand: 'zoperand => option('zoperand),
      ~move_cursor_left_zbinop: 'zoperator => option('zoperator),
      ~place_after_operand: 'operand => 'zoperand,
      ~place_after_binop: 'operator => option('zoperator),
      ~erase_zoperand: 'zoperand => 'operand,
      ~erase_zbinop: 'zoperator => 'operator,
      zseq: t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : option(t('operand, 'operator, 'zoperand, 'zoperator)) =>
  switch (zseq) {
  | ZOperand(zoperand, surround) =>
    switch (move_cursor_left_zoperand(zoperand), surround) {
    | (None, (E, _)) => None
    | (None, (A(prefix_hd, prefix_tl), suffix)) =>
      switch (prefix_hd |> place_after_binop) {
      | None =>
        let S(operand, new_prefix) = prefix_tl;
        let new_zoperand = operand |> place_after_operand;
        let new_suffix =
          Seq.A(prefix_hd, S(zoperand |> erase_zoperand, suffix));
        Some(ZOperand(new_zoperand, (new_prefix, new_suffix)));
      | Some(zoperator) =>
        let new_prefix = prefix_tl;
        let new_suffix = Seq.S(zoperand |> erase_zoperand, suffix);
        Some(ZOperator(zoperator, (new_prefix, new_suffix)));
      }
    | (Some(zoperand), _) => Some(ZOperand(zoperand, surround))
    }
  | ZOperator(zop, surround) =>
    switch (move_cursor_left_zbinop(zop), surround) {
    | (None, (S(prefix_hd, new_prefix), suffix)) =>
      let zoperand = prefix_hd |> place_after_operand;
      let new_suffix = Seq.A(zop |> erase_zbinop, suffix);
      Some(ZOperand(zoperand, (new_prefix, new_suffix)));
    | (Some(zop), _) => Some(ZOperator(zop, surround))
    }
  };

let move_cursor_right =
    (
      ~move_cursor_right_zoperand: 'zoperand => option('zoperand),
      ~move_cursor_right_zbinop: 'zoperator => option('zoperator),
      ~place_before_operand: 'operand => 'zoperand,
      ~place_before_binop: 'operator => option('zoperator),
      ~erase_zoperand: 'zoperand => 'operand,
      ~erase_zbinop: 'zoperator => 'operator,
      zseq: t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : option(t('operand, 'operator, 'zoperand, 'zoperator)) =>
  switch (zseq) {
  | ZOperand(zoperand, surround) =>
    switch (move_cursor_right_zoperand(zoperand), surround) {
    | (None, (_, E)) => None
    | (None, (prefix, A(suffix_hd, suffix_tl))) =>
      switch (suffix_hd |> place_before_binop) {
      | None =>
        let S(operand, new_suffix) = suffix_tl;
        let new_zoperand = operand |> place_before_operand;
        let new_prefix =
          Seq.A(suffix_hd, S(zoperand |> erase_zoperand, prefix));
        Some(ZOperand(new_zoperand, (new_prefix, new_suffix)));
      | Some(zoperator) =>
        let new_prefix = Seq.S(zoperand |> erase_zoperand, prefix);
        let new_suffix = suffix_tl;
        Some(ZOperator(zoperator, (new_prefix, new_suffix)));
      }
    | (Some(zoperand), _) => Some(ZOperand(zoperand, surround))
    }
  | ZOperator(zop, surround) =>
    switch (move_cursor_right_zbinop(zop), surround) {
    | (None, (prefix, S(suffix_hd, new_suffix))) =>
      let zoperand = suffix_hd |> place_before_operand;
      let new_prefix = Seq.A(zop |> erase_zbinop, prefix);
      Some(ZOperand(zoperand, (new_prefix, new_suffix)));
    | (Some(zop), _) => Some(ZOperator(zop, surround))
    }
  };
