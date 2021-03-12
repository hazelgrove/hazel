[@deriving sexp]
type t('operand, 'operator, 'zoperand, 'zoperator) =
  | ZOpSeq(
      Skel.t('operator),
      ZSeq.t('operand, 'operator, 'zoperand, 'zoperator),
    );

let mk =
    (
      ~associate: Seq.t('operand, 'operator) => Skel.t('operator),
      ~erase_zoperand: 'zoperand => 'operand,
      ~erase_zbinop: 'zoperator => 'operator,
      zseq: ZSeq.t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : t('operand, 'operator, 'zoperand, 'zoperator) =>
  switch (zseq) {
  | ZOperand(zoperand, surround) =>
    let seq =
      Seq.t_of_operand_and_surround(erase_zoperand(zoperand), surround);
    let skel = associate(seq);
    ZOpSeq(skel, ZOperand(zoperand, surround));
  | ZOperator(zoperator, surround) =>
    let seq =
      Seq.t_of_operator_and_surround(erase_zbinop(zoperator), surround);
    let skel = associate(seq);
    ZOpSeq(skel, ZOperator(zoperator, surround));
  };

let wrap = zoperand => ZOpSeq(Placeholder(0), zoperand |> ZSeq.wrap);

let skel_contains_cursor =
    (skel: Skel.t('operator), zseq: ZSeq.t(_, 'operator, _, _)): bool =>
  switch (zseq) {
  | ZOperator(_, (prefix, suffix)) =>
    let seq_len = Seq.length(prefix) + Seq.length(suffix);
    let zoperator_index = seq_len + Seq.length(prefix) - 1;
    let leftmost_operator_index = seq_len + Skel.leftmost_tm_index(skel);
    let rightmost_operator_index =
      seq_len + Skel.rightmost_tm_index(skel) - 1;
    leftmost_operator_index <= zoperator_index
    && zoperator_index <= rightmost_operator_index;
  | ZOperand(_, (prefix, _)) =>
    let zoperand_index = Seq.length_of_affix(prefix);
    let leftmost_operand_index = Skel.leftmost_tm_index(skel);
    let rightmost_operand_index = Skel.rightmost_tm_index(skel);
    leftmost_operand_index <= zoperand_index
    && zoperand_index <= rightmost_operand_index;
  };

let skel_is_rooted_at_cursor =
    (skel: Skel.t('operator), zseq: ZSeq.t(_, 'operator, _, _)): bool =>
  switch (zseq) {
  | ZOperand(_, (prefix, _)) =>
    switch (skel) {
    | Placeholder(n) => n == Seq.length_of_affix(prefix)
    | BinOp(_) => false
    }
  | ZOperator(_, (prefix, _)) =>
    switch (skel) {
    | Placeholder(_) => false
    | BinOp(_, _, skel1, _) =>
      Skel.rightmost_tm_index(skel1) == Seq.length(prefix) - 1
    }
  };

let set_err_status =
    (
      ~set_err_status_zoperand: (ErrStatus.t, 'zoperand) => 'zoperand,
      err: ErrStatus.t,
      ZOpSeq(skel, zseq): t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : t('operand, 'operator, 'zoperand, 'zoperator) => {
  let (skel: Skel.t(_), zseq: ZSeq.t(_)) =
    switch (skel, zseq) {
    | (Placeholder(_), ZOperator(_, _)) => assert(false)
    | (BinOp(_, op, skel1, skel2), ZOperator(zop, surround)) => (
        BinOp(err, op, skel1, skel2),
        ZOperator(zop, surround),
      )
    | (Placeholder(_), ZOperand(zoperand, surround)) => (
        skel,
        ZOperand(zoperand |> set_err_status_zoperand(err), surround),
      )
    | (BinOp(_, op, skel1, skel2), ZOperand(zoperand, surround)) => (
        BinOp(err, op, skel1, skel2),
        ZOperand(zoperand, surround),
      )
    };
  ZOpSeq(skel, zseq);
};

let mk_inconsistent =
    (
      ~mk_inconsistent_zoperand:
         (MetaVarGen.t, 'zoperand) => ('zoperand, MetaVarGen.t),
      u_gen: MetaVarGen.t,
      ZOpSeq(skel, zseq): t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : (t('operand, 'operator, 'zoperand, 'zoperator), MetaVarGen.t) => {
  let (skel: Skel.t(_), zseq: ZSeq.t(_), u_gen) =
    switch (skel, zseq) {
    | (Placeholder(_), ZOperator(_, _)) => assert(false)
    | (BinOp(_, op, skel1, skel2), ZOperator(zop, surround)) =>
      let (u, u_gen) = u_gen |> MetaVarGen.next;
      (
        BinOp(InHole(TypeInconsistent, u), op, skel1, skel2),
        ZOperator(zop, surround),
        u_gen,
      );
    | (Placeholder(_), ZOperand(zoperand, surround)) =>
      let (zoperand, u_gen) = zoperand |> mk_inconsistent_zoperand(u_gen);
      (skel, ZOperand(zoperand, surround), u_gen);
    | (BinOp(_, op, skel1, skel2), ZOperand(zoperand, surround)) =>
      let (u, u_gen) = u_gen |> MetaVarGen.next;
      (
        BinOp(InHole(TypeInconsistent, u), op, skel1, skel2),
        ZOperand(zoperand, surround),
        u_gen,
      );
    };
  (ZOpSeq(skel, zseq), u_gen);
};

let erase =
    (
      ~erase_zoperand: 'zoperand => 'operand,
      ~erase_zbinop: 'zoperator => 'operator,
      ZOpSeq(skel, zseq): t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : OpSeq.t('operand, 'operator) =>
  OpSeq(skel, zseq |> ZSeq.erase(~erase_zoperand, ~erase_zbinop));

let is_before =
    (
      ~is_before_zoperand: 'zoperand => bool,
      ZOpSeq(_, zseq): t(_, _, 'zoperand, _),
    )
    : bool =>
  zseq |> ZSeq.is_before(~is_before_zoperand);

let is_after =
    (
      ~is_after_zoperand: 'zoperand => bool,
      ZOpSeq(_, zseq): t(_, _, 'zoperand, _),
    )
    : bool =>
  zseq |> ZSeq.is_after(~is_after_zoperand);

let is_outer =
    (
      ~is_outer_zoperand: 'zoperand => bool,
      ZOpSeq(skel, zseq): t(_, _, 'zoperand, _),
    )
    : bool => {
  switch (zseq) {
  | ZOperator(_) => skel_is_rooted_at_cursor(skel, zseq)
  | ZOperand(operand, (E, E)) => is_outer_zoperand(operand)
  | ZOperand(_) => false
  };
};

let place_before =
    (
      ~place_before_operand: 'operand => 'zoperand,
      OpSeq(skel, seq): OpSeq.t('operand, 'operator),
    )
    : t('operand, 'operator, 'zoperand, 'zoperator) => {
  let (first, suffix) = seq |> Seq.split_first_and_suffix;
  ZOpSeq(skel, ZOperand(first |> place_before_operand, (E, suffix)));
};

let place_after =
    (
      ~place_after_operand: 'operand => 'zoperand,
      OpSeq(skel, seq): OpSeq.t('operand, 'operator),
    )
    : t('operand, 'operator, 'zoperand, 'zoperator) => {
  let (prefix, last) = seq |> Seq.split_prefix_and_last;
  ZOpSeq(skel, ZOperand(last |> place_after_operand, (prefix, E)));
};

let move_cursor_left =
    (
      ~move_cursor_left_zoperand: 'zoperand => option('zoperand),
      ~move_cursor_left_zbinop: 'zoperator => option('zoperator),
      ~place_after_operand: 'operand => 'zoperand,
      ~place_after_binop: 'operator => option('zoperator),
      ~erase_zoperand: 'zoperand => 'operand,
      ~erase_zbinop: 'zoperator => 'operator,
      ZOpSeq(skel, zseq): t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : option(t('operand, 'operator, 'zoperand, 'zoperator)) =>
  zseq
  |> ZSeq.move_cursor_left(
       ~move_cursor_left_zoperand,
       ~move_cursor_left_zbinop,
       ~place_after_operand,
       ~place_after_binop,
       ~erase_zoperand,
       ~erase_zbinop,
     )
  |> Option.map(zseq => ZOpSeq(skel, zseq));

let move_cursor_right =
    (
      ~move_cursor_right_zoperand: 'zoperand => option('zoperand),
      ~move_cursor_right_zbinop: 'zoperator => option('zoperator),
      ~place_before_operand: 'operand => 'zoperand,
      ~place_before_binop: 'operator => option('zoperator),
      ~erase_zoperand: 'zoperand => 'operand,
      ~erase_zbinop: 'zoperator => 'operator,
      ZOpSeq(skel, zseq): t('operand, 'operator, 'zoperand, 'zoperator),
    )
    : option(t('operand, 'operator, 'zoperand, 'zoperator)) =>
  zseq
  |> ZSeq.move_cursor_right(
       ~move_cursor_right_zoperand,
       ~move_cursor_right_zbinop,
       ~place_before_operand,
       ~place_before_binop,
       ~erase_zoperand,
       ~erase_zbinop,
     )
  |> Option.map(zseq => ZOpSeq(skel, zseq));
