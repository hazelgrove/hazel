open OptUtil.Syntax;

[@deriving sexp]
type zsum_body_operator = (CursorPosition.t, Operators_SumBody.t);

[@deriving sexp]
type t = zopseq
and zopseq = ZOpSeq.t(UHTyp.operand, UHTyp.operator, zoperand, zoperator)
and zoperand =
  | CursorT(CursorPosition.t, UHTyp.operand)
  | ParenthesizedZ(t)
  | ListZ(t)
  | FiniteSumZ(zsum_body)
  | ElidedSumZ(zsum_body_operand)
and zoperator = (CursorPosition.t, UHTyp.operator)
and zsum_body =
  ZOpSeq.t(
    UHTyp.sum_body_operand,
    UHTyp.sum_body_operator,
    zsum_body_operand,
    zsum_body_operator,
  )
and zsum_body_operand =
  | CursorArgTag(CursorPosition.t, UHTag.t, UHTyp.t)
  | ConstTagZ(ZTag.t)
  | ArgTagZT(ZTag.t, UHTyp.t)
  | ArgTagZA(UHTag.t, t);

type operand_surround = Seq.operand_surround(UHTyp.operand, UHTyp.operator);
type operator_surround = Seq.operator_surround(UHTyp.operand, UHTyp.operator);
type zseq = ZSeq.t(UHTyp.operand, UHTyp.operator, zoperand, zoperator);

type sum_body_operand_surround =
  Seq.operand_surround(UHTyp.sum_body_operand, UHTyp.sum_body_operator);
type sum_body_operator_surround =
  Seq.operator_surround(UHTyp.sum_body_operand, UHTyp.sum_body_operator);
type sum_body_zseq =
  ZSeq.t(
    UHTyp.sum_body_operand,
    UHTyp.sum_body_operator,
    zsum_body_operand,
    zsum_body_operator,
  );
let valid_cursors_operand: UHTyp.operand => list(CursorPosition.t) =
  fun
  | Hole
  | Unit
  | Int
  | Float
  | Bool => CursorPosition.delim_cursors(1)
  | Parenthesized(_)
  | List(_)
  | FiniteSum(_)
  | ElidedSum(_) => CursorPosition.delim_cursors(2);

let valid_cursors_operator: UHTyp.operator => list(CursorPosition.t) =
  fun
  | _ => [OnOp(Before), OnOp(After)];

let valid_cursors_sum_body_operand:
  UHTyp.sum_body_operand => list(CursorPosition.t) =
  fun
  | ConstTag(tag) => ZTag.valid_cursors(tag)
  | ArgTag(_, _) => CursorPosition.delim_cursors(2);

let valid_cursors_sum_body_operator =
    (_: UHTyp.sum_body_operator): list(CursorPosition.t) => [
  OnOp(Before),
  OnOp(After),
];

let is_valid_cursor_operand =
    (cursor: CursorPosition.t, operand: UHTyp.operand): bool =>
  valid_cursors_operand(operand) |> List.mem(cursor);
let is_valid_cursor_operator =
    (cursor: CursorPosition.t, operator: UHTyp.operator): bool =>
  valid_cursors_operator(operator) |> List.mem(cursor);

let is_valid_cursor_sum_body_operand =
    (cursor: CursorPosition.t, sum_body_operand: UHTyp.sum_body_operand): bool =>
  valid_cursors_sum_body_operand(sum_body_operand) |> List.mem(cursor);
let is_valid_cursor_sum_body_operator =
    (cursor: CursorPosition.t, sum_body_operator: UHTyp.sum_body_operator)
    : bool =>
  valid_cursors_sum_body_operator(sum_body_operator) |> List.mem(cursor);

let erase_zoperator =
  fun
  | (_, operator) => operator;
let erase_zsum_body_operator =
  fun
  | (_, sum_body_op) => sum_body_op;

let rec erase = (zty: t): UHTyp.t => zty |> erase_zopseq
and erase_zopseq = zopseq =>
  ZOpSeq.erase(~erase_zoperand, ~erase_zoperator, zopseq)
and erase_zoperand =
  fun
  | CursorT(_, operand) => operand
  | ParenthesizedZ(zty) => Parenthesized(erase(zty))
  | FiniteSumZ(zsum_body) => FiniteSum(Some(erase_zsum_body(zsum_body)))
  | ElidedSumZ(zoperand) => ElidedSum(erase_zsum_body_operand(zoperand))
  | ListZ(zty) => List(erase(zty))
and erase_zsum_body = zsum_body =>
  ZOpSeq.erase(
    ~erase_zoperand=erase_zsum_body_operand,
    ~erase_zoperator,
    zsum_body,
  )
and erase_zsum_body_operand =
  fun
  | ConstTagZ(ztag) => UHTyp.ConstTag(ZTag.erase(ztag))
  | CursorArgTag(_, tag, ty) => UHTyp.ArgTag(tag, ty)
  | ArgTagZT(ztag, ty) => UHTyp.ArgTag(ZTag.erase(ztag), ty)
  | ArgTagZA(tag, zty) => UHTyp.ArgTag(tag, erase(zty));

let mk_ZOpSeq: zseq => zopseq =
  ZOpSeq.mk(~associate=UHTyp.associate, ~erase_zoperand, ~erase_zoperator);

let mk_sum_body_ZOpSeq: sum_body_zseq => zsum_body =
  ZOpSeq.mk(
    ~associate=UHTyp.associate_sum_body,
    ~erase_zoperand=erase_zsum_body_operand,
    ~erase_zoperator=erase_zsum_body_operator,
  );

let erase_zseq = zseq =>
  zseq |> ZSeq.erase(~erase_zoperand, ~erase_zoperator);

let rec is_before = (zty: t): bool => zty |> is_before_zopseq
and is_before_zopseq = zopseq => ZOpSeq.is_before(~is_before_zoperand, zopseq)
and is_before_zoperand =
  fun
  | CursorT(cursor, Hole)
  | CursorT(cursor, Unit)
  | CursorT(cursor, Int)
  | CursorT(cursor, Float)
  | CursorT(cursor, Bool)
  | CursorT(cursor, Parenthesized(_))
  | CursorT(cursor, FiniteSum(_))
  | CursorT(cursor, ElidedSum(_))
  | CursorT(cursor, List(_)) => cursor == OnDelim(0, Before)
  | ParenthesizedZ(_)
  | FiniteSumZ(_)
  | ElidedSumZ(_)
  | ListZ(_) => false;
let is_before_zoperator: zoperator => bool =
  fun
  | (OnOp(Before), _) => true
  | _ => false;

let is_before_zsum_body_operand =
  fun
  | CursorArgTag(_, _, _) => false
  | ConstTagZ(ztag)
  | ArgTagZT(ztag, _) => ZTag.is_before(ztag)
  | ArgTagZA(_, _) => false;

let is_before_zsum_body_operator = ((cursor, _): zsum_body_operator): bool =>
  switch (cursor) {
  | OnOp(Before) => true
  | _ => false
  };

let is_before_zsum_body = (zsum_body: zsum_body): bool =>
  ZOpSeq.is_before(
    ~is_before_zoperand=is_before_zsum_body_operand,
    zsum_body,
  );

let rec is_after = (zty: t): bool => zty |> is_after_zopseq
and is_after_zopseq = zopseq => ZOpSeq.is_after(~is_after_zoperand, zopseq)
and is_after_zoperand =
  fun
  | CursorT(cursor, Hole)
  | CursorT(cursor, Unit)
  | CursorT(cursor, Int)
  | CursorT(cursor, Float)
  | CursorT(cursor, Bool) => cursor == OnDelim(0, After)
  | CursorT(cursor, Parenthesized(_))
  | CursorT(cursor, FiniteSum(_))
  | CursorT(cursor, ElidedSum(_))
  | CursorT(cursor, List(_)) => cursor == OnDelim(1, After)
  | ParenthesizedZ(_)
  | FiniteSumZ(_)
  | ElidedSumZ(_)
  | ListZ(_) => false;
let is_after_zoperator: zoperator => bool =
  fun
  | (OnOp(After), _) => true
  | _ => false;

let is_after_zsum_body_operand =
  fun
  | CursorArgTag(cursor, _, _) => cursor == OnDelim(1, After)
  | ArgTagZT(_, _)
  | ArgTagZA(_, _) => false
  | ConstTagZ(ztag) => ZTag.is_after(ztag);

let is_after_zsum_body_operator = ((cursor, _): zsum_body_operator): bool =>
  switch (cursor) {
  | OnOp(After) => true
  | _ => false
  };

let is_after_zsum_body = (zsum_body: zsum_body): bool =>
  ZOpSeq.is_after(~is_after_zoperand=is_after_zsum_body_operand, zsum_body);

let rec place_before = (ty: UHTyp.t): t => ty |> place_before_opseq
and place_before_opseq = opseq =>
  ZOpSeq.place_before(~place_before_operand, opseq)
and place_before_operand =
  fun
  | (
      Hole | Unit | Int | Float | Bool | Parenthesized(_) | FiniteSum(_) |
      ElidedSum(_) |
      List(_)
    ) as operand =>
    CursorT(OnDelim(0, Before), operand);
let place_before_operator = (op: UHTyp.operator): option(zoperator) =>
  Some((OnOp(Before), op));

let place_before_sum_body_operator =
    (sum_body_op: UHTyp.sum_body_operator): option(zsum_body_operator) =>
  Some((OnOp(Before), sum_body_op));
let place_before_sum_body_operand: UHTyp.sum_body_operand => zsum_body_operand =
  fun
  | ConstTag(tag) => ConstTagZ(ZTag.place_before(tag))
  | ArgTag(tag, ty) => ArgTagZT(ZTag.place_before(tag), ty);
let place_before_sum_body = (sum_body: UHTyp.sum_body): zsum_body =>
  ZOpSeq.place_before(
    ~place_before_operand=place_before_sum_body_operand,
    sum_body,
  );

let rec place_after = (ty: UHTyp.t): t => ty |> place_after_opseq
and place_after_opseq = opseq =>
  ZOpSeq.place_after(~place_after_operand, opseq)
and place_after_operand =
  fun
  | (Hole | Unit | Int | Float | Bool) as operand =>
    CursorT(OnDelim(0, After), operand)
  | (Parenthesized(_) | FiniteSum(_) | ElidedSum(_) | List(_)) as operand =>
    CursorT(OnDelim(1, After), operand);
let place_after_operator = (op: UHTyp.operator): option(zoperator) =>
  Some((OnOp(After), op));
let place_after_sum_body_operand =
    (sum_body_operand: UHTyp.sum_body_operand): zsum_body_operand =>
  switch (sum_body_operand) {
  | UHTyp.ConstTag(tag) => ConstTagZ(ZTag.place_after(tag))
  | UHTyp.ArgTag(tag, ty) => CursorArgTag(OnDelim(1, After), tag, ty)
  };
let place_after_sum_body = (sum_body: UHTyp.sum_body): zsum_body =>
  ZOpSeq.place_after(
    ~place_after_operand=place_after_sum_body_operand,
    sum_body,
  );
let place_after_sum_body_operator =
    (op: UHTyp.sum_body_operator): option(zsum_body_operator) =>
  Some((OnOp(After), op));

let place_cursor_operand =
    (cursor: CursorPosition.t, operand: UHTyp.operand): option(zoperand) =>
  is_valid_cursor_operand(cursor, operand)
    ? Some(CursorT(cursor, operand)) : None;
let place_cursor_operator =
    (cursor: CursorPosition.t, operator: UHTyp.operator): option(zoperator) =>
  is_valid_cursor_operator(cursor, operator)
    ? Some((cursor, operator)) : None;

let place_cursor_sum_body_operand =
    (cursor: CursorPosition.t, operand: UHTyp.sum_body_operand)
    : option(zsum_body_operand) =>
  is_valid_cursor_sum_body_operand(cursor, operand)
    ? switch (operand) {
      | ConstTag(tag) => Some(ConstTagZ(ZTag.CursorTag(cursor, tag)))
      | ArgTag(tag, ty) => Some(CursorArgTag(cursor, tag, ty))
      }
    : None;

let place_cursor_sum_body_operator =
    (cursor: CursorPosition.t, sum_body_operator: UHTyp.sum_body_operator)
    : option(zsum_body_operator) =>
  is_valid_cursor_sum_body_operator(cursor, sum_body_operator)
    ? Some((cursor, sum_body_operator)) : None;

let move_cursor_left_zoperator: zoperator => option(zoperator) =
  fun
  | (OnText(_) | OnDelim(_, _), _) => None
  | (OnOp(Before), _) => None
  | (OnOp(After), op) => Some((OnOp(Before), op));

let move_cursor_left_zsum_body_operator:
  zsum_body_operator => option(zsum_body_operator) =
  fun
  | (OnText(_) | OnDelim(_, _), _) => None
  | (OnOp(Before), _) => None
  | (OnOp(After), op) => Some((OnOp(Before), op));

let rec move_cursor_left = (zty: t): option(t) =>
  zty |> move_cursor_left_zopseq
and move_cursor_left_zopseq = zopseq =>
  ZOpSeq.move_cursor_left(
    ~move_cursor_left_zoperand,
    ~move_cursor_left_zoperator,
    ~place_after_operand,
    ~place_after_operator,
    ~erase_zoperand,
    ~erase_zoperator,
    zopseq,
  )
and move_cursor_left_zoperand =
  fun
  | z when is_before_zoperand(z) => None
  | CursorT(OnOp(_) | OnText(_), _) => None
  | CursorT(OnDelim(k, After), ty) =>
    Some(CursorT(OnDelim(k, Before), ty))
  | CursorT(OnDelim(_, Before), Hole | Unit | Int | Float | Bool) => None
  | CursorT(OnDelim(_k, Before), Parenthesized(ty1)) =>
    // _k == 1
    Some(ParenthesizedZ(place_after(ty1)))

  | CursorT(OnDelim(_1, Before), FiniteSum(None) as operand) =>
    place_cursor_operand(OnDelim(0, Before), operand)
  | CursorT(OnDelim(_1, Before), FiniteSum(Some(sum_body))) =>
    Some(FiniteSumZ(place_after_sum_body(sum_body)))
  | CursorT(OnDelim(_1, Before), ElidedSum(operand)) =>
    Some(ElidedSumZ(place_after_sum_body_operand(operand)))

  | CursorT(OnDelim(_k, Before), List(ty1)) =>
    // _k == 1
    Some(ListZ(place_after(ty1)))
  | ParenthesizedZ(zty1) =>
    switch (move_cursor_left(zty1)) {
    | Some(zty1) => Some(ParenthesizedZ(zty1))
    | None => Some(CursorT(OnDelim(0, After), Parenthesized(erase(zty1))))
    }
  | FiniteSumZ(zsum_body) =>
    switch (move_cursor_left_zsum_body(zsum_body)) {
    | Some(zsum_body) => Some(FiniteSumZ(zsum_body))
    | None =>
      let sum_body = erase_zsum_body(zsum_body);
      Some(CursorT(OnDelim(0, After), FiniteSum(Some(sum_body))));
    }
  | ElidedSumZ(zoperand) =>
    switch (move_cursor_left_zsum_body_operand(zoperand)) {
    | Some(zoperand') => Some(ElidedSumZ(zoperand'))
    | None =>
      let operand = erase_zsum_body_operand(zoperand);
      Some(CursorT(OnDelim(0, After), ElidedSum(operand)));
    }
  | ListZ(zty1) =>
    switch (move_cursor_left(zty1)) {
    | Some(zty1) => Some(ListZ(zty1))
    | None => Some(CursorT(OnDelim(0, After), List(erase(zty1))))
    }
and move_cursor_left_zsum_body = zsum_body =>
  ZOpSeq.move_cursor_left(
    ~move_cursor_left_zoperand=move_cursor_left_zsum_body_operand,
    ~move_cursor_left_zoperator=move_cursor_left_zsum_body_operator,
    ~place_after_operand=place_after_sum_body_operand,
    ~place_after_operator=place_after_sum_body_operator,
    ~erase_zoperand=erase_zsum_body_operand,
    ~erase_zoperator=erase_zsum_body_operator,
    zsum_body,
  )
and move_cursor_left_zsum_body_operand =
  fun
  | z when is_before_zsum_body_operand(z) => None
  | ConstTagZ(ztag) => {
      let+ ztag = ZTag.move_cursor_left(ztag);
      ConstTagZ(ztag);
    }
  /* Invalid movement actions */
  | CursorArgTag(OnOp(_) | OnText(_), _, _) => None

  | CursorArgTag(OnDelim(j, side), tag, ty) =>
    switch (j, side) {
    | (0, Before) => Some(ArgTagZT(ZTag.place_after(tag), ty))
    | (1, Before) => Some(ArgTagZA(tag, place_after(ty)))
    | (_, After) => Some(CursorArgTag(OnDelim(j, Before), tag, ty))
    | (_, _) => None
    }

  | ArgTagZT(ztag, ty) =>
    switch (ZTag.move_cursor_left(ztag)) {
    | Some(ztag) => Some(ArgTagZT(ztag, ty))
    | None => Some(CursorArgTag(OnDelim(0, Before), ZTag.erase(ztag), ty))
    }

  | ArgTagZA(tag, zty) =>
    switch (move_cursor_left(zty)) {
    | Some(zty) => Some(ArgTagZA(tag, zty))
    | None => Some(CursorArgTag(OnDelim(0, After), tag, erase(zty)))
    };

let move_cursor_right_zoperator: zoperator => option(zoperator) =
  fun
  | (OnText(_) | OnDelim(_, _), _) => None
  | (OnOp(After), _) => None
  | (OnOp(Before), op) => Some((OnOp(After), op));
let move_cursor_right_zsum_body_operator:
  zsum_body_operator => option(zsum_body_operator) =
  fun
  | (OnText(_) | OnDelim(_, _), _) => None
  | (OnOp(After), _) => None
  | (OnOp(Before), op) => Some((OnOp(After), op));

let rec move_cursor_right = (zty: t): option(t) =>
  zty |> move_cursor_right_zopseq
and move_cursor_right_zopseq = zopseq =>
  ZOpSeq.move_cursor_right(
    ~move_cursor_right_zoperand,
    ~move_cursor_right_zoperator,
    ~place_before_operand,
    ~place_before_operator,
    ~erase_zoperand,
    ~erase_zoperator,
    zopseq,
  )
and move_cursor_right_zoperand =
  fun
  | z when is_after_zoperand(z) => None
  | CursorT(OnOp(_) | OnText(_), _) => None
  | CursorT(OnDelim(k, Before), ty) =>
    Some(CursorT(OnDelim(k, After), ty))
  | CursorT(OnDelim(_, After), Hole | Unit | Int | Float | Bool) => None
  | CursorT(OnDelim(_k, After), Parenthesized(ty1)) =>
    // _k == 0
    Some(ParenthesizedZ(place_before(ty1)))

  | CursorT(OnDelim(_0, After), FiniteSum(None) as operand) =>
    place_cursor_operand(OnDelim(1, After), operand)
  | CursorT(OnDelim(_0, After), FiniteSum(Some(sum_body))) =>
    Some(FiniteSumZ(place_before_sum_body(sum_body)))
  | CursorT(OnDelim(_0, After), ElidedSum(operand)) =>
    Some(ElidedSumZ(place_before_sum_body_operand(operand)))

  | CursorT(OnDelim(_k, After), List(ty1)) =>
    // _k == 0
    Some(ListZ(place_before(ty1)))
  | ParenthesizedZ(zty1) =>
    switch (move_cursor_right(zty1)) {
    | Some(zty1) => Some(ParenthesizedZ(zty1))
    | None =>
      Some(CursorT(OnDelim(1, Before), Parenthesized(erase(zty1))))
    }
  | FiniteSumZ(zsum_body) =>
    switch (move_cursor_right_zsum_body(zsum_body)) {
    | Some(zsum_body) => Some(FiniteSumZ(zsum_body))
    | None =>
      let sum_body = erase_zsum_body(zsum_body);
      Some(CursorT(OnDelim(1, Before), FiniteSum(Some(sum_body))));
    }
  | ElidedSumZ(zoperand) =>
    switch (move_cursor_right_zsum_body_operand(zoperand)) {
    | Some(zoperand') => Some(ElidedSumZ(zoperand'))
    | None =>
      let operand = erase_zsum_body_operand(zoperand);
      Some(CursorT(OnDelim(1, Before), ElidedSum(operand)));
    }
  | ListZ(zty1) =>
    switch (move_cursor_right(zty1)) {
    | Some(zty1) => Some(ListZ(zty1))
    | None => Some(CursorT(OnDelim(1, Before), List(erase(zty1))))
    }
and move_cursor_right_zsum_body = zsum_body =>
  ZOpSeq.move_cursor_right(
    ~move_cursor_right_zoperand=move_cursor_right_zsum_body_operand,
    ~move_cursor_right_zoperator=move_cursor_right_zsum_body_operator,
    ~place_before_operand=place_before_sum_body_operand,
    ~place_before_operator=place_before_sum_body_operator,
    ~erase_zoperand=erase_zsum_body_operand,
    ~erase_zoperator=erase_zsum_body_operator,
    zsum_body,
  )
and move_cursor_right_zsum_body_operand =
    // fun
    (zoperand: zsum_body_operand) =>
  switch (zoperand) {
  | z when is_after_zsum_body_operand(z) => None
  | ConstTagZ(ztag) =>
    let+ ztag = ZTag.move_cursor_right(ztag);
    ConstTagZ(ztag);
  | CursorArgTag(OnOp(_) | OnText(_), _, _) => None
  | CursorArgTag(OnDelim(k, Before), tag, ty) =>
    Some(CursorArgTag(OnDelim(k, After), tag, ty))
  | CursorArgTag(OnDelim(k, After), tag, ty) =>
    switch (k) {
    | 0 => Some(ArgTagZT(ZTag.place_before(tag), ty))
    | 1 => Some(ArgTagZA(tag, place_before(ty)))
    | _ => None
    }
  | ArgTagZT(ztag, ty) =>
    switch (ZTag.move_cursor_right(ztag)) {
    | Some(ztag) => Some(ArgTagZT(ztag, ty))
    | None => Some(CursorArgTag(OnDelim(0, Before), ZTag.erase(ztag), ty))
    }
  | ArgTagZA(tag, zty) =>
    switch (move_cursor_right(zty)) {
    | Some(zty) => Some(ArgTagZA(tag, zty))
    | None => Some(CursorArgTag(OnDelim(1, Before), tag, erase(zty)))
    }
  };

let rec cursor_on_EmptyTagHole = zty => cursor_on_EmptyTagHole_zopseq(zty)
and cursor_on_EmptyTagHole_zopseq: t => option(MetaVar.t) =
  fun
  | ZOpSeq(_, ZOperator(_)) => None
  | ZOpSeq(_, ZOperand(zoperand, _)) =>
    cursor_on_EmptyTagHole_zoperand(zoperand)
and cursor_on_EmptyTagHole_zoperand =
  fun
  | CursorT(_, _) => None
  | ParenthesizedZ(zty) => cursor_on_EmptyTagHole(zty)
  | ListZ(zty) => cursor_on_EmptyTagHole(zty)
  | FiniteSumZ(zsum_body) => cursor_on_EmptyTagHole_zsum_body(zsum_body)
  | ElidedSumZ(zoperand) =>
    cursor_on_EmptyTagHole_zsum_body_operand(zoperand)

and cursor_on_EmptyTagHole_zsum_body: zsum_body => option(MetaVar.t) =
  fun
  | ZOpSeq(_, ZOperator(_)) => None
  | ZOpSeq(_, ZOperand(zoperand, _)) =>
    cursor_on_EmptyTagHole_zsum_body_operand(zoperand)

and cursor_on_EmptyTagHole_zsum_body_operand:
  zsum_body_operand => option(MetaVar.t) =
  fun
  | CursorArgTag(_, _, _) => None
  | ConstTagZ(ztag)
  | ArgTagZT(ztag, _) => ZTag.cursor_on_EmptyTagHole(ztag)
  | ArgTagZA(_, zty) => cursor_on_EmptyTagHole(zty);
