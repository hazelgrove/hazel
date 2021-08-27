open OptUtil.Syntax;

[@deriving sexp]
type zsumbody_operator = (CursorPosition.t, Operators_SumBody.t);

[@deriving sexp]
type t = zopseq
and zopseq = ZOpSeq.t(UHTyp.operand, UHTyp.operator, zoperand, zoperator)
and zoperand =
  | CursorT(CursorPosition.t, UHTyp.operand)
  | ParenthesizedZ(t)
  | ListZ(t)
  | SumZ(zsumbody)
and zoperator = (CursorPosition.t, UHTyp.operator)
and zsumbody =
  ZOpSeq.t(
    UHTyp.sumbody_operand,
    UHTyp.sumbody_operator,
    zsumbody_operand,
    zsumbody_operator,
  )
and zsumbody_operand =
  | CursorArgTag(CursorPosition.t, UHTag.t, UHTyp.t)
  | ConstTagZ(ZTag.t)
  | ArgTagZT(ZTag.t, UHTyp.t)
  | ArgTagZA(UHTag.t, t);

type operand_surround = Seq.operand_surround(UHTyp.operand, UHTyp.operator);
type operator_surround = Seq.operator_surround(UHTyp.operand, UHTyp.operator);
type zseq = ZSeq.t(UHTyp.operand, UHTyp.operator, zoperand, zoperator);

type sumbody_operand_surround =
  Seq.operand_surround(UHTyp.sumbody_operand, UHTyp.sumbody_operator);
type sumbody_operator_surround =
  Seq.operator_surround(UHTyp.sumbody_operand, UHTyp.sumbody_operator);
type sumbody_zseq =
  ZSeq.t(
    UHTyp.sumbody_operand,
    UHTyp.sumbody_operator,
    zsumbody_operand,
    zsumbody_operator,
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
  | Sum(_) => CursorPosition.delim_cursors(2);

let valid_cursors_operator: UHTyp.operator => list(CursorPosition.t) =
  fun
  | _ => [OnOp(Before), OnOp(After)];

let valid_cursors_sumbody_operand:
  UHTyp.sumbody_operand => list(CursorPosition.t) =
  fun
  | ConstTag(tag) => ZTag.valid_cursors(tag)
  | ArgTag(_, _) => CursorPosition.delim_cursors(2);

let valid_cursors_sumbody_operator =
    (_: UHTyp.sumbody_operator): list(CursorPosition.t) => [
  OnOp(Before),
  OnOp(After),
];

let is_valid_cursor_operand =
    (cursor: CursorPosition.t, operand: UHTyp.operand): bool =>
  valid_cursors_operand(operand) |> List.mem(cursor);
let is_valid_cursor_operator =
    (cursor: CursorPosition.t, operator: UHTyp.operator): bool =>
  valid_cursors_operator(operator) |> List.mem(cursor);

let is_valid_cursor_sumbody_operand =
    (cursor: CursorPosition.t, sumbody_operand: UHTyp.sumbody_operand): bool =>
  valid_cursors_sumbody_operand(sumbody_operand) |> List.mem(cursor);
let is_valid_cursor_sumbody_operator =
    (cursor: CursorPosition.t, sumbody_operator: UHTyp.sumbody_operator): bool =>
  valid_cursors_sumbody_operator(sumbody_operator) |> List.mem(cursor);

let erase_zoperator =
  fun
  | (_, operator) => operator;
let erase_zsumbody_operator =
  fun
  | (_, sumbody_op) => sumbody_op;

let rec erase = (zty: t): UHTyp.t => zty |> erase_zopseq
and erase_zopseq = zopseq =>
  ZOpSeq.erase(~erase_zoperand, ~erase_zoperator, zopseq)
and erase_zoperand =
  fun
  | CursorT(_, operand) => operand
  | ParenthesizedZ(zty) => Parenthesized(erase(zty))
  | SumZ(zsumbody) => Sum(Some(erase_zsumbody(zsumbody)))
  | ListZ(zty) => List(erase(zty))
and erase_zsumbody = zsumbody =>
  ZOpSeq.erase(
    ~erase_zoperand=erase_zsumbody_operand,
    ~erase_zoperator,
    zsumbody,
  )
and erase_zsumbody_operand =
  fun
  | ConstTagZ(ztag) => UHTyp.ConstTag(ZTag.erase(ztag))
  | CursorArgTag(_, tag, ty) => UHTyp.ArgTag(tag, ty)
  | ArgTagZT(ztag, ty) => UHTyp.ArgTag(ZTag.erase(ztag), ty)
  | ArgTagZA(tag, zty) => UHTyp.ArgTag(tag, erase(zty));

let mk_ZOpSeq: zseq => zopseq =
  ZOpSeq.mk(~associate=UHTyp.associate, ~erase_zoperand, ~erase_zoperator);

let mk_sumbody_ZOpSeq: sumbody_zseq => zsumbody =
  ZOpSeq.mk(
    ~associate=UHTyp.associate_sumbody,
    ~erase_zoperand=erase_zsumbody_operand,
    ~erase_zoperator=erase_zsumbody_operator,
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
  | CursorT(cursor, Sum(_))
  | CursorT(cursor, List(_)) => cursor == OnDelim(0, Before)
  | ParenthesizedZ(_)
  | SumZ(_)
  | ListZ(_) => false;
let is_before_zoperator: zoperator => bool =
  fun
  | (OnOp(Before), _) => true
  | _ => false;

let is_before_zsumbody_operand =
  fun
  | CursorArgTag(cursor, _, _) => cursor == OnDelim(0, Before)
  | ConstTagZ(ztag)
  | ArgTagZT(ztag, _) => ZTag.is_before(ztag)
  | ArgTagZA(_, _) => false;

let is_before_zsumbody_operator = ((cursor, _): zsumbody_operator): bool =>
  switch (cursor) {
  | OnOp(Before) => true
  | _ => false
  };

let is_before_zsumbody = (zsumbody: zsumbody): bool =>
  ZOpSeq.is_before(~is_before_zoperand=is_before_zsumbody_operand, zsumbody);

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
  | CursorT(cursor, Sum(_))
  | CursorT(cursor, List(_)) => cursor == OnDelim(1, After)
  | ParenthesizedZ(_)
  | SumZ(_)
  | ListZ(_) => false;
let is_after_zoperator: zoperator => bool =
  fun
  | (OnOp(After), _) => true
  | _ => false;

let is_after_zsumbody_operand =
  fun
  | CursorArgTag(cursor, _, _) => cursor == OnDelim(2, After)
  | ArgTagZT(_, _) => false
  | ArgTagZA(_, zty) => is_after(zty)
  | ConstTagZ(ztag) => ZTag.is_after(ztag);

let is_after_zsumbody_operator = ((cursor, _): zsumbody_operator): bool =>
  switch (cursor) {
  | OnOp(After) => true
  | _ => false
  };

let is_after_zsumbody = (zsumbody: zsumbody): bool =>
  ZOpSeq.is_after(~is_after_zoperand=is_after_zsumbody_operand, zsumbody);

let rec place_before = (ty: UHTyp.t): t => ty |> place_before_opseq
and place_before_opseq = opseq =>
  ZOpSeq.place_before(~place_before_operand, opseq)
and place_before_operand =
  fun
  | (Hole | Unit | Int | Float | Bool | Parenthesized(_) | Sum(_) | List(_)) as operand =>
    CursorT(OnDelim(0, Before), operand);
let place_before_operator = (op: UHTyp.operator): option(zoperator) =>
  Some((OnOp(Before), op));

let place_before_sumbody_operator =
    (sumbody_op: UHTyp.sumbody_operator): option(zsumbody_operator) =>
  Some((OnOp(Before), sumbody_op));
let place_before_sumbody_operand: UHTyp.sumbody_operand => zsumbody_operand =
  fun
  | ConstTag(tag) => ConstTagZ(ZTag.place_before(tag))
  | ArgTag(tag, ty) => ArgTagZT(ZTag.place_before(tag), ty);
let place_before_sumbody = (sumbody: UHTyp.sumbody): zsumbody =>
  ZOpSeq.place_before(
    ~place_before_operand=place_before_sumbody_operand,
    sumbody,
  );

let rec place_after = (ty: UHTyp.t): t => ty |> place_after_opseq
and place_after_opseq = opseq =>
  ZOpSeq.place_after(~place_after_operand, opseq)
and place_after_operand =
  fun
  | (Hole | Unit | Int | Float | Bool) as operand =>
    CursorT(OnDelim(0, After), operand)
  | (Parenthesized(_) | Sum(_) | List(_)) as operand =>
    CursorT(OnDelim(1, After), operand);
let place_after_operator = (op: UHTyp.operator): option(zoperator) =>
  Some((OnOp(After), op));
let place_after_sumbody_operand =
    (sumbody_operand: UHTyp.sumbody_operand): zsumbody_operand =>
  switch (sumbody_operand) {
  | UHTyp.ConstTag(tag) => ConstTagZ(ZTag.place_after(tag))
  | UHTyp.ArgTag(tag, ty) => CursorArgTag(OnDelim(0, After), tag, ty)
  };
let place_after_sumbody = (sumbody: UHTyp.sumbody): zsumbody =>
  ZOpSeq.place_after(
    ~place_after_operand=place_after_sumbody_operand,
    sumbody,
  );
let place_after_sumbody_operator =
    (op: UHTyp.sumbody_operator): option(zsumbody_operator) =>
  Some((OnOp(After), op));

let place_cursor_operand =
    (cursor: CursorPosition.t, operand: UHTyp.operand): option(zoperand) =>
  is_valid_cursor_operand(cursor, operand)
    ? Some(CursorT(cursor, operand)) : None;
let place_cursor_operator =
    (cursor: CursorPosition.t, operator: UHTyp.operator): option(zoperator) =>
  is_valid_cursor_operator(cursor, operator)
    ? Some((cursor, operator)) : None;

let place_cursor_sumbody_operand =
    (cursor: CursorPosition.t, operand: UHTyp.sumbody_operand)
    : option(zsumbody_operand) =>
  is_valid_cursor_sumbody_operand(cursor, operand)
    ? switch (operand) {
      | ConstTag(tag) => Some(ConstTagZ(ZTag.CursorTag(cursor, tag)))
      | ArgTag(tag, ty) => Some(CursorArgTag(cursor, tag, ty))
      }
    : None;

let place_cursor_sumbody_operator =
    (cursor: CursorPosition.t, sumbody_operator: UHTyp.sumbody_operator)
    : option(zsumbody_operator) =>
  is_valid_cursor_sumbody_operator(cursor, sumbody_operator)
    ? Some((cursor, sumbody_operator)) : None;

let move_cursor_left_zoperator: zoperator => option(zoperator) =
  fun
  | (OnText(_) | OnDelim(_, _), _) => None
  | (OnOp(Before), _) => None
  | (OnOp(After), op) => Some((OnOp(Before), op));

let move_cursor_left_zsumbody_operator:
  zsumbody_operator => option(zsumbody_operator) =
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

  | CursorT(OnDelim(_1, Before), Sum(None) as operand) =>
    place_cursor_operand(OnDelim(0, Before), operand)
  | CursorT(OnDelim(_1, Before), Sum(Some(sumbody))) =>
    Some(SumZ(place_after_sumbody(sumbody)))

  | CursorT(OnDelim(_k, Before), List(ty1)) =>
    // _k == 1
    Some(ListZ(place_after(ty1)))
  | ParenthesizedZ(zty1) =>
    switch (move_cursor_left(zty1)) {
    | Some(zty1) => Some(ParenthesizedZ(zty1))
    | None => Some(CursorT(OnDelim(0, After), Parenthesized(erase(zty1))))
    }
  | SumZ(zsumbody) =>
    switch (move_cursor_left_zsumbody(zsumbody)) {
    | Some(zsumbody) => Some(SumZ(zsumbody))
    | None =>
      let sumbody = erase_zsumbody(zsumbody);
      Some(CursorT(OnDelim(0, After), Sum(Some(sumbody))));
    }
  | ListZ(zty1) =>
    switch (move_cursor_left(zty1)) {
    | Some(zty1) => Some(ListZ(zty1))
    | None => Some(CursorT(OnDelim(0, After), List(erase(zty1))))
    }
and move_cursor_left_zsumbody = zsumbody =>
  ZOpSeq.move_cursor_left(
    ~move_cursor_left_zoperand=move_cursor_left_zsumbody_operand,
    ~move_cursor_left_zoperator=move_cursor_left_zsumbody_operator,
    ~place_after_operand=place_after_sumbody_operand,
    ~place_after_operator=place_after_sumbody_operator,
    ~erase_zoperand=erase_zsumbody_operand,
    ~erase_zoperator=erase_zsumbody_operator,
    zsumbody,
  )
and move_cursor_left_zsumbody_operand =
  fun
  | z when is_before_zsumbody_operand(z) => None
  | ConstTagZ(ztag) => {
      let+ ztag = ZTag.move_cursor_left(ztag);
      ConstTagZ(ztag);
    }
  | CursorArgTag(OnOp(_) | OnText(_), _, _) => None
  | CursorArgTag(OnDelim(k, After), tag, ty) =>
    Some(CursorArgTag(OnDelim(k, Before), tag, ty))
  | CursorArgTag(OnDelim(k, Before), tag, ty) =>
    switch (k) {
    | 1 => Some(ArgTagZT(ZTag.place_after(tag), ty))
    | 2 => Some(ArgTagZA(tag, place_after(ty)))
    | _ => None
    }
  | ArgTagZT(ztag, ty) =>
    switch (ZTag.move_cursor_left(ztag)) {
    | Some(ztag) => Some(ArgTagZT(ztag, ty))
    | None => Some(CursorArgTag(OnDelim(0, Before), ZTag.erase(ztag), ty))
    }
  | ArgTagZA(tag, zty) =>
    switch (move_cursor_left(zty)) {
    | Some(zty) => Some(ArgTagZA(tag, zty))
    | None => Some(CursorArgTag(OnDelim(1, After), tag, erase(zty)))
    };

let move_cursor_right_zoperator: zoperator => option(zoperator) =
  fun
  | (OnText(_) | OnDelim(_, _), _) => None
  | (OnOp(After), _) => None
  | (OnOp(Before), op) => Some((OnOp(After), op));
let move_cursor_right_zsumbody_operator:
  zsumbody_operator => option(zsumbody_operator) =
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

  | CursorT(OnDelim(_0, After), Sum(None) as operand) =>
    place_cursor_operand(OnDelim(1, After), operand)
  | CursorT(OnDelim(_0, After), Sum(Some(sumbody))) =>
    Some(SumZ(place_before_sumbody(sumbody)))

  | CursorT(OnDelim(_k, After), List(ty1)) =>
    // _k == 0
    Some(ListZ(place_before(ty1)))
  | ParenthesizedZ(zty1) =>
    switch (move_cursor_right(zty1)) {
    | Some(zty1) => Some(ParenthesizedZ(zty1))
    | None =>
      Some(CursorT(OnDelim(1, Before), Parenthesized(erase(zty1))))
    }
  | SumZ(zsumbody) =>
    switch (move_cursor_right_zsumbody(zsumbody)) {
    | Some(zsumbody) => Some(SumZ(zsumbody))
    | None =>
      let sumbody = erase_zsumbody(zsumbody);
      Some(CursorT(OnDelim(1, Before), Sum(Some(sumbody))));
    }
  | ListZ(zty1) =>
    switch (move_cursor_right(zty1)) {
    | Some(zty1) => Some(ListZ(zty1))
    | None => Some(CursorT(OnDelim(1, Before), List(erase(zty1))))
    }
and move_cursor_right_zsumbody = zsumbody =>
  ZOpSeq.move_cursor_right(
    ~move_cursor_right_zoperand=move_cursor_right_zsumbody_operand,
    ~move_cursor_right_zoperator=move_cursor_right_zsumbody_operator,
    ~place_before_operand=place_before_sumbody_operand,
    ~place_before_operator=place_before_sumbody_operator,
    ~erase_zoperand=erase_zsumbody_operand,
    ~erase_zoperator=erase_zsumbody_operator,
    zsumbody,
  )
and move_cursor_right_zsumbody_operand =
    // fun
    (zoperand: zsumbody_operand) =>
  switch (zoperand) {
  | z when is_after_zsumbody_operand(z) => None
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
    | None => Some(CursorArgTag(OnDelim(1, Before), ZTag.erase(ztag), ty))
    }
  | ArgTagZA(tag, zty) =>
    switch (move_cursor_right(zty)) {
    | Some(zty) => Some(ArgTagZA(tag, zty))
    | None => Some(CursorArgTag(OnDelim(2, Before), tag, erase(zty)))
    }
  };

let rec is_cursor_in_sum = (ZOpSeq(_, zseq): t): bool =>
  switch (zseq) {
  | ZOperator(_, _) => false
  | ZOperand(zoperand, _) =>
    switch (zoperand) {
    | CursorT(OnDelim(0, After) | OnDelim(1, Before), Sum(None)) => true
    | CursorT(_, _) => false
    | ParenthesizedZ(zty) => is_cursor_in_sum(zty)
    | ListZ(zty) => is_cursor_in_sum(zty)
    | SumZ(_) => true
    }
  };
