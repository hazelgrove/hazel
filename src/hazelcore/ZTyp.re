[@deriving sexp]
type t = zopseq
and zopseq = ZOpSeq.t(UHTyp.operand, UHTyp.operator, zoperand, zoperator)
and zoperand =
  | CursorT(CursorPosition.t, UHTyp.operand)
  | ParenthesizedZ(t)
  | ListZ(t)
and zoperator = (CursorPosition.t, UHTyp.operator);

type operand_surround = Seq.operand_surround(UHTyp.operand, UHTyp.operator);
type operator_surround = Seq.operator_surround(UHTyp.operand, UHTyp.operator);
type zseq = ZSeq.t(UHTyp.operand, UHTyp.operator, zoperand, zoperator);

let valid_cursors_operand: UHTyp.operand => list(CursorPosition.t) =
  fun
  | Hole
  | Unit
  | Int
  | Float
  | Bool => CursorPosition.delim_cursors(1)
  | Parenthesized(_)
  | List(_) => CursorPosition.delim_cursors(2)
  | Label(_, l) => CursorPosition.text_cursors(Label.length(l));

let valid_cursors_operator: UHTyp.operator => list(CursorPosition.t) =
  fun
  | _ => [OnOp(Before), OnOp(After)];

let is_valid_cursor_operand =
    (cursor: CursorPosition.t, operand: UHTyp.operand): bool =>
  valid_cursors_operand(operand) |> List.mem(cursor);
let is_valid_cursor_operator =
    (cursor: CursorPosition.t, operator: UHTyp.operator): bool =>
  valid_cursors_operator(operator) |> List.mem(cursor);

let erase_zoperator =
  fun
  | (_, operator) => operator;

let rec erase = (zty: t): UHTyp.t => zty |> erase_zopseq
and erase_zopseq = zopseq =>
  ZOpSeq.erase(~erase_zoperand, ~erase_zoperator, zopseq)
and erase_zoperand =
  fun
  | CursorT(_, operand) => operand
  | ParenthesizedZ(zty) => Parenthesized(erase(zty))
  | ListZ(zty) => List(erase(zty));

let mk_ZOpSeq: zseq => zopseq =
  ZOpSeq.mk(~associate=UHTyp.associate, ~erase_zoperand, ~erase_zoperator);

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
  | CursorT(cursor, List(_)) => cursor == OnDelim(0, Before)
  | CursorT(cursor, Label(_)) => cursor == OnText(0)
  | ParenthesizedZ(_) => false
  | ListZ(_) => false;
let is_before_zoperator: zoperator => bool =
  fun
  | (OnOp(Before), _) => true
  | _ => false;

let rec is_after = (zty: t): bool => zty |> is_after_zopseq
and is_after_zopseq = zopseq => ZOpSeq.is_after(~is_after_zoperand, zopseq)
and is_after_zoperand =
  fun
  | CursorT(cursor, Hole)
  | CursorT(cursor, Unit)
  | CursorT(cursor, Int)
  | CursorT(cursor, Float)
  | CursorT(cursor, Bool) => cursor == OnDelim(0, After)
  | CursorT(cursor, Label(_, l)) => cursor == OnText(Label.length(l))
  | CursorT(cursor, Parenthesized(_))
  | CursorT(cursor, List(_)) => cursor == OnDelim(1, After)
  | ParenthesizedZ(_) => false
  | ListZ(_) => false;
let is_after_zoperator: zoperator => bool =
  fun
  | (OnOp(After), _) => true
  | _ => false;
let is_on_label_zoperand =
  fun
  | CursorT(_, Label(_)) => true
  | _ => false;

let rec place_before = (ty: UHTyp.t): t => ty |> place_before_opseq
and place_before_opseq = opseq =>
  ZOpSeq.place_before(~place_before_operand, opseq)
and place_before_operand =
  fun
  | (Hole | Unit | Int | Float | Bool | Parenthesized(_) | List(_)) as operand =>
    CursorT(OnDelim(0, Before), operand)
  | Label(_) as operand => CursorT(OnText(0), operand);
let place_before_operator = (op: UHTyp.operator): option(zoperator) =>
  switch (op) {
  | Space => None
  | _ => Some((OnOp(Before), op))
  };

let rec place_after = (ty: UHTyp.t): t => ty |> place_after_opseq
and place_after_opseq = opseq =>
  ZOpSeq.place_after(~place_after_operand, opseq)
and place_after_operand =
  fun
  | (Hole | Unit | Int | Float | Bool) as operand =>
    CursorT(OnDelim(0, After), operand)
  | Label(_, label) as operand =>
    CursorT(OnText(Label.length(label)), operand)
  | (Parenthesized(_) | List(_)) as operand =>
    CursorT(OnDelim(1, After), operand);
let place_after_operator = (op: UHTyp.operator): option(zoperator) =>
  switch (op) {
  | Space => None
  | _ => Some((OnOp(After), op))
  };

let place_cursor_operand =
    (cursor: CursorPosition.t, operand: UHTyp.operand): option(zoperand) =>
  is_valid_cursor_operand(cursor, operand)
    ? Some(CursorT(cursor, operand)) : None;
let place_cursor_operator =
    (cursor: CursorPosition.t, operator: UHTyp.operator): option(zoperator) =>
  is_valid_cursor_operator(cursor, operator)
    ? Some((cursor, operator)) : None;

let move_cursor_left_zoperator: zoperator => option(zoperator) =
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
  | CursorT(OnText(j), Label(lerr, l)) =>
    Some(CursorT(OnText(j - 1), Label(lerr, l)))
  | CursorT(OnOp(_) | OnText(_), _) => None
  | CursorT(OnDelim(k, After), ty) =>
    Some(CursorT(OnDelim(k, Before), ty))
  | CursorT(OnDelim(_, Before), Hole | Unit | Int | Float | Bool | Label(_)) =>
    None
  | CursorT(OnDelim(_k, Before), Parenthesized(ty1)) =>
    // _k == 1
    Some(ParenthesizedZ(place_after(ty1)))
  | CursorT(OnDelim(_k, Before), List(ty1)) =>
    // _k == 1
    Some(ListZ(place_after(ty1)))
  | ParenthesizedZ(zty1) =>
    switch (move_cursor_left(zty1)) {
    | Some(zty1) => Some(ParenthesizedZ(zty1))
    | None => Some(CursorT(OnDelim(0, After), Parenthesized(erase(zty1))))
    }
  | ListZ(zty1) =>
    switch (move_cursor_left(zty1)) {
    | Some(zty1) => Some(ListZ(zty1))
    | None => Some(CursorT(OnDelim(0, After), List(erase(zty1))))
    };

let move_cursor_right_zoperator: zoperator => option(zoperator) =
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
  //   Some(CursorT(OnDelim(1, Before), Hole))
  | z when is_after_zoperand(z) => None
  | CursorT(OnText(j), Label(lerr, label)) =>
    Some(CursorT(OnText(j + 1), Label(lerr, label)))
  | CursorT(OnOp(_) | OnText(_), _) => None
  | CursorT(OnDelim(k, Before), ty) =>
    Some(CursorT(OnDelim(k, After), ty))
  | CursorT(OnDelim(_, After), Hole | Unit | Int | Float | Bool | Label(_)) =>
    None
  | CursorT(OnDelim(_k, After), Parenthesized(ty1)) =>
    // _k == 0
    Some(ParenthesizedZ(place_before(ty1)))
  | CursorT(OnDelim(_k, After), List(ty1)) =>
    // _k == 0
    Some(ListZ(place_before(ty1)))
  | ParenthesizedZ(zty1) =>
    switch (move_cursor_right(zty1)) {
    | Some(zty1) => Some(ParenthesizedZ(zty1))
    | None =>
      Some(CursorT(OnDelim(1, Before), Parenthesized(erase(zty1))))
    }
  | ListZ(zty1) =>
    switch (move_cursor_right(zty1)) {
    | Some(zty1) => Some(ListZ(zty1))
    | None => Some(CursorT(OnDelim(1, Before), List(erase(zty1))))
    };
