open OptUtil.Syntax;
[@deriving sexp]
type t = zopseq
and zopseq = ZOpSeq.t(UHPat.operand, UHPat.operator, zoperand, zoperator)
and zoperand =
  | CursorP(CursorPosition.t, UHPat.operand)
  | ParenthesizedZ(t)
  | TypeAnnZP(ErrStatus.t, zoperand, UHTyp.t)
  | TypeAnnZA(ErrStatus.t, UHPat.operand, ZTyp.t)
  | InjZ(ErrStatus.t, InjSide.t, t)
and zoperator = (CursorPosition.t, UHPat.operator);

type operand_surround = Seq.operand_surround(UHPat.operand, UHPat.operator);
type operator_surround = Seq.operator_surround(UHPat.operand, UHPat.operator);
type zseq = ZSeq.t(UHPat.operand, UHPat.operator, zoperand, zoperator);

let rec erase: t => UHPat.t = zp => erase_zopseq(zp)
and erase_zopseq: zopseq => UHPat.opseq =
  zos => ZOpSeq.erase(~erase_zoperand, ~erase_zoperator, zos)
and erase_zoperand: zoperand => UHPat.operand =
  zop =>
    switch (zop) {
    | CursorP(_, operand) => operand
    | ParenthesizedZ(zp) => UHPat.Parenthesized(erase(zp))
    | TypeAnnZP(err, zoperand, ty) =>
      UHPat.TypeAnn(err, erase_zoperand(zoperand), ty)
    | TypeAnnZA(err, operand, zty) =>
      UHPat.TypeAnn(err, operand, ZTyp.erase(zty))
    | InjZ(err, side, zp) => UHPat.Inj(err, side, erase(zp))
    }
and erase_zoperator = ((_, operator)) => operator;

let valid_cursors_operand: UHPat.operand => list(CursorPosition.t) =
  CursorPosition.(
    fun
    | EmptyHole(_) => delim_cursors(1)
    | Wild(_) => delim_cursors(1)
    | InvalidText(_, t) => text_cursors(String.length(t))
    | Var(_, _, x) => text_cursors(Var.length(x))
    | IntLit(_, n) => text_cursors(String.length(n))
    | FloatLit(_, f) => text_cursors(String.length(f))
    | BoolLit(_, b) => text_cursors(b ? 4 : 5)
    | ListNil(_) => delim_cursors(1)
    | Inj(_, _, _) => delim_cursors(2)
    | Parenthesized(_) => delim_cursors(2)
    | TypeAnn(_) => delim_cursors(1)
  );
let valid_cursors_operator: UHPat.operator => list(CursorPosition.t) =
  fun
  | _ => [OnOp(Before), OnOp(After)];

let is_valid_cursor_operand =
    (cursor: CursorPosition.t, operand: UHPat.operand): bool =>
  valid_cursors_operand(operand) |> List.mem(cursor);
let is_valid_cursor_operator =
    (cursor: CursorPosition.t, operator: UHPat.operator): bool =>
  valid_cursors_operator(operator) |> List.mem(cursor);

let get_err_status = zp => zp |> erase |> UHPat.get_err_status;
let get_err_status_zopseq = zopseq =>
  zopseq |> erase_zopseq |> UHPat.get_err_status_opseq;
let get_err_status_zoperand = zoperand =>
  zoperand |> erase_zoperand |> UHPat.get_err_status_operand;

let rec set_err_status = (err: ErrStatus.t, zp: t): t =>
  zp |> set_err_status_zopseq(err)
and set_err_status_zopseq = (err, zopseq) =>
  ZOpSeq.set_err_status(~set_err_status_zoperand, err, zopseq)
and set_err_status_zoperand = (err, zoperand) =>
  switch (zoperand) {
  | CursorP(cursor, operand) =>
    CursorP(cursor, operand |> UHPat.set_err_status_operand(err))
  | ParenthesizedZ(zp) => ParenthesizedZ(set_err_status(err, zp))
  | InjZ(_, inj_side, zp) => InjZ(err, inj_side, zp)
  | TypeAnnZP(_, zop, ann) => TypeAnnZP(err, zop, ann)
  | TypeAnnZA(_, op, zann) => TypeAnnZA(err, op, zann)
  };

let rec mk_inconsistent = (id_gen: IDGen.t, zp: t): (t, IDGen.t) =>
  zp |> mk_inconsistent_zopseq(id_gen)
and mk_inconsistent_zopseq =
    (id_gen: IDGen.t, zopseq: zopseq): (zopseq, IDGen.t) =>
  ZOpSeq.mk_inconsistent(~mk_inconsistent_zoperand, id_gen, zopseq)
and mk_inconsistent_zoperand = (id_gen, zoperand) =>
  switch (zoperand) {
  | CursorP(cursor, operand) =>
    let (operand, id_gen) = operand |> UHPat.mk_inconsistent_operand(id_gen);
    (CursorP(cursor, operand), id_gen);
  | InjZ(InHole(TypeInconsistent, _), _, _) => (zoperand, id_gen)
  | InjZ(NotInHole | InHole(WrongLength, _), inj_side, zp) =>
    let (u, id_gen) = id_gen |> IDGen.next_hole;
    (InjZ(InHole(TypeInconsistent, u), inj_side, zp), id_gen);
  | ParenthesizedZ(zp) =>
    let (zp, id_gen) = zp |> mk_inconsistent(id_gen);
    (ParenthesizedZ(zp), id_gen);
  | TypeAnnZP(InHole(TypeInconsistent, _), _, _) => (zoperand, id_gen)
  | TypeAnnZP(NotInHole | InHole(WrongLength, _), zop, ann) =>
    let (u, id_gen) = id_gen |> IDGen.next_hole;
    (TypeAnnZP(InHole(TypeInconsistent, u), zop, ann), id_gen);
  | TypeAnnZA(InHole(TypeInconsistent, _), _, _) => (zoperand, id_gen)
  | TypeAnnZA(NotInHole | InHole(WrongLength, _), op, zann) =>
    let (u, id_gen) = id_gen |> IDGen.next_hole;
    (TypeAnnZA(InHole(TypeInconsistent, u), op, zann), id_gen);
  };

let rec erase = (zp: t): UHPat.t => zp |> erase_zopseq
and erase_zopseq =
  fun
  | ZOpSeq(skel, zseq) => OpSeq(skel, zseq |> erase_zseq)
and erase_zseq = zseq => zseq |> ZSeq.erase(~erase_zoperand, ~erase_zoperator)
and erase_zoperand =
  fun
  | CursorP(_, operand) => operand
  | InjZ(err, inj_side, zp) => Inj(err, inj_side, erase(zp))
  | ParenthesizedZ(zp) => Parenthesized(erase(zp))
  | TypeAnnZP(err, zop, ann) => TypeAnn(err, erase_zoperand(zop), ann)
  | TypeAnnZA(err, op, zann) => TypeAnn(err, op, ZTyp.erase(zann))
and erase_zoperator =
  fun
  | (_, op) => op;

let mk_ZOpSeq =
  ZOpSeq.mk(~associate=UHPat.associate, ~erase_zoperand, ~erase_zoperator);

let rec is_before = (zp: t): bool => is_before_zopseq(zp)
and is_before_zopseq = zopseq => ZOpSeq.is_before(~is_before_zoperand, zopseq)
and is_before_zoperand =
  fun
  | CursorP(cursor, EmptyHole(_))
  | CursorP(cursor, Wild(_))
  | CursorP(cursor, ListNil(_)) => cursor == OnDelim(0, Before)
  | CursorP(cursor, InvalidText(_, _))
  | CursorP(cursor, Var(_, _, _))
  | CursorP(cursor, IntLit(_, _))
  | CursorP(cursor, FloatLit(_, _))
  | CursorP(cursor, BoolLit(_, _)) => cursor == OnText(0)
  | CursorP(cursor, Inj(_, _, _))
  | CursorP(cursor, Parenthesized(_)) => cursor == OnDelim(0, Before)
  | CursorP(_, TypeAnn(_)) => false
  | InjZ(_, _, _)
  | ParenthesizedZ(_) => false
  | TypeAnnZP(_, zop, _) => is_before_zoperand(zop)
  | TypeAnnZA(_) => false;
let is_before_zoperator: zoperator => bool =
  fun
  | (OnOp(Before), _) => true
  | _ => false;

let rec is_after = (zp: t): bool => is_after_zopseq(zp)
and is_after_zopseq = zopseq => ZOpSeq.is_after(~is_after_zoperand, zopseq)
and is_after_zoperand =
  fun
  | CursorP(cursor, EmptyHole(_))
  | CursorP(cursor, Wild(_))
  | CursorP(cursor, ListNil(_)) => cursor == OnDelim(0, After)
  | CursorP(cursor, InvalidText(_, t)) =>
    cursor == OnText(String.length(t))
  | CursorP(cursor, Var(_, _, x)) => cursor == OnText(Var.length(x))
  | CursorP(cursor, IntLit(_, n)) => cursor == OnText(String.length(n))
  | CursorP(cursor, FloatLit(_, f)) => cursor == OnText(String.length(f))
  | CursorP(cursor, BoolLit(_, b)) => cursor == OnText(b ? 4 : 5)
  | CursorP(cursor, Inj(_, _, _))
  | CursorP(cursor, Parenthesized(_)) => cursor == OnDelim(1, After)
  | CursorP(_, TypeAnn(_)) => false
  | InjZ(_, _, _)
  | ParenthesizedZ(_) => false
  | TypeAnnZP(_) => false
  | TypeAnnZA(_, _, zann) => ZTyp.is_after(zann);
let is_after_zoperator: zoperator => bool =
  fun
  | (OnOp(After), _) => true
  | _ => false;

let rec place_before = (p: UHPat.t): t => place_before_opseq(p)
and place_before_opseq = opseq =>
  ZOpSeq.place_before(~place_before_operand, opseq)
and place_before_operand = operand =>
  switch (operand) {
  | EmptyHole(_)
  | Wild(_)
  | ListNil(_) => CursorP(OnDelim(0, Before), operand)
  | InvalidText(_, _)
  | Var(_, _, _)
  | IntLit(_, _)
  | FloatLit(_, _)
  | BoolLit(_, _) => CursorP(OnText(0), operand)
  | Inj(_, _, _)
  | Parenthesized(_) => CursorP(OnDelim(0, Before), operand)
  | TypeAnn(err, op, ann) => TypeAnnZP(err, place_before_operand(op), ann)
  };
let place_before_operator = (op: UHPat.operator): option(zoperator) =>
  switch (op) {
  | Space => None
  | _ => Some((OnOp(Before), op))
  };

let rec place_after = (p: UHPat.t): t => place_after_opseq(p)
and place_after_opseq = opseq =>
  ZOpSeq.place_after(~place_after_operand, opseq)
and place_after_operand = operand =>
  switch (operand) {
  | EmptyHole(_)
  | Wild(_)
  | ListNil(_) => CursorP(OnDelim(0, After), operand)
  | InvalidText(_, t) => CursorP(OnText(String.length(t)), operand)
  | Var(_, _, x) => CursorP(OnText(Var.length(x)), operand)
  | IntLit(_, n) => CursorP(OnText(String.length(n)), operand)
  | FloatLit(_, f) => CursorP(OnText(String.length(f)), operand)
  | BoolLit(_, b) => CursorP(OnText(b ? 4 : 5), operand)
  | Inj(_, _, _) => CursorP(OnDelim(1, After), operand)
  | Parenthesized(_) => CursorP(OnDelim(1, After), operand)
  | TypeAnn(err, zp, za) => TypeAnnZA(err, zp, ZTyp.place_after(za))
  };
let place_after_operator = (op: UHPat.operator): option(zoperator) =>
  switch (op) {
  | Space => None
  | _ => Some((OnOp(After), op))
  };

let place_cursor_operand =
    (cursor: CursorPosition.t, operand: UHPat.operand): option(zoperand) =>
  is_valid_cursor_operand(cursor, operand)
    ? Some(CursorP(cursor, operand)) : None;
let place_cursor_operator =
    (cursor: CursorPosition.t, operator: UHPat.operator): option(zoperator) =>
  is_valid_cursor_operator(cursor, operator)
    ? Some((cursor, operator))
    : None /* helper function for constructing a new empty hole */;

let new_EmptyHole = (id_gen: IDGen.t): (zoperand, IDGen.t) => {
  let (hole, id_gen) = UHPat.new_EmptyHole(id_gen);
  (place_before_operand(hole), id_gen);
};

let move_cursor_left_zoperator: zoperator => option(zoperator) =
  fun
  | (OnText(_) | OnDelim(_, _), _) => None
  | (OnOp(Before), _) => None
  | (OnOp(After), op) => Some((OnOp(Before), op));

let rec move_cursor_left = (zp: t): option(t) =>
  zp |> move_cursor_left_zopseq
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
  | CursorP(OnOp(_), _) => None
  | CursorP(OnText(j), operand) => Some(CursorP(OnText(j - 1), operand))
  | CursorP(OnDelim(k, After), operand) =>
    Some(CursorP(OnDelim(k, Before), operand))
  | CursorP(OnDelim(_, Before), EmptyHole(_) | Wild(_) | ListNil(_)) => None
  | CursorP(OnDelim(_k, Before), Parenthesized(p)) =>
    // _k == 1
    Some(ParenthesizedZ(place_after(p)))
  | CursorP(OnDelim(_k, Before), TypeAnn(err, p, a)) =>
    Some(TypeAnnZP(err, place_after_operand(p), a))
  | CursorP(OnDelim(_k, Before), Inj(err, side, p)) =>
    // _k == 1
    Some(InjZ(err, side, place_after(p)))
  | CursorP(
      OnDelim(_, _),
      InvalidText(_, _) | Var(_, _, _) | BoolLit(_, _) | IntLit(_, _) |
      FloatLit(_, _),
    ) =>
    // invalid cursor position
    None
  | ParenthesizedZ(zp) =>
    switch (move_cursor_left(zp)) {
    | Some(zp) => Some(ParenthesizedZ(zp))
    | None => Some(CursorP(OnDelim(0, After), Parenthesized(erase(zp))))
    }
  | InjZ(err, side, zp) =>
    switch (move_cursor_left(zp)) {
    | Some(zp) => Some(InjZ(err, side, zp))
    | None => Some(CursorP(OnDelim(0, After), Inj(err, side, erase(zp))))
    }
  | TypeAnnZP(err, zop, ann) => {
      let+ zop = move_cursor_left_zoperand(zop);
      TypeAnnZP(err, zop, ann);
    }
  | TypeAnnZA(err, zp, za) =>
    switch (ZTyp.move_cursor_left(za)) {
    | Some(za) => Some(TypeAnnZA(err, zp, za))
    | None =>
      Some(CursorP(OnDelim(0, After), TypeAnn(err, zp, ZTyp.erase(za))))
    };

let move_cursor_right_zoperator: zoperator => option(zoperator) =
  fun
  | (OnText(_) | OnDelim(_, _), _) => None
  | (OnOp(After), _) => None
  | (OnOp(Before), op) => Some((OnOp(After), op));

let rec move_cursor_right = (zp: t): option(t) =>
  zp |> move_cursor_right_zopseq
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
  | CursorP(OnOp(_), _) => None
  | CursorP(OnText(j), p) => Some(CursorP(OnText(j + 1), p))
  | CursorP(OnDelim(k, Before), p) => Some(CursorP(OnDelim(k, After), p))
  | CursorP(OnDelim(_, After), EmptyHole(_) | Wild(_) | ListNil(_)) => None
  | CursorP(OnDelim(_k, After), TypeAnn(err, p, a)) =>
    Some(TypeAnnZA(err, p, ZTyp.place_before(a)))
  | CursorP(OnDelim(_k, After), Parenthesized(p)) =>
    // _k == 0
    Some(ParenthesizedZ(place_before(p)))
  | CursorP(OnDelim(_k, After), Inj(err, side, p)) =>
    // _k == 0
    Some(InjZ(err, side, place_before(p)))
  | CursorP(
      OnDelim(_, _),
      InvalidText(_, _) | Var(_, _, _) | BoolLit(_, _) | IntLit(_, _) |
      FloatLit(_, _),
    ) =>
    // invalid cursor position
    None
  | ParenthesizedZ(zp) =>
    switch (move_cursor_right(zp)) {
    | Some(zp) => Some(ParenthesizedZ(zp))
    | None => Some(CursorP(OnDelim(1, Before), Parenthesized(erase(zp))))
    }
  | InjZ(err, side, zp) =>
    switch (move_cursor_right(zp)) {
    | Some(zp) => Some(InjZ(err, side, zp))
    | None => Some(CursorP(OnDelim(1, Before), Inj(err, side, erase(zp))))
    }
  | TypeAnnZP(err, zop, ann) =>
    switch (move_cursor_right_zoperand(zop)) {
    | Some(zop) => Some(TypeAnnZP(err, zop, ann))
    | None =>
      Some(
        CursorP(
          OnDelim(0, Before),
          TypeAnn(err, erase_zoperand(zop), ann),
        ),
      )
    }
  | TypeAnnZA(err, op, zann) => {
      let+ zann = ZTyp.move_cursor_right(zann);
      TypeAnnZA(err, op, zann);
    };
