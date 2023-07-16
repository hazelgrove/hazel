let rec matches = (e: DHExp.t, p: DHPat.t): bool =>
  switch (e, p) {
  | (_, Var(_)) => true
  | (_, Wild) => true
  | (BoolLit(x), BoolLit(y)) => x == y
  | (IntLit(x), IntLit(y)) => x == y
  | (FloatLit(x), FloatLit(y)) => x == y
  | (StringLit(x), StringLit(y)) => x == y
  | (Inj(_, side1, x), Inj(side2, y)) => side1 == side2 && matches(x, y)
  // I don't know if my algorithm is correct or not.
  | (ListLit(_, _, _, _, []), ListLit(_, []))
  | (Tuple([]), Tuple([])) => true
  | (ListLit(_, _, _, _, [hd1, ...tl1]), ListLit(_, [hd2, ...tl2]))
  | (Tuple([hd1, ...tl1]), Tuple([hd2, ...tl2])) =>
    matches(hd1, hd2) && matches(Tuple(tl1), Tuple(tl2))
  | (_, _) => false
  };

let rec does_not_match = (e: DHExp.t, p: DHPat.t): bool =>
  switch (e, p) {
  | (BoolLit(x), BoolLit(y)) => x != y
  | (IntLit(x), IntLit(y)) => x != y
  | (FloatLit(x), FloatLit(y)) => x != y
  | (StringLit(x), StringLit(y)) => x != y
  | (ListLit(_, _, _, _, []), ListLit(_, []))
  | (Tuple([]), Tuple([])) => false
  | (ListLit(_, _, _, _, [hd1, ...tl1]), ListLit(_, [hd2, ...tl2]))
  | (Tuple([hd1, ...tl1]), Tuple([hd2, ...tl2])) =>
    does_not_match(hd1, hd2) || does_not_match(Tuple(tl1), Tuple(tl2))
  | (Inj(_, side1, x), Inj(side2, y)) =>
    side1 != side2 || does_not_match(x, y)
  | (_, _) => false
  };

// It is difficult to write indet_match, so I used the theorem to simplify it. Probably it will execute slower.
let indet_match = (e: DHExp.t, p: DHPat.t): bool =>
  !(matches(e, p) || does_not_match(e, p));

let rec is_val = (e: DHExp.t): bool =>
  switch (e) {
  | BoolLit(_)
  | IntLit(_)
  | StringLit(_)
  | Fun(_, _, _, _)
  | ListLit(_, _, _, _, [])
  | Tuple([]) => true
  | ListLit(_, _, _, _, inner)
  | Tuple(inner) =>
    List.fold_left((last_val, e) => last_val && is_val(e), true, inner)
  | BinBoolOp(_, e1, e2)
  | BinIntOp(_, e1, e2)
  | BinFloatOp(_, e1, e2)
  | BinStringOp(_, e1, e2) => is_val(e1) ? is_val(e2) : false
  | Inj(_, _, e) => is_val(e)
  | _ => false
  };

let rec is_indet = (e: DHExp.t): bool =>
  switch (e) {
  | EmptyHole(_, _) => true
  | NonEmptyHole(_, _, _, e) => is_final(e)
  | _ => false
  }
and is_final = (e: DHExp.t): bool => is_val(e) && is_indet(e);
