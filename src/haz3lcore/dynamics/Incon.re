let rec matches = (e: DHExp.t, p: DHPat.t): bool =>
  switch (e, p) {
  | (_, Var(_)) => true
  | (_, Wild) => true
  | (BoolLit(x), BoolLit(y)) => x == y
  | (IntLit(x), IntLit(y)) => x == y
  | (FloatLit(x), FloatLit(y)) => x == y
  | (StringLit(x), StringLit(y)) => x == y
  | (Inj(_, side1, x), Inj(side2, y)) => side1 == side2 && matches(x, y)
  // I don't know how to extend the algorithm for projection into that of list or tuples.
  | (_, _) => false
  };

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
