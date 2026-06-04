open Util;

let rec left_edge_id = (exp: Exp.t): Id.t =>
  switch (Exp.term_of(exp)) {
  | BinOp(_, left, _)
  | Ap(_, left, _)
  | Dot(left, _)
  | TupLabel(left, _)
  | Seq(left, _)
  | TupleExtension(left, _)
  | Asc(left, _) => left_edge_id(left)
  | _ => Exp.rep_id(exp)
  };

let rec right_edge_id = (exp: Exp.t): Id.t =>
  switch (Exp.term_of(exp)) {
  | BinOp(_, _, right)
  | Ap(_, _, right)
  | Dot(_, right)
  | TupLabel(_, right)
  | Seq(_, right)
  | HintedTest(_, right)
  | TupleExtension(_, right) => right_edge_id(right)
  | Asc(exp, _) => right_edge_id(exp)
  | _ => Exp.rep_id(exp)
  };

let left_boundary_id = (op, left: Exp.t): Id.t =>
  switch (Exp.term_of(left)) {
  | BinOp(left_op, _, left_right) when left_op == op =>
    left_edge_id(left_right)
  | _ => left_edge_id(left)
  };

let right_boundary_id = (op, right: Exp.t): Id.t =>
  switch (Exp.term_of(right)) {
  | BinOp(right_op, right_left, _) when right_op == op =>
    right_edge_id(right_left)
  | _ => right_edge_id(right)
  };

/* Given a BinOp tile ID and a statics map, returns boundary ids that define
   the "snapped" visual selection. Same-op chains snap one level inward (e.g.
   for `(1+2)+3`, the outer `+` snaps to cover `2+3`). Mixed-precedence
   operands use their visible outer edges, so `4*5*6 + x` starts at `4`, not
   at the inner `*` tile. Returns [id] for non-BinOp expressions. */
let find_assoc_for_id = (id: Id.t, info_map: Statics.Map.t): list(Id.t) => {
  let statics_opt = Statics.Map.lookup(id, info_map);
  switch (statics_opt) {
  | Some(InfoExp(exp)) =>
    switch (exp.user_term.term) {
    | BinOp(op, left, right) => [
        left_boundary_id(op, left),
        id,
        right_boundary_id(op, right),
      ]
    | _ => [id]
    }
  | _ => [id]
  };
};
