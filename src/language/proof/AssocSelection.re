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

let left_reparenthesize_boundary_id = (op, left: Exp.t): Id.t =>
  switch (Exp.term_of(left)) {
  | BinOp(left_op, _, left_right) when left_op == op =>
    switch (Exp.term_of(left_right)) {
    | BinOp(left_right_op, left_right_left, _) when left_right_op == op =>
      Exp.rep_id(left_right_left)
    | _ => Exp.rep_id(left_right)
    }
  | _ => Exp.rep_id(left)
  };

let right_reparenthesize_boundary_id = (op, right: Exp.t): Id.t =>
  switch (Exp.term_of(right)) {
  | BinOp(right_op, _, right_right) when right_op == op =>
    Exp.rep_id(right_right)
  | _ => Exp.rep_id(right)
  };

/* Given a BinOp tile ID and a statics map, returns boundary ids that define
   the "snapped" visual selection. Same-op chains snap one level inward (e.g.
   for `(1+2)+3`, the outer `+` snaps to cover `2+3`). Mixed-precedence
   operands use their visible outer edges, so `4*5*6 + x` starts at `4`, not
   at the inner `*` tile. Returns [id] for non-BinOp expressions. */
let find_assoc_for_id = (id: Id.t, info_map: Statics.Map.t): list(Id.t) => {
  switch (Statics.Map.lookup(id, info_map)) {
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

/* Like [find_assoc_for_id], but returns operand ids suitable for rebuilding a
   same-op chain. Mixed-precedence operands stay whole instead of using visual
   edge ids, so selecting `4 * 5 + 5` passes the `4 * 5` operand id to the
   reparenthesizer rather than the id of `4`. */
let find_reparenthesize_for_id =
    (id: Id.t, info_map: Statics.Map.t): list(Id.t) => {
  switch (Statics.Map.lookup(id, info_map)) {
  | Some(InfoExp(exp)) =>
    switch (exp.user_term.term) {
    | BinOp(op, left, right) => [
        left_reparenthesize_boundary_id(op, left),
        id,
        right_reparenthesize_boundary_id(op, right),
      ]
    | _ => [id]
    }
  | _ => [id]
  };
};

/* Returns true if the id points to a BinOp where the semantic selection differs
   from the raw AST grouping, i.e. reparenthesization would change the tree. */
let needs_reparenthesization = (id: Id.t, info_map: Statics.Map.t): bool =>
  switch (find_reparenthesize_for_id(id, info_map)) {
  | [left_left_id, op_id, _] when op_id == id =>
    switch (Statics.Map.lookup(id, info_map)) {
    | Some(InfoExp({user_term: {term: BinOp(_, left, _), _}, _})) =>
      Exp.rep_id(left) != left_left_id
    | _ => false
    }
  | _ => false
  };
