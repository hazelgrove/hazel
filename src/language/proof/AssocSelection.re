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

/* A virtual slice is sound only when regrouping preserves the operator's
 * meaning. Keep this semantic classification separate from parser
 * associativity: division and subtraction parse left-associatively, but are
 * not associative operations. */
let supports_virtual_slice: Operators.op_bin => bool =
  fun
  | Int(Plus | Times)
  | SInt(Plus | Times)
  | Nat(Plus | Times) => true
  | Float(_)
  | Bool(_)
  | String(_)
  | Poly(_)
  | Int(_)
  | SInt(_)
  | Nat(_) => false;

let is_additive_pair = (op: Operators.op_bin, left_op: Operators.op_bin): bool =>
  switch (op, left_op) {
  | (Int(Minus), Int(Plus))
  | (SInt(Minus), SInt(Plus)) => true
  | _ => false
  };

let is_additive_suffix = (op: Operators.op_bin, left: Exp.t): bool =>
  switch (Exp.term_of(left)) {
  | BinOp(left_op, _, _) => is_additive_pair(op, left_op)
  | _ => false
  };

let left_boundary_id = (op, left: Exp.t): Id.t =>
  switch (Exp.term_of(left)) {
  | BinOp(left_op, _, left_right)
      when left_op == op || is_additive_pair(op, left_op) =>
    left_edge_id(left_right)
  | _ => left_edge_id(left)
  };

let right_boundary_id = (op, right: Exp.t): Id.t =>
  switch (Exp.term_of(right)) {
  | BinOp(right_op, right_left, _) when right_op == op =>
    right_edge_id(right_left)
  | _ => right_edge_id(right)
  };

let ids_with_ancestors =
    (~info_map: Statics.Map.t, ids: list(Id.t)): list(Id.t) =>
  ids
  |> List.concat_map(id =>
       switch (Statics.Map.lookup(id, info_map)) {
       | Some(info) => [id] @ Info.ancestors_of(info)
       | None => [id]
       }
     )
  |> ListUtil.dedup;

/* Return only branch-local effective-selection snaps. Standard term
 * selection stays in Select.re, which matches dev. */
let find_assoc_for_id = (id: Id.t, info_map: Statics.Map.t): list(Id.t) =>
  switch (Statics.Map.lookup(id, info_map)) {
  | Some(InfoExp({user_term, _})) =>
    switch (user_term.term) {
    | BinOp(op, left, right)
        when supports_virtual_slice(op) || is_additive_suffix(op, left) => [
        left_boundary_id(op, left),
        id,
        right_boundary_id(op, right),
      ]
    | _ => []
    }
  | _ => []
  };

let find_assoc_root_for_id =
    (id: Id.t, info_map: Statics.Map.t): option(Id.t) =>
  switch (Statics.Map.lookup(id, info_map)) {
  | Some(InfoExp({user_term, _})) =>
    find_assoc_for_id(id, info_map) == []
      ? None : Some(Exp.rep_id(user_term))
  | _ => None
  };

let find_assoc_for_ids =
    (ids: list(Id.t), info_map: Statics.Map.t): list(Id.t) =>
  ids
  |> List.concat_map(id => find_assoc_for_id(id, info_map))
  |> ListUtil.dedup;

let find_assoc_roots_for_ids =
    (ids: list(Id.t), info_map: Statics.Map.t): list(Id.t) =>
  (ids |> List.filter_map(id => find_assoc_root_for_id(id, info_map)))
  @ (
    ids_with_ancestors(~info_map, ids)
    |> List.filter_map(id => find_assoc_root_for_id(id, info_map))
  )
  |> ListUtil.dedup;

let find_assoc_root_for_ids =
    (ids: list(Id.t), info_map: Statics.Map.t): option(Id.t) =>
  find_assoc_roots_for_ids(ids, info_map) |> ListUtil.hd_opt;
