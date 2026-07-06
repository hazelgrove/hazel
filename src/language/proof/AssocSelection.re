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

let rec left_edge_pat = (pat: Pat.t): Id.t =>
  switch (Pat.term_of(pat)) {
  | Ap(left, _)
  | TupLabel(left, _)
  | Cons(left, _)
  | Asc(left, _) => left_edge_pat(left)
  | _ => Pat.rep_id(pat)
  };

let rec right_edge_pat = (pat: Pat.t): Id.t =>
  switch (Pat.term_of(pat)) {
  | Ap(_, right)
  | TupLabel(_, right)
  | Cons(_, right) => right_edge_pat(right)
  | Asc(pat, _) => right_edge_pat(pat)
  | _ => Pat.rep_id(pat)
  };

let rec right_edge_typ = (typ: Typ.t): Id.t =>
  switch (Typ.term_of(typ)) {
  | Arrow(_, right)
  | TupLabel(_, right)
  | ProdProjection(_, right)
  | ProdExtension(_, right) => right_edge_typ(right)
  | Rec(_, typ)
  | Poly(_, typ)
  | Parens(typ)
  | List(typ) => right_edge_typ(typ)
  | Sum(variants) =>
    switch (List.rev(variants)) {
    | [ConstructorMap.Variant(_, ann, Some(typ)), ..._] =>
      ignore(ann);
      right_edge_typ(typ);
    | [ConstructorMap.Variant(_, ann, None), ..._] =>
      switch (ann.ids |> List.rev) {
      | [id, ..._] => id
      | [] => Typ.rep_id(typ)
      }
    | [ConstructorMap.BadEntry(typ), ..._] => right_edge_typ(typ)
    | [] => Typ.rep_id(typ)
    }
  | Prod(typs) =>
    switch (List.rev(typs)) {
    | [last, ..._] => right_edge_typ(last)
    | [] => Typ.rep_id(typ)
    }
  | _ => Typ.rep_id(typ)
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

let rec right_cons_tail_edge_id = (exp: Exp.t): Id.t =>
  switch (Exp.term_of(exp)) {
  | Cons(_, right) => right_cons_tail_edge_id(right)
  | Asc(_, typ) => right_edge_typ(typ)
  | _ => right_edge_id(exp)
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
    | BinOp(op, left, right) => [
        left_boundary_id(op, left),
        id,
        right_boundary_id(op, right),
      ]
    | Cons(left, right) => [
        left_edge_id(left),
        id,
        right_cons_tail_edge_id(right),
      ]
    | UnOp(_, exp) => [id, right_edge_id(exp)]
    | If(cond, _, alt) => [left_edge_id(cond), id, right_edge_id(alt)]
    | _ => []
    }
  | Some(InfoPat({user_term, _})) =>
    switch (user_term.term) {
    | Cons(left, right) => [left_edge_pat(left), id, right_edge_pat(right)]
    | _ => []
    }
  | Some(InfoTyp({user_term, _})) =>
    switch (user_term.term) {
    | Arrow(left, right) => [Typ.rep_id(left), id, right_edge_typ(right)]
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
  | Some(InfoPat({user_term, _})) =>
    find_assoc_for_id(id, info_map) == []
      ? None : Some(Pat.rep_id(user_term))
  | Some(InfoTyp({user_term, _})) =>
    find_assoc_for_id(id, info_map) == []
      ? None : Some(Typ.rep_id(user_term))
  | _ => None
  };

let find_assoc_for_ids =
    (ids: list(Id.t), info_map: Statics.Map.t): list(Id.t) => {
  let snapped_from_ids =
    ids |> List.concat_map(id => find_assoc_for_id(id, info_map));
  let snapped_from_ancestors =
    ids_with_ancestors(~info_map, ids)
    |> List.concat_map(id =>
         switch (Statics.Map.lookup(id, info_map)) {
         | Some(InfoExp({user_term, _})) =>
           switch (user_term.term) {
           | UnOp(_, exp) =>
             List.mem(Exp.rep_id(exp), ids)
               ? [Exp.rep_id(user_term), right_edge_id(exp)] : []
           | _ => []
           }
         | _ => []
         }
       );
  snapped_from_ids @ snapped_from_ancestors |> ListUtil.dedup;
};

let find_assoc_root_for_ids =
    (ids: list(Id.t), info_map: Statics.Map.t): option(Id.t) =>
  switch (ids |> List.find_map(id => find_assoc_root_for_id(id, info_map))) {
  | Some(_) as root => root
  | None =>
    ids_with_ancestors(~info_map, ids)
    |> List.find_map(id => find_assoc_root_for_id(id, info_map))
  };
