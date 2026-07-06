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

let rec left_edge_typ = (typ: Typ.t): Id.t =>
  switch (Typ.term_of(typ)) {
  | Arrow(left, _)
  | TupLabel(left, _)
  | ProdProjection(left, _)
  | ProdExtension(left, _) => left_edge_typ(left)
  | Prod([first, ..._]) => left_edge_typ(first)
  | _ => Typ.rep_id(typ)
  };

let rec right_edge_typ = (typ: Typ.t): Id.t =>
  switch (Typ.term_of(typ)) {
  | Arrow(_, right)
  | TupLabel(_, right)
  | ProdProjection(_, right)
  | ProdExtension(_, right) => right_edge_typ(right)
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

let comma_spanned_exp_ids =
    (~id: Id.t, ~delimiter_ids: list(Id.t), exps: list(Exp.t))
    : option(list(Id.t)) =>
  if (!List.mem(id, delimiter_ids)) {
    None;
  } else {
    switch (exps, List.rev(exps)) {
    | ([first, ..._], [last, ..._]) =>
      Some([left_edge_id(first)] @ delimiter_ids @ [right_edge_id(last)])
    | _ => None
    };
  };

let comma_spanned_pat_ids =
    (~id: Id.t, ~delimiter_ids: list(Id.t), pats: list(Pat.t))
    : option(list(Id.t)) =>
  if (!List.mem(id, delimiter_ids)) {
    None;
  } else {
    switch (pats, List.rev(pats)) {
    | ([first, ..._], [last, ..._]) =>
      Some(
        [left_edge_pat(first)] @ delimiter_ids @ [right_edge_pat(last)],
      )
    | _ => None
    };
  };

let comma_spanned_typ_ids =
    (~id: Id.t, ~delimiter_ids: list(Id.t), typs: list(Typ.t))
    : option(list(Id.t)) =>
  if (!List.mem(id, delimiter_ids)) {
    None;
  } else {
    switch (typs, List.rev(typs)) {
    | ([first, ..._], [last, ..._]) =>
      Some(
        [left_edge_typ(first)] @ delimiter_ids @ [right_edge_typ(last)],
      )
    | _ => None
    };
  };

let comma_ids_for_enclosed = (ids: list(Id.t)): list(Id.t) =>
  switch (ids) {
  | [_outer_delimiter, ...comma_ids] => comma_ids
  | [] => []
  };

let comma_ids_for_items = (~item_count: int, ids: list(Id.t)): list(Id.t) =>
  ListUtil.take(max(0, item_count - 1), ids);

let comma_ids_for_enclosed_items =
    (~item_count: int, ids: list(Id.t)): list(Id.t) =>
  ids |> comma_ids_for_enclosed |> comma_ids_for_items(~item_count);

let intersects = (left: list(Id.t), right: list(Id.t)): bool =>
  left |> List.exists(id => List.mem(id, right));

let selected_item_count =
    (~selected_ids: list(Id.t), item_ids: list(list(Id.t))): int =>
  item_ids |> List.filter(ids => intersects(selected_ids, ids)) |> List.length;

let selected_crosses_comma_separated =
    (
      ~selected_ids: list(Id.t),
      ~delimiter_ids: list(Id.t),
      item_ids: list(list(Id.t)),
    )
    : bool =>
  intersects(selected_ids, delimiter_ids)
  || selected_item_count(~selected_ids, item_ids) >= 2;

let comma_spanned_exp_ids_for_selection =
    (
      ~selected_ids: list(Id.t),
      ~delimiter_ids: list(Id.t),
      exps: list(Exp.t),
    )
    : option(list(Id.t)) =>
  if (!
        selected_crosses_comma_separated(
          ~selected_ids,
          ~delimiter_ids,
          exps |> List.map(IdTagged.ids),
        )) {
    None;
  } else {
    switch (exps, List.rev(exps)) {
    | ([first, ..._], [last, ..._]) =>
      Some([left_edge_id(first)] @ delimiter_ids @ [right_edge_id(last)])
    | _ => None
    };
  };

let comma_spanned_pat_ids_for_selection =
    (
      ~selected_ids: list(Id.t),
      ~delimiter_ids: list(Id.t),
      pats: list(Pat.t),
    )
    : option(list(Id.t)) =>
  if (!
        selected_crosses_comma_separated(
          ~selected_ids,
          ~delimiter_ids,
          pats |> List.map(IdTagged.ids),
        )) {
    None;
  } else {
    switch (pats, List.rev(pats)) {
    | ([first, ..._], [last, ..._]) =>
      Some(
        [left_edge_pat(first)] @ delimiter_ids @ [right_edge_pat(last)],
      )
    | _ => None
    };
  };

let comma_spanned_typ_ids_for_selection =
    (
      ~selected_ids: list(Id.t),
      ~delimiter_ids: list(Id.t),
      typs: list(Typ.t),
    )
    : option(list(Id.t)) =>
  if (!
        selected_crosses_comma_separated(
          ~selected_ids,
          ~delimiter_ids,
          typs |> List.map(IdTagged.ids),
        )) {
    None;
  } else {
    switch (typs, List.rev(typs)) {
    | ([first, ..._], [last, ..._]) =>
      Some(
        [left_edge_typ(first)] @ delimiter_ids @ [right_edge_typ(last)],
      )
    | _ => None
    };
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

/* Given a BinOp tile ID and a statics map, returns boundary ids that define
   the "snapped" visual selection. Same-op chains snap one level inward (e.g.
   for `(1+2)+3`, the outer `+` snaps to cover `2+3`). Mixed-precedence
   operands use their visible outer edges, so `4*5*6 + x` starts at `4`, not
   at the inner `*` tile. Returns [id] for non-BinOp expressions. */
let find_assoc_for_id = (id: Id.t, info_map: Statics.Map.t): list(Id.t) => {
  let statics_opt = Statics.Map.lookup(id, info_map);
  switch (statics_opt) {
  | Some(InfoExp({user_term, _} as exp)) =>
    let annotation_ids = IdTagged.ids(user_term);
    switch (exp.user_term.term) {
    | BinOp(op, left, right) => [
        left_boundary_id(op, left),
        id,
        right_boundary_id(op, right),
      ]
    | If(cond, _, alt) => [left_edge_id(cond), id, right_edge_id(alt)]
    | Tuple(exps) =>
      switch (
        comma_spanned_exp_ids(
          ~id,
          ~delimiter_ids=
            comma_ids_for_items(
              ~item_count=List.length(exps),
              annotation_ids,
            ),
          exps,
        )
      ) {
      | Some(ids) => ids
      | None => [id]
      }
    | ListLit(exps) =>
      switch (
        comma_spanned_exp_ids(
          ~id,
          ~delimiter_ids=
            comma_ids_for_enclosed_items(
              ~item_count=List.length(exps),
              annotation_ids,
            ),
          exps,
        )
      ) {
      | Some(ids) => ids
      | None => [id]
      }
    | Cons(left, right) => [left_edge_id(left), id, right_edge_id(right)]
    | Seq(left, right) => [left_edge_id(left), id, right_edge_id(right)]
    | ListConcat(left, right)
    | TupleExtension(left, right)
    | Dot(left, right)
    | TupLabel(left, right)
    | Ap(_, left, right) => [left_edge_id(left), id, right_edge_id(right)]
    | Asc(left, right) => [left_edge_id(left), id, right_edge_typ(right)]
    | TypAp(left, right) => [left_edge_id(left), id, right_edge_typ(right)]
    | _ => [id]
    };
  | Some(InfoPat({user_term, _} as pat)) =>
    let annotation_ids = IdTagged.ids(user_term);
    switch (pat.user_term.term) {
    | Tuple(pats) =>
      switch (
        comma_spanned_pat_ids(
          ~id,
          ~delimiter_ids=
            comma_ids_for_items(
              ~item_count=List.length(pats),
              annotation_ids,
            ),
          pats,
        )
      ) {
      | Some(ids) => ids
      | None => [id]
      }
    | ListLit(pats) =>
      switch (
        comma_spanned_pat_ids(
          ~id,
          ~delimiter_ids=
            comma_ids_for_enclosed_items(
              ~item_count=List.length(pats),
              annotation_ids,
            ),
          pats,
        )
      ) {
      | Some(ids) => ids
      | None => [id]
      }
    | Cons(left, right) => [left_edge_pat(left), id, right_edge_pat(right)]
    | TupLabel(left, right)
    | Ap(left, right) => [left_edge_pat(left), id, right_edge_pat(right)]
    | Asc(left, right) => [left_edge_pat(left), id, right_edge_typ(right)]
    | _ => [id]
    };
  | Some(InfoTyp({user_term, _} as typ)) =>
    let annotation_ids = IdTagged.ids(user_term);
    switch (typ.user_term.term) {
    | Prod(typs) =>
      switch (
        comma_spanned_typ_ids(
          ~id,
          ~delimiter_ids=
            comma_ids_for_items(
              ~item_count=List.length(typs),
              annotation_ids,
            ),
          typs,
        )
      ) {
      | Some(ids) => ids
      | None => [id]
      }
    | Arrow(left, right) => [
        left_edge_typ(left),
        id,
        right_edge_typ(right),
      ]
    | ProdExtension(left, right)
    | ProdProjection(left, right)
    | TupLabel(left, right) => [
        left_edge_typ(left),
        id,
        right_edge_typ(right),
      ]
    | _ => [id]
    };
  | _ => [id]
  };
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
           let annotation_ids = IdTagged.ids(user_term);
           switch (user_term.term) {
           | Tuple(exps) =>
             comma_spanned_exp_ids_for_selection(
               ~selected_ids=ids,
               ~delimiter_ids=
                 comma_ids_for_items(
                   ~item_count=List.length(exps),
                   annotation_ids,
                 ),
               exps,
             )
             |> Option.value(~default=[])
           | ListLit(exps) =>
             comma_spanned_exp_ids_for_selection(
               ~selected_ids=ids,
               ~delimiter_ids=
                 comma_ids_for_enclosed_items(
                   ~item_count=List.length(exps),
                   annotation_ids,
                 ),
               exps,
             )
             |> Option.value(~default=[])
           | _ => []
           };
         | Some(InfoPat({user_term, _})) =>
           let annotation_ids = IdTagged.ids(user_term);
           switch (user_term.term) {
           | Tuple(pats) =>
             comma_spanned_pat_ids_for_selection(
               ~selected_ids=ids,
               ~delimiter_ids=
                 comma_ids_for_items(
                   ~item_count=List.length(pats),
                   annotation_ids,
                 ),
               pats,
             )
             |> Option.value(~default=[])
           | ListLit(pats) =>
             comma_spanned_pat_ids_for_selection(
               ~selected_ids=ids,
               ~delimiter_ids=
                 comma_ids_for_enclosed_items(
                   ~item_count=List.length(pats),
                   annotation_ids,
                 ),
               pats,
             )
             |> Option.value(~default=[])
           | _ => []
           };
         | Some(InfoTyp({user_term, _})) =>
           let annotation_ids = IdTagged.ids(user_term);
           switch (user_term.term) {
           | Prod(typs) =>
             comma_spanned_typ_ids_for_selection(
               ~selected_ids=ids,
               ~delimiter_ids=
                 comma_ids_for_items(
                   ~item_count=List.length(typs),
                   annotation_ids,
                 ),
               typs,
             )
             |> Option.value(~default=[])
           | _ => []
           };
         | _ => []
         }
       );
  snapped_from_ids @ snapped_from_ancestors |> ListUtil.dedup;
};

let left_reparenthesize_boundary_id = (op, left: Exp.t): Id.t =>
  switch (Exp.term_of(left)) {
  | BinOp(left_op, _, left_right) when left_op == op =>
    Exp.rep_id(left_right)
  | _ => Exp.rep_id(left)
  };

let right_reparenthesize_boundary_id = (op, right: Exp.t): Id.t =>
  switch (Exp.term_of(right)) {
  | BinOp(right_op, right_left, _) when right_op == op =>
    Exp.rep_id(right_left)
  | _ => Exp.rep_id(right)
  };

let find_reparenthesize_for_id =
    (id: Id.t, info_map: Statics.Map.t): list(Id.t) => {
  switch (Statics.Map.lookup(id, info_map)) {
  | Some(InfoExp({user_term: {term: BinOp(op, left, right), _}, _})) => [
      left_reparenthesize_boundary_id(op, left),
      id,
      right_reparenthesize_boundary_id(op, right),
    ]
  | _ => [id]
  };
};

/* Returns true if the id points to a BinOp where the visual selection differs
   from the raw AST grouping — i.e., reparenthesization would change the tree. */
let needs_reparenthesization = (id: Id.t, info_map: Statics.Map.t): bool =>
  switch (find_reparenthesize_for_id(id, info_map)) {
  | [left_id, op_id, right_id] when op_id == id =>
    switch (Statics.Map.lookup(id, info_map)) {
    | Some(InfoExp({user_term: {term: BinOp(_, left, right), _}, _})) =>
      Exp.rep_id(left) != left_id || Exp.rep_id(right) != right_id
    | _ => false
    }
  | _ => false
  };
