open Util;

/* Given a BinOp tile ID and a statics map, returns the ids that constitute
   the "snapped" visual selection: [left_operand_id, op_id, right_operand_id],
   where same-op chains snap one level inward (e.g. for `(1+2)+3`, the outer
   `+` snaps to cover `2+3`). Returns [id] for non-BinOp expressions. */
let find_assoc_for_id' =
    (~same_op_right_only: bool, id: Id.t, info_map: Statics.Map.t)
    : list(Id.t) => {
  let statics_opt = Statics.Map.lookup(id, info_map);
  switch (statics_opt) {
  | Some(InfoExp(exp)) =>
    switch (exp.user_term.term) {
    | BinOp(op, left, right) =>
      let left_id = left |> Exp.rep_id;
      let right_id = right |> Exp.rep_id;
      let (left_assoc, left_assoc_from_same_op) =
        switch (Statics.Map.lookup(left_id, info_map)) {
        | Some(InfoExp(left_contents)) =>
          switch (left_contents.user_term.term) {
          | BinOp(left_op, _, left_right) when left_op == op => (
              left_right |> Exp.rep_id,
              true,
            )
          | _ => (left_id, false)
          }
        | _ => (left_id, false)
        };
      let left_left_id =
        if (same_op_right_only && !left_assoc_from_same_op) {
          left_assoc;
        } else {
          switch (Statics.Map.lookup(left_assoc, info_map)) {
          | Some(InfoExp(left_contents)) =>
            switch (left_contents.user_term.term) {
            | BinOp(left_assoc_op, left_left, _)
                when !same_op_right_only || left_assoc_op == op =>
              left_left |> Exp.rep_id
            | _ => left_assoc
            }
          | _ => left_assoc
          };
        };
      let right_assoc =
        switch (Statics.Map.lookup(right_id, info_map)) {
        | Some(InfoExp(right_contents)) =>
          switch (right_contents.user_term.term) {
          | BinOp(right_op, _, right_right)
              when !same_op_right_only || right_op == op =>
            right_right |> Exp.rep_id
          | _ => right_id
          }
        | _ => right_id
        };
      [left_left_id, id, right_assoc];
    | _ => [id]
    }
  | _ => [id]
  };
};

let find_assoc_for_id = (id: Id.t, info_map: Statics.Map.t): list(Id.t) =>
  find_assoc_for_id'(~same_op_right_only=false, id, info_map);

let find_reparenthesize_for_id =
    (id: Id.t, info_map: Statics.Map.t): list(Id.t) =>
  find_assoc_for_id'(~same_op_right_only=true, id, info_map);

/* Returns true if the id points to a BinOp where the visual selection differs
   from the raw AST grouping — i.e., reparenthesization would change the tree. */
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
