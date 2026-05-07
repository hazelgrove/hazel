open Util;

/* Given a BinOp tile ID and a statics map, returns the ids that constitute
   the "snapped" visual selection: [left_operand_id, op_id, right_operand_id],
   where same-op chains snap one level inward (e.g. for `(1+2)+3`, the outer
   `+` snaps to cover `2+3`). Returns [id] for non-BinOp expressions. */
let find_assoc_for_id = (id: Id.t, info_map: Statics.Map.t): list(Id.t) => {
  let statics_opt = Statics.Map.lookup(id, info_map);
  switch (statics_opt) {
  | Some(InfoExp(exp)) =>
    switch (exp.user_term.term) {
    | BinOp(op, left, right) =>
      let left_id = left |> Exp.rep_id;
      let right_id = right |> Exp.rep_id;
      let left_assoc =
        switch (Statics.Map.lookup(left_id, info_map)) {
        | Some(InfoExp(left_contents)) =>
          switch (left_contents.user_term.term) {
          | BinOp(left_op, _, left_right) when left_op == op =>
            left_right |> Exp.rep_id
          | _ => left_id
          }
        | _ => left_id
        };
      let left_left_id =
        switch (Statics.Map.lookup(left_assoc, info_map)) {
        | Some(InfoExp(left_contents)) =>
          switch (left_contents.user_term.term) {
          | BinOp(_, left_left, _) => left_left |> Exp.rep_id
          | _ => left_assoc
          }
        | _ => left_assoc
        };
      let right_assoc =
        switch (Statics.Map.lookup(right_id, info_map)) {
        | Some(InfoExp(right_contents)) =>
          switch (right_contents.user_term.term) {
          | BinOp(_, _, right_right) => right_right |> Exp.rep_id
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
