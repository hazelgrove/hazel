open Util;
open OptUtil.Syntax;

/* Restructure exp so the visual selection at selected_id becomes a proper
      sub-term.

      Example:
        exp = (1 + 2) + 3  (AST: BinOp(+, BinOp(+, 1, 2), 3))
        selected_id = rep_id of outer +
        left_left_id = rep_id of 2  (snapped left boundary from AssocSelection)
      Returns:
        Some((1 + (2 + 3), new_inner_id))
        where new_inner_id = rep_id of the fresh BinOp(+, 2, 3)
   */
let reparenthesize =
    (~selected_id: Id.t, ~left_left_id: Id.t, exp: Exp.t)
    : option((Exp.t, Id.t)) => {
  let* outer = ProofHacks.find_exp_id(selected_id, exp);
  switch (outer.term) {
  | BinOp(op, inner_exp, c) =>
    switch (inner_exp.term) {
    | BinOp(_, a, b) when Exp.rep_id(b) == left_left_id =>
      let new_inner = Exp.fresh(BinOp(op, b, c));
      let new_outer = Exp.fresh(BinOp(op, a, new_inner));
      let new_exp = ProofHacks.replace_exp_id(selected_id, exp, new_outer);
      Some((new_exp, Exp.rep_id(new_inner)));
    | _ => None
    }
  | _ => None
  };
};

let rec split_chain = (op: Operators.op_bin, exp: Exp.t): list(Exp.t) =>
  switch (exp.term) {
  | BinOp(op', l, r) when op' == op =>
    split_chain(op, l) @ split_chain(op, r)
  | _ => [exp]
  };

let combine_chain = (op: Operators.op_bin, exps: list(Exp.t)): option(Exp.t) =>
  switch (exps) {
  | [] => None
  | [e] => Some(e)
  | [first, second, ...rest] =>
    Some(
      List.fold_left(
        (acc, exp) => Exp.fresh(BinOp(op, acc, exp)),
        Exp.fresh(BinOp(op, first, second)),
        rest,
      ),
    )
  };

let selected_bounds = (selected_ids: list(Id.t), operands: list(Exp.t)) => {
  operands
  |> List.mapi((i, exp) =>
       List.mem(Exp.rep_id(exp), selected_ids) ? Some(i) : None
     )
  |> List.filter_map(Fun.id)
  |> (
    fun
    | [] => None
    | [i, ...is] =>
      Some((List.fold_left(min, i, is), List.fold_left(max, i, is)))
  );
};

let replace_selected_chain =
    (selected_ids: list(Id.t), exp: Exp.t): option((Exp.t, Id.t)) => {
  switch (exp.term) {
  | BinOp(op, _, _) =>
    let operands = split_chain(op, exp);
    switch (selected_bounds(selected_ids, operands)) {
    | Some((lo, hi)) when hi > lo =>
      let before = ListUtil.sublist((0, lo), operands);
      let selected = ListUtil.sublist((lo, hi + 1), operands);
      let after =
        ListUtil.sublist((hi + 1, List.length(operands)), operands);
      let* selected_chain = combine_chain(op, selected);
      let parens = Exp.fresh(Parens(selected_chain));
      let rebuilt_operands = before @ [parens] @ after;
      let* rebuilt = combine_chain(op, rebuilt_operands);
      Some((rebuilt, Exp.rep_id(parens)));
    | _ => None
    };
  | _ => None
  };
};

let rec reparenthesize_selection =
        (~selected_ids: list(Id.t), exp: Exp.t): option((Exp.t, Id.t)) => {
  switch (replace_selected_chain(selected_ids, exp)) {
  | Some(_) as result => result
  | None =>
    switch (exp.term) {
    | BinOp(op, l, r) =>
      switch (reparenthesize_selection(~selected_ids, l)) {
      | Some((l', selected_id)) =>
        Some((Exp.fresh(BinOp(op, l', r)), selected_id))
      | None =>
        switch (reparenthesize_selection(~selected_ids, r)) {
        | Some((r', selected_id)) =>
          Some((Exp.fresh(BinOp(op, l, r')), selected_id))
        | None => None
        }
      }
    | Parens(e) =>
      let* (e', selected_id) = reparenthesize_selection(~selected_ids, e);
      Some((Exp.fresh(Parens(e')), selected_id));
    | _ => None
    }
  };
};
