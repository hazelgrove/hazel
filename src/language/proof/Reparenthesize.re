open Util;
open OptUtil.Syntax;

type result = {
  exp: Exp.t,
  selected_id: Id.t,
  selected_is_single_binop: bool,
};

let rec binop_count = (exp: Exp.t): int =>
  switch (exp.term) {
  | BinOp(_, l, r) => 1 + binop_count(l) + binop_count(r)
  | Parens(e) => binop_count(e)
  | _ => 0
  };

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
    (~selected_id: Id.t, ~left_left_id: Id.t, exp: Exp.t): option(result) => {
  let* outer = ProofHacks.find_exp_id(selected_id, exp);
  switch (outer.term) {
  | BinOp(op, inner_exp, c) =>
    switch (inner_exp.term) {
    | BinOp(_, a, b) when Exp.rep_id(b) == left_left_id =>
      let new_inner = Exp.fresh(BinOp(op, b, c));
      let new_outer = Exp.fresh(BinOp(op, a, new_inner));
      let new_exp = ProofHacks.replace_exp_id(selected_id, exp, new_outer);
      Some({
        exp: new_exp,
        selected_id: Exp.rep_id(new_inner),
        selected_is_single_binop: binop_count(new_inner) == 1,
      });
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
    (selected_ids: list(Id.t), exp: Exp.t): option(result) => {
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
      Some({
        exp: rebuilt,
        selected_id: Exp.rep_id(selected_chain),
        selected_is_single_binop: binop_count(selected_chain) == 1,
      });
    | _ => None
    };
  | _ => None
  };
};

let rec reparenthesize_selection =
        (~selected_ids: list(Id.t), exp: Exp.t): option(result) => {
  switch (replace_selected_chain(selected_ids, exp)) {
  | Some(_) as result => result
  | None =>
    switch (exp.term) {
    | BinOp(op, l, r) =>
      switch (reparenthesize_selection(~selected_ids, l)) {
      | Some({exp: l', _} as result) =>
        Some({
          ...result,
          exp: Exp.fresh(BinOp(op, l', r)),
        })
      | None =>
        switch (reparenthesize_selection(~selected_ids, r)) {
        | Some({exp: r', _} as result) =>
          Some({
            ...result,
            exp: Exp.fresh(BinOp(op, l, r')),
          })
        | None => None
        }
      }
    | Parens(e) =>
      let* {exp: e', _} as result =
        reparenthesize_selection(~selected_ids, e);
      Some({
        ...result,
        exp: Exp.fresh(Parens(e')),
      });
    | _ => None
    }
  };
};
