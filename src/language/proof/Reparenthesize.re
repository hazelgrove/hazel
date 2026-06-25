open Util;
open OptUtil.Syntax;

type result = {
  exp: Exp.t,
  selected_id: Id.t,
  selected_is_single_binop: bool,
};

let selected_exp = (result: result): option(Exp.t) =>
  ProofHacks.find_exp_id(result.selected_id, result.exp);

let replace_selected = (result: result, with_exp: Exp.t): Exp.t =>
  ProofHacks.replace_exp_id(result.selected_id, result.exp, with_exp);

let is_associative_op = (op: Operators.op_bin): bool =>
  switch (op) {
  | Int(Plus)
  | Int(Times)
  | SInt(Plus)
  | SInt(Times)
  | Nat(Plus)
  | Nat(Times)
  | Float(Plus)
  | Float(Times)
  | Bool(And)
  | Bool(Or) => true
  | _ => false
  };

let rec exp_size = (exp: Exp.t): int =>
  1
  + (
    switch (exp.term) {
    | BinOp(_, l, r)
    | Ap(_, l, r) => exp_size(l) + exp_size(r)
    | Parens(e)
    | Asc(e, _)
    | Projector(_, e) => exp_size(e)
    | _ => 0
    }
  );

let unparenthesize_direct = (~selected_id: Id.t, exp: Exp.t): option(Exp.t) => {
  let* selected = ProofHacks.find_exp_id(selected_id, exp);
  switch (selected.term) {
  | Parens(inner) =>
    Some(ProofHacks.replace_exp_id(selected_id, exp, inner))
  | _ => None
  };
};

let rec split_chain = (op: Operators.op_bin, exp: Exp.t): list(Exp.t) =>
  switch (exp.term) {
  | BinOp(op', l, r) when op' == op && is_associative_op(op) =>
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

let rec split_chain_unparenthesizing =
        (~selected_id: Id.t, op: Operators.op_bin, exp: Exp.t)
        : option(list(Exp.t)) =>
  switch (exp.term) {
  | Parens({term: BinOp(inner_op, _, _), _} as inner)
      when
        Exp.rep_id(exp) == selected_id
        && inner_op == op
        && is_associative_op(op) =>
    Some(split_chain(op, inner))
  | BinOp(op', l, r) when op' == op && is_associative_op(op) =>
    switch (split_chain_unparenthesizing(~selected_id, op, l)) {
    | Some(l_operands) => Some(l_operands @ split_chain(op, r))
    | None =>
      switch (split_chain_unparenthesizing(~selected_id, op, r)) {
      | Some(r_operands) => Some(split_chain(op, l) @ r_operands)
      | None => None
      }
    }
  | _ => None
  };

let rec unparenthesize_associative =
        (~selected_id: Id.t, exp: Exp.t): option(Exp.t) =>
  switch (exp.term) {
  | BinOp(op, _, _) =>
    switch (split_chain_unparenthesizing(~selected_id, op, exp)) {
    | Some(operands) => combine_chain(op, operands)
    | None =>
      switch (exp.term) {
      | BinOp(op, l, r) =>
        switch (unparenthesize_associative(~selected_id, l)) {
        | Some(l') => Some(Exp.fresh(BinOp(op, l', r)))
        | None =>
          switch (unparenthesize_associative(~selected_id, r)) {
          | Some(r') => Some(Exp.fresh(BinOp(op, l, r')))
          | None => None
          }
        }
      | _ => None
      }
    }
  | Parens(e) =>
    let* e' = unparenthesize_associative(~selected_id, e);
    Some(Exp.fresh(Parens(e')));
  | _ => None
  };

let unparenthesize = (~selected_id: Id.t, exp: Exp.t): option(Exp.t) =>
  switch (unparenthesize_associative(~selected_id, exp)) {
  | Some(_) as result => result
  | None => unparenthesize_direct(~selected_id, exp)
  };

let unparenthesize_any =
    (~selected_ids: list(Id.t), exp: Exp.t): option(Exp.t) =>
  selected_ids
  |> List.filter_map(id =>
       switch (ProofHacks.find_exp_id(id, exp)) {
       | Some({term: Parens(_), _} as selected) =>
         Some((id, exp_size(selected)))
       | _ => None
       }
     )
  |> List.sort(((_, a_size), (_, b_size)) => Int.compare(b_size, a_size))
  |> List.find_map(((id, _)) => unparenthesize(~selected_id=id, exp));

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
  | BinOp(op, inner_exp, c) when is_associative_op(op) =>
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
  | BinOp(op, _, _) when is_associative_op(op) =>
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
