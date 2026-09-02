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
  | Real(Plus)
  | Real(Times)
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

/* A visual suffix [b - c] in [(a + b) - c] crosses the parser's
 * left-associated boundary.  Make that range a real subtree without changing
 * its value.  Natural and floating subtraction are intentionally excluded:
 * the former is not associative with addition, and the latter is not exact. */
let replace_selected_plus_minus_suffix =
    (selected_ids: list(Id.t), exp: Exp.t): option(result) => {
  let contains_selected_id = exp =>
    IdTagged.ids(exp) |> List.exists(id => List.mem(id, selected_ids));
  let compatible = (minus_op, plus_op) =>
    switch (minus_op, plus_op) {
    | (Operators.Int(Minus), Operators.Int(Plus))
    | (Operators.SInt(Minus), Operators.SInt(Plus))
    | (Operators.Real(Minus), Operators.Real(Plus)) => true
    | _ => false
    };
  switch (exp.term) {
  | BinOp(minus_op, {term: BinOp(plus_op, a, b), _}, c)
      when
        compatible(minus_op, plus_op)
        && List.mem(Exp.rep_id(exp), selected_ids)
        && contains_selected_id(b)
        && contains_selected_id(c)
        && !contains_selected_id(a) =>
    let selected = Exp.fresh(BinOp(minus_op, b, c));
    Some({
      exp: Exp.fresh(BinOp(plus_op, a, selected)),
      selected_id: Exp.rep_id(selected),
      selected_is_single_binop: true,
    });
  | _ => None
  };
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

type subtraction_operand = {
  operator_id: Id.t,
  exp: Exp.t,
};

let rec split_subtraction_chain =
        (op: Operators.op_bin, exp: Exp.t)
        : (Exp.t, list(subtraction_operand)) =>
  switch (exp.term) {
  | BinOp(op', left, right) when op' == op =>
    let (base, operands) = split_subtraction_chain(op, left);
    (
      base,
      operands
      @ [
        {
          operator_id: Exp.rep_id(exp),
          exp: right,
        },
      ],
    );
  | _ => (exp, [])
  };

let plus_and_neg_for_minus:
  Operators.op_bin => option((Operators.op_bin, Operators.op_un)) =
  fun
  | Operators.Int(Operators.Minus) =>
    Some((Operators.Int(Operators.Plus), Operators.Int(Operators.Minus)))
  | Operators.SInt(Operators.Minus) =>
    Some((Operators.SInt(Operators.Plus), Operators.SInt(Operators.Minus)))
  | Operators.Float(Operators.Minus) =>
    Some((
      Operators.Float(Operators.Plus),
      Operators.Float(Operators.Minus),
    ))
  | Operators.Real(Operators.Minus) =>
    Some((Operators.Real(Operators.Plus), Operators.Real(Operators.Minus)))
  | _ => None;

let is_signed_additive_op: Operators.op_bin => bool =
  fun
  | Operators.Int(Operators.Plus | Operators.Minus)
  | Operators.SInt(Operators.Plus | Operators.Minus)
  | Operators.Real(Operators.Plus | Operators.Minus) => true
  | _ => false;

type additive_operand = {
  operator_id: Id.t,
  op: Operators.op_bin,
  exp: Exp.t,
};

/* Preserve each displayed sign while exposing a suffix of an additive chain.
 * This covers both [a - b - c] and [a - b + c] without treating [minus] as
 * associative. */
let rec split_additive_chain = (exp: Exp.t): (Exp.t, list(additive_operand)) =>
  switch (exp.term) {
  | BinOp(op, left, right) when is_signed_additive_op(op) =>
    let (base, operands) = split_additive_chain(left);
    (
      base,
      operands
      @ [
        {
          operator_id: Exp.rep_id(exp),
          op,
          exp: right,
        },
      ],
    );
  | _ => (exp, [])
  };

let combine_additive_operands =
    (base: Exp.t, operands: list(additive_operand)): Exp.t =>
  operands
  |> List.fold_left(
       (acc, operand) => Exp.fresh(BinOp(operand.op, acc, operand.exp)),
       base,
     );

let additive_operand_is_selected =
    (selected_ids: list(Id.t), operand: additive_operand): bool =>
  IdTagged.ids(operand.exp) |> List.exists(id => List.mem(id, selected_ids));

/* A mouse drag commonly begins on the first operand rather than on its
 * preceding infix sign.  Complete that boundary only when the drag also
 * contains a following additive operand.  Thus [b + c] in [a - b + c]
 * denotes the signed chunk [-b + c], while selecting [b] alone remains the
 * ordinary subexpression [b]. */
let complete_implicit_signed_additive_prefix =
    (selected_ids: list(Id.t), exp: Exp.t): option(list(Id.t)) => {
  let (base, operands) = split_additive_chain(exp);
  switch (
    operands
    |> List.mapi((index, operand) =>
         additive_operand_is_selected(selected_ids, operand)
           ? Some((index, operand)) : None
       )
    |> List.filter_map(Fun.id)
  ) {
  | [(first, first_operand), ..._]
      when
        !List.mem(first_operand.operator_id, selected_ids)
        && plus_and_neg_for_minus(first_operand.op) != None
        && !List.mem(Exp.rep_id(base), selected_ids) =>
    let selected_operands =
      ListUtil.sublist((first, List.length(operands)), operands);
    switch (selected_operands) {
    | [_, ...[_, ..._] as following]
        when
          following
          |> List.for_all(operand =>
               List.mem(operand.operator_id, selected_ids)
               && additive_operand_is_selected(selected_ids, operand)
             ) =>
      Some([first_operand.operator_id, ...selected_ids])
    | _ => None
    };
  | _ => None
  };
};

let rec complete_signed_additive_selection_ids =
        (selected_ids: list(Id.t), exp: Exp.t): option(list(Id.t)) =>
  switch (complete_implicit_signed_additive_prefix(selected_ids, exp)) {
  | Some(_) as completed => completed
  | None =>
    switch (exp.term) {
    | BinOp(_, left, right)
    | Ap(_, left, right) =>
      switch (complete_signed_additive_selection_ids(selected_ids, left)) {
      | Some(_) as completed => completed
      | None => complete_signed_additive_selection_ids(selected_ids, right)
      }
    | Parens(inner)
    | Asc(inner, _)
    | Projector(_, inner) =>
      complete_signed_additive_selection_ids(selected_ids, inner)
    | _ => None
    }
  };

let replace_selected_additive_suffix =
    (selected_ids: list(Id.t), exp: Exp.t): option(result) => {
  let selected_ids =
    complete_implicit_signed_additive_prefix(selected_ids, exp)
    |> Option.value(~default=selected_ids);
  let (base, operands) = split_additive_chain(exp);
  let selected_indices =
    operands
    |> List.mapi((index, operand) =>
         List.mem(operand.operator_id, selected_ids)
         && additive_operand_is_selected(selected_ids, operand)
           ? Some(index) : None
       )
    |> List.filter_map(Fun.id);
  switch (selected_indices) {
  | [first, ..._] =>
    /* The base is the unselected prefix of a signed suffix. If it is itself
     * selected, this is a whole-chain selection and must retain the normal
     * term-selection behavior rather than being narrowed to a virtual slice. */
    if (List.mem(Exp.rep_id(base), selected_ids)) {
      None;
    } else {
      let prefix_operands = ListUtil.take(first, operands);
      let selected_operands =
        ListUtil.sublist((first, List.length(operands)), operands);
      let suffix_is_fully_selected =
        selected_operands
        |> List.for_all(operand =>
             List.mem(operand.operator_id, selected_ids)
             && additive_operand_is_selected(selected_ids, operand)
           );
      if (!suffix_is_fully_selected) {
        None;
      } else {
        switch (selected_operands) {
        | [{op, exp: first_exp, _}, ...rest] =>
          switch (plus_and_neg_for_minus(op)) {
          | None => None
          | Some((plus_op, neg_op)) =>
            let selected_base = Exp.fresh(UnOp(neg_op, first_exp));
            let selected = combine_additive_operands(selected_base, rest);
            let prefix = combine_additive_operands(base, prefix_operands);
            let selected_parens = Exp.fresh(Parens(selected));
            Some({
              exp: Exp.fresh(BinOp(plus_op, prefix, selected_parens)),
              selected_id: Exp.rep_id(selected),
              selected_is_single_binop: binop_count(selected) == 1,
            });
          }
        | [] => None
        };
      };
    }
  | [] => None
  };
};

let operand_is_selected =
    (selected_ids: list(Id.t), operand: subtraction_operand): bool =>
  IdTagged.ids(operand.exp) |> List.exists(id => List.mem(id, selected_ids));

let combine_subtraction_operands =
    (op: Operators.op_bin, base: Exp.t, operands: list(subtraction_operand))
    : Exp.t =>
  operands
  |> List.fold_left(
       (acc, operand: subtraction_operand) =>
         Exp.fresh(BinOp(op, acc, operand.exp)),
       base,
     );

let replace_selected_subtraction_suffix =
    (selected_ids: list(Id.t), exp: Exp.t): option(result) => {
  switch (exp.term) {
  | BinOp(op, _, _) =>
    let* (plus_op, neg_op) = plus_and_neg_for_minus(op);
    let (base, operands) = split_subtraction_chain(op, exp);
    let selected_indices =
      operands
      |> List.mapi((index, operand: subtraction_operand) =>
           List.mem(operand.operator_id, selected_ids)
           && operand_is_selected(selected_ids, operand)
             ? Some(index) : None
         )
      |> List.filter_map(Fun.id);
    switch (selected_indices) {
    | [first, ..._] =>
      let prefix_operands = ListUtil.take(first, operands);
      let selected_operands =
        ListUtil.sublist((first, List.length(operands)), operands);
      let suffix_is_fully_selected =
        selected_operands
        |> List.for_all((operand: subtraction_operand) =>
             List.mem(operand.operator_id, selected_ids)
             && operand_is_selected(selected_ids, operand)
           );
      if (!suffix_is_fully_selected) {
        None;
      } else {
        switch (selected_operands) {
        | [] => None
        | [first_operand, ...rest] =>
          let selected_base = Exp.fresh(UnOp(neg_op, first_operand.exp));
          let selected =
            combine_subtraction_operands(op, selected_base, rest);
          let prefix =
            combine_subtraction_operands(op, base, prefix_operands);
          let selected_parens = Exp.fresh(Parens(selected));
          Some({
            exp: Exp.fresh(BinOp(plus_op, prefix, selected_parens)),
            selected_id: Exp.rep_id(selected),
            selected_is_single_binop: binop_count(selected) == 1,
          });
        };
      };
    | [] => None
    };
  | _ => None
  };
};

let rec reparenthesize_selection =
        (
          ~whole_selected_ids: list(Id.t)=[],
          ~selected_ids: list(Id.t),
          exp: Exp.t,
        )
        : option(result) =>
  if (List.mem(Exp.rep_id(exp), whole_selected_ids)) {
    None;
  } else {
    switch (replace_selected_plus_minus_suffix(selected_ids, exp)) {
    | Some(_) as result => result
    | None =>
      switch (replace_selected_additive_suffix(selected_ids, exp)) {
      | Some(_) as result => result
      | None =>
        switch (replace_selected_subtraction_suffix(selected_ids, exp)) {
        | Some(_) as result => result
        | None =>
          switch (replace_selected_chain(selected_ids, exp)) {
          | Some(_) as result => result
          | None =>
            switch (exp.term) {
            | BinOp(op, l, r) =>
              switch (
                reparenthesize_selection(
                  ~whole_selected_ids,
                  ~selected_ids,
                  l,
                )
              ) {
              | Some({exp: l', _} as result) =>
                Some({
                  ...result,
                  exp: Exp.fresh(BinOp(op, l', r)),
                })
              | None =>
                switch (
                  reparenthesize_selection(
                    ~whole_selected_ids,
                    ~selected_ids,
                    r,
                  )
                ) {
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
                reparenthesize_selection(
                  ~whole_selected_ids,
                  ~selected_ids,
                  e,
                );
              Some({
                ...result,
                exp: Exp.fresh(Parens(e')),
              });
            | _ => None
            }
          }
        }
      }
    };
  };
