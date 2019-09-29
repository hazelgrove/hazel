open Sexplib.Std;

/*
type operator_affix('operand, 'operator) =
  | Operand('operand, operand_affix('operand, 'operator))
and operand_affix('operand, 'operator) =
  | Empty
  | Operator('operator, operator_affix('operand, 'operator));
*/

[@deriving sexp]
type t('operand, 'operator) = list(OpPair.t('operand, 'operator));

let operators = List.map(OpPair.operator);
let operands = List.map(OpPair.operand);

/**
 * Returns the nth operator in `affix` if it exists,
 * otherwise raises `Invalid_argument`
 */
let nth_operator = (n: int, affix: t(_, 'operator)): 'operator =>
  switch (List.nth_opt(affix |> operators, n)) {
  | None => raise(Invalid_argument("Affix.nth_operator"))
  | Some(operator) => operator
  };
/**
 * Returns the nth operand in `affix` if it exists,
 * otherwise raises `Invalid_argument`
 */
let nth_operand = (n: int, affix: t('operand, _)): 'operand =>
  switch(List.nth_opt(affix |> operands, n)) {
  | None => raise(Invalid_argument("Affix.nth_operand"))
  | Some(operand) => operand
  };

let rec update_nth_operand = (n: int, operand: 'operand, affix: t('operand, 'operator)): t('operand, 'operator) =>
  switch (affix) {
  | [] => raise(Invalid_argument("Affix.update_nth_operand"))
  | [hd, ...tl] =>
    n === 0
    ? [hd |> OpPair.update_operand(operand), ...rest]
    : [hd, ...(tl |> update_nth_operand(n - 1, operand))]
  }