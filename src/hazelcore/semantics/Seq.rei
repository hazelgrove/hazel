/**
 * An unassociated infix operator sequence.
 * Also used to represent the prefix or suffix
 * of a selected operator in a seq, in both
 * cases such that the head operand neighbors
 * the selected operator.
 */
[@deriving sexp]
type t('operand, 'operator) =
  /* Seq */
  | S('operand, affix('operand, 'operator))
/**
 * An unassociated infix operator sequence
 * without a head operand. Used to represent
 * the prefix or suffix of a selected operand
 * in a seq, in both cases such that the head
 * operator neighbors the selected operand.
 */
and affix('operand, 'operator) =
  /* Empty */
  | E
  /* Affix */
  | A('operator, t('operand, 'operator));

let mk_affix: list(('operator, 'operand))  => affix('operand, 'operator);
let mk: ('operand, list(('operator, 'operand))) => t('operand, 'operator);
let wrap: 'operand => t('operand, 'operator);

let rev: t('operand, 'operator) => t('operand, 'operator);

let affix_affix: (affix('operand, 'operator), affix('operand, 'operator)) =>
  affix('operand, 'operator);

let seq_op_seq: (t('operand, 'operator), 'operator, t('operand, 'operator)) =>
  t('operand, 'operator);

let affix_seq: (affix('operand, 'operator), t('operand, 'operator)) =>
  t('operand, 'operator);

let seq_affix: (t('operand, 'operator), affix('operand, 'operator)) =>
  t('operand, 'operator)

let length: t('operand, 'operator) => int;

let length_of_affix: affix('operand, 'operator) => int;

let nth_operand: (int, t('operand, _)) => 'operand;
let nth_operand_of_affix: (int, affix('operand, _)) => 'operand;

let operands_in_range: ((int, int), t('operand, _)) => list('operand);

let operands: t('operand, _) => list('operand);
let operands_of_affix: affix('operand, _) => list('operand);

let operators: t(_, 'operator) => list('operator);
let operators_of_affix: affix(_, 'operator) => list('operator);

let opt_update_nth_operand: (int, 'operand, t('operand, 'operator)) =>
  option(t('operand, 'operator));
let opt_update_nth_operand_of_affix: (int, 'operand, t('operand, 'operator)) =>
  option(affix('operand, 'operator));

let update_nth_operand: (int, 'operand, t('operand, 'operator)) =>
  t('operand, 'operator);

[@deriving sexp]
type operand_surround('operand, 'operator) = (
  affix('operand, 'operator),
  affix('operand, 'operator),
);
[@deriving sexp]
type operator_surround('operand, 'operator) = (
  t('operand, 'operator),
  t('operand, 'operator),
);

let opt_split_nth_operand: (int, t('operand, 'operator)) =>
  option(('operand, operand_surround('operand, 'operator)));
let split_nth_operand: (int, t('operand, 'operator)) =>
  ('operand, operand_surround('operand, 'operator))

let opt_split_nth_operator: (int, t('operand, 'operator)) =>
  option(('operator, operator_surround('operand, 'operator)));
let split_nth_operator: (int, t('operand, 'operator)) =>
  ('operator, operator_surround('operand, 'operator));

let split_first_and_suffix: t('operand, 'operator) =>
  ('operand, affix('operand, 'operator));
let split_prefix_and_last: t('operand, 'operator) =>
  (affix('operand, 'operator), 'operand);

let t_of_operand_and_surround: 
  ('operand, operand_surround('operand, 'operator)) => t('operand, 'operator);

let t_of_operator_and_surround:
  ('operator, operator_surround('operand, 'operator)) => t('operand, 'operator);
