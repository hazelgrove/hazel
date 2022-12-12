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
 * The term `affix` is used in the rest of this
 * module's documentation when directionality
 * is not important, otherwise the terms `prefix`
 * and `suffix` are used.
 */
and affix('operand, 'operator) =
  /* Empty */
  | E /* Affix */
  | A('operator, t('operand, 'operator));

let mk: ('operand, list(('operator, 'operand))) => t('operand, 'operator);
let wrap: 'operand => t('operand, 'operator);

let rev: t('operand, 'operator) => t('operand, 'operator);

/**
 * `affix_affix(inner, outer)` produces a new affix that consists
 * of `inner` concatenated with `outer`.  For example:
 *
 * affix_affix(+ 1, + 2) --> + 1 + 2
 * affix_affix(1 +, 2 +) --> 2 + 1 +
 *
 * Note: `inner` and `outer` should either both be prefixes or both be suffixes.
 */
let affix_affix:
  (affix('operand, 'operator), affix('operand, 'operator)) =>
  affix('operand, 'operator);

/**
 * `seq_op_seq(seq1, op, seq2)` produces a new seq that consists of
 * `seq1` joined by `op` with `seq2`. For example:
 *
 * seq_op_seq(1 + 2, *, 3 + 4)  -->  1 + 2 * 3 + 4
 */
let seq_op_seq:
  (t('operand, 'operator), 'operator, t('operand, 'operator)) =>
  t('operand, 'operator);

/**
 * `seq_affix(inner, outer)` produces a new seq consisting of
 * `inner` concatenated with `outer`. For example:
 *
 * seq_affix(2 + 1, 3 +)  -->  3 + 2 + 1 (prefix)
 * seq_affix(1 + 2, + 3)  -->  1 + 2 + 3 (suffix)
 *
 * Note: `inner` and `outer` should either both be prefixes or both be suffixes.
 */
let seq_affix:
  (t('operand, 'operator), affix('operand, 'operator)) =>
  t('operand, 'operator);

/**
 * `prefix_seq(prefix, seq)` produces a new seq by
 * prepending `prefix` to `seq`. For example:
 *
 * prefix_seq(1 +, 2 + 3)  -->  1 + 2 + 3
 */
let prefix_seq:
  (affix('operand, 'operator), t('operand, 'operator)) =>
  t('operand, 'operator);

/**
 * `seq_suffix(seq, suffix)` produces a new seq by
 * appending `suffix` to `seq`. For example:
 *
 * seq_suffix(1 + 2, + 3)  -->  1 + 2 + 3
 */
let seq_suffix:
  (t('operand, 'operator), affix('operand, 'operator)) =>
  t('operand, 'operator);

/**
 * Returns the number of operands.
 */
let length: t('operand, 'operator) => int;

let length_of_affix: affix('operand, 'operator) => int;

/**
 * Returns the nth operand in seq if it exists,
 * otherwise raises `Invalid_argument`
 */
let nth_operand: (int, t('operand, _)) => 'operand;

let operands: t('operand, _) => list('operand);
let operators: t(_, 'operator) => list('operator);

let update_nth_operand:
  (int, 'operand, t('operand, 'operator)) => t('operand, 'operator);

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

let opt_split_nth_operand:
  (int, t('operand, 'operator)) =>
  option(('operand, operand_surround('operand, 'operator)));

let opt_split_nth_operator:
  (int, t('operand, 'operator)) =>
  option(('operator, operator_surround('operand, 'operator)));

let split_first_and_suffix:
  t('operand, 'operator) => ('operand, affix('operand, 'operator));
let split_prefix_and_last:
  t('operand, 'operator) => (affix('operand, 'operator), 'operand);

let t_of_operand_and_surround:
  ('operand, operand_surround('operand, 'operator)) => t('operand, 'operator);

let t_of_operator_and_surround:
  ('operator, operator_surround('operand, 'operator)) =>
  t('operand, 'operator);

let update_last_operand:
  ('operand => 'operand, t('operand, 'operator)) => t('operand, 'operator);
