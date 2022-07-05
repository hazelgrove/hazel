/**
 * An OpSeq consists of a `seq` representing the unassociated
 * sequence of operands and operators and a `skel` representing
 * the association of `seq`. `seq` is useful for editing,
 * `skel` is necessary for typechecking and evaluation.
 */
[@deriving sexp]
type t('operand, 'operator) =
  | OpSeq(skel('operator), seq('operand, 'operator))
and skel('operator) = Skel.t('operator)
and seq('operand, 'operator) = Seq.t('operand, 'operator);

let mk:
  (
    ~associate: seq('operand, 'operator) => Skel.t('operator),
    seq('operand, 'operator)
  ) =>
  t('operand, 'operator);

let wrap: 'operand => t('operand, _);

let get_err_status:
  (
    ~get_err_status_operand: 'operand => ErrStatus.t,
    t('operand, 'operator)
  ) =>
  ErrStatus.t;

let set_err_status:
  (
    ~set_err_status_operand: (ErrStatus.t, 'operand) => 'operand,
    ErrStatus.t,
    t('operand, 'operator)
  ) =>
  t('operand, 'operator);

let mk_inconsistent:
  (
    ~mk_inconsistent_operand: (IDGen.t, 'operand) => ('operand, IDGen.t),
    IDGen.t,
    t('operand, 'operator)
  ) =>
  (t('operand, 'operator), IDGen.t);

/**
 * `is_complete(is_complete_operand, opseq)` returns
 * whether `opseq` is complete, using `is_complete_operand` to check
 * completeness of operands.
 */
let is_complete: ('operand => bool, t('operand, 'operator)) => bool;
