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

let wrap: 'operand => t('operand, 'a);

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
    ~mk_inconsistent_operand: (MetaVarGen.t, 'operand) =>
                              ('operand, MetaVarGen.t),
    MetaVarGen.t,
    t('operand, 'operator)
  ) =>
  (t('operand, 'operator), MetaVarGen.t);

let is_complete_skel:
  (
    ('operand, bool) => bool,
    skel('operator),
    seq('operand, 'operator),
    bool
  ) =>
  bool;

let is_complete:
  (('operand, bool) => bool, t('operand, 'operator), bool) => bool;
