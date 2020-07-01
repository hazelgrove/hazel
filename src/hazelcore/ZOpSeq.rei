[@deriving sexp]
type t('operand, 'operator, 'zoperand, 'zoperator) =
  | ZOpSeq(
      Skel.t('operator),
      ZSeq.t('operand, 'operator, 'zoperand, 'zoperator),
    );

let mk:
  (
    ~associate: Seq.t('operand, 'operator) => Skel.t('operator),
    ~erase_zoperand: 'zoperand => 'operand,
    ~erase_zoperator: 'zoperator => 'operator,
    ZSeq.t('operand, 'operator, 'zoperand, 'zoperator)
  ) =>
  t('operand, 'operator, 'zoperand, 'zoperator);

let wrap: 'a => t('b, 'c, 'a, 'd);

let skel_contains_cursor:
  (Skel.t('operator), ZSeq.t('a, 'operator, 'b, 'c)) => bool;

let skel_is_rooted_at_cursor:
  (Skel.t('operator), ZSeq.t('a, 'operator, 'b, 'c)) => bool;

let set_err_status:
  (
    ~set_err_status_zoperand: (ErrStatus.t, 'zoperand) => 'zoperand,
    ErrStatus.t,
    t('operand, 'operator, 'zoperand, 'zoperator)
  ) =>
  t('operand, 'operator, 'zoperand, 'zoperator);

let mk_inconsistent:
  (
    ~mk_inconsistent_zoperand: (MetaVarGen.t, 'zoperand) =>
                               ('zoperand, MetaVarGen.t),
    MetaVarGen.t,
    t('operand, 'operator, 'zoperand, 'zoperator)
  ) =>
  (t('operand, 'operator, 'zoperand, 'zoperator), MetaVarGen.t);

let erase:
  (
    ~erase_zoperand: 'zoperand => 'operand,
    ~erase_zoperator: 'zoperator => 'operator,
    t('operand, 'operator, 'zoperand, 'zoperator)
  ) =>
  OpSeq.t('operand, 'operator);

let is_before:
  (~is_before_zoperand: 'zoperand => bool, t('a, 'b, 'zoperand, 'c)) => bool;

let is_after:
  (~is_after_zoperand: 'zoperand => bool, t('a, 'b, 'zoperand, 'c)) => bool;

let is_outer:
  (~is_outer_zoperand: 'zoperand => bool, t('a, 'b, 'zoperand, 'c)) => bool;

let place_before:
  (
    ~place_before_operand: 'operand => 'zoperand,
    OpSeq.t('operand, 'operator)
  ) =>
  t('operand, 'operator, 'zoperand, 'zoperator);

let place_after:
  (
    ~place_after_operand: 'operand => 'zoperand,
    OpSeq.t('operand, 'operator)
  ) =>
  t('operand, 'operator, 'zoperand, 'zoperator);

let move_cursor_left:
  (
    ~move_cursor_left_zoperand: 'zoperand => option('zoperand),
    ~move_cursor_left_zoperator: 'zoperator => option('zoperator),
    ~place_after_operand: 'operand => 'zoperand,
    ~place_after_operator: 'operator => option('zoperator),
    ~erase_zoperand: 'zoperand => 'operand,
    ~erase_zoperator: 'zoperator => 'operator,
    t('operand, 'operator, 'zoperand, 'zoperator)
  ) =>
  option(t('operand, 'operator, 'zoperand, 'zoperator));

let move_cursor_right:
  (
    ~move_cursor_right_zoperand: 'zoperand => option('zoperand),
    ~move_cursor_right_zoperator: 'zoperator => option('zoperator),
    ~place_before_operand: 'operand => 'zoperand,
    ~place_before_operator: 'operator => option('zoperator),
    ~erase_zoperand: 'zoperand => 'operand,
    ~erase_zoperator: 'zoperator => 'operator,
    t('operand, 'operator, 'zoperand, 'zoperator)
  ) =>
  option(t('operand, 'operator, 'zoperand, 'zoperator));
