[@deriving sexp]
type t('operand, 'operator, 'zoperand, 'zoperator) =
  | ZOperand('zoperand, Seq.operand_surround('operand, 'operator))
  | ZOperator('zoperator, Seq.operator_surround('operand, 'operator));

let wrap: 'a => t('b, 'c, 'a, 'd);

let erase:
  (
    ~erase_zoperand: 'zoperand => 'operand,
    ~erase_zoperator: 'zoperator => 'operator,
    t('operand, 'operator, 'zoperand, 'zoperator)
  ) =>
  Seq.t('operand, 'operator);

let is_before:
  (~is_before_zoperand: 'zoperand => bool, t('a, 'b, 'zoperand, 'c)) => bool;

let is_after:
  (~is_after_zoperand: 'zoperand => bool, t('a, 'b, 'zoperand, 'c)) => bool;

let place_before:
  (
    ~place_before_operand: 'operand => 'zoperand,
    Seq.t('operand, 'operator)
  ) =>
  t('operand, 'operator, 'zoperand, 'zoperator);

let place_after:
  (~place_after_operand: 'operand => 'zoperand, Seq.t('operand, 'operator)) =>
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
