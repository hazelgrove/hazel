type t =
  | Lt
  | Eq
  | Gt;

module Result = {
  type t('lt, 'eq, 'gt) =
    | Lt('lt)
    | Eq('eq)
    | Gt('gt);
};
