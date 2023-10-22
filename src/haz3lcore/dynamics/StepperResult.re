type t('a) =
  | Expr('a)
  | Indet('a)
  | BoxedValue('a);

let unbox =
  fun
  | Expr(d)
  | Indet(d)
  | BoxedValue(d) => d;
