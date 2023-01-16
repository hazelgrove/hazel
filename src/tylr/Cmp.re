type t =
  | In
  | Lt
  | Eq
  | Gt;

module Result = {
  type t('in_, 'lt, 'eq, 'gt) =
    | In('in_)
    | Lt('lt)
    | Eq('eq)
    | Gt('gt);

  let get = (r: t('a, 'a, 'a, 'a)): 'a =>
    switch (r) {
    | In(a)
    | Lt(a)
    | Eq(a)
    | Gt(a) => a
    };
};
