type t =
  | In // incomparable
  | Lt
  | Eq
  | Gt;

module Result = {
  type t('in_, 'lt, 'eq, 'gt) =
    | In('in_)
    | Lt('lt)
    | Eq('eq)
    | Gt('gt);
  type s('a) = t('a, 'a, 'a, 'a);

  let get = (r: s('a)): 'a =>
    switch (r) {
    | In(a)
    | Lt(a)
    | Eq(a)
    | Gt(a) => a
    };
};
