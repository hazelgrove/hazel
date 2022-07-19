module Make:
  (S: {type t;}) =>
   {
    include Monads.MONAD with type t('a) = S.t => (S.t, 'a);

    let get: t(S.t);
    let put: S.t => t(unit);
  };
