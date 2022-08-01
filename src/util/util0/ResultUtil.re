module type E = {type error;};

module T = (X: E) => {
  include X;

  type t('a) = result('a, error);
  let return = x => Ok(x);
  let bind = Result.bind;
  let map = (x, f) => Result.map(f, x);
};

module Make = (X: E) => {
  module T = T(X);
  include T;
  include Monads.Make_Monad_Z((Monads.Make_Zip(T)));
};
