module type E = {type error;};

module T = (X: E) => {
  include X;

  type t('a) = result('a, error);
  let map = Monads.MapDefinition.Custom((x, f) => Result.map(f, x));
  let bind = Result.bind;
  let return = x => Ok(x);
};

module Make = (X: E) => {
  include T(X);
  include Monads.Make((T(X)));
};
