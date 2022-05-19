module T = {
  type error = ..;

  type t('a) = result('a, error);
  let map = Monads.MapDefinition.Custom((x, f) => Result.map(f, x));
  let bind = Result.bind;
  let return = x => Ok(x);
};

include T;
include Monads.Make(T);
