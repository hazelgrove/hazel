module Make = (S: {type t;}) => {
  module T = {
    [@deriving sexp]
    type t('a) = S.t => (S.t, 'a);

    let return = (x, s) => (s, x);

    let bind = (xf, f, s) => {
      let (s', x) = xf(s);
      f(x, s');
    };

    let map = Monads.MapDefinition.Define_using_bind;

    let get = s => (s, s);

    let put = (x, _) => (x, ());
  };

  include T;
  include Monads.Make(T);
};
