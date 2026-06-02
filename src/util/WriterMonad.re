module type WRITER = {
  type t;
  let empty: t;
  let append: (t, t) => t;
};

module type S = {
  type writer;

  type t('a);
  let return: 'a => t('a);
  let bind: (t('a), 'a => t('b)) => t('b);

  let tell: writer => t(unit);
  let listen: t('a) => t(('a, writer));
  let pass: t(('a, writer => writer)) => t('a);
};

module Make = (W: WRITER) => {
  type writer = W.t;

  module T = {
    type t('a) = (writer, 'a);

    let return = x => (W.empty, x);

    let bind = ((w1, x), f) => {
      let (w2, y) = f(x);
      (W.append(w1, w2), y);
    };

    let tell = w => (w, ());

    let listen = ((w, x)) => (w, (x, w));

    let pass = ((w, (x, f))) => (f(w), x);
  };

  include T;

  include Monads.Make_Monad_B(T);
};
