/* TODO: we might want to just use this API?

    https://github.com/rgrinberg/ocaml-mtl/blob/master/lib/mtl.ml

   Though it's a bit heavy, especially with the extra type parameter...

   In any case, that's a good reference. */

module MapDefinition = {
  type t('custom) =
    | Define_using_bind
    | Custom('custom);
};

module type MONAD_BASIC = {
  type t('a);
  let return: 'a => t('a);
  let bind: (t('a), 'a => t('b)) => t('b);
  let map: MapDefinition.t((t('a), 'a => 'b) => t('b));
};

module type MONAD = {
  include MONAD_BASIC;

  let map: (t('a), 'a => 'b) => t('b);
  let zip: (t('a), t('b)) => t(('a, 'b));
  let sequence: list(t('a)) => t(list('a));
  let skip: (t('a), t('b)) => t('b);
  let liftM: ('a => 'b, t('a)) => t('b);
  let ap: (t('a => 'b), t('a)) => t('b);

  module Syntax: {
    let ( let* ): (t('a), 'a => t('b)) => t('b);
    let (let+): (t('a), 'a => 'b) => t('b);
    let (and+): (t('a), t('b)) => t(('a, 'b));
  };

  module Infix: {
    let ($++): (t('a), t('b)) => t('b);
    let ($+.): (t('a), 'b) => t('b);
    let ($.+): ('a, t('b)) => t('b);
    let ($..): ('a, 'b) => t('b);
    let (@+>): (t('a), 'a => t('b)) => t('b);
    let (@>+): ('a => t('b), t('a)) => t('b);
    let (@.>): ('a, 'a => t('b)) => t('b);
    let (@>.): ('a => t('b), 'a) => t('b);
    let (^~+): (t('a => 'b), t('a)) => t('b);
    let (^+~): (t('a), t('a => 'b)) => t('b);
    let (^~.): (t('a => 'b), 'a) => t('b);
    let (^.~): ('a, t('a => 'b)) => t('b);
    let (^-+): ('a => 'b, t('a)) => t('b);
    let (^+-): (t('a), 'a => 'b) => t('b);
    let (^-.): ('a => 'b, 'a) => t('b);
    let (^.-): ('a, 'a => 'b) => t('b);
    let (+~+): (t('a => 'b), t('a)) => t('b);
    let (++~): (t('a), t('a => 'b)) => t('b);
    let (+~.): (t('a => 'b), 'a) => t('b);
    let (+.~): ('a, t('a => 'b)) => t('b);
    let (+-+): ('a => 'b, t('a)) => t('b);
    let (++-): (t('a), 'a => 'b) => t('b);
    let (+-.): ('a => 'b, 'a) => t('b);
    let (+.-): ('a, 'a => 'b) => t('b);
    let (%--): ('b => 'c, 'a => 'b, t('a)) => t('c);
    let (%-~): ('b => 'c, t('a => 'b), t('a)) => t('c);
    let (%~-): (t('b => 'c), 'a => 'b, t('a)) => t('c);
    let (%~~): (t('b => 'c), t('a => 'b), t('a)) => t('c);
    let ( **~+ ): (t('a => 'b), t('a)) => t('b);
    let ( **+~ ): (t('a), t('a => 'b)) => t('b);
    let ( **~. ): (t('a => 'b), 'a) => t('b);
    let ( **.~ ): ('a, t('a => 'b)) => t('b);
    let ( **-+ ): ('a => 'b, t('a)) => t('b);
    let ( **+- ): (t('a), 'a => 'b) => t('b);
    let ( **-. ): ('a => 'b, 'a) => t('b);
    let ( **.- ): ('a, 'a => 'b) => t('b);
  };
};

module Make = (M: MONAD_BASIC) => {
  include M;

  let map = (x, f) =>
    switch (M.map) {
    | Define_using_bind => bind(x, a => M.return(f(a)))
    | Custom(mapper) => mapper(x, f)
    };

  let zip = (x, y) => bind(x, a => bind(y, b => M.return((a, b))));

  module Syntax = {
    let ( let* ) = bind;
    let (let+) = map;
    let (and+) = zip;
  };

  let sequence = (xs: list(t('a))): t(list('a)) =>
    List.fold_right(
      (x, result) => {
        open Syntax;
        let* x = x;
        let+ xs = result;
        [x, ...xs];
      },
      xs,
      return([]),
    );

  let skip = (mx: t('a), my: t('b)): t('b) => {
    open Syntax;
    let* _ = mx;
    let+ y = my;
    y;
  };

  let liftM = (f: 'a => 'b, mx: t('a)): t('b) => {
    open Syntax;
    let+ x = mx;
    f(x);
  };

  let ap = (mf: t('a => 'b), mx: t('a)): t('b) => {
    open Syntax;
    let* f = mf;
    let+ x = mx;
    f(x);
  };

  module Infix = {
    /* sequence lifted value and lifted value */
    let ($++) = (mx, my) => skip(mx, my);

    /* sequence lifted value and ordinary value */
    let ($+.) = (mx, y) => skip(mx, return(y));

    /* sequence ordinary value and lifted value */
    let ($.+) = (x, my) => skip(return(x), my);

    /* sequence ordinary value and ordinary value */
    let ($..) = (x, y) => skip(return(x), return(y));

    /* bind lifted value */
    let (@+>) = (mx, f) => bind(mx, f);
    let (@>+) = (f, mx) => bind(mx, f);

    /* bind ordinary value */
    let (@.>) = (x, f) => bind(return(x), f);
    let (@>.) = (f, x) => bind(return(x), f);

    /* apply lifted function to lifted value (low, right) */
    let (^~+) = (mf, mx) => ap(mf, mx);
    let (^+~) = (mx, mf) => ap(mf, mx);

    /* apply lifted function to ordinary value (low, right) */
    let (^~.) = (mf, x) => ap(mf, return(x));
    let (^.~) = (x, mf) => ap(mf, return(x));

    /* apply ordinary function to lifted value (low, right) */
    let (^-+) = (f, mx) => map(mx, f);
    let (^+-) = (mx, f) => map(mx, f);

    /* apply ordinary function to ordinary value (low, right) */
    let (^-.) = (f, x) => return(f(x));
    let (^.-) = (x, f) => return(f(x));

    /* apply lifted function to lifted value (mid, left) */
    let (+~+) = (mf, mx) => ap(mf, mx);
    let (++~) = (mx, mf) => ap(mf, mx);

    /* apply lifted function to ordinary value (mid, left) */
    let (+~.) = (mf, x) => ap(mf, return(x));
    let (+.~) = (x, mf) => ap(mf, return(x));

    /* apply ordinary function to lifted value (mid, left) */
    let (+-+) = (f, mx) => map(mx, f);
    let (++-) = (mx, f) => map(mx, f);

    /* apply ordinary function to ordinary value (mid, left) */
    let (+-.) = (f, x) => return(f(x));
    let (+.-) = (x, f) => return(f(x));

    /* compose ordinary function with ordinary function */
    let (%--) = (g, f, mx) => map(mx, x => g(f(x)));

    /* compose ordinary function with lifted function */
    let (%-~) = (g, mf, mx) => map(ap(mf, mx), g);

    /* compose lifted function with ordinary function */
    let (%~-) = (mg, f, mx) => ap(mg, map(mx, f));

    /* compose lifted function with lifted function */
    let (%~~) = (mg, mf, mx) => ap(mg, ap(mf, mx));

    /* apply lifted function to lifted value (high, left) */
    let ( **~+ ) = (mf, mx) => ap(mf, mx);
    let ( **+~ ) = (mx, mf) => ap(mf, mx);

    /* apply lifted function to ordinary value (high, left) */
    let ( **~. ) = (mf, x) => ap(mf, return(x));
    let ( **.~ ) = (x, mf) => ap(mf, return(x));

    /* apply ordinary function to lifted value (high, left) */
    let ( **-+ ) = (f, mx) => map(mx, f);
    let ( **+- ) = (mx, f) => map(mx, f);

    /* apply ordinary function to ordinary value (high, left) */
    let ( **-. ) = (f, x) => return(f(x));
    let ( **.- ) = (x, f) => return(f(x));
  };
};

/* State Monad */

module type STATE_MONAD_BASIC = {
  type state;
  include MONAD_BASIC with type t('result) = state => ('result, state);
};

module type STATE_MONAD = {
  include STATE_MONAD_BASIC;
  include MONAD with type t('result) = state => ('result, state);

  let run: (t('result), state) => ('result, state);
  let eval: (t('result), state) => 'result;
  let exec: (t('result), state) => state;

  let get: unit => t(state);
  let put: state => t(unit);
};

module State = {
  module Make = (M: STATE_MONAD_BASIC) => {
    include M;
    include Make(M);

    let run = (m: t('result), s: state): ('result, state) => m(s);
    let eval = (m: t('result), s: state): 'result => s |> run(m) |> fst;
    let exec = (m: t('result), s: state): state => s |> run(m) |> snd;

    let get = (): t(state) => s => (s, s);
    let put = (s: state): t(unit) => _ => ((), s);
  };
};
