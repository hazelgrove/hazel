module type L = {
  [@deriving (sexp, eq, ord)]
  type t;

  let of_int: int => t;
  let to_int: t => int;

  let init: t;
  let next: t => t;

  let max: (t, t) => t;
};

module type S = {
  [@deriving sexp]
  type label;

  [@deriving sexp]
  type t;

  let init: t;
  let next: t => (label, t);

  let of_label: label => t;
};

module Make = (L: L) : (S with type label = L.t) => {
  [@deriving sexp]
  type label = L.t;

  [@deriving sexp]
  type t = L.t;

  let init = L.init;

  let next = gen => {
    let l = L.next(gen);
    (l, l);
  };

  let of_label = l => l;
};

module type MonadS = {
  [@deriving sexp]
  type label;

  [@deriving sexp]
  type gen;

  include Util.StateMonad.S with type state = gen;

  let init: gen;
  let next: t(label);
};

module Monad = (G: S) : (MonadS with type label = G.label and type gen = G.t) => {
  [@deriving sexp]
  type label = G.label;

  [@deriving sexp]
  type gen = G.t;

  include Util.StateMonad.Make({
    [@deriving sexp]
    type t = gen;
  });

  let init = G.init;
  let next = modify(G.next);
};
