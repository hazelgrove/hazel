module type L = {
  [@deriving sexp]
  type t;

  let of_int: t => int;
  let to_int: t => int;

  let init: t;
  let next: t => t;

  let compare: (t, t) => int;
  let equal: (t, t) => bool;
};

module type S = {
  [@deriving sexp]
  type label;

  [@deriving sexp]
  type t;

  let init: t;
  let next: t => (label, t);
};

module Make: (L: L) => S with type label = L.t;

module type MonadS = {
  [@deriving sexp]
  type label;

  [@deriving sexp]
  type gen;

  include StateMonad.S with type state = gen;

  let init: gen;
  let next: t(label);
};

module Monad: (G: S) => MonadS with type label = G.label and type gen = G.t;
