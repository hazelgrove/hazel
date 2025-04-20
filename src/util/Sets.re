module IntSet =
  Set.Make({
    type t = Bigint.t;
    let compare = Bigint.compare;
  });

module SIntSet =
  Set.Make({
    type t = int;
    let compare = compare;
  });

module BoolSet =
  Set.Make({
    type t = bool;
    let compare = compare;
  });

module FloatSet =
  Set.Make({
    type t = float;
    let compare = compare;
  });

module StringSet =
  Set.Make({
    type t = string;
    let compare = compare;
  });
