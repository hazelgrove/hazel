module IntSet =
  Stdlib.Set.Make({
    type t = Bigint.t;
    let compare = Bigint.compare;
  });

module SIntSet =
  Stdlib.Set.Make({
    type t = int;
    let compare = Int.compare;
  });

module BoolSet =
  Stdlib.Set.Make({
    type t = bool;
    let compare = Bool.compare;
  });

module FloatSet =
  Stdlib.Set.Make({
    type t = float;
    let compare = Float.compare;
  });

module StringSet =
  Stdlib.Set.Make({
    type t = string;
    let compare = String.compare;
  });
