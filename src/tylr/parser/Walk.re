module Step = {
  type t = Molded.Label.t;
};

module Stride = {
  type t =
    | Eq(Molded.Sort.t)
    | Neq(Bound.t(Molded.Sort.t), Molded.Sort.t);
  let eq = sort => Eq(sort);
  let neq = (bound, sort) => Neq(bound, sort);
};

type t = Chain.t(Rel.t, Tok.t);
