module Eq = {
  type t = GMtrl.Map.t(list(GWalk.Eq.t));
};

module Neq = {
  type t = GMtrl.Map.t(list(GWalk.Neq.t));
};
