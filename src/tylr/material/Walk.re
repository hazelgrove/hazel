module Step = {
  include Molded.Sym;
  type t = Molded.Sym.t;

  module Ord = {
    type nonrec t = t;
    let compare = compare;
  };
  module Map = Map.Make(Ord);
};

// >< | _ ><      |
//    |      _ >< |
//    |           | <>
module Base = {
  type t = Chain.t(list(Step.t), unit);
};
include Base;

module Set = {
  type t = Step.Map.t(list(Base.t));
  let empty = Step.Map.empty;
  let mk = (elems: list(Base.t)) => failwith("todo");
  let single = elem => mk([elem]);
  let union_all = _ => failwith("todo");

  let bind = (walked: t, f: Base.t => t) =>
    {
      open ListUtil.Syntax;
      let* (_, ws) = Step.Map.bindings(walked);
      let* w = ws;
      f(w);
    }
    |> union_all;

  module Syntax = {
    let return = single;
    let ( let* ) = bind;
  };
};
