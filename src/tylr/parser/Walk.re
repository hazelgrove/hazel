module Step = {
  include Molded.Sym;
  type t = Molded.Sym.t;

  module Ord = {
    type nonrec t = t;
    let compare = compare;
  };
  module Map = Map.Make(Ord);
};

module Enter = Molded.Sort.t;

// >< | _ ><      |
//    |      _ >< |
//    |           | <>
module Base = {
  type t = Chain.t(list(Step.t), Enter.t);
  let empty = Chain.of_loop([]);
};
include Base;

let append_enter = (enter, w) => Chain.knil(w, enter, []);

module Set = {
  // walks indexed by destination
  type t = Step.Map.t(list(Base.t));
  let empty = Step.Map.empty;
  // let mk = (elems: list(Base.t)) => failwith("todo");
  // let single = elem => mk([elem]);
  let single = (dst, walk) => Step.Map.singleton(dst, [walk]);
  let union_all = _ => failwith("todo");

  let init = (first_steps: list(Step.t)): t =>
    first_steps
    |> List.map(step => (step, [Base.empty]))
    |> Step.Map.of_list;

  let bind = (walked: t, f: Base.t => t) =>
    {
      open ListUtil.Syntax;
      let* (dst, walks) = Step.Map.bindings(walked);
      let* w = walks;
      f(dst, w);
    }
    |> union_all;

  module Syntax = {
    // let return = single;
    let ( let* ) = bind;
  };
};
