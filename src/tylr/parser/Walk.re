open Util;

module Step = Molded.Label;
module Stride = {
  type t = Chain.t(Bound.t(Molded.Sort.t), unit);
  let height = Chain.length;
  let is_eq = s => height(s) == 1;
  let base = Chain.lst;
};
module Base = {
  type t = Chain.t(Stride.t, Step.t);
};
include Base;

let height = Chain.length;
let is_eq = (~prime=false, w) => List.for_all(Stride.is_eq, Chain.loops(w));
let is_neq = (~prime=false, w) => !is_eq(w);
let bound = (bound: Bound.t(Molded.Sort.t)) =>
  Chain.map_fst(Chain.link(bound, ()));

module End = {
  type t = Bound.t(Step.t);
  module Map =
    Map.Make({
      type nonrec t = t;
      let compare = compare;
    });
};
module Index = {
  include End.Map;
  type t = End.Map.t(list(Walk.t));
  let find = (_, _) => failwith("todo");
  let union = union((_, l, r) => Some(l @ r));
  let union_all = List.fold_left(union, empty);
};
