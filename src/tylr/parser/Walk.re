open Util;

module Step = Molded.Label;
module Stride = {
  open Sexplib.Std;
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Chain.t(Bound.t(Molded.Sort.t), unit);
  let height = Chain.length;
  let is_eq = s => height(s) == 1;
  let base = Chain.lst;
};
module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Chain.t(Stride.t, Step.t);
};
include Base;

let height = Chain.length;
let is_eq = w => List.for_all(Stride.is_eq, Chain.loops(w));
let is_neq = w => !is_eq(w);
let bound = (bound: Bound.t(Molded.Sort.t)) =>
  Chain.map_fst(Chain.link(bound, ()));

module End = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Bound.t(Step.t);
  module Map =
    Map.Make({
      type nonrec t = t;
      let compare = compare;
    });
};
module Index = {
  include End.Map;
  type t = End.Map.t(list(Base.t));
  let find = (_end: End.t, _map: t) => failwith("todo");
  let union: (t, t) => t = union((_, l, r) => Some(l @ r));
  let union_all: list(t) => t = List.fold_left(union, empty);
};
