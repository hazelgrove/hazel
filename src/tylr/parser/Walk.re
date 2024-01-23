open Util;

module Step = Molded.Label;
module Stride = {
  open Sexplib.Std;
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Chain.t(Bound.t(Molded.Sort.t), unit);
  let height = Chain.length;
  let eq = Chain.unit;
  let is_eq = s => height(s) == 1;
  let base = Chain.lst;
};
module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Chain.t(Stride.t, Step.t);
};
include Base;

let height = Chain.length;

let fst = Chain.fst;
let lst = Chain.lst;

let is_eq = w => List.for_all(Stride.is_eq, Chain.loops(w));
let is_neq = w => !is_eq(w);

let singleton = Chain.unit;

let append = Chain.append;

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
  let map = f => map(List.map(f));
  let union: (t, t) => t = union((_, l, r) => Some(l @ r));
  let union_all: list(t) => t = List.fold_left(union, empty);
  let to_list = bindings;
  let of_list = bs => of_seq(List.to_seq(bs));
  module Syntax = {
    let return = (walk, dst) => singleton(dst, [walk]);
    let ( let* ) = (ind, f) =>
      to_list(ind)
      |> List.concat_map(((dst, walks)) =>
           walks |> List.map(w => (w, dst))
         )
      |> List.map(f)
      |> union_all;
  };
};
