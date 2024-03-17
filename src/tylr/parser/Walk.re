open Sexplib.Std;

// stance phase of gait cycle is when foot is on the ground.
// in this setting, this is when grammar walk reaches a T sym.
module Stance = Molded.T;
// swing phase of gait cycle is when foot is in the air.
// in this setting, this is when grammar walk reaches an NT sym.
module Swing = {
  // a swing is represented by a non-empty list of NTs where all
  // but the last are swung "into" (ie expanded in a grammar derivation
  // step) and the last is swung "over" to arrive at the next stance
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Chain.t(Bound.t(Molded.NT.t), unit);
  let height = Chain.length;
  let mk_eq = Chain.unit;
  let is_eq = s => height(s) == 1;
  let is_neq = s => !is_eq(s);
  let top = Chain.fst;
  let bot = Chain.lst;
  // let has_sort = s =>
  //   Chain.loops(s)
  //   |> List.exists(
  //        fun
  //        | Bound.Root
  //        | Node(Molded.{mtrl: Mtrl.Tile(_), _}) => true
  //        | Node({mtrl: Space | Grout, _}) => false,
  //      );
};
module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Chain.t(Swing.t, Stance.t);
};
include Base;

let strides = Chain.loops;
let height = w =>
  strides(w) |> List.filter(Swing.is_neq) |> List.length |> (+)(1);

// let has_sort = w => List.exists(Swing.has_sort, strides(w));

let fst = Chain.fst;
let lst = Chain.lst;

let is_eq = w => List.for_all(Swing.is_eq, Chain.loops(w));
let is_neq = w => !is_eq(w);

let singleton = Chain.unit;

let append = Chain.append;

let cons = (bound: Bound.t(Molded.NT.t)) =>
  Chain.map_fst(Chain.link(bound, ()));

module End = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Bound.t(Stance.t);
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
