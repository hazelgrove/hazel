/**
   The type of a variable-indexed map backed by a binary search tree.
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type t_('a);

/**
   [empty] is the empty map.
 */
let empty: t_('a);

/**
   [is_empty ctx] is true if [ctx] has no bindings.
 */
let is_empty: t_('a) => bool;

/**
   [singleton (x, a)] is the map containing a single binding [a] for [x].
 */
let singleton: ((Var.t, 'a)) => t_('a);

/**
   [extend ctx (x, a)] is [ctx'], where [ctx'] is [ctx] with [x] mapped to [a].
 */
let extend: (t_('a), (Var.t, 'a)) => t_('a);

/**
   [union ctx ctx'] is the map with the bindings of both [ctx] and [ctx'].
   Bindings of [ctx'] are shadowed by those of [ctx] where conflicts exist.
 */
let union: (t_('a), t_('a)) => t_('a);

/**
   [lookup ctx x] is [Some(a)] if [x] is mapped in [ctx] and [None] otherwise.
 */
let lookup: (t_('a), Var.t) => option('a);

/**
   [contains ctx x] is [true] if [x] is mapped in [ctx].
 */
let contains: (t_('a), Var.t) => bool;

/**
   [map f ctx] is maps [f] over the bindings of [ctx].
 */
let map: (((Var.t, 'a)) => 'b, t_('a)) => t_('b);

/**
   [filter f ctx] filters the bindings of [ctx] via [f].
 */
let filter: (((Var.t, 'a)) => bool, t_('a)) => t_('a);

/**
   [fold f init ctx] folds the bindings of [ctx] via [f] into [init].
 */
let fold: (((Var.t, 'a), 'b) => 'b, 'b, t_('a)) => 'b;

/**
   [length ctx] is the number of bindings in [ctx].
 */
let length: t_('a) => int;

/**
   [to_list ctx] is the list of bindings.
 */
let to_list: t_('a) => list((Var.t, 'a));

/**
  [of_list bindings] is the map given by the list of bindings.
 */
let of_list: list((Var.t, 'a)) => t_('a);

/**

 */
module Ordered: {
  /**
    The type of a variable-indexed map backed by a binary search tree with
    insertion order tracking.
   */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t_('a);

  /**
    [empty] is the empty map.
   */
  let empty: t_('a);

  /**
    [is_empty ctx] is true if [ctx] has no bindings.
   */
  let is_empty: t_('a) => bool;

  /**
    [singleton (x, a)] is the map containing a single binding [a] for [x].
   */
  let singleton: ((Var.t, 'a)) => t_('a);

  /**
    [extend ctx (x, a)] is [ctx'], where [ctx'] is [ctx] with [x] mapped to [a].
   */
  let extend: (t_('a), (Var.t, 'a)) => t_('a);

  /**
    [union ctx ctx'] is the map with the bindings of both [ctx] and [ctx'].
    Bindings of [ctx'] are shadowed by those of [ctx] where conflicts exist.
   */
  let union: (t_('a), t_('a)) => t_('a);

  /**
    [lookup ctx x] is [Some(a)] if [x] is mapped in [ctx] and [None] otherwise.
   */
  let lookup: (t_('a), Var.t) => option('a);

  /**
    [contains ctx x] is [true] if [x] is mapped in [ctx].
   */
  let contains: (t_('a), Var.t) => bool;

  /**
    [mapk f ctx] is maps [f] over the bindings of [ctx] in key order.
   */
  let mapk: (((Var.t, 'a)) => 'b, t_('a)) => t_('b);

  /**
    [mapo f ctx] is maps [f] over the bindings of [ctx] in insert order.
   */
  let mapo: (((Var.t, 'a)) => 'b, t_('a)) => t_('b);

  /**
    [filterk f ctx] filters the bindings of [ctx] via [f] in key order.
   */
  let filterk: (((Var.t, 'a)) => bool, t_('a)) => t_('a);

  /**
    [filtero f ctx] filters the bindings of [ctx] via [f] in insert order.
   */
  let filtero: (((Var.t, 'a)) => bool, t_('a)) => t_('a);

  /**
    [foldk f init ctx] folds the bindings of [ctx] via [f] into [init] in key
    order.
   */
  let foldk: (((Var.t, 'a), 'b) => 'b, 'b, t_('a)) => 'b;

  /**
    [foldo f init ctx] folds the bindings of [ctx] via [f] into [init] in
    insert order.
   */
  let foldo: (((Var.t, 'a), 'b) => 'b, 'b, t_('a)) => 'b;

  /**
    [length ctx] is the number of bindings in [ctx].
   */
  let length: t_('a) => int;

  /**
    [to_list ctx] is the list of bindings in key order.
   */
  let to_listk: t_('a) => list((Var.t, 'a));

  /**
    [to_list ctx] is the list of bindings in insert order.
   */
  let to_listo: t_('a) => list((Var.t, 'a));

  /**
    [of_list bindings] is the map given by the list of bindings, with insertion
    order given by element order.
   */
  let of_list: list((Var.t, 'a)) => t_('a);
};
