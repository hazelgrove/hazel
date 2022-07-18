/**
   The type of a variable-indexed map.
 */
[@deriving sexp]
type t_('a) = list((Var.t, 'a));

/**
   [empty] is the empty map.
 */
let empty: t_('a);

/**
   [is_empty ctx] is true if [ctx] has no members.
 */
let is_empty: t_('a) => bool;

/**
   [extend ctx (x, a)] is [ctx'], where [ctx'] is [ctx] with [x] mapped to [a].
 */
let extend: (t_('a), (Var.t, 'a)) => t_('a);

/**
   [union ctx ctx'] is the map with the members of both [ctx] and [ctx'].
   Members of [ctx'] are shadowed by those of [ctx] where conflicts exist.
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
   [map f ctx] is maps [f] over the members of [ctx].
 */
let map: (((Var.t, 'a)) => 'b, t_('a)) => t_('b);

/**
   [filter f ctx] filters the members of [ctx] via [f].
 */
let filter: (((Var.t, 'a)) => bool, t_('a)) => t_('a);

/**
   [length ctx] is the number of members in [ctx].
 */
let length: t_('a) => int;

/**
   [to_list ctx] is [ctx] is a list of pairs.
 */
let to_list: t_('a) => list((Var.t, 'a));
