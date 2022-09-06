/**
  Auxiliary data structure for constructing a {!type:HoleInstanceInfo.t}.
 */

/* FIXME: Make this abstract. */
[@deriving sexp]
type t;

/**
  [empty] is the empty info map.
 */
let empty: t;

/**
  [add_instance hii u env] binds a unique hole instance id for the
  [(u, env)] pair representing a hole instance, assocating it in [hii] and
  returning [(map', i)], where [map'] is the augmented [map] and [i] is the
  hole instance id.

  If the pair already exists in [hii], the existing id is returned as [i];
  otherwise, a unique id is assigned and returned as [i].
 */
let add_instance:
  (t, MetaVar.t, ClosureEnvironment.t) => (t, HoleInstanceId.t);

/**
  [to_hole_instance_info hii] converts [hii] into {!type:HoleInstanceInfo.t}.
 */
let to_hole_instance_info: t => HoleInstanceInfo.t;
