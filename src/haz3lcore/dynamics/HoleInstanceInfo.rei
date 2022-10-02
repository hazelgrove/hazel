/**
  Stores information about all hole instances reachable by a program's
  evaluation result. Used in the context inspector.

  Constructed using {!val:HoleInstanceInfo_.to_hole_instance_info}.
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = MetaVarMap.t(list((ClosureEnvironment.t, HoleInstanceParents.t)));

/**
  [empty] is the empty info map.
 */
let empty: t;

/**
  [num_unique_his hii u] is the number of unique hole instances for a given
  hole (given by the id [u]).
 */
let num_instances: (t, MetaVar.t) => int;

/**
  [find_instance hii u i] is the information for the given hole and hole
  instance id, if found.
 */
let find_instance:
  (t, MetaVar.t, HoleInstanceId.t) =>
  option((ClosureEnvironment.t, HoleInstanceParents.t));

/**
  [add_parent (u, i) hip hii] adds the parent [hip] to the hole given by [(u,
  i)]. Assumes both the parent and the hole exist in [hii].
 */
let add_parent: (HoleInstance.t, HoleInstanceParents.t_, t) => t;

let fast_equal: (t, t) => bool;
