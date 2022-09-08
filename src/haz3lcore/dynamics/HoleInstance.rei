/**
  Representation of a unique hole instantiation (the set of hole instances with
  the same hole number and environment).
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = (MetaVar.t, HoleInstanceId.t);

/**
  [u_of (u, i)] is [u], where [u] is the hole metavariable.
 */
let u_of: t => MetaVar.t;

/**
  [i_of (u, i)] is [i], where [i] is the hole instance id.
 */
let i_of: t => HoleInstanceId.t;

/**
  [result] is the special instance used to represent the parent "hole instance"
  of the result; that is to say, if a hole instance has this value as its
  parent, then it is directly in the result.
 */
let result: t;
