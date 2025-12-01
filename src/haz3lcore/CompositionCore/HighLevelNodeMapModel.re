open Util;
open Language;
open Language.Statics;

[@deriving (show({with_path: false}), sexp, yojson)]
type node = {
  // The term associated with this node
  info: Info.t,
  // The high-level path to this node from the root of the AST, as a list of their ids
  // Ordering: the first element is the root of the AST, and the last element is this node
  path: list(Id.t),
  // Ids of the outgoing children nodes in the AST
  children: list(Id.t),
  // Ids of the outgoing sibling nodes in the AST
  siblings: list(Id.t),
  // Index of this node in the list of siblings
  sibling_idx: int,
  // The name of this node. Constructed through recursively
  // unwrapping the pattern(s) associated with the node
  name: string,
  // ...
  // May optionally choose to store more metadata about terms here, such as type, probes, etc.
  // ...
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Id.Map.t(node);
