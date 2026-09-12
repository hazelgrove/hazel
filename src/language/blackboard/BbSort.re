/**
  The sort of Blackboard terms.

  Blackboard's core logic has one syntactic class, so [Term] is the only
  sort the grammar and the molds ever use. [Assumed] and [Constructed] are
  display-only refinements: statics knows which block a node sits in, and
  [Info.refine_sort_from_mold] hands that to the code view so the editor can
  tint a block by what it owes. They are deliberately absent from [all], so
  nothing tries to mold or remold in them.

  This mirrors DrvSort, where statics likewise refines a single molding sort
  into the finer sorts the display wants.
 */

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type t =
  | Term
  | Assumed
  | Constructed;

let all = [Term];

let class_of =
  fun
  | Term => "Bb"
  | Assumed => "BbAssume"
  | Constructed => "BbConstruct";

let to_string =
  fun
  | Term => "BbTerm"
  | Assumed => "BbAssumed"
  | Constructed => "BbConstructed";

let to_string_short =
  fun
  | Term => "Blackboard"
  | Assumed => "Assumed"
  | Constructed => "Constructed";
