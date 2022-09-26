open Sexplib.Std;

// abababa (a = whitespace/grout/comment (possibly empty)) (b = tile)
[@deriving (show({with_path: false}), sexp, yojson)]
type segment = list(piece)
and piece =
  | Tile(tile)
  | Grout(Grout.t)
  | Whitespace(Whitespace.t)
// example: [let x = 1 in] x
and tile = {
  // invariants:
  // - length(mold.in_) + 1 == length(label)
  // - length(shards) <= length(label)
  // - length(shards) == length(children) + 1
  // - sort(shards) == shards
  id: Id.t,
  label: Label.t, // ["let", "=", "in"]
  mold: Mold.t, // {out: Exp, in_: [Pat, Exp]}
  shards: list(int), // [0, 1, 2]
  children: list(segment) // [[x], [1]]
} /* let x = 1 # this is a comment # in */;

// This is for comment insertion
let mk_whitespace = (id, content) => [Whitespace({id, content})] /* ADDED */;
