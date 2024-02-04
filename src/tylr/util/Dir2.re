// 2d compass directions
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | U // north
  | R // east
  | D // south
  | L; // west
