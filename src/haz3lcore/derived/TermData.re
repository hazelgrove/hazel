open Util;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Id.Map.t(((int, int), Segment.t));

let extremes = (id: Id.t, data: t) => {
  let ((l, r), seg) = Id.Map.find(id, data);
  try((List.nth(seg, l), List.nth(seg, r))) {
  | Not_found => failwith("TermData: Invalid range")
  };
};
