include Slot;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Slot.t(EMeld.t);

let has_no_tiles =
  fun
  | Empty => Some("")
  | Full(m) => EMeld.has_no_tiles(m);
