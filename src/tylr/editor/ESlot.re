include Slot;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Slot.t(EMeld.t);

let clear =
  fun
  | Empty => []
  | Full((_, M(l, w, r))) => clear(l) @ clear_wald(w) @ clear(r)
and clear_wald = w |> Chain.to_list(Piece.clear, clear) |> List.concat;

let has_no_tiles =
  fun
  | Empty => Some("")
  | Full(m) => EMeld.has_no_tiles(m);
