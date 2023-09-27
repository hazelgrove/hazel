open Sexplib.Std;

include Meld;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = EPath.Marked.t(Meld.t(t, Piece.t));

// let mk = EPath.Marked.mk;

let lift_path = (n: int, slot: Slot.t(t)) =>
  switch (slot) {
  | Empty => (None, slot)
  | Full((path, m)) => (Option.map(Path.cons(n), path), Full((None, m)))
  };

let mk = (~l=Slot.Empty, ~r=Slot.Empty, (ps, slots): EWald.t) => {
  let (path_l, l) = lift_path(0, l);
  let (path_r, r) = lift_path(1 + List.length(slots), r);
  let (paths, slots) =
    slots |> List.mapi(i => lift_path(1 + i)) |> List.split;
  let path = OptUtil.exists([path_l] @ paths @ [path_r]);
  (path, M(l, Wald.mk(ps, slots), r));
};
