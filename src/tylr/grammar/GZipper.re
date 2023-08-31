[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('focus) = {
  sort: Sort.t,
  prec: Prec.t,
  zipper: Regex.Zipper.t('focus),
};

let map = failwith("todo");
let map_opt = failwith("todo");

[@warning "-27"]
let enter = (~from: Dir.t, ~bound=Prec.min, s: Sort.t) =>
  Grammar.v
  |> Sort.Map.find(s)
  |> Prec.Table.map((p, a, r) =>
    Regex.Zipper.enter(~from=)
  )

