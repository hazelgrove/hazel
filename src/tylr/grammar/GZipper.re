open Util;

[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('focus) = {
  sort: Sort.t,
  prec: Prec.t,
  zipper: Regex.Zipper.t('focus),
};

let mk = (~sort, ~prec, zipper) => {sort, prec, zipper};

let map = failwith("todo");
let map_opt = failwith("todo");

let enter = (~from: Dir.t, ~bound=Prec.min, s: Sort.t) =>
  Grammar.v
  |> Sort.Map.find(s)
  |> Prec.Table.map((p, a, r) =>
       r
       |> Regex.Zipper.enter(~from)
       |> List.filter(((atom, _)) =>
            Prec.gt(~a, p, bound) || Atom.is_tok(atom)
          )
       |> List.map(mk(~sort=s, ~prec=p))
     )
  |> List.concat;
