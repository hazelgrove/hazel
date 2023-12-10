module Atom: {
  type t =
    | Mold(Label.t)
    | Meld(Sort.t)
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Sort.Map.t(Prec.Table.t(Regex.t(Atom.t)));
let v: t;
