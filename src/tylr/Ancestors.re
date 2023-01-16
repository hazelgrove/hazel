open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(Generation.t);

let empty = [];

let pop = _ => failwith("todo");

let rec zip = (kid: Chain.Padded.t, anc: t) =>
  switch (anc) {
  | [] => kid
  | [(par, sib), ...anc] =>
    let kid = Parent.zip(kid, par);
    let kid = Siblings.zip(~sel=Segment.of_chain(kid), sib);
    zip(kid, anc);
  };
