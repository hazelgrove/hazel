include GZipper;
type t = GZipper.t(Grammar.Atom.t);

let either = ({sort, prec, zipper}: t): Either.t(GMold.t, GSort.t) =>
  switch (zipper) {
  | (Tile(lbl), ctx) => Left({sort, prec, zipper: (lbl, ctx)})
  | (Meld(s), ctx) => Right({sort, prec, zipper: (s, ctx)})
  };