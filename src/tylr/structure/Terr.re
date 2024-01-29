module Base = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    cell: Cell.t,
    wald: Wald.t,
  };
};
include Base;

let sort = (terr: t) => Wald.sort(terr.wald);
let face = (terr: t) => Wald.face(terr.wald);
let cells = (terr: t) => Wald.cells(terr.wald) @ [terr.cell];

let extend = (tl, terr: t) => {...terr, wald: Wald.extend(tl, terr.wald)};

module L = {
  // L2R: wald cell
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Base.t;
};
module R = {
  // L2R: cell wald
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Base.t;
};
