module Open = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Slope.Dn.t, Slope.Up.t);
  let empty = Slope.(empty, empty);
  let cons = (~onto: Dir.t, terr: Terr.t, (dn, up)) =>
    switch (onto) {
    | L => ([terr, ...dn], up)
    | R => (dn, [terr, ...up])
    };
  let cat = ((dn', up'), (dn, up)) => Slope.(cat(dn', dn), cat(up', up));
  let face = (~side: Dir.t, (dn, up): t) =>
    Slope.face(Dir.pick(side, (dn, up)));
  let extend = (~side: Dir.t, tl, (dn, up): t) =>
    switch (side) {
    | L => Slope.extend(tl, dn) |> Option.map(dn => (dn, up))
    | R => Slope.extend(tl, up) |> Option.map(up => (dn, up))
    };
};

module Closed = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Terr.R.t, Terr.L.t);
};
