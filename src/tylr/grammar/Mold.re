include GZipper;
[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = GZipper.t(Label.t);

let of_atom = (z: GZipper.t(Atom.t)) =>
  switch (z.zipper) {
  | (Tok(lbl), _) => Ok(GZipper.put(lbl, z))
  | (Kid(s), _) => Error(s)
  };

module Map =
  Map.Make({
    type nonrec t = t;
    let compare = compare;
  });

let sort_ = m => m.sort;
let prec_ = m => m.prec;

let label = GZipper.focus;

// let mk = (~ctx=Regex.Ctx.empty, sort, prec, lbl) => {
//   sort,
//   prec,
//   zipper: (lbl, ctx),
// };
