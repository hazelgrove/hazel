// walk criteria
// - overall length
// - height
// - if there's a middle kid, whether there's a slot that accommodates

module GZipper = {
  // a zipper into a Grammar.t
  type t('subj) = {
    sort: Sort.t,
    prec: Prec.t,
    zipper: Regex.Zipper.t('subj),
  };

  let map = failwith("todo");
  let map_opt = failwith("todo");
};

module Mold = {
  [@deriving (sexp, ord)]
  type t = GZipper.t(Label.t);
  module Ord = {
    type nonrec t = t;
    let compare = compare;
  };
  module Map = Map.Make(Ord);
  module Set = Set.Make(Ord);

  // mold imposes bound on neighbors
  let bound: (Dir.t, t) => Prec.Bound.t = failwith("todo");
};

// module Slot = {
//   type t = GZipper.t(Regex.t);

//   let empty = (~sort, ~prec) => GZipper.{sort, prec, zipper: (Regex.empty, Regex.Ctx.empty)};

//   // slot is bounded by neighboring molds
//   // let bound: (Dir.t, t) => option(Prec.Bound.t) = failwith("todo");
// };

module Piece = {
  // todo rename
  type t =
    | Grout
    | Tile(Mold.t);
};

// module Walk = {
//   type t = Chain.t(Slot.t, Piece.t);
// };

module Result = {
  include Result;
  type t = Result.t(Slope.Dn.t, Meld.t);

  let merge = (l, r) =>
    switch (l, r) {
    | (Error(_), _) => r
    | (_, Error(_)) => l
    | (Ok(s_l), Ok(s_r)) => Slope.Dn.compare(s_l, s_r) <= 0 ? l : r
    };
  let merge_all = err => List.fold_left(merge, Error(err));
};

let is_operator = (operand_side: Dir.t, r: Regex.t) =>
  Regex.enter(~from=operand_side, r)
  |> List.exists(((a, _)) => Regex.Atom.is_kid(a));

// wrap terrace around kid and complement as needed
let wrap = (_terr, _kid) => failwith("todo");

module Terrace = {
  let mold = (terr: Terrace.R.t, ~kid=Meld.empty(), t: Token.t): Result.t =>
    Molds.of_token(t) |> Result.merge_all(wrap(terr, kid));
};

module Slope = {
  let rec mold = (slope: Slope.Dn.t, ~kid=Meld.empty(), t: Token.t): Result.t =>
    switch (slope.terrs) {
    | [] => Error(kid)
    | [terr, ...terrs] =>
      // todo: push slope space onto kid
      let tl_molded = mold({...slope, terrs}, ~kid=wrap(terr, kid), t);
      switch (Terrace.mold(terr, ~kid, t)) {
      | Error(_) => tl_molded
      | Ok(hd) =>
        switch (tl_molded) {
        | Ok(tl) when Slope.Dn.compare(hd, tl) > 0 => tl_molded
        | _ => Ok(Slope.Dn.cat(Slope.mk(terrs), hd))
        }
      };
    };
};
