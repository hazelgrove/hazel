// module Walk = {
//   type t = Chain.t(Slot.t, Piece.t);
// };

module Result = {
  include Result;
  type t('a) = Result.t('a, Meld.t);
  // let merge = (l, r) =>
  //   switch (l, r) {
  //   | (Error(_), _) => r
  //   | (_, Error(_)) => l
  //   | (Ok(s_l), Ok(s_r)) => Slope.Dn.compare(s_l, s_r) <= 0 ? l : r
  //   };
  // let merge_all = err => List.fold_left(merge, Error(err));
};

let is_operator = (operand_side: Dir.t, r: Regex.t) =>
  Regex.enter(~from=operand_side, r)
  |> List.exists(((a, _)) => Regex.Atom.is_kid(a));

// wrap terrace around kid and complement as needed
let wrap = (_terr, _kid) => failwith("todo");

module Terrace = {
  let mold =
      (terr: Terrace.R.t, ~kid=Meld.empty(), t: Token.t)
      : Result.t(Slope.Dn.t) =>
    Molds.of_token(t)
    |> List.map(m =>
         Walker.walk(Terrace.R.face(terr).mold)
         |> Walker.Result.filter(failwith("only walks that end with m"))
       )
    |> Walker.Result.concat
    |> Walker.Result.pick(
         ~from=terr,
         ~over=kid,
         ~to_=failwith("get the piece made of m"),
       );
};

module Slope = {
  module Dn = {
    let rec mold =
            (slope: Slope.Dn.t, ~kid=Meld.empty(), t: Token.t): Result.t => {
      let kid = Meld.pad(~l=dn.space, kid);
      switch (slope.terrs) {
      | [] => Error(kid)
      | [hd, ...tl] =>
        let tl = Slope.Dn.mk(tl);
        let tl_molded = mold(tl, ~kid=Terrace.R.unmk(hd, kid), t);
        let hd_molded = Terrace.mold(hd, ~kid, t);
        switch (hd_molded) {
        | Error(_) => tl_molded
        | Ok(hd_walk) =>
          switch (tl_molded) {
          | Ok((tl_walk, rest))
              when Slope.Dn.compare(hd_molded, tl_molded) >= 0 => tl_molded
          // todo: change Result type so that it supports ok-slope-pair
          | _ => Ok((hd_walk, tl))
          }
        };
      };
    };
    let mold = (slope, ~kid=Meld.empty(), t) =>
      mold(slope, ~kid, t)
      |> Result.map(((walk, rest)) => Slope.Dn.cat(rest, walk));
  };
};
