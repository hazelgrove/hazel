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
  [@warning "-27"]
  let pick = (~compare: ('a, 'a) => int, rs: list(t('a))): Result.t =>
    failwith("todo Molder.Result.pick");
};

let is_operator = (operand_side: Dir.t, r: Regex.t) =>
  Regex.enter(~from=operand_side, r)
  |> List.exists(((a, _)) => Regex.Atom.is_kid(a));

// wrap terrace around kid and complement as needed
let wrap = (_terr, _kid) => failwith("todo");

module Terrace = {
  let rec mold =
          (~eq_only=false, terr: Terrace.R.t, ~kid=Meld.empty(), t: Token.t)
          : Result.t(Slope.Dn.t) => {
    let hd_molded =
      Molds.of_token(t)
      |> List.map(m =>
           Walker.walk(Terrace.R.face(terr).mold)
           |> (eq_only ? Walker.Result.eq_only : Fun.id)
           |> Walker.Result.filter(failwith("only walks that end with m"))
         )
      |> Walker.Result.concat
      |> Walker.Result.pick(
           ~from=terr,
           ~over=kid,
           ~to_=failwith("get the piece made of m"),
         );
    let tl_molded = {
      switch (Terrace.R.unlink(terr)) {
      | Some((terr, kid', {mold: Tile(mold), _} as p))
          when !Piece.is_complete(p) =>
        let mold = Mold.grout_of_tile(mold);
        let grout = {...p, mold: Grout(mold)};
        // todo: prune away unnecessary prefix/postfix grout
        let kid = Meld.of_piece(~l=kid', grout, ~r=kid);
        mold(~eq_only=true, terr, ~kid, t);
      | _ => Error(Terrace.R.unmk(terr, kid))
      };
    };
    Result.pick(~compare=Slope.Dn.compare, hd_molded, tl_molded);
  };
};

module Slope = {
  module Dn = {
    let rec mold =
            (slope: Slope.Dn.t, ~kid=Meld.empty(), t: Token.t)
            : Result.t((Slope.Dn.t, Slope.Dn.t)) => {
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
          | Ok((rest, tl_walk)) when Slope.Dn.compare(hd_walk, tl_walk) >= 0 => tl_molded
          | _ => Ok((tl, hd_walk))
          }
        };
      };
    };
    let mold = (slope, ~kid=Meld.empty(), t) =>
      mold(slope, ~kid, t)
      |> Result.map(((rest, walk)) => Slope.Dn.cat(rest, walk));
  };
};
