exception Empty_slope;

let mold =
    (~eq_only, m: Mold.t, ~kid: option(Material.t(Sort.t))=?, t: Token.t)
    : option(Slope.Dn.m) =>
  Molds.of_token(t)
  |> List.map(n =>
       Walker.step(m)
       |> (eq_only ? Walker.Result.eq_only : Fun.id)
       |> Walker.Result.filter(failwith("only walks that end with n"))
     )
  |> Walker.Result.concat
  |> Walker.Result.pick(
       ~from=m,
       ~over=?kid,
       ~to_=failwith("get the piece made of n"),
     );

// let is_operator = (operand_side: Dir.t, r: Regex.t) =>
//   Regex.enter(~from=operand_side, r)
//   |> List.exists(((a, _)) => Regex.Atom.is_kid(a));

// wrap terrace around kid and complement as needed
// let wrap = (_terr, _kid) => failwith("todo");

// replace ghost with piece above bridge
// let x = 1 >in< x + 1
// let x = 1 >in< x + 1 [in]
// let x = 1 >< x + 1 [in]
// let x = 1 >< x + 1 in <>

// replace ghost with piece under bridge
// let x = 1 + 2 >in< x + 1
// let x = 1 [in] + 2 >in< x + 1
//
// let x = 1 in <> + 2 >< x + 1

// replacing even solid bridges?
// let x = 1 + 2 in x + 1
// let x = 1 [in] + 2 in x + 1
//
// let x = 1 in <> + 2 >in< x + 1
// or
// let x = 1 in <> + 2 >< <in> >< x + 1

module Result = {
  include Result;
  type t = Result.t(Slope.Dn.m, option(Material.sorted));

  // if both error, then return r
  let pick = (l, r) =>
    switch (l, r) {
    | (Error(_), _) => r
    | (_, Error(_)) => l
    | (Ok(slope_l), Ok(slope_r)) =>
      Slope.Dn.compare(slope_l, slope_r) <= 0 ? l : r
    };
};

module Terrace = {
  include Terrace;
  let rec mold =
          (
            ~eq_only=false,
            terr: R.p,
            ~kid: option(Material.sorted)=?,
            t: Token.t,
          )
          : Result.t => {
    let hd_molded = mold(~eq_only, R.face(terr).mold, ~kid?, t);
    let tl_molded =
      switch (R.unlink(terr)) {
      | Some((terr, kid', {matter: Tile(_), _} as p))
          when !Piece.is_complete(p) =>
        // let mold = Mold.grout_of_tile(mold);
        // let grout = {...p, mold: Grout(mold)};
        // todo: prune away unnecessary prefix/postfix grout
        // let kid = Meld.of_piece(~l=kid', grout, ~r=kid);
        mold(~eq_only=true, terr, ~kid=Grout, t)
      | _ => Error(R.unmk(terr, kid))
      };
    Result.pick(hd_molded, tl_molded);
  };
};

module Slope = {
  include Slope;
  let rec mold =
          (slope: Dn.p, ~kid: option(Material.sorted)=?, t: Token.t)
          : Result.t =>
    switch (slope.terrs) {
    | [] => Error(kid)
    | [hd, ...tl] =>
      let hd_molded = Terrace.mold(hd, ~kid?, t);
      let tl_molded =
        mold(Slope.Dn.mk(tl), ~kid=Terrace.R.unmk(hd, kid), t);
      Result.pick(hd_molded, tl_molded);
    };
};

// let slopes_mold =
//     ((pre, suf): Slopes.t, ~kid=Meld.empty(), t)
//     : Result.t(Slopes.t, (Meld.t, Slope.Up.t)) =>
//   slope_mold(pre, ~kid, t)
//   |> Result.map(((rest, walk)) => (Slope.Dn.cat(rest, walk), suf))
//   |> Result.map_error(kid => (kid, suf));

module Stepwell = {
  let rec mold =
          (well: Stepwell.t, ~kid: option(Material.sorted)=?, t: Token.t)
          : Result.t => {
    let (pre, _) = Stepwell.get_slopes(well);
    switch (Slope.mold(pre, ~kid?, t)) {
    | Ok(_) as r => r
    | Error(kid) as r =>
      switch (Stepwell.unlink(well)) {
      | None => r
      | Some((_, (l, _r), _well)) => Terrace.mold(l, ~kid, t)
      }
    };
  };

  let mold = (well: Stepwell.t, t: Token.t): option(Mold.t) =>
    mold(well, t)
    |> Result.to_option
    |> Option.map(Slope.Dn.face)
    |> Option.map(OptUtil.get_or_raise(Empty_slope));
};
