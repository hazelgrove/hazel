let mold =
    (~eq_only, m: Mold.t, ~kid: option(Matter.t(Sort.t))=?, t: Token.t)
    : option(Slope.Dn.t(Mold.t)) =>
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

module Terrace = {
  include Terrace;
  let rec mold =
          (
            ~eq_only=false,
            terr: R.p,
            ~kid: option(Matter.s)=?,
            t: Token.t,
          )
          : Result.t(Mold.t, Matter.s) => {
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
    Result.pick(~compare=Slope.Dn.compare, hd_molded, tl_molded);
  };
};

module Slope = {
  include Slope;
  let rec mold =
          (slope: Dn.p, ~kid: option(Matter.s)=?, t: Token.t)
          : Result.t((Dn.p, Slope.Dn.t), Meld.t) => {
    let kid = Meld.pad(~l=dn.space, kid);
    switch (slope.terrs) {
    | [] => Error(kid)
    | [hd, ...tl] =>
      let tl = Slope.Dn.mk(tl);
      let tl_molded = slope_mold(tl, ~kid=Terrace.R.unmk(hd, kid), t);
      let hd_molded = terrace_mold(hd, ~kid, t);
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
};

let slopes_mold =
    ((pre, suf): Slopes.t, ~kid=Meld.empty(), t)
    : Result.t(Slopes.t, (Meld.t, Slope.Up.t)) =>
  slope_mold(pre, ~kid, t)
  |> Result.map(((rest, walk)) => (Slope.Dn.cat(rest, walk), suf))
  |> Result.map_error(kid => (kid, suf));

let rec stepwell_mold =
        (well: Stepwell.t, ~kid=Meld.empty(), t: Token.t)
        : Result.t(Stepwell.t, (Meld.t, Stepwell.t)) => {
  switch (slopes_mold(Stepwell.get_slopes(well), ~kid, t)) {
  | Ok(slopes) => Stepwell.put_slopes(slopes, well)
  | Error((kid, suf)) =>
    switch (Stepwell.unlink(well)) {
    | None =>
      let well = Stepwell.put_slopes(Slopes.mk(~r=suf, ()), well);
      Error((kid, well));
    | Some((_, (l, r) as b, well)) =>
      switch (terrace_mold(l, ~kid, t)) {
      | Error(_) =>
        // may need to revisit this
        // may want to break bridges with ghosts
        let well = Stepwell.link(Slopes.mk(~r=suf, ()), b, well);
        Error((kid, well));
      | Ok(slope) =>
        // slope contains l kid t
        // left to right: slope suf r
        assert(Slope.height(slope) > 0);
        // hd contains l
        let (hd, tl) = Option.get(Slope.Dn.uncons(slope));
        if (Slope.height(tl) > 0) {
          // l lt-molded t and remains matched to r
          let well = Stepwell.link((tl, suf), (hd, r), well);
          Ok(well);
        } else {
          // l eq-molded t
          // r now unmatched and left in suffix for suffix remolding
          let suf = Slope.Up.snoc(suf, r);
          Ok(Stepwell.cons_slopes((slope, suf), well));
        };
      }
    }
  };
};
