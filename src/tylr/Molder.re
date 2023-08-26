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
  type t('err) = Result.t(Ziggurat.m, 'err);
  // prioritizes second arg error
  let pick = (l, r) =>
    switch (l, r) {
    | (Error(_), _) => r
    | (Ok(_), Error(_)) => l
    | (Ok(l), Ok(r)) => Ok(Scorer.compare(l, r) <= 0 ? l : r)
    };
};

module Piece = {
  let mold = ({mold, token, _}: Piece.t, ~kid=?, t: Token.t): Result.t(unit) => {
    let molder = (mold, token);
    Molds.of_token(t)
    |> List.filter_map(m => {
         let candidate = (m, t);
         Comparator.cmp(molder, ~kid?, candidate);
       })
    |> List.stable_sort(Scorer.compare)
    |> ListUtil.hd_opt
    |> Result.of_option(~error=());
  };
};

module Terrace = {
  include Terrace;
  let rec mold = (terr: R.p, ~kid=None, t: Token.t): Result.t(Kid.Profile.t) => {
    let err =
      Terrace.profile(terr) |> Kid.Profile.add_tokens(kid.has_tokens);
    let hd_molded =
      Piece.mold(R.face(terr), ~kid, t) |> Result.map_error(() => err);
    let tl_molded =
      switch (R.unlink(terr)) {
      | Some((terr, k, p)) when !Piece.has_token(p) =>
        open // let mold = Mold.grout_of_tile(mold);
             // let grout = {...p, mold: Grout(mold)};
             // todo: prune away unnecessary prefix/postfix grout
             // let kid = Meld.of_piece(~l=kid', grout, ~r=kid);
             Result.Syntax;
        let kid = Kid.(Profile.merge(profile(k), kid));
        let* z = mold(terr, ~kid, t);
        // take only eq molds from tl
        Ziggurat.is_singleton(z) ? Ok(z) : Error(err);
      | _ => Error(err)
      };
    Result.pick(hd_molded, tl_molded);
  };
};

module Slope = {
  include Slope;
  let rec mold = (dn: Dn.p, ~kid=None, t: Token.t): Result.t(Kid.Profile.t) =>
    switch (dn) {
    | [] => Error(kid)
    | [hd, ...tl] =>
      let hd_molded = Terrace.mold(hd, ~kid, t);
      let kid =
        Terrace.profile(hd) |> Kid.Profile.add_tokens(kid.has_tokens);
      let tl_molded = mold(tl, ~kid, t);
      Result.pick(hd_molded, tl_molded);
    };
};

module Ziggurat = {
  include Ziggurat;
  let mold = (zigg: Ziggurat.p, t: Token.t): Mold.t =>
    switch (Slope.mold(dn, t)) {
    | Ok(z) => Piece.mold(Ziggurat.face(R, z))
    | Error(kid) =>
      switch (Terrace.(mold(mk(zigg.top), ~kid, t))) {
      | Ok(z) => Piece.mold(Ziggurat.face(R, z))
      | Error(_) => default(t)
      }
    };
};

module Stepwell = {
  include Stepwell;
  let mold = (well: Stepwell.t, ~kid=None, t: Token.t): Mold.t => {
    let top =
      switch (unlink(well)) {
      | Error(_) => Wald.root
      | Ok((_, (l, _), _)) => l
      };
    let (dn, _) = get_slopes(well);
    Ziggurat.(mold(mk(top, ~dn), t));
  };
};

let mold = Stepwell.mold;
