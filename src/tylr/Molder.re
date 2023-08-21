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
  type t('err) = Result.t((Mold.t, int), 'err);
  // prioritizes second arg error
  let pick = (l, r) =>
    switch (l, r) {
    | (Error(_), Error(_) | Ok(_)) => r
    | (Ok(_), Error(_)) => l
    | (Ok((_, s_l)), Ok((_, s_r))) => Score.compare(s_l, s_r) <= 0 ? l : r
    };
};

module Piece = {
  let mold = ({mold, token, _}: Piece.t, ~kid=?, t: Token.t): Result.t(unit) => {
    let molder = (mold, token);
    Molds.of_token(t)
    |> List.filter_map(m => {
         let candidate = (m, t);
         switch (Comparator.cmp(molder, ~kid?, candidate)) {
         | None
         | Some(Gt(_)) => Error
         | Some(Eq(w)) => Ok((m, Scorer.score(Slope.mk(Terrace.mk(w)))))
         | Some(Lt(s)) => Ok((m, Scorer.score(s)))
         };
       })
    |> List.hd_opt;
  };
};

module Terrace = {
  include Terrace;
  let rec mold =
          (~eq_only=false, terr: R.p, ~kid=None, t: Token.t)
          : Result.t(Kid.Profile.t) => {
    let hd_molded =
      Piece.mold(~eq_only, R.face(terr), ~kid, t)
      |> Result.of_option(
           ~error=
             Kid.Profile.mk(
               ~has_tokens=
                 !Terrace.has_tokens(terr) || Kid.Profile.has_tokens(kid),
               Terrace.sort(terr),
             ),
         );
    let tl_molded =
      switch (R.unlink(terr)) {
      | Some((terr, kid', p)) when !Piece.has_token(p) =>
        // let mold = Mold.grout_of_tile(mold);
        // let grout = {...p, mold: Grout(mold)};
        // todo: prune away unnecessary prefix/postfix grout
        // let kid = Meld.of_piece(~l=kid', grout, ~r=kid);
        let kid = Kid.Profile.merge(kid', kid);
        mold(~eq_only=true, terr, ~kid, t);
      | _ => None
      };
    Result.pick(hd_molded, tl_molded);
  };
};

module Slope = {
  include Slope;
  let rec mold =
          (slope: Dn.p, ~kid=None, t: Token.t): Result.t(Kid.Profile.t) =>
    switch (slope.terrs) {
    | [] => Error(kid)
    | [hd, ...tl] =>
      let hd_molded = Terrace.mold(hd, ~kid, t);
      let tl_molded =
        mold(
          Slope.Dn.mk(tl),
          ~kid=
            Kid.Profile.mk(
              ~has_tokens=
                !Terrace.has_tokens(terr) || Kid.Profile.has_tokens(kid),
              Terrace.sort(terr),
            ),
          t,
        );
      Result.pick(hd_molded, tl_molded);
    };
};

module Stepwell = {
  include Stepwell;
  // does not attempt to mold beyond first bridge
  let rec mold =
          (well: Stepwell.t, ~kid=None, t: Token.t): Result.t(Kid.Profile.t) => {
    let (pre, _) = Stepwell.get_slopes(well);
    let pre_molded = Slope.mold(pre, ~kid, t);
    switch (pre_molded) {
    | Ok(_) => pre_molded
    | Error(kid) =>
      switch (Stepwell.unlink(well)) {
      | None => pre_molded
      | Some((_, (l, _r), _well)) => Terrace.mold(l, ~kid, t)
      }
    };
  };
};

let mold = (well: Stepwell.t, t: Token.t): option(Mold.t) =>
  Stepwell.mold(well, t) |> Result.to_option |> Option.map(fst);
