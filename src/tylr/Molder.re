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
  let mold =
      ({mold, token, _}: Piece.t, ~slot=?, t: Token.t): Result.t(unit) => {
    let molder = (mold, token);
    Molds.of_token(t)
    |> List.filter_map(m => {
         let candidate = (m, t);
         Comparator.cmp(molder, ~slot?, candidate);
       })
    |> List.stable_sort(Scorer.compare)
    |> ListUtil.hd_opt
    |> Result.of_option(~error=());
  };
};

module Wald = {
  include Wald;
  let rec mold = (w: p, ~slot=None, t: Token.t): Result.t(Kid.Profile.t) => {
    let err = profile(terr) |> Kid.Profile.add_tokens(kid.has_tokens);
    let hd_molded =
      Piece.mold(face(R, w), ~slot, t) |> Result.map_error(() => err);
    let tl_molded =
      switch (unknil(terr)) {
      | Some((w, k, p)) when !Piece.has_token(p) =>
        open Result.Syntax; // let mold = Mold.grout_of_tile(mold);
        // let grout = {...p, mold: Grout(mold)};
        // todo: prune away unnecessary prefix/postfix grout
        // let kid = Meld.of_piece(~l=kid', grout, ~r=kid);

        let kid = Kid.(Profile.merge(profile(k), kid));
        let* z = mold(w, ~slot, t);
        // take only eq molds from tl
        Ziggurat.is_singleton(z) ? Ok(z) : Error(err);
      | _ => Error(err)
      };
    Result.pick(hd_molded, tl_molded);
  };
};

module Slope = {
  include Slope;
  let rec mold = (dn: Dn.p, ~slot=None, t: Token.t): Result.t(Kid.Profile.t) =>
    switch (dn) {
    | [] => Error(kid)
    | [hd, ...tl] =>
      let hd_molded = Wald.mold(hd.wal, ~slot, t);
      let kid =
        Wald.profile(hd.wal) |> Kid.Profile.add_tokens(kid.has_tokens);
      let tl_molded = mold(tl, ~slot, t);
      Result.pick(hd_molded, tl_molded);
    };
};

module Ziggurat = {
  include Ziggurat;
  let mold = (zigg: Ziggurat.p, t: Token.t): Mold.t =>
    switch (Slope.mold(dn, t)) {
    | Ok(z) => Piece.mold(Ziggurat.face(R, z))
    | Error(kid) =>
      switch (Terrace.(mold(mk(zigg.top), ~slot, t))) {
      | Ok(z) => Piece.mold(Ziggurat.face(R, z))
      | Error(_) => default(t)
      }
    };
};

module Stepwell = {
  let mold = (well: Stepwell.t, t: Token.t) =>
    Molds.of_token(t)
    |> List.map(m => {
      let candidate = Wald.singleton(Piece.mk(~token=t, m));
      switch (Stepwell.unlink(well)) {
      | Error((dn, up)) => failwith("todo")
      | Ok(((dn, up), (l, r), well)) =>
        switch (Melder.Slope.Dn.hsup(dn, candidate)) {
        | Ok(zdn) =>
        | Error(slot) =>

        }
      }
    })

  let mold = (well: Stepwell.t, ~slot=Slot.Empty, t: Token.t) =>
    switch (Stepwell.unlink(well)) {
    | Error((dn, up)) => failwith("todo")
    | Ok(((dn, up), (l, r), well)) =>
      switch (Slope.mold(dn, ~slot, t)) {
      }
    };


  let mold = (well: Stepwell.t, ~slot=None, t: Token.t): Mold.t => {
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
