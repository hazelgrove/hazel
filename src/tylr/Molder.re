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

// module Molded = {
//   type t('a) = Result.t(('a, ESlope.Dn.t), ESlot.t);
// };

let candidates = (t: Token.t): list(Material.Molded.t) =>
  failwith("todo");

let pick = _ => failwith("todo pick slopes based on obligation scores");

module Piece = {
  let lt = (~without=?, l: Bound.t(Piece.t), ~slot=ESlot.Empty, r: Piece.t) =>
    candidates(r.token)
    |> List.filter_map(material =>
      Melder.Piece.lt(~without?, l, ~slot, {...r, material})
    )
    |> pick;
};

module Wald = {
  let lt = (~without=?, l: Bound.t(EWald.t), ~slot=ESlot.Empty, r: Piece.t): option(ETerrace.R.t) => {
    let l = Bound.map(EWald.bound(~side=R), l);
    // let (hd_r, tl_r) = EWald.split_face(~side=L, r);
    Piece.lt(~without?, l, ~slot, hd_r)
    // |> Option.map(ESlope.Dn.extend_face(tl_r))
  };

  let eq = (~without=?, l: EWald.t, ~slot=ESlot.Empty, r: Piece.t): option(EWald.t) => {
    open OptUtil.Syntax;
    let/ () =
      // remove unfinished tile and attempt molding against remaining
      switch (EWald.unknil(l)) {
      | Ok((tl, s, p)) when Piece.is_unfinished(p) =>
        let slot =
          failwith("todo: unroll s and slot and remold (mold not meld to remove grout where possible)");
        let without = failwith("todo fold in no unfinished tiles");
        eq(~without, tl, ~slot, r);
      | Error(p)
          when Piece.is_unfinished(p)
            && p.material == EWald.face(~side=L, r).material
            && ESlot.has_no_tiles(slot) =>
        Some(Wald.singleton(r))
      | _ => None
      };

    let (hd_l, tl_l) = EWald.split_face(l, ~side=R);
    // let (hd_r, tl_r) = EWald.split_face(~side=L, r);
    Piece.eq(~without?, hd_l, ~slot, r)
    |> Option.map(EWald.extend(~side=L, tl_l));
  };
};

module Slope = {
  let rec mold =
      (~without=?, dn: ESlope.Dn.t, ~slot=ESlot.Empty, p: Piece.t)
      : Result.t((ESlope.Dn.t, ESlope.Dn.t), ESlot.t) =>
    switch (dn) {
    | [] => Error(slot)
    | [hd, ...tl] =>
      open Result.Syntax;
      let (rest, face) = ESlope.Dn.unroll_terrace(hd);
      let/ _ =
        switch (face.material) {
        | Grout(tips) =>
          let without = failwith("todo fold in grout tips");
          mold(~without, ESlope.Dn.cat(tl, rest), ~slot, p);
        | _ => None
        };
      switch (Wald.leq(~without?, hd.wald, ~slot, p)) {
      | Some((eq, lt)) => Ok(([{...hd, wald: eq}, ...tl], lt))
      | None =>
        let slot = ESlot.Full(M(hd.slot, hd.wald, slot));
        mold(~without?, tl, ~slot, p);
      };
    };
};

module Stepwell = {
  let rec mold =
      (well: EStepwell.t, ~slot=ESlot.Empty, p: Piece.t)
      : Result.t((EStepwell.t, ESlope.Dn.t), ESlot.t) => {
    open Result.Syntax;
    let (dn, up) = EStepwell.get_slopes(well);
    let/ slot =
      Slope.Dn.mold(dn, ~slot, p)
      |> Result.map(((dn, lt)) => (EStepwell.put_slopes((dn, up), well), lt));
    switch (EStepwell.unlink(well)) {
    | None =>
      let+ lt = Result.of_option(~error=slot, Piece.lt(Root, ~slot, p));
      (EStepwell.singleton(([], up)), lt)
    | Some((_, (l, r), well)) =>
      let _ = failwith("todo: check for stable bridge / p unfinished");
      let well = EStepwell.map_slopes(Slopes.cat(([l], [r])), well);
      let+ (well, dn) = mold(well, ~slot, t);
      let well =
        EStepwell.rebridge(well)
        |> EStepwell.map_slopes(Slopes.cat(([], up)));
      (well, dn);
    };
  };
};


let mold = (well: EStepwell.t, t: Token.t): (EStepwell.t, ESlope.Dn.t) =>
  switch (Stepwell.mold(well, t)) {
  | Ok(ok) => ok
  | Error(_) =>
    let w = Wald.singleton(Piece.mk_unmolded(t));
    Option.get(Melder.Stepwell.hsup(well, w));
  };

