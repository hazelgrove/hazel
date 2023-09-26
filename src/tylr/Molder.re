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

// let mold = (well: Stepwell.t, t: Token.t) =>
//   Molds.of_token(t)
//   |> List.map(m => {
//     let w = EWald.singleton(Piece.mk(~token=t, Tile(Molded(m))));
//     Melder.Stepwell.hsup(well, w);
//   })
//   |> List.stable_sort(
//     ((well_l, melded_l) (well_r, melded_r)) =>
//   )

// module Wald = {
//   let mold =
//       (w: EWald.t, ~slot=Slot.Empty, t: Token.t)
//       : Result.t(ESlope.Dn.t)
// };

module Molded = {
  type t('a) = Result.t(('a, ESlope.Dn.t), ESlot.t);
};

let candidates = (t: Token.t) =>
  Molds.of_token(t) |> List.map(t => Wald.singleton(Piece.mk(~token=t, m)));

module Bound = {
  let mold = (b: Bound.t, ~slot=ESlot.Empty, t: Token.t): Result.t(unit) =>
    candidates(t)
    |> List.filter_map(Melder.lt(b, ~slot))
    |> List.stable_sort(Obligation.Score.compare)
    |> ListUtil.hd_opt
    |> Option.map(s => ((), s))
    |> Result.of_option(~error=slot);
};

// module Wald = {
//   let mold =
//       (w: Wald.t, ~slot=ESlot.Empty, t: Token.t)
//       : option
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
      let well = EStepwell.map_slopes(Slopes.cat(([l], [r])), well);
      let+ (well, dn) = mold(well, ~slot, t);
      let well =
        EStepwell.rebridge(well)
        |> EStepwell.map_slopes(Slopes.cat(([], up)));
      (well, dn);
    };
  };
};

module Slope = {
  let rec mold =
      (~without=?, dn: ESlope.Dn.t, ~slot=ESlot.Empty, p: Piece.t)
      : Result.t((ESlope.Dn.t, ESlope.Dn.t), ESlot.t) =>
    switch (dn) {
    | [] => Error(slot)
    | [hd, ...tl] =>
      switch (leq(~without?, hd.wald, ~slot, p)) {
      | Some((wald, dn)) => Ok(([{...hd, wald}, ...tl], dn))
      | None =>
        let slot = ESlot.Full(M(hd.slot, hd.wald, slot));
        hsup(~without?, tl, ~slot, p);
      }
    };
}

module Slope = {
  module Dn = {
    // melding assuming fixed context
    let rec hsup =
            (~without=?, dn: ESlope.Dn.t, ~slot=ESlot.Empty, w: EWald.t): Result.t((ESlope.Dn.t, ESlope.Dn.t), ESlot.t) =>
      switch (dn) {
      | [] => Error(slot)
      | [hd, ...tl] =>
        switch (Melder.leq(~without?, hd.wald, ~slot, w)) {
        | Some((wald, dn)) => Ok(([{...hd, wald}, ...tl], dn))
        | None =>
          let slot = ESlot.Full(M(hd.slot, hd.wald, slot));
          hsup(~repair?, tl, ~slot, w);
        }
      };

    // melding across grout-pruned contexts
    let hsup = (dn: ESlope.Dn.t, w: EWald.t) =>
      ESlope.Dn.split_obligations(dn)
      |> List.fold_left(
           // not 100% sure that without bound can just be each split
           // obligation and not an accumulated minimum
           (r, (dn, o)) => Result.pick([hsup(~without=o, dn, w), r]),
           hsup(dn, w),
         );
  };
};

module Stepwell = {
  let hsup = (well: EStepwell.t, w: EWald.t) => {
    let (dn, up) = EStepwell.get_slopes(well);
    Slope.Dn.hsup(dn, w)
    |> Result.map(((dn, lt)) => (EStepwell.put_slopes((dn, up), well), lt))
  };

  let candidates = ((m, token): Labeled.t) =>
    (
      switch (m) {
      | Space => [Material.Space]
      | Grout(tips) => [Grout(tips)]
      | Tile(None) => []
      | Tile(Some(lbl)) => Molds.with_label(lbl) |> List.map(m => Tile(Molded(m)))
      }
    )
    |> List.map(Piece.mk(~token))
    |> List.map(Wald.singleton);

  // caller should ignore error slot
  let rec mold = (well: EStepwell.t, ~slot=ESlot.Empty, l: Labeled.t)
                 : Result.t((EStepwell.t, ESlope.Dn.t), ESlot.t) => {
    open Result.Syntax;
    let (dn, up) = EStepwell.get_slopes(well);
    let/ slot =
      candidates(l) |> List.map(Slope.Dn.hsup(dn, ~slot)) |> Result.pick;
    switch (EStepwell.unlink(well)) {
    | Some((_, (l, r), well)) =>
      let well = EStepwell.map_slopes(Slopes.cat(([l], [r])), well);
      mold(well, ~slot, t)
      |> Result.map(((well, dn)) => {
           let well =
             well
             |> EStepwell.bridge_faces
             |> EStepwell.map_slopes(Slopes.cat(([], up)));
           (well, dn);
         });
    | None =>
      root_mold(~slot, t)
      |> Result.map((((), molded)) =>
           (EStepwell.singleton(([], up)), molded)
         )
    };
  };

  let insert = (well: EStepwell.t, l: Labeled.t): EStepwell.t =>
    switch (mold(well, l)) {
    | Ok((ctx, dn)) => EStepwell.map_slopes(Slopes.cat((dn, [])), ctx)
    | Error(_) =>
      let (_, t) = l;
      let w = Wald.singleton(Piece.mk_default(t));
      hsup(well, w) |> Result.ok_or_fail("bug: failed to meld unmolded piece");
    };

  let remold = (~slot=ESlot.Empty, well: EStepwell.t): EStepwell.t =>
    switch (EStepwell.get_slopes(well)) {
    | (_, []) => failwith("todo: check prefix face gt with bridge right")
    | (dn, [hd, ...tl]) =>
      // let l = Piece.to_labeled(ETerrace.face(hd));
      // let inserted = well |> EStepwell.put_slopes((dn, tl)) |> insert;
      // switch (EStepwell.face(~side=L, inserted)) {
      // |
      // }
      switch (ETerrace.face(hd).material) {
      | Space =>
        assert(ESlot.is_empty(slot));
        let (face, _empty) = ESlope.Up.split_face(hd);
        let dn = [ETerrace.singleton(face), ...dn];
        remold(EStepwell.put_slopes((dn, up), well));
      | Grout(_) =>
        switch (Slope.Dn.hsup(dn, ~slot, hd.wald)) {
        | Ok((dn, lt)) =>
          let dn = ESlope.cat(lt, dn);
          remold(EStepwell.put_slopes((dn, up), well));
        | Error(_) =>
          let (_, rest) = ESlope.Up.split_face(hd);
          let up = ESlope.cat(rest, tl);
          remold(EStepwell.put_slopes((dn, up), well));
        }
      | Tile(Unmolded(_)) =>
        let (face, rest) = ESlope.Up.split_face(hd);
        let well = EStepwell.put_slopes((dn, ESlope.cat(rest, tl)), well);
        switch (mold(well, ~slot, face)) {
        | Error(_) =>

        }

      | Tile(Molded(m)) =>
        switch ()
      }

    }

  let remold = (well: EStepwell.t) => {
    switch (EStepwell.get_slopes(well)) {
    | (_, []) => well
    | (dn, [hd, ...up]) =>
      let (face, rest) = ESlope.Up.split_face(hd);
      switch (face.material) {
      | Space =>
        assert(List.is_empty(rest));
        let dn = [ETerrace.singleton(face), ...dn];
        EStepwell.put_slopes((dn, up), well)
        |> remold
      | Grout(_) =>
        switch (Slope.Dn.hsup(dn, hd.wald)) {
        | Ok((dn, ))
        }

      }

    }



    switch (ESlope.Up.split_face(up)) {
    | None => well
    | Some((p, up)) => failwith("todo")
    // switch (mold(EStepwell.put_slopes(dn, up), p.token))
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
  let mold = (t: Token.t, well: Stepwell.t): Stepwell.t => {
    let molded =
      Molds.of_token(t)
      |> List.map(m => {
           let candidate = Wald.singleton(Pi2ece.mk(~token=t, m));
           Melder.Stepwell.push(~onto=L, candidate, well);
         })
      |> List.stable_sort(((r_l, well_l), (r_r, well_r)) => {
           let c = Result.compare(Slope.compare, r_l, r_r);
           if (c == 0) {
             let ((_, up_l), (_, up_r)) = (
               get_slopes(well_l),
               get_slopes(well_r),
             );
             Slope.compare(up_l, up_r);
           } else {
             c;
           };
         })
      |> ListUtil.hd_opt;
    switch (molded) {
    | None
    | Some((Error(_), _)) => failwith("todo")
    | Some((Ok(bounded), well)) =>
      Stepwell.map_slopes(Slopes.cat((bounded, [])), well)
    };
  };

  let mold = (well: Stepwell.t, t: Token.t) =>
    Molds.of_token(t)
    |> List.map(m => {
         let candidate = Wald.singleton(Piece.mk(~token=t, m));
         switch (Stepwell.unlink(well)) {
         | Error((dn, up)) => failwith("todo")
         | Ok(((dn, up), (l, r), well)) =>
           switch (Melder.Slope.Dn.hsup(dn, candidate)) {
           | Ok(zdn) => failwith("todo")
           | Error(slot) => failwith("todo")
           }
         };
       });

  let mold = (well: Stepwell.t, ~slot=Slot.Empty, t: Token.t) =>
    switch (Stepwell.unlink(well)) {
    | Error((dn, up)) => failwith("todo")
    | Ok(((dn, up), (l, r), well)) =>
      // switch (Slope.mold(dn, ~slot, t)) {
      // }
      failwith("todo")
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
