module Result = {
  include Result;
  type t('x) = Result.t('x, Meld.t);
};

module Terrace = {
  module L = {
    include Terrace.L;
  };
  module R = {
    include Terrace.R;
    let connect = (terr: t, ~kid=Meld.empty(), w: Wald.t) => {
      // step to nearest mold
      let rec step = (z: GZipper.t(Atom.t)) => {
        let last_kid =
          switch (z) {
          | (Tok(_), _) => Meld.empty()
          | (Kid(s), _) => failwith("todo mk grout with s")
          };
        Regex.step(R, z.zipper)
        |> List.map(stop_or_step(~last_kid))
        |> Result.union_all;
      }
      and stop_or_step = (~last_kid, z: GZipper.t(Atom.t)) =>
        switch (z.zipper) {
        // stop
        | (Tok(lbl), ctx) =>
          let m = failwith("todo of lbl ctx sort prec");
          Result.singleton(m, Path.init(last_kid));
        // keep going
        | (Kid(s), _) =>
          let bound = Sort.eq(l.sort, s) ? bound : Prec.min;
          let stepped_lt =
            Grammar.enter(~from=L, ~bound, s)
            |> List.map(stop_or_step(~last_kid))
            |> List.map(Result.newline);
          let stepped_eq =
            step(z)
        };
      step(Mold.to_atom(face(terr).mold));
    };


    // let connect = (terr: t, ~kid=Meld.empty(), w: Wald.t) =>
    //   Walker.step_leq(face(terr).mold)
    //   |> Walker.Result.pick(
    //     ~over=?Meld.sort(kid),
    //     ~dest=Wald.fst(w).mold,
    //   )
    //   |> Option.map(plug(kid))
    //   |>
  };
};

module Slope = {
  module Dn = {
    include Slope.Dn;
    let rec connect = (dn: t, ~kid=Meld.empty(), t: Terrace.L.t): Result.t(t) => {
      let kid = Meld.pad(~l=dn.space, kid);
      switch (dn.terrs) {
      | [] => Error(kid)
      | [hd, ...tl] =>
        let tl = Slope.Dn.mk(tl);
        // left-to-right: tl hd kid terr
        switch (Terrace.R.connect(hd, ~kid, t)) {
        | Some(slope) => Ok(Slope.Dn.(cat(tl, slope)))
        | None =>
          // assuming well-fitted so must be gt
          let kid = Terrace.R.unmk(hd, kid);
          connect(tl, ~kid, t);
        };
      };
    };
  };
  module Up = {
    include Slope.Up;
  };
};

module Stepwell = {
  let push_terr_l = (well: Stepwell.t, ~kid=Meld.empty(), t: Terrace.L.t): Stepwell.t => {
    let (dn, up) = Stepwell.get_slopes(well);
    switch (Slope.Dn.push_terr(dn, ~kid, t)) {
    | Ok(dn) => Stepwell.put_slopes((dn, up), well)
    | Error(kid) =>
      switch (Chain.unlink(well)) {
      | None =>
        let dn = Slope.Dn.of_meld(Terrace.L.unmk(kid, t));
        Stepwell.put_slopes((dn, up), well);
      | Some((_slopes, (l, r), well)) =>
        switch (Stepper.cmp(l, ~kid, t)) {
        | None => raise(Bridge.Convex_inner_tips)
        | Some(Lt())
        | Some(Lt(kid_mel)) =>
          let slopes = (Slope.Dn.of_meld(kid_mel), up);
          Stepwell.put_slopes(slopes, well)
        | Some(Eq(l_kid_mel)) =>
          let dn = Dn.of_meld(l_kid_mel);
          let up = Up.cat(up, Up.of_terr(r));
          Stepwell.cons_slopes((dn, up), well);
        | Some(Gt(l_kid)) =>
          let up = Up.cat(up, Up.of_terr(r));
          well
          |> Stepwell.cons_slopes(Slopes.mk(~r=up, ()))
          |> push_wald_l(~kid=l_kid, mel);
        }
      }
    };

  }
}