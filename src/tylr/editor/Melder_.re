open Util;

module Mtrl = {
  // let memo = f =>
  //   FunUtil.uncurry2(f)
  //   |> Core.Memo.general
  //   |> FunUtil.curry2;

  // todo: add directional arg
  let eq = (l: Mtrl.t, r: Mtrl.t): list(GWald.t) =>
    GWalker.walk_eq(R, l) |> GWalker.Walked.find(r);

  let lt = (l: Bound.t(Mtrl.t), r: Mtrl.t): list(GSlope.Dn.t) =>
    GWalker.walk_lt(l) |> GWalker.Walked.find(r);

  let gt = (l: Mtrl.t, r: Bound.t(Mtrl.t)): list(GSlope.Up.t) =>
    GWalker.walk_gt(r) |> GWalker.Walked.find(l);
};

module Token = {
  let eq = (~onto: Dir.t, l: EToken.t, ~cell: ECell.t, r: EToken.t) => {
    let (t_onto, t_from) = Dir.choose(onto, l, r);
    GWalker.walk_eq(onto, t_from);
  };
};

module Wald = {
  let lt =
      (l: Bound.t(EWald.t), ~cell=ECell.empty, r: EWald.t)
      : option(ESlope.Dn.t) => {
    let hd_l = l |> Bound.map(EWald.hd) |> Bound.map(EToken.mtrl_);
    let hd_r = EWald.hd(r).mtrl;
    EWalk.lt(hd_l, hd_r)
    |> Oblig.Delta.minimize(ESlope.Dn.bake(~face=r, ~fill=cell));
  };

  let gt =
      (l: EWald.t, ~cell=ECell.empty, r: Bound.t(EWald.t))
      : option(ESlope.Up.t) => {
    let hd_l = EWald.hd(l).mtrl;
    let hd_r = r |> Bound.map(EWald.hd) |> Bound.map(EToken.mtrl_);
    EWalk.gt(hd_l, hd_r)
    |> Oblig.Delta.minimize(ESlope.Up.bake(~face=l, ~fill=cell));
  };

  let rec eq =
          (~merge, ~onto: Dir.t, l: EWald.t, ~cell=ECell.empty, r: EWald.t) => {
    open OptUtil.Syntax;
    let (hd_l, tl_l) = EWald.split_hd(l);
    let (hd_r, tl_r) = EWald.split_hd(r);
    let (w_onto, w_from) = Dir.choose(onto, l, r);
    // try zipping
    let/ () = {
      let+ zipped = EToken.zip(hd_l, hd_r);
      EWald.unit(zipped)
      |> Fun.flip(EWald.append, tl_foot)
      |> EWald.prepend(tl_face);
    };
    // try passing
    let/ () =
      switch (onto, tl_l, tl_r) {
      | (L, ([c, ...cs], ts), _) when EToken.passes(onto, hd_l, hd_r) =>
        let l = EWald.mk(ts, cs);
        let cell = merge(c, cell);
        eq(~merge, ~onto, l, ~cell, r) |> Effects.perform_if(Remove(hd_l));
      | (R, _, ([c, ...cs], ts)) when EToken.passes(onto, hd_l, hd_r) =>
        let r = EWald.mk(ts, cs);
        let cell = merge(cell, c);
        eq(~merge, ~onto, l, ~cell, r) |> Effects.perform_if(Remove(hd_r));
      | _ => None
      };
    // try walking
    EWalk.eq(~from=onto, hd_l.mtrl, hd_r.mtrl)
    |> Oblig.Delta.minimize(
         EWald.bake(~face=w_from, ~fill=cell, ~foot=w_onto),
       );
  };
};

module Slope = {
  let add = (ds: list(Oblig.Delta.t), r) =>
    switch (r) {
    | Ok((ok, d)) => Ok((ok, Oblig.Delta.add([d, ...ds])))
    | Error((err, d)) => Error((err, Oblig.Delta.add([d, ...ds])))
    };

  module Dn = {
    let rec push_wald =
            (~top=Bound.Root, dn: ESlope.Dn.t, ~cell=ECell.empty, w: EWald.t)
            : Result.t(ESlope.Dn.t, ECell.t) =>
      switch (dn) {
      | [] => Wald.lt(top, ~cell, w) |> Result.of_option(~error=cell)
      | [hd, ...tl] =>
        switch (
          Wald.eq(hd.wald, ~cell, w),
          Wald.lt(Node(hd.wald), ~cell, w),
          Wald.gt(hd.wald, ~cell, Node(w)),
        ) {
        | (Some(eq), _, _) => Ok([{...hd, wald: eq}, ...tl])
        | (_, Some(lt), _) => Ok(ESlope.Dn.cat(dn, lt))
        | (_, _, Some(gt)) =>
          let cell = ESlope.Up.roll(~cell=hd.cell, gt);
          push_wald(~top, tl, ~cell, w);
        | (None, None, None) =>
          open Result.Syntax;
          let g = EToken.mk(Grout((Concave, Concave)));
          let* dn = push_wald(~top, dn, ~cell, EWald.unit(g));
          push_wald(~top, dn, w);
        }
      };

    let push = (~top=Bound.Root, dn, ~cell=ECell.empty, t) =>
      push_wald(~top, dn, ~cell, EWald.unit(t));
  };
};

module Ctx = {
  open OptUtil.Syntax;

  let push =
      (~onto: Dir.t, w: EWald.t, ~cell=ECell.empty, ctx: ECtx.t)
      : option(ECtx.t) =>
    switch (onto, ECtx.unlink(ctx)) {
    | (L, Error((dn, up))) =>
      let+ dn = Slope.Dn.push(dn, ~slot, w);
      ECtx.unit((dn, up));
    | (L, Ok(((dn, up), (l, r), ctx))) =>
      switch (Slope.Dn.push(~top=Node(l.wald), dn, ~cell, w)) {
      | Some(dn) => Some(ECtx.unit((dn, up)))
      | None =>
        let+ w = Wald.eq(l.wald, ~cell=ESlope.Dn.roll(dn), w);
        let (dn, up) = ([{...l, wald: w}], [r, ...up]);
        ECtx.map_fst(EFrame.Open.cat((dn, up)), ctx);
      }
    | (R, _) => failwith("todo: symmetric to L")
    };
};
