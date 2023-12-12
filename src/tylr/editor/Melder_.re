open Util;

module Mtrl = {
  // let memo = f =>
  //   FunUtil.uncurry2(f)
  //   |> Core.Memo.general
  //   |> FunUtil.curry2;

  // todo: add directional arg
  let eq = (l: Mtrl.t, r: Mtrl.t): list(GWald.t) =>
    GWalker.walk_eq(R, l)
    |> GWalker.Walked.find(r);

  let lt = (l: Bound.t(Mtrl.t), r: Mtrl.t): list(GSlope.Dn.t) =>
    GWalker.walk_lt(l)
    |> GWalker.Walked.find(r);

  let gt = (l: Mtrl.t, r: Bound.t(Mtrl.t)): list(GSlope.Up.t) =>
    GWalker.walk_gt(r)
    |> GWalker.Walked.find(l);

};

module Token = {
  let eq = (~onto: Dir.t, l: EToken.t, ~cell: ECell.t, r: EToken.t) => {
    let (t_onto, t_from) = Dir.choose(onto, l, r);
    GWalker.walk_eq(onto, t_from)
  };
}

module Wald = {
  let lt = (l: Bound.t(EWald.t), ~cell=ECell.empty, r: EWald.t): option(ESlope.Dn.t) => {
    let hd_l = l |> Bound.map(EWald.hd) |> Bound.map(EToken.mtrl_);
    let hd_r = EWald.hd(r).mtrl;
    EWalk.walk(R, Node(Mold(hd_l)))
    |> EWalk.Set.neq(Mold(hd_r))
    |> List.filter_map(ESlope.Dn.bake(~cell, ~face=r))
    |> Oblig.Delta.pick;
  };

  let gt = (l: EWald.t, ~cell=ECell.empty, r: Bound.t(EWald.t)): option(ESlope.Up.t) => {
    let hd_l = EWald.hd(l).mtrl;
    let hd_r = r |> Bound.map(EWald.hd) |> Bound.map(EToken.mtrl_);
    EWalk.walk(L, Node(Mold(hd_r)))
    |> EWalk.Set.neq(Mold(hd_l))
    |> List.filter_map(ESlope.Up.bake(~cell, ~face=l))
    |> Oblig.Delta.pick;
  };

  let rec eq = (
    ~merge,
    ~onto: Dir.t,
    l: EWald.t,
    ~cell=ECell.empty,
    r: EWald.t,
  ) => {
    open OptUtil.Syntax;
    let (hd_l, tl_l) = EWald.split_hd(l);
    let (hd_r, tl_r) = EWald.split_hd(r);
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
        eq(~merge, ~onto, l, ~cell=merge(c, cell), r);
      | (R, _, ([c, ...cs], ts)) when EToken.passes(onto, hd_l, hd_r) =>
        let r = EWald.mk(ts, cs);
        eq(~merge, ~onto, l, ~cell=merge(cell, c), r);
      | _ => None
      };
    // try walking
    let (src, dst) = Dir.choose(onto, hd_l, hd_r);
    let (w_src, w_dst) = Dir.choose(onto, l, r);
    EWalk.walk(Dir.toggle(onto), Node(Mold(src.mtrl)))
    |> EWalk.Set.eq(Mold(dst.mtrl))
    |> List.filter_map(EWald.bake(~face=w_dst, ~cell, ~foot=w_src))
    |> Oblig.Delta.pick;
  };
};

module Slope = {
  let add = (ds: list((Oblig.Delta.t)), r) =>
    switch (r) {
    | Ok((ok, d)) => Ok((ok, Oblig.Delta.add([d, ...ds])))
    | Error((err, d)) => Error((err, Oblig.Delta.add([d, ...ds])))
    };

  module Dn = {
    let rec push_wald = (
      ~top=Bound.Root,
      dn: ESlope.Dn.t,
      ~cell=ECell.empty,
      w: EWald.t,
    )
    : Result.t(ESlope.Dn.t, ECell.t) =>
      switch (dn) {
      | [] =>
        Wald.lt(top, ~cell, w)
        |> Result.of_option(~error=cell)
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

    let push =
        (~top=Bound.Root, dn, ~cell=ECell.empty, t) =>
      push_wald(~top, dn, ~cell, EWald.unit(t));
  }
};

module Ctx = {
  open OptUtil.Syntax;

  let push = (
    ~onto: Dir.t,
    w: EWald.t,
    ~cell=ECell.empty,
    ctx: ECtx.t,
  )
  : option((ECtx.t, Oblig.Delta.t)) =>
    switch (onto, ECtx.unlink(ctx)) {
    | (L, Error((dn, up))) =>
      let+ (dn, delta) = Slope.Dn.push(dn, ~slot, w);
      (ECtx.unit((dn, up)), delta);
    | (L, Ok(((dn, up), (l, r), ctx))) =>
      switch (Slope.Dn.push(~top=Node(l.wald), dn, ~cell, w)) {
      | Some((dn, delta)) => Some((ECtx.unit((dn, up)), delta))
      | None =>
        let+ (w, delta) = Wald.eq(l.wald, ~cell=ESlope.Dn.roll(dn), w);
        let dn = [{...l, wald: w}];
        let up = [r, ...up];
        let ctx = ECtx.map_fst(EFrame.Open.cat((dn, up)), ctx);
        (ctx, delta);
      }
    | (R, _) => failwith("todo")
    };
};
