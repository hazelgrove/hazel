open Util;

module Melded = {
  type t = Rel.t(Wald.t, Slope.t);

  let mk_eq = (_src: Wald.t, _bake, _dst: Wald.t) => failwith("todo");
  let mk_neq = (_bake: Baked.t, _dst: Wald.t) => failwith("todo");
  let mk = (src: Wald.t, bake: Baked.t, dst: Wald.t) =>
    switch (Baked.is_eq(bake)) {
    | Some(eq) => Rel.Eq(mk_eq(src, eq, dst))
    | None => Rel.Neq(mk_neq(bake, dst))
    };
};

module W = Wald;
module Wald = {
  let lt = (l: W.t, r: W.t) =>
    Walker.lt(Node(W.face(l)), Node(W.face(r))) != [];
  let gt = (l: W.t, r: W.t) =>
    Walker.gt(Node(W.face(l)), Node(W.face(r))) != [];
  let eq = (l: W.t, r: W.t) =>
    Walker.eq(Node(W.face(l)), Node(W.face(r))) != [];

  let attach = (wald: W.t, baked: Baked.t): Terr.t =>
    baked
    |> Baked.fold(
         fun
         | Rel.Neq(_) => raise(Invalid_argument("Melder.Terr.attach"))
         | Eq(cell) => (cell, wald),
         ((cell, wald), tok) =>
         fun
         | Rel.Neq(_) => raise(Invalid_argument("Melder.Terr.attach"))
         | Eq(c) => (c, W.link(tok, cell, wald))
       )
    |> (((cell, wald)) => Terr.{wald: W.rev(wald), cell});

  // assumes w is already oriented toward side
  let round = (~side: Dir.t, ~fill=[], w: W.t): Terr.t => {
    let bake = Baker.bake(~from=Dir.toggle(side));
    let exited = Walker.exit(~from=Dir.toggle(side), Node(W.face(w)));
    switch (Oblig.Delta.minimize(bake(~fill), exited)) {
    | Some(baked) => attach(w, baked)
    | None =>
      let exited =
        ListUtil.hd_opt(exited)
        |> OptUtil.get_or_fail("bug: expected at least one exit");
      let baked =
        bake(exited)
        |> OptUtil.get_or_fail(
             "bug: bake expected to succeed if no fill required",
           );
      attach(w, baked);
    };
  };

  let meld =
      (~repair=false, ~from: Dir.t, src: W.t, ~fill=[], dst: W.t)
      : option(Melded.t) => {
    open OptUtil.Syntax;
    let walk = repair ? Walker.walk : Walker.step;
    let rec go = (src, fill) => {
      let/ () = repair ? rm_ghost_and_go(src, fill) : None;
      let+ bake =
        walk(~from, Node(W.face(src)), Node(W.face(dst)))
        |> Oblig.Delta.minimize(~to_zero=!repair, Baker.bake(~from, ~fill));
      Melded.mk(src, bake, dst);
    }
    and rm_ghost_and_go = (src, fill) =>
      switch (W.unlink(src)) {
      | Ok((hd, cell, tl)) when Token.is_ghost(hd) =>
        let fill =
          Cell.get(cell)
          |> Option.map(m => [m, ...fill])
          |> Option.value(~default=fill);
        switch (go(tl, fill)) {
        // require eq match further in to accept removing hd
        | Some(Rel.Eq(_)) as r =>
          Effects.remove(hd);
          r;
        | _ => None
        };
      | _ => None
      };
    let/ () = {
      // first try zipping
      let+ zipped = W.zip_hds(~from, src, dst);
      assert(Filling.is_empty(fill));
      Rel.Eq(zipped);
    };
    go(src, fill);
  };

  let meld_root =
      (~repair=false, ~from: Dir.t, ~fill=[], dst: W.t): option(Slope.t) =>
    Walker.walk(~from, Root, Node(W.face(dst)))
    |> Oblig.Delta.minimize(~to_zero=!repair, Baker.bake(~from, ~fill))
    |> Option.map(bake => Melded.mk_neq(bake, dst));
};

module T = Terr;
module Terr = {
  let attach = (baked: Baked.t, terr: T.t) =>
    baked
    |> Chain.fold_left(
         fun
         | Rel.Neq(_) => raise(Invalid_argument("Melder.Terr.attach"))
         | Eq(cell) => Meld.M(terr.cell, terr.wald, cell),
         (meld, tok) =>
         fun
         | Rel.Neq(_) => raise(Invalid_argument("Melder.Terr.attach"))
         | Eq(cell) => Meld.link(~cell, tok, meld)
       );

  let roll = (~onto: Dir.t, ~fill=[], terr: T.t): Filling.t => {
    let bake = Baker.bake(~from=onto);
    let exited = Walker.exit(~from=onto, Node(T.face(terr)));
    let orient = Dir.pick(onto, (Meld.rev, Fun.id));
    switch (Oblig.Delta.minimize(bake(~fill), exited)) {
    | Some(baked) => [orient(attach(baked, terr))]
    | None =>
      let exited =
        ListUtil.hd_opt(exited)
        |> OptUtil.get_or_fail("bug: expected at least one exit");
      let baked =
        bake(exited)
        |> OptUtil.get_or_fail(
             "bug: bake expected to succeed if no fill required",
           );
      [orient(attach(baked, terr)), ...fill];
    };
  };

  module L = {
    let roll = roll(~onto=R);
  };
  module R = {
    let roll = roll(~onto=L);
  };
};

module S = Slope;
module Slope = {
  let unroll = (~from: Dir.t, cell: Cell.t) => {
    let rec go = (cell: Cell.t, unrolled) =>
      switch (Cell.get(cell)) {
      | None => unrolled
      | Some(M(l, w, r)) =>
        let (cell, terr) =
          switch (from) {
          | L => (r, T.{wald: W.rev(w), cell: l})
          | R => (l, T.{wald: w, cell: r})
          };
        go(cell, [terr, ...unrolled]);
      };
    go(cell, []);
  };

  let roll = (~onto: Dir.t, ~fill=[]) =>
    S.fold(fill => Terr.roll(~onto, ~fill), fill);
  // let roll_fail =

  let push =
      (~repair=false, ~onto: Dir.t, w: W.t, ~fill=[], slope: S.t)
      : Result.t(S.t, Filling.t) => {
    let meld = Wald.meld(~repair, ~from=onto);
    let roll = Terr.roll(~onto);
    let rec go = (fill, slope: S.t) =>
      switch (slope) {
      | [] => Error(fill)
      | [{wald: W(([tok, ...toks], cells)), cell}, ...tl]
          when Token.is_grout(tok) =>
        Effects.remove(tok);
        let (cell, slope) =
          switch (cells) {
          | [] => (cell, tl)
          | [c, ...cs] =>
            let hd = T.{wald: W.mk(toks, cs), cell};
            (c, [hd, ...tl]);
          };
        go(fill, S.cat(unroll(~from=onto, cell), slope));
      | [hd, ...tl] =>
        switch (meld(hd.wald, ~fill, w)) {
        | None => go(roll(~fill, hd), tl)
        | Some(Eq(wald)) => Ok([{...hd, wald}, ...tl])
        | Some(Neq(s)) => Ok(S.cat(s, slope))
        }
      };
    go(fill, slope);
  };

  let pull = (~from: Dir.t, slope: S.t): option((Token.t, S.t)) =>
    switch (slope) {
    | [] => None
    | [hd, ...tl] =>
      let (tok, rest) = W.split_hd(hd.wald);
      let slope =
        switch (rest) {
        | ([], _) => S.cat(unroll(~from, hd.cell), tl)
        | ([cell, ...cells], toks) =>
          let hd = {...hd, wald: W.mk(toks, cells)};
          S.cat(unroll(~from, cell), [hd, ...tl]);
        };
      Some((tok, slope));
    };

  module Dn = {
    let unroll = unroll(~from=L);
    let roll = roll(~onto=L);
    let push = push(~onto=L);
    let pull = pull(~from=L);
  };
  module Up = {
    let unroll = unroll(~from=R);
    let roll = roll(~onto=R);
    let push = push(~onto=R);
    let pull = pull(~from=R);
  };
};

module Z = Zigg;
// note: naming convention for from and onto directional parameters
// for zigg functions are reverse of convention for parameters, not ideal
module Zigg = {
  let unroll = (c: Cell.t) => {
    open OptUtil.Syntax;
    let+ M(l, top, r) = Cell.get(c);
    let (up, dn) = Slope.(Up.unroll(l), Dn.unroll(r));
    Z.{up, top, dn};
  };
  let of_dn = dn =>
    ListUtil.split_last_opt(dn)
    |> Option.map(((dn, t: T.t)) =>
         Z.{up: Slope.Up.unroll(t.cell), top: W.rev(t.wald), dn}
       );
  let of_up = up =>
    ListUtil.split_last_opt(up)
    |> Option.map(((up, t: T.t)) =>
         Z.{up, top: W.rev(t.wald), dn: Slope.Up.unroll(t.cell)}
       );

  let push_wald =
      (~side as d: Dir.t, w: W.t, ~fill=[], zigg: Z.t): Result.t(Z.t, S.t) => {
    let b = Dir.toggle(d);
    let (s_d, top, s_b) = Z.orient(d, zigg);
    let unorient = Z.unorient(d);
    switch (Slope.push(~onto=b, w, ~fill, s_d)) {
    | Ok(s_d) => Ok(unorient((s_d, top, s_b)))
    | Error(fill) =>
      switch (Wald.meld(~from=b, top, ~fill, w)) {
      | Some(Eq(top)) => Ok(unorient(([], top, s_b)))
      | Some(Neq(s_d)) => Ok(unorient((s_d, top, s_b)))
      | None => Error(s_b @ [Wald.round(~side=d, ~fill, top)])
      }
    };
  };
  let push = (~side: Dir.t, tok: Token.t) => push_wald(~side, W.unit(tok));

  let pull = (~side as d: Dir.t, zigg: Z.t): (Token.t, option(Z.t)) => {
    let b = Dir.toggle(d);
    let (s_d, s_b) = Dir.order(d, (zigg.up, zigg.dn));
    switch (Slope.pull(~from=b, s_d)) {
    | Some((tok, s_d)) =>
      let (up, dn) = Dir.order(d, (s_d, s_b));
      (tok, Some({...zigg, up, dn}));
    | None =>
      let top = Dir.pick(d, (Fun.id, W.rev), zigg.top);
      let (tok, rest) = W.split_hd(top);
      switch (rest) {
      | ([], _) => (tok, Dir.pick(d, (of_up, of_dn), s_b))
      | ([c, ...cs], ts) =>
        let s_d = Slope.unroll(~from=b, c);
        let (up, dn) = Dir.order(d, (s_d, s_b));
        let top = Dir.pick(d, (Fun.id, W.rev), W.mk(ts, cs));
        (tok, Some(Z.{up, top, dn}));
      };
    };
  };

  let grow = (~side: Dir.t, tok: Token.t, zigg: Z.t) =>
    switch (push(~side, tok, zigg)) {
    | Ok(zigg) => zigg
    | Error(s_b) => Z.unorient(side, ([], W.unit(tok), s_b))
    };

  let rec take_leq = (zigg: Z.t, ~fill=[], suf: S.Up.t) =>
    switch (suf) {
    | [] => ([], suf)
    | [hd, ...tl] =>
      switch (push_wald(~side=R, hd.wald, ~fill, zigg)) {
      | Error(_) => ([], suf)
      | Ok(zigg) =>
        let fill = Filling.init(hd.cell);
        let (leq, gt) = take_leq(zigg, ~fill, tl);
        ([hd, ...leq], gt);
      }
    };
  let rec take_geq = (pre: S.Dn.t, ~fill=[], zigg: Z.t) =>
    switch (pre) {
    | [] => (pre, [])
    | [hd, ...tl] =>
      switch (push_wald(~side=L, hd.wald, ~fill, zigg)) {
      | Error(_) => (pre, [])
      | Ok(zigg) =>
        let fill = Filling.init(hd.cell);
        let (lt, geq) = take_geq(tl, ~fill, zigg);
        (lt, [hd, ...geq]);
      }
    };
};

module C = Ctx;
module Ctx = {
  let push_wald =
      (~onto as d: Dir.t, w: W.t, ~fill=[], ctx: C.t): option(C.t) => {
    open OptUtil.Syntax;
    let (slopes, tl) = Ctx.split_fst(ctx);
    let (s_d, s_b) = Dir.order(d, slopes);
    switch (Slope.push(~onto=d, w, ~fill, s_d)) {
    | Ok(s_d) =>
      let slopes = Dir.order(d, (s_d, s_b));
      Some(Ctx.zip(slopes, ~suf=tl));
    | Error(fill) =>
      switch (Ctx.Tl.split_fst(tl)) {
      | None =>
        let+ s_d = Wald.meld_root(~from=d, ~fill, w);
        C.unit(Dir.order(d, (s_d, s_b)));
      | Some((terrs, ctx)) =>
        let (t_d, t_b) = Dir.order(d, terrs);
        let+ melded = Wald.meld(~from=d, t_d.wald, ~fill, w);
        switch (melded) {
        | Neq(s_d) =>
          let slopes = Dir.order(d, (s_d, s_b));
          C.link(~slopes, terrs, ctx);
        | Eq(wald) =>
          let slopes = Dir.order(d, ([T.{...t_d, wald}], s_b @ [t_b]));
          C.map_fst(Frame.Open.cat(slopes), ctx);
        };
      }
    };
  };
  let push = (~onto: Dir.t, t: Token.t) => push_wald(~onto, W.unit(t));
  let push_fail = (~onto: Dir.t, tok, ctx) =>
    push(~onto, tok, ctx) |> OptUtil.get_or_fail("bug: failed push");

  let rec push_slope = (~onto: Dir.t, s: S.t, ~fill=[], ctx: C.t) =>
    switch (s) {
    | [] => Some(ctx)
    | [hd, ...tl] =>
      open OptUtil.Syntax;
      let* ctx = push_wald(~onto, hd.wald, ~fill, ctx);
      push_slope(~onto, tl, ~fill=Filling.init(hd.cell), ctx);
    };
  let push_zigg = (~onto as d: Dir.t, zigg: Z.t, ctx: C.t) => {
    let get_fail = OptUtil.get_or_fail("bug: failed to push zigg");
    let (s_d, s_b) = Dir.order(d, (zigg.dn, zigg.up));
    let ctx = get_fail(push_slope(~onto=d, s_d, ctx));
    let top = Dir.pick(d, (Fun.id, W.rev), zigg.top);
    let ctx = get_fail(push_wald(~onto=d, top, ctx));
    let rest = Dir.order(d, ([], s_b));
    Ctx.map_fst(Frame.Open.cat(rest), ctx);
  };

  let rec pull = (~from as d: Dir.t, ctx: C.t): option((Token.t, C.t)) => {
    open OptUtil.Syntax;
    let order = Dir.order(d);
    switch (Ctx.unlink(ctx)) {
    | Error(slopes) =>
      let (s_d, s_b) = order(slopes);
      let+ (tok, s_d) = Slope.pull(~from=d, s_d);
      (tok, Ctx.unit(order((s_d, s_b))));
    | Ok((slopes, terrs, ctx)) =>
      let (s_d, s_b) = order(slopes);
      switch (Slope.pull(~from=d, s_d)) {
      | Some((tok, s_d)) =>
        Some((tok, Ctx.link(~slopes=order((s_d, s_b)), terrs, ctx)))
      | None =>
        let (t_d, t_b) = Dir.order(d, terrs);
        let slopes = order(([t_d], s_b @ [t_b]));
        pull(~from=d, Ctx.map_fst(Frame.Open.cat(slopes), ctx));
      };
    };
  };

  let close = (~sel=?, ctx: C.t) => {
    let rec go = (ctx: C.t) =>
      switch (C.fst(ctx)) {
      | ([], _)
      | (_, []) => ctx
      | ([l, ..._] as pre, [r, ...suf]) when Wald.lt(l.wald, r.wald) =>
        ctx
        |> C.put_fst((pre, suf))
        |> go
        |> C.map_fst(Frame.Open.cons(~onto=R, r))
      | ([l, ...pre], [r, ..._] as suf) when Wald.gt(l.wald, r.wald) =>
        ctx
        |> C.put_fst((pre, suf))
        |> go
        |> C.map_fst(Frame.Open.cons(~onto=L, l))
      | ([l, ...pre], [r, ...suf]) =>
        assert(Wald.eq(l.wald, r.wald));
        ctx |> C.put_fst((pre, suf)) |> go |> C.link((l, r));
      };
    switch (sel) {
    | None => go(ctx)
    | Some(zigg) =>
      let (pre, suf) = C.fst(ctx);
      let (pre_lt, pre_geq) = Zigg.take_geq(pre, zigg);
      let (suf_leq, suf_gt) = Zigg.take_leq(zigg, suf);
      ctx
      |> C.put_fst((pre_lt, suf_gt))
      |> go
      |> C.map_fst(Frame.Open.cat((pre_geq, suf_leq)));
    };
  };
};
