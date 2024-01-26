open Util;

module Melded = {
  type t = Rel.t(Wald.t, Slope.t);

  let mk_eq = (src: Wald.t, bake, dst: Wald.t) => failwith("todo");
  let mk_neq = (bake: Bake.t, dst: Wald.t) => failwith("todo");
  let mk = (src: Wald.t, bake: Bake.t, dst: Wald.t) =>
    switch (Bake.is_eq(bake)) {
    | Some(eq) => Rel.Eq(mk_eq(src, eq, dst))
    | None => Rel.Neq(mk_neq(bake, dst))
    };
};

module W = Wald;
module Wald = {
  let meld_root =
      (~repair=false, ~from: Dir.t, ~fill=[], dst: W.t): option(Slope.t) =>
    Walker.walk(~from, Root, Node(W.face(dst)))
    |> Oblig.Delta.minimize(~to_zero=!repair, Baker.bake(~from, ~fill))
    |> Option.map(bake => Melded.mk_neq(bake, dst));

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
        let fill = Option.to_list(cell.meld) @ fill;
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
      let+ zipped = W.merge(~from, src, dst);
      assert(fill == []);
      Rel.Eq(zipped);
    };
    go(src, fill);
  };
};

module T = Terr;
module Terr = {
  module R = {
    include Terr.R;

    let connect = (terr: T.R.t, bake) =>
      bake
      |> Chain.fold_left(
           cell => Meld.M(terr.cell, terr.wald, cell),
           (meld, tok, cell) => Meld.link(~cell, tok, meld),
         )
      |> Meld.rev;

    let round = (~fill=[], terr: T.R.t) => {
      let bake = Baker.bake(~from=L);
      let exited = Walker.exit(R, Node(T.face(terr)));
      switch (Oblig.Delta.minimize(bake(~fill), exited)) {
      | Some(baked) => [connect(terr, baked)]
      | None =>
        let exited =
          ListUtil.hd_opt(exited)
          |> OptUtil.get_or_fail("bug: expected at least one exit");
        let baked =
          bake(exited)
          |> OptUtil.get_or_fail(
               "bug: bake expected to succeed if no fill required",
             );
        [connect(terr, baked), ...fill];
      };
    };
  };
};

module S = Slope;
module Slope = {
  // todo: distribute paths
  let unroll = (~from: Dir.t, cell: Cell.t) => {
    let rec go = (cell: Cell.t, unrolled) =>
      switch (cell.meld) {
      | None => unrolled
      | Some(M(l, W(w), r)) =>
        let (cell, terr) =
          switch (from) {
          | L => (r, Terr.{wald: Wald.rev(w), cell: l})
          | R => (l, Terr.{wald: w, cell: r})
          };
        go(cell, [terr, ...unrolled]);
      };
    go(cell, []);
  };

  let roll = (~onto: Dir.t, slope: t) =>
    List.fold_left(
      (cell, terr) => {
        let w = onto == L ? Wald.rev(terr.wald) : terr.wald;
        let (l, r) = Dir.order(onto, (terr.cell, cell));
        Cell.mk(Meld.mk(~l, w, ~r));
      },
      Cell.empty,
    );

  let push =
      (~repair=false, ~onto: Dir.t, w: W.t, ~fill=[], slope: S.t)
      : Result.t(S.t, list(Meld.t)) => {
    let meld = Wald.meld(~repair, ~from=onto);
    let round = Dir.pick(onto, (Terr.R.round, Terr.L.round));
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
        go(fill, S.cat(unroll(cell), slope));
      | [hd, ...tl] =>
        switch (meld(hd.wald, ~fill, w)) {
        | None => go(round(~fill, hd), tl)
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
      let unroll = unroll(Dir.toggle(from));
      let slope =
        switch (rest) {
        | ([], _) => S.cat(unroll(hd.cell), tl)
        | ([cell, ...cells], toks) =>
          let hd = {...hd, wald: Wald.mk(toks, cells)};
          S.cat(unroll(cell), [hd, ...tl]);
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
module Zigg = {
  let push_wald = (~onto: Dir.t, w: W.t, ~fill=[], zigg: Z.t): option(Z.t) =>
    switch (onto) {
    | L =>
      let top = zigg.top;
      switch (Slope.Up.push(w, ~fill, zigg.up, ~top)) {
      | Ok(up) => Some({...zigg, up})
      | Error(fill) =>
        Wald.meld_eq(~from=R, w, ~fill, top)
        |> Option.map(top => {...zigg, up: [], top})
      };
    | R =>
      let top = W.rev(zigg.top);
      switch (Slope.Dn.push(w, ~fill, zigg.dn, ~top)) {
      | Ok(dn) => Some({...zigg, dn})
      | Error(fill) =>
        Wald.meld_eq(~from=L, top, ~fill, w)
        |> Option.map(top => {...zigg, top, dn: []})
      };
    };
  let push = (~onto: Dir.t, tok: Token.t) => push_wald(~onto, W.unit(tok));

  let grow = (~side: Dir.t, tok: Token.t, zigg: Z.t) =>
    switch (push(~onto=side, tok, zigg)) {
    | Some(z) => z
    | None =>
      switch (side) {
      | L =>
        let cell = Slope.Up.roll(zigg.up);
        let dn = zigg.dn @ [T.{wald: Wald.rev(zigg.top), cell}];
        Zigg.mk(W.unit(tok), ~dn);
      | R =>
        let cell = Slope.Dn.roll(zigg.dn);
        let up = zigg.up @ [T.{wald: zigg.top, cell}];
        Zigg.mk(~up, W.unit(tok));
      }
    };

  let rec take_leq = (zigg: t, ~fill=[], suf: S.Up.t) =>
    switch (suf) {
    | [] => ([], suf)
    | [hd, ...tl] =>
      switch (push(~onto=R, hd.wald, ~fill, zigg)) {
      | None => ([], suf)
      | Some(zigg) =>
        let fill = Option.to_list(hd.cell.meld);
        let (leq, gt) = take_leq(zigg, ~fill, tl);
        ([hd, ...leq], gt);
      }
    };
  let rec take_geq = (pre: S.Dn.t, ~fill=[], zigg: t) =>
    switch (pre) {
    | [] => (pre, [])
    | [hd, ...tl] =>
      switch (push(~onto=L, hd.wald, ~fill, zigg)) {
      | None => (pre, [])
      | Some(zigg) =>
        let fill = Option.to_list(hd.cell.meld);
        let (lt, geq) = take_geq(tl, ~fill, zigg);
        (lt, [hd, ...geq]);
      }
    };
};

module C = Ctx;
module Ctx = {
  let push = (~onto as d: Dir.t, w: W.t, ~fill=[], ctx: C.t): option(C.t) => {
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
          C.link(slopes, terrs, ctx);
        | Eq(wald) =>
          let slopes = Dir.order(d, ([T.{...t_d, wald}], s_b @ [t_b]));
          C.map_fst(Frame.Open.cat(slopes), ctx);
        };
      }
    };
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
        Some((tok, Ctx.link(order((s_d, s_b)), terrs, ctx)))
      | None =>
        let (t_d, t_b) = Dir.order(d, terrs);
        let slopes = order(([t_d], s_b @ [t_b]));
        pull(~from=d, Ctx.map_fst(Frame.Open.cat(slopes), ctx));
      };
    };
  };

  let close = (~sel=?, ctx: C.t) => {
    let rec go = ctx =>
      switch (C.fst(ctx)) {
      | ([], _)
      | (_, []) => ctx
      | ([l, ..._] as pre, [r, ...suf]) when W.lt(l, r) =>
        ctx
        |> C.put_fst((pre, suf))
        |> go
        |> C.map_fst(Frame.Open.cons(~onto=R, r))
      | ([l, ...pre], [r, ..._] as suf) when W.gt(l, r) =>
        ctx
        |> C.put_fst((pre, suf))
        |> go
        |> C.map_fst(Frame.Open.cons(~onto=L, l))
      | ([l, ...pre], [r, ...suf]) =>
        assert(W.eq(l, r));
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
