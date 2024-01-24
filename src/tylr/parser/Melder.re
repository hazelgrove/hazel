open Util;

module Melded = {
  type t =
    | Eq(Wald.t)
    | Neq(Slope.t);

  let mk_eq = (src: Wald.t, bake: Bake.t, dst: Wald.t) => failwith("todo");
  let mk_neq = (bake: Bake.t, dst: Wald.t) => failwith("todo");
  let mk = (src: Wald.t, bake: Bake.t, dst: Wald.t) =>
    Bake.is_eq(bake) ? mk_eq(src, bake, dst) : mk_neq(bake, dst);
};

module W = Wald;
module Wald = {
  let meld =
      (~repair=false, ~from: Dir.t, src: Bound.t(W.t), ~fill=[], dst: W.t)
      : option(Melded.t) => {
    open OptUtil.Syntax;
    let rec go = (src, fill) => {
      let/ () = repair ? rm_ghost_and_go(src, fill) : None;
      let face_src = src |> Bound.map(w => W.face(w).mtrl);
      let face_dst = W.face(dst).mtrl;
      let walk = repair ? Walker.walk : Walker.step;
      let+ bake =
        walk(~from, face_src, Node(face_dst))
        |> Oblig.Delta.minimize(~to_zero=!repair, Baker.bake(~from, ~fill));
      Melded.mk(src, bake, dst);
    }
    and rm_ghost_and_go = (src, fill) =>
      switch (W.unlink(src)) {
      | Ok((hd, cell, tl)) when Token.is_ghost(hd) =>
        let fill = Option.to_list(cell.meld) @ fill;
        switch (go(tl, fill)) {
        // require eq match further in to accept removing hd
        | Some(Eq(_)) as r =>
          Effect.perform(Remove(hd));
          r;
        | _ => None
        };
      | _ => None
      };
    let/ () = {
      // first try zipping
      let* src = Bound.to_opt(src);
      let+ zipped = W.zip(~from, src, dst);
      assert(fill == []);
      Melded.Eq(zipped);
    };
    go(src, fill);
  };

  let meld_eq =
      (~repair=false, ~from: Dir.t, l: W.t, ~fill=[], r: W.t): option(W.t) =>
    switch (meld(~repair, ~from, Node(l), ~fill, r)) {
    | Some(Eq(w)) => Some(w)
    | _ => None
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
           (meld, tok, cell) => Meld.link(cell, tok, meld),
         )
      |> Meld.rev;

    let round = (~fill=[], terr: T.R.t) => {
      let bake = Baker.bake(~from=L);
      let exited = Walker.exit(R, T.face(terr));
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
  // distribute paths
  let unroll = (side: Dir.t, cell: Cell.t) => {
    let rec go = (cell, unrolled) =>
      switch (cell.meld) {
      | None => unrolled
      | Some(M(l, W(w), r)) =>
        let (cell, terr) =
          switch (side) {
          | L => (l, Terr.{wald: w, cell: r})
          | R => (r, Terr.{wald: Wald.rev(w), cell: l})
          };
        go(cell, [terr, ...unrolled]);
      };
    go(cell, []);
  };

  let push =
      (
        ~repair=false,
        ~onto: Dir.t,
        ~top=Bound.Root,
        w: W.t,
        ~fill=[],
        slope: S.t,
      )
      : Result.t(S.t, list(Meld.t)) => {
    let meld = Wald.meld(~repair, ~from=onto);
    let round = Dir.pick(onto, (Terr.R.round, Terr.L.round));
    let rec go = (fill, slope) =>
      switch (slope) {
      | [] =>
        switch (meld(top, ~fill, w)) {
        | None
        | Some(Eq(_)) => Error(fill)
        | Some(Neq(slope)) => Ok(slope)
        }
      | [{wald: W(([tok, ...toks], cells)), cell}, ...tl]
          when Token.is_grout(tok) =>
        Effect.perform(Remove(tok));
        let (cell, slope) =
          switch (cells) {
          | [] => (cell, tl)
          | [c, ...cs] =>
            let hd = Terr.{wald: W.mk(toks, cs), cell};
            (c, [hd, ...tl]);
          };
        go(fill, S.cat(unroll(cell), slope));
      | [hd, ...tl] =>
        switch (meld(Node(hd.wald), ~fill, w)) {
        | None => go(round(~fill, hd), tl)
        | Some(Eq(wald)) => Ok([{...hd, wald}, ...tl])
        | Some(Neq(s)) => Ok(S.cat(s, slope))
        }
      };
    go(fill, slope);
  };

  let pull = (~char=false, ~from: Dir.t, slope: S.t): option((Token.t, S.t)) =>
    switch (slope) {
    | [] => None
    | [hd, ...tl] =>
      let (tok, rest) = W.split_hd(hd.wald);
      switch (Token.pull(~from=Dir.toggle(from), tok)) {
      | Some((c, tok)) when char =>
        let hd = {...hd, wald: W.zip(tok, ~suf=rest)};
        Some((c, [hd, ...tl]));
      | _ =>
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
    };

  module Dn = {
    let unroll = unroll(R);
    let push = push(~onto=L);
    let pull = pull(~from=L);
  };
  module Up = {
    let unroll = unroll(L);
    let push = push(~onto=R);
    let pull = pull(~from=R);
  };
};

module Z = Zigg;
module Zigg = {
  let push = (~onto: Dir.t, w: W.t, ~fill=[], zigg: Z.t): option(Z.t) =>
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
      switch (Slope.Dn.push(~top, zigg.dn, ~fill, w)) {
      | Ok(dn) => Some({...zigg, dn})
      | Error(fill) =>
        Wald.meld_eq(~from=L, top, ~fill, w)
        |> Option.map(top => {...zigg, top, dn: []})
      };
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
  open OptUtil.Syntax;

  let push = (~onto: Dir.t, w: W.t, ~fill=[], ctx: C.t): option(C.t) =>
    switch (onto, C.unlink(ctx)) {
    | (L, Error((dn, up))) =>
      let+ dn = Slope.Dn.push(dn, ~fill, w);
      C.unit((dn, up));
    | (R, Error((dn, up))) =>
      let+ up = Slope.Up.push(w, ~fill, up);
      C.unit((dn, up));
    | (L, Ok(((dn, up), (l, r), ctx))) =>
      switch (Slope.Dn.push(~top=Node(l.wald), dn, ~fill, w)) {
      | Ok(dn) => Some(C.link((dn, up), (l, r), ctx))
      | Error(fill) =>
        let+ wald = Wald.meld_eq(l.wald, ~fill, w);
        let (dn, up) = ([{...l, wald}], up @ [r]);
        C.map_fst(Frame.Open.cat((dn, up)), ctx);
      }
    | (R, Ok(((dn, up), (l, r), ctx))) =>
      switch (Slope.Up.push(w, ~fill, up, ~top=Node(r.wald))) {
      | Ok(up) => Some(C.link((dn, up), (l, r), ctx))
      | Error(fill) =>
        let+ wald = Wald.meld_eq(w, ~fill, r.wald);
        let (dn, up) = (dn @ [l], [{...r, wald}]);
        C.map_fst(Frame.Open.cat((dn, up)), ctx);
      }
    };

  let close = (~sel=?, ctx: C.t) => {
    let rec go = ctx =>
      switch (C.fst(frame)) {
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

  let pull_lexeme = (~char=false, ~from: Dir.t, well: Stepwell.t) =>
    switch (Slopes.pull_lexeme(~char, ~from, Stepwell.get_slopes(well))) {
    | Some((a, sib)) => Some((a, Stepwell.put_slopes(sib, well)))
    | None =>
      open OptUtil.Syntax;
      let+ (slopes, bridge, well) = Chain.unlink(well);
      let (lx, slopes') = Bridge.pull_lexeme(~char, ~from, bridge);
      let well =
        well |> push_slopes(Slopes.cat(slopes, slopes')) |> assemble;
      (lx, well);
    };
};
