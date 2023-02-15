// todo: unify as
// type t = {hd: Piece.t, tl: Meld.t};
type l = (Piece.t, Meld.t);
type r = (Meld.t, Piece.t);

// maybe move this to zigg
type t = Chain.t(Piece.t, Meld.t);

let open_l = (kid, (p, mel): l) => Meld.link(~kid, p, mel);
let open_r = ((mel, p): r, kid) => Meld.knil(mel, p, ~kid);

let mk_l = (mel: Meld.t): option((Meld.t, l)) =>
  Meld.unlink(mel)
  |> Result.to_option
  |> Option.map(((kid, p, tl)) => (kid, (p, tl)));
let mk_r = (mel: Meld.t): option((r, Meld.t)) =>
  Meld.unknil(mel)
  |> Result.to_option
  |> Option.map(((tl, p, kid)) => ((tl, p), kid));

let prepend = (_: Closed.r, _: t) => failwith("todo prepend");
let append = (_: t, _: Closed.l) => failwith("todo append");

let lt = (l: r, ~kid=empty(), r: l): option(t) => {
  open OptUtil.Syntax;
  let (_, p_l) = l;
  let (p_r, _) = r;
  let+ _ = Piece.lt(p_l, p_r);
  let kid =
    switch (Piece.tip(L, p_r)) {
    | Convex =>
      assert(Option.is_some(is_empty(kid)));
      kid;
    | Concave(s, _) =>
      // todo review strict flag
      Meld.complete(~expected=Sort.Ana.mk(~sort=s, ()), kid)
    };
  open_l(kid, r);
};

let gt = (l: r, ~kid=empty(), r: l): option(t) => {
  open OptUtil.Syntax;
  let (_, p_l) = l;
  let (p_r, _) = r;
  let+ _ = Piece.gt(p_l, p_r);
  let kid =
    switch (Piece.tip(R, p_l)) {
    | Convex =>
      assert(Option.is_some(is_empty(kid)));
      kid;
    | Concave(s, _) =>
      // todo review strict flag
      Meld.complete(~expected=Sort.Ana.mk(~sort=s, ()), kid)
    };
  open_r(l, kid);
};

let rec eq = (l: r, ~kid=empty(), r: l): option(Meld.t) => {
  open OptUtil.Syntax;
  let (tl_l, p_l) = l;
  let (p_r, tl_r) = r;
  switch (is_empty(kid), Piece.replaces(p_l, p_r), Piece.passes(p_l, p_r)) {
  | (Some(s), Some(L), _) => return(append(pad(tl_l, ~r=s), r))
  | (Some(s), Some(R), _) => return(prepend(l, pad(~l=s, tl_r)))
  | (Some(s), _, Some(L)) =>
    let* (l, kid) = mk_r(tl_l);
    let kid = Meld.pad(kid, ~r);
    eq(l, ~kid, r);
  | (Some(s), _, Some(R)) =>
    let (kid, r) = mk_l(tl_r);
    let kid = Meld.pad(~l, kid);
    eq(l, ~kid, r);
  | _ =>
    let+ compl = Piece.eq(p_l, p_r);
    // todo: abstract into some join-complement fn
    let (hd_r, tl_r) =
      List.fold_right(
        ((sugg, mold), (p, tl)) =>
          switch (Mold.tip(R, mold)) {
          | Convex => raise(Invalid_prec)
          | Concave(s, _) =>
            let kid = Some(of_grout(Grout.mk_convex(s)));
            let g = Piece.of_grout(Grout.mk(~sugg, mold));
            (g, Meld.link(~kid, p, tl));
          },
        compl,
        (p_r, tl_r),
      );
    prepend(l, link(~kid, hd_r, tl_r));
  };
};

type cmp = {
  lt: option(t),
  eq: option(t),
  gt: option(t),
};
let cmp = (l: r, ~kid=empty(), r: l) => {
  lt: lt(l, ~kid, r),
  eq: eq(l, ~kid, r),
  gt: gt(l, ~kid, r),
};
