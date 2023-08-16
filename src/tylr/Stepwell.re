open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Chain.t(Slopes.t, Bridge.t);

// base slopes
let of_slopes: Slopes.t => t = Chain.of_loop;
let get_slopes: t => Slopes.t = Chain.fst;
let map_slopes: (_, t) => t = Chain.map_fst;
let put_slopes = sib => map_slopes(_ => sib);
let cons_slopes = (sib, rel) => map_slopes(Slopes.cat(sib), rel);

let cons_bridge = bridge => Chain.link(Slopes.empty, bridge);

let empty = of_slopes(Slopes.empty);

let cat: (t, t) => t = Chain.cat(Slopes.cat);
let concat = (rels: list(t)) => List.fold_right(cat, rels, empty);

// todo: rename relative to cons_slopes
let cons_slope = (~onto: Dir.t, slope: Slope.t, rel) =>
  List.fold_left(
    (rel, terr) => cons(~onto, terr, rel),
    rel |> cons_space(~onto, slope.space),
    slope.terrs,
  );

let cons_zigg = (~onto: Dir.t, {up, top, dn}: Ziggurat.t, rel) => {
  let cons_top =
    switch (top) {
    | None => Fun.id
    | Some(top) => cons(~onto, Terrace.of_wald(top))
    };
  switch (onto) {
  | L =>
    rel
    |> cons_slope(~onto, up)
    |> cons_top
    |> cons_slopes(Slopes.mk(~l=dn, ()))
  | R =>
    rel
    |> cons_slope(~onto, dn)
    |> cons_top
    |> cons_slopes(Slopes.mk(~r=up, ()))
  };
};

let cons_lexeme = (~onto: Dir.t, lx: Lexeme.t(_)) =>
  switch (lx) {
  | S(s) => cons_space(~onto, s)
  | T(p) => cons(~onto, Terrace.of_piece(p))
  };

let pull_lexeme = (~char=false, ~from: Dir.t, well) =>
  switch (Slopes.pull_lexeme(~char, ~from, get_slopes(well))) {
  | Some((a, sib)) => Some((a, put_slopes(sib, well)))
  | None =>
    open OptUtil.Syntax;
    let+ (sib, par, well) = Chain.unlink(well);
    let (a, par) = Bridge.uncons_lexeme(~char, ~from, par);
    let well = well |> cons_slopes(Slopes.cat(sib, par)) |> assemble;
    (a, well);
  };

let pull_lexable = (~from: Dir.t, ctx: t): (option(Piece.t), Stepwell.t) =>
  switch (pull_lexeme(~from=L, ctx)) {
  | Some((T(p), ctx)) when Piece.(is_finished(p) || is_grout(p)) => (
      Some(p),
      ctx,
    )
  | _ => (None, ctx)
  };

// let bounds = (rel: t): (option(Terrace.R.t), option(Terrace.L.t)) => {
//   let bounds = Slopes.bounds(get_slopes(rel));
//   switch (bounds, Chain.unlink(rel)) {
//   | (_, None)
//   | ((Some(_), Some(_)), _) => bounds
//   | ((None, _) | (_, None), Some((_, (l, r), _))) =>
//     let l = Option.value(fst(bounds), ~default=l);
//     let r = Option.value(snd(bounds), ~default=r);
//     (Some(l), Some(r));
//   };
// };

// let zip = (rel: t): Meld.t =>
//   rel
//   |> Chain.fold_left(Slopes.zip_init, (zipped, par, sib) =>
//        zipped |> Bridge.zip(par) |> Slopes.zip(sib)
//      );

let unzip_end = (~unzipped, side: Dir.t, mel: Meld.t) =>
  unzipped
  |> cons_slopes(
       side == L
         ? Slope.(Dn.empty, Up.of_meld(mel))
         : Slope.(Dn.of_meld(mel), Up.empty),
     );
// |> mk;

// todo: standardize a la unzip_piece
let unzip_space = (s: Space.t, unzipped) => {
  let (paths, s) = (s.paths, Space.clear_paths(s));
  let (l, _sel, r) =
    switch (paths) {
    | [] => Space.(empty, empty, s)
    | [n] =>
      let (l, r) = Space.split(n, s);
      (l, Space.empty, r);
    | [m, n, ..._] =>
      assert(m <= n);
      let (s, r) = Space.split(n, s);
      let (l, s) = Space.split(m, s);
      (l, s, r);
    };
  unzipped |> cons_slopes(Slope.(Dn.mk(~s=l, []), Up.mk(~s=r, [])));
  // |> mk(~sel=Segment.s(sel));
};

let unzip_piece = (p: Piece.t) => {
  let (paths, p) = (p.paths, Piece.clear_paths(p));
  let ret = (~l=?, ~sel=?, ~r=?, ()) => (l, sel, r);
  let single_path = n =>
    switch (Piece.unzip(n, p)) {
    | L(L) => ret(~r=p, ())
    | L(R) => ret(~l=p, ())
    | R((l, r)) => ret(~l, ~r, ())
    };
  switch (paths) {
  | [] => ret(~r=p, ())
  | [n] => single_path(n)
  // only handling up to two paths marking selection atm
  | [m, n, ..._] =>
    assert(m <= n);
    switch (Piece.unzip(n, p)) {
    | L(L) => ret(~r=p, ())
    | L(R) => single_path(m)
    | R((p, r)) =>
      switch (Piece.unzip(m, p)) {
      | L(L) => ret(~sel=p, ~r, ())
      | L(R) => ret(~l=p, ~r, ())
      | R((l, p)) => ret(~l, ~sel=p, ~r, ())
      }
    };
  };
};

let unzip_lex = (lex: Path.Lex.t, mel: Meld.t, unzipped) =>
  switch (lex) {
  | Space(L) =>
    let (l, r) = mel.space;
    let mel = {...mel, space: (Space.empty, r)};
    unzipped
    |> cons_slopes(Slopes.mk(~r=Slope.Up.of_meld(mel), ()))
    |> unzip_space(l);
  | Space(R) =>
    let (l, r) = mel.space;
    let mel = {...mel, space: (l, Space.empty)};
    unzipped
    |> cons_slopes(Slopes.mk(~l=Slope.Dn.of_meld(mel), ()))
    |> unzip_space(r);
  | Piece(n) =>
    let (mel_l, p, mel_r) = Meld.split_piece(n, mel);
    // todo: restore for unzipping selections
    let (p_l, _p_sel, p_r) = unzip_piece(p);
    // let sel =
    //   p_sel
    //   |> Option.map(Segment.of_piece)
    //   |> Option.value(~default=Segment.empty);
    let l =
      p_l
      |> Option.map(p => Meld.knil(mel_l, p))
      |> Option.value(~default=mel_l)
      |> Slope.Dn.of_meld;
    let r =
      p_r
      |> Option.map(p => Meld.link(p, mel_r))
      |> Option.value(~default=mel_r)
      |> Slope.Up.of_meld;
    unzipped |> cons_slopes(Slopes.mk(~l, ~r, ()));
  // |> mk(~sel);
  };

let rec unzip = (~unzipped=empty, mel: Meld.t): t => {
  let (paths, mel) = (mel.paths, Meld.distribute_paths(mel));
  switch (Paths.hd_kid(paths)) {
  | Some(kid) =>
    let mel = Meld.distribute_space(mel);
    switch (Bridge.unzip(kid, mel)) {
    | Some((kid, b)) =>
      let unzipped = cons_bridge(b, unzipped);
      unzip(~unzipped, kid);
    | None =>
      let (kid, slopes) =
        if (kid == 0) {
          let (kid, t) =
            Terrace.L.mk(mel) |> OptUtil.get_or_raise(Path.Invalid);
          (kid, Slope.(Dn.empty, Up.of_terr(t)));
        } else {
          let (t, kid) =
            Terrace.R.mk(mel) |> OptUtil.get_or_raise(Path.Invalid);
          (kid, Slope.(Dn.of_terr(t), Up.empty));
        };
      let unzipped = cons_slopes(slopes, unzipped);
      unzip(~unzipped, kid);
    };
  | None =>
    switch (Paths.hd_lex(paths)) {
    | Some(lex) => unzip_lex(lex, mel, unzipped)
    | None =>
      // failwith("todo: unzipping selections")
      print_endline("zero or multiple paths found");
      unzip_end(~unzipped, L, mel);
    }
  };
};
