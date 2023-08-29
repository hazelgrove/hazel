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
