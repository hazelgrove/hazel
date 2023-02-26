open Sexplib.Std;
open Util;

module Shape = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | T(Tile.t)
    | G(Grout.t);
};

module Path = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = int;
  let shift = (n, p) => p + n;
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  shape: Shape.t,
  paths: list(Path.t),
};

let mk = (~paths=[], shape) => {paths, shape};
let of_grout = (~paths=[], g) => mk(~paths, G(g));
let of_tile = (~paths=[], t) => mk(~paths, T(t));

let add_paths = (ps, p) => {...p, paths: ps @ p.paths};
let clear_paths = p => {...p, paths: []};

let is_porous = _ => failwith("todo is_porous");

let id = p =>
  switch (p.shape) {
  | G(g) => g.id
  | T(t) => t.id
  };

let length = (p: t) => {
  switch (p.shape) {
  | T(t) => Tile.length(t)
  | G(g) => Grout.length(g)
  };
};

let zip = (l: t, r: t): option(t) => {
  open OptUtil.Syntax;
  let paths = l.paths @ List.map(Path.shift(length(l)), r.paths);
  let+ shape =
    switch (l.shape, r.shape) {
    | (G(_), T(_))
    | (T(_), G(_)) => None
    | (G(g_l), G(g_r)) =>
      let+ g = Grout.zip(g_l, g_r);
      Shape.G(g);
    | (T(t_l), T(t_r)) =>
      let+ t = Tile.zip(t_l, t_r);
      Shape.T(t);
    };
  mk(~paths, shape);
};

let mold = p =>
  switch (p.shape) {
  | T(t) => t.mold
  | G(g) => g.mold
  };
let sort = p => Mold.sort_(mold(p));
let prec = p => Mold.prec_(mold(p));
let tip = (side, p) => Mold.tip(side, mold(p));
let tips = (side, p) => Mold.tips(side, mold(p));

let convexable = (side, p) => List.mem(Tip.Convex, tips(side, p));

let is_grout = p =>
  switch (p.shape) {
  | G(_) => true
  | T(_) => false
  };

let is_strict = p =>
  switch (p.shape) {
  | T(_) => true
  | G(g) => Grout.has_sugg(g)
  };

let zipper = (p: t): Gram.Zipper.a(_) => {
  let t =
    switch (p.shape) {
    | G(g) => g.sugg
    | T(t) => t.token
    };
  (Tok(LangUtil.shape_of_token(t)), mold(p).frames);
};

let complement = (~side: Dir.t, p: t): list((Token.t, Mold.t)) => {
  let rec go = z =>
    switch (Gram.Zipper.move_to_tok(~skip_nullable=true, side, z)) {
    // default to first alternative
    | [(Tok(Const(t)), frames) as z, ..._] =>
      let m = {...mold(p), frames};
      [(t, m), ...go(z)];
    | _ => []
    };
  go(zipper(p));
};

// let eq = (l: t, r: t): option(Sort.Ana.t) => {
//   switch (Mold.tip(R, mold(l))) {
//   | Convex => None
//   | Concave(sort, _) =>
//     let (z_l, z_r) = (zipper(l), zipper(r));
//     let (moved_l, moved_r) =
//       Gram.Zipper.(move_to_tok(R, z_l), move_to_tok(L, z_r));
//     let strict = is_strict(l) || is_strict(r);
//     List.mem(z_l, moved_r) && List.mem(z_r, moved_l)
//       ? Some(Sort.Ana.mk(~strict, ~sort, ())) : None;
//   };
// };
let eq = (_, _) => failwith("todo: find and return complement");

let eq_transitive = (l: t, r: t): bool => {
  let rec go = (z_l, z_r) => {
    let moved_r = Gram.Zipper.move_to_tok(L, z_r);
    List.mem(z_l, moved_r) ? true : List.exists(go(z_l), moved_r);
  };
  go(zipper(l), zipper(r));
};

// separate from cmp bc these are only relevant
// based on surrounding meld kids (see Meld.merge/degrout)
type dg =
  | Degrout
  | Fill(Dir.t)
  | Pass(Dir.t);

type degrouted =
  | Removed(Space.t, Space.t)
  | Replaced(Space.t, t, Space.t)
  | Passed;

let degrout = (l: t, r: t): option(dg) =>
  switch (l.shape, r.shape) {
  | (T(_), T(_)) => None
  | (G(_), _) when mold(l) == mold(r) => Some(Fill(L))
  | (_, G(_)) when mold(l) == mold(r) => Some(Fill(R))
  | (G(_), _) when eq_transitive(r, l) => Some(Pass(L))
  | (_, G(_)) when eq_transitive(r, l) => Some(Pass(R))
  | (G(g), _) when Grout.has_sugg(g) => None
  | (_, G(g)) when Grout.has_sugg(g) => None
  // todo: probably need strengthen this check for degrouting
  | (G(_), G(_))
      when Tip.fits(Mold.tip(L, mold(l)), Mold.tip(R, mold(r))) =>
    Some(Degrout)
  | (G(_), _)
      when Tip.same_shape(Mold.tip(L, mold(l)), Mold.tip(L, mold(r))) =>
    Some(Fill(L))
  | (_, G(_))
      when Tip.same_shape(Mold.tip(R, mold(l)), Mold.tip(R, mold(r))) =>
    Some(Fill(R))
  | _ => None
  };

let fst_set = _ => failwith("todo fst");

let lt_ = (l: t, r: t): list(Sort.Ana.t) => {
  open ListUtil.Syntax;
  let (l, r) = (mold(l), mold(r));
  let* (s, p) = Mold.concave_tips(R, l);
  let* tip = Sort.leq(s, r.sort) ? Mold.tips(L, r) : [];
  switch (tip) {
  | Concave(t, q)
      when Sort.geq(l.sort, t) && !Prec.lt(~a=LangUtil.assoc(l.sort), p, q) =>
    []
  | _ =>
    // todo: strict based on piece shape
    [Sort.Ana.mk(~sort=s, ())]
  };
};
// todo: avoid arbitrary preemptive choice
let lt = (l, r) => ListUtil.hd_opt(lt_(l, r));

let gt_ = (l: t, r: t): list(Sort.Ana.t) => {
  open ListUtil.Syntax;
  let (l, r) = (mold(l), mold(r));
  let* (s, p) = Mold.concave_tips(L, r);
  let* tip = Sort.geq(l.sort, s) ? Mold.tips(R, l) : [];
  switch (tip) {
  | Concave(t, q)
      when Sort.geq(r.sort, t) && !Prec.lt(~a=LangUtil.assoc(r.sort), p, q) =>
    []
  | _ => [Sort.Ana.mk(~sort=s, ())]
  };
};
let gt = (l, r) => ListUtil.hd_opt(gt_(l, r));

let replaces = (_, _): option(Dir.t) => failwith("todo Piece.replaces");

let unzip = (path: Path.t, p: t): Either.t(Dir.t, (t, t)) => {
  switch (p.shape) {
  | G(g) =>
    Grout.unzip(path, g)
    |> Either.map_r(((l, r)) => (of_grout(l), of_grout(r)))
  | T(t) =>
    Tile.unzip(path, t)
    |> Either.map_r(((l, r)) => (of_tile(l), of_tile(r)))
  };
};
