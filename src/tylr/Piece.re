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

// todo: fix hack, reorg gram zipper
let zipper_const = ((a, frames): Gram.Zipper.a(_)) =>
  switch (a) {
  | Tok(Const(t)) => Some((t, frames))
  | _ => None
  };
let complement_beyond = (~side: Dir.t, p: t): list((Token.t, Mold.t)) => {
  let rec go = z =>
    switch (Gram.Zipper.move_to_tok(~skip_nullable=true, side, z)) {
    // | [z, ..._] when Option.map(zipper, upto) == Some(z) => []
    // default to first alternative
    | [(Tok(Const(t)), frames) as z, ..._] =>
      let m = {...mold(p), frames};
      [(t, m), ...go(z)];
    | _ => []
    };
  go(zipper(p));
};
let complement_between = (l: t, r: t): option(list((Token.t, Mold.t))) => {
  let mk_mold = frames => {...mold(l), frames};
  let rec go = (l, r) => {
    let moved = Gram.Zipper.move_to_tok(~skip_nullable=true, R, l);
    if (List.mem(r, moved)) {
      zipper_const(l)
      |> Option.map(((t, frames)) => [(t, mk_mold(frames))]);
    } else {
      moved
      |> List.map(l => (l, go(l, r)))
      |> List.filter(((_, found_r)) => Option.is_some(found_r))
      |> List.filter_map(((l, found_r)) =>
           zipper_const(l)
           |> Option.map(((t, frames)) => ((t, mk_mold(frames)), found_r))
         )
      |> ListUtil.hd_opt
      |> Option.map(((l, compl)) => [l, ...Option.get(compl)]);
    };
  };
  go(zipper(l), zipper(r)) |> Option.map(List.tl);
};

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
