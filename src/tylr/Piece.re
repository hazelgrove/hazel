open Sexplib.Std;
open Util;

module Path = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = int;
  let shift = (n, p) => p + n;
};

// todo: rename to something like Material
module Shape = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | T(Tile.t)
    | G(Grout.t);

  let t = t => T(t);
  let g = g => G(g);
};

// invariant: !is_empty(proto.label) ==> is_prefix(filled, proto.label)
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  paths: list(Path.t),
  shape: Shape.t,
};

let mk = (~id=?, ~paths=[], shape: Shape.t) => {
  let id = id |> OptUtil.get(() => Id.Gen.next());
  {id, paths, shape};
};
let of_grout = (~id=?, ~paths=[], g) => mk(~id?, ~paths, G(g));
let of_tile = (~id=?, ~paths=[], t) => mk(~id?, ~paths, T(t));

let add_paths = (ps, p) => {...p, paths: ps @ p.paths};
let clear_paths = p => {...p, paths: []};

// let is_porous = _ => failwith("todo is_porous");

// todo: rename
let is_empty = p =>
  switch (p.shape) {
  | G(g) when Grout.is_empty(g) => true
  | _ => false
  };

let id = p => p.id;

let length = (p: t) => {
  switch (p.shape) {
  | T(t) => Tile.length(t)
  | G(g) => Grout.length(g)
  };
};

let proto = p =>
  switch (p.shape) {
  | G(g) => g.proto
  | T(t) => t.proto
  };
let label = p => proto(p).label;
let mold = p => proto(p).mold;
let sort = p => Mold.sort_(mold(p));
let prec = p => Mold.prec_(mold(p));
let tip = (side, p) => Mold.tip(side, mold(p));
let tips = (side, p) => Mold.tips(side, mold(p));

let convexable = (side, p) => List.mem(Tip.Convex, tips(side, p));

// let is_strict = p =>
//   switch (material(p)) {
//   | Tile => true
//   | Grout => false
//   };

let zipper = (p: t): Gram.Zipper.a(_) => (
  Tok(LangUtil.shape_of_token(label(p))),
  mold(p).frames,
);

// todo: fix hack, reorg gram zipper
let zipper_const = ((a, frames): Gram.Zipper.a(_)) =>
  switch (a) {
  | Tok(Const(t)) => Some((t, frames))
  | _ => None
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

let zip = (l: t, r: t): option(t) => {
  open OptUtil.Syntax;
  let+ () = OptUtil.of_bool(Id.eq(l.id, r.id));
  let shape =
    switch (l.shape, r.shape) {
    | (G(_), T(_))
    | (T(_), G(_)) =>
      failwith("unexpected: same-id pieces of different shapes")
    | (G(l), G(r)) => Shape.G(Grout.zip(l, r))
    | (T(l), T(r)) => Shape.T(Tile.zip(l, r))
    };
  let paths = l.paths @ List.map(Path.shift(length(l)), r.paths);
  mk(~paths, shape);
};
let zips = (l, r) => Option.is_some(zip(l, r));

let replace = (l: t, r: t): option(t) => {
  let replaced_l = add_paths(List.map(_ => 0, l.paths), r);
  let replaced_r = add_paths(List.map(_ => length(l), r.paths), l);
  switch (l.shape, r.shape) {
  | (G(g), _)
      when Grout.is_empty(g) && Tips.consistent(tips(L, l), tips(L, r)) =>
    Some(replaced_l)
  | (_, G(g))
      when Grout.is_empty(g) && Tips.consistent(tips(R, l), tips(R, r)) =>
    Some(replaced_r)
  | (T(t), _) when !Tile.is_filled(t) && proto(l) == proto(r) =>
    Some(replaced_l)
  | (_, T(t)) when !Tile.is_filled(t) && proto(l) == proto(r) =>
    Some(replaced_r)
  | _ => None
  };
};

let merge = (l: t, r: t): option(t) =>
  switch (l.shape, r.shape) {
  | (G(g_l), G(g_r)) =>
    Grout.merge(g_l, g_r)
    |> Option.map(of_grout(~id=l.id, ~paths=l.paths @ r.paths))
  | _ => None
  };

let fuse = (l: t, r: t): option(t) => {
  open OptUtil.Syntax;
  let/ () = zip(l, r);
  let/ () = replace(l, r);
  merge(l, r);
};

let matches = (l: t, r: t): option(Complement.t) => {
  let mk_mold = frames => {...mold(l), frames};
  let rec go = (l, r) => {
    let moved = Gram.Zipper.move_to_tok(~skip_nullable=true, R, l);
    if (List.mem(r, moved)) {
      zipper_const(l)
      |> Option.map(((label, frames)) =>
           [Proto.{label, mold: mk_mold(frames)}]
         );
    } else {
      moved
      |> List.map(l => (l, go(l, r)))
      |> List.filter(((_, found_r)) => Option.is_some(found_r))
      |> List.filter_map(((l, found_r)) =>
           zipper_const(l)
           |> Option.map(((label, frames)) =>
                (Proto.{label, mold: mk_mold(frames)}, found_r)
              )
         )
      |> ListUtil.hd_opt
      |> Option.map(((l, compl)) => [l, ...Option.get(compl)]);
    };
  };
  go(zipper(l), zipper(r)) |> Option.map(List.tl);
};

let complement_beyond = (~side: Dir.t, p: t): Complement.t => {
  let rec go = z =>
    switch (Gram.Zipper.move_to_tok(~skip_nullable=true, side, z)) {
    // | [z, ..._] when Option.map(zipper, upto) == Some(z) => []
    // default to first alternative
    | [(Tok(Const(label)), frames) as z, ..._] =>
      let mold = {...mold(p), frames};
      [Proto.{label, mold}, ...go(z)];
    | _ => []
    };
  go(zipper(p));
};

let fst_mold = (cmpl: Complement.t, p: t) =>
  ListUtil.hd_opt(cmpl)
  |> Option.map(Proto.mold_)
  |> Option.value(~default=mold(p));
let lst_mold = (p: t, cmpl: Complement.t) =>
  ListUtil.last_opt(cmpl)
  |> Option.map(Proto.mold_)
  |> Option.value(~default=mold(p));

let lt = (l: t, r: t): option(Complement.t) => {
  open OptUtil.Syntax;
  let* (s_l, p_l) = Mold.concavable(R, mold(l));
  let* _ = LangUtil.takes(s_l, sort(r));
  let cmpl = complement_beyond(~side=L, r);
  switch (Mold.concavable(L, fst_mold(cmpl, r))) {
  | Some((s_r, p_r))
      when Sort.eq(s_l, s_r) && !Prec.lt(~a=LangUtil.assoc(s_l), p_l, p_r) =>
    None
  | _ => Some(cmpl)
  };
};

let gt = (l: t, r: t): option(Complement.t) => {
  open OptUtil.Syntax;
  let* (s_r, p_r) = Mold.concavable(L, mold(r));
  let* _ = LangUtil.takes(s_r, sort(l));
  let cmpl = complement_beyond(~side=R, l);
  switch (Mold.concavable(R, lst_mold(l, cmpl))) {
  | Some((s_l, p_l))
      when Sort.eq(s_l, s_r) && !Prec.gt(~a=LangUtil.assoc(s_l), p_l, p_r) =>
    None
  | _ => Some(cmpl)
  };
};
