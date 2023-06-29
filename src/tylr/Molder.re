// walk criteria
// - overall length
// - height
// - if there's a middle kid, whether there's a slot that accommodates

module GZipper = {
  // a zipper into a Grammar.t
  type t('subj) = {
    sort: Sort.t,
    prec: Prec.t,
    zipper: Regex.Zipper.t('subj),
  };

  let map = failwith("todo");
  let map_opt = failwith("todo");
};

module Mold = {
  [@deriving (sexp, ord)]
  type t = GZipper.t(Label.t);
  module Ord = {
    type nonrec t = t;
    let compare = compare;
  };
  module Map = Map.Make(Ord);
  module Set = Set.Make(Ord);

  // mold imposes bound on neighbors
  let bound: (Dir.t, t) => Prec.Bound.t = failwith("todo");
};

// module Slot = {
//   type t = GZipper.t(Regex.t);

//   let empty = (~sort, ~prec) => GZipper.{sort, prec, zipper: (Regex.empty, Regex.Ctx.empty)};

//   // slot is bounded by neighboring molds
//   // let bound: (Dir.t, t) => option(Prec.Bound.t) = failwith("todo");
// };

module Piece = {
  // todo rename
  type t =
    | Grout
    | Tile(Mold.t);
};

// module Walk = {
//   type t = Chain.t(Slot.t, Piece.t);
// };

module Result = {
  include Result;
  type t = Result.t(Slope.Dn.t, Meld.t);
};

let union = Mold.Map.union((_, l, r) => Some(choose(~over?, l, r)));
let union_all = List.fold_left(union, Mold.Map.empty);

let is_operator = (operand_side: Dir.t, r: Regex.t) =>
  Regex.enter(~from=operand_side, r)
  |> List.exists(((a, _)) => Regex.Atom.is_kid(a));

module Stepped = {
  type t = (list(Terrace.R.t), list(Terrace.R.t));
  let cat = ((eq, lt), (eq', lt')) => (eq @ eq', lt @ lt');
  let concat = List.fold_left(cat, ([], []));
  let filter = f => TupleUtil.map2(List.filter(f));
};

let step_leq = (m: Mold.t): Stepped.t => {
  let rec step = (~lt=false, z: GZipper.t(Atom.t)) => {
    let last_kid =
      switch (z) {
      | (Tok(_), _) => Meld.empty()
      | (Kid(s), _) => failwith("todo mk grout with s");
      };
    Regex.step(R, z.zipper)
    |> List.map(stop_or_step(~lt, ~last_kid))
    |> Stepped.concat
  }
  and stop_or_step = (~lt, ~last_kid, z: GZipper.t(Atom.t)) =>
    switch (z.zipper) {
    | (Kid(s), _) =>
      let bound = Sort.eq(l.sort, s) ? bound : Prec.min;
      Grammar.enter(~from=L, ~bound, s)
      |> List.map(stop_or_step(~lt=true, ~last_kid))
      |> Stepped.concat
      |> Stepped.cat(step(~lt, z))
    | (Tok(lbl), ctx) =>
      let m = failwith("todo of lbl ctx sort prec");
      let p = failwith("todo using m");
      let ts = [Terrace.{mel: last_kid, wal: Wald.of_piece(p)}];
      lt ? ([], ts) : (ts, []);
    };
  step(Mold.to_atom(z));
};
// todo: maybe incorporate kid sort to sort results
let leq = (l: Mold.t, r: Mold.t): Stepped.t =>
  step_leq(l)
  |> Stepped.filter(t => Mold.eq(r, Terrace.R.face(t).mold));

let rec walk_leq = (~stepped_to=Mold.Set.empty, m: Mold.t): list(Slope.Dn.t) =>
  if (Mold.Set.mem(m, stepped_to)) {
    [];
  } else {
    let p = failwith("todo mk piece from m");
    let (eq, lt) = step_leq(m);
    let walk_tl = t => {
      let n = Terrace.face(t).mold;
      walk_leq(~stepped_to=Mold.Set.add(n, stepped_to), n);
    };
    let walked_eq =
      eq
      |> List.concat_map(walk_tl)
      |> List.map(Slope.Dn.map_top(Terrace.R.link(p)));
    let walked_lt =
      lt
      |> List.concat_map(walk_tl)
      |> List.map(Slope.Dn.cons(Terrace.of_piece(p)));
    walked_eq @ walked_lt;
  };
// todo: maybe incorporate kid sort to sort results
let leq_trans = (l: Mold.t, r: Mold.t): list(Slope.t) =>
  walk_leq(l)
  |> List.filter(s => Mold.eq(r, Slope.Dn.face(s).mold));
