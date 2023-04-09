type t = list((Sort.t, Precex.t));

let enter_eq = {
  let go =
    Core.Memo.general(((bound, from, s)) =>
      List.assoc(s, v)
      |> Precex.enter_eq(~from, ~bound?)
      |> List.mapi((prec, zs) =>
           zs
           |> List.map(((label, unzipped)) => {
                let mold = Mold.{sort: s, prec, unzipped};
                Proto.{mold, label};
              })
         )
      |> List.concat
    );
  (~bound=?, ~from: Dir.t, s: Sort.t) => go((bound, from, s));
};

let step_eq =
  Core.Memo.general(((d, Proto.{label, mold})) =>
    Regex.Zipper.move_to_tok(d, (label, mold.unzipped))
    |> List.map(((label, unzipped)) => {
         let mold = {...mold, unzipped};
         Proto.{mold, label};
       })
  )
  |> Util.FunUtil.curry2;

let step_lt =
  Core.Memo.general(((d, Proto.{label, mold})) => {
    open Regex;
    let b = Util.Dir.toggle(d);
    let bound = Unzipped.nullable(d, mold.unzipped) ? Some(mold.prec) : None;
    let neighbor_sorts =
      Zipper.step(d, (label, mold.unzipped))
      |> List.filter_map(((a, _)) => Atom.is_kid(a))
      |> List.concat_map(s => [s, ...SortDeps.uni_deps(b, s)])
      |> Util.ListUtil.dedup;
    neighbor_sorts |> List.concat_map(enter_eq(~from=b, ~bound));
  })
  |> Util.FunUtil.curry2;

module Walk = {
  open Util;

  module Level = {
    type t = list(Proto.t);

    let compare = (l, r) => Int.compare(List.length(l), List.length(r));
  };
  type t = Chain.t(Level.t, unit);

  let compare = (l, r) => {
    let (m, n) = Chain.(length(l), length(r));
    if (m < n) {
      (-1);
    } else if (m > n) {
      1;
    } else {
      List.fold_left2();
    };
  };

  let add_step = (side: Dir.t, p: Proto.t) =>
    switch (side) {
    | L => Chain.map_fst(List.cons(p))
    | R => Chain.map_lst(eq => eq @ [p])
    };
  let add_level = (side: Dir.t, w: t) =>
    switch (side) {
    | L => Chain.link([], (), w)
    | R => Chain.knil(w, (), [])
    };
};

let rec walk =
        (~seen=Proto.Set.empty, d: Util.Dir.t, p: Proto.t)
        : Proto.Map.t(list(Walk.t)) =>
  if (Proto.Set.mem(p, seen)) {
    Proto.Map.empty;
  } else {
    open Util;
    let b = Dir.toggle(d);
    let walk_tl = q =>
      q
      |> walk(~seen=Proto.Set.add(q, seen), d)
      |> Proto.Map.map(List.map(Walk.add_step(b, q)));
    let walked_eq = p |> step_eq(d) |> List.map(walk_tl);
    let walked_lt =
      p
      |> step_lt(d)
      |> List.map(walk_tl)
      |> List.map(Proto.Map.map(List.map(Walk.add_level(b))));
    walked_eq
    @ walked_lt
    |> List.fold_left(
         Proto.Map.union((_, l, r) => List.merge(Walk.compare, l, r)),
         Proto.Map.empty,
       );
  };
let walk =
  Core.Memo.general(((d, proto)) => walk(d, proto)) |> Util.FunUtil.curry2;
