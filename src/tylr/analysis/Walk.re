open Util;

// todo: move elsewhere
let assoc = (s, p): Assoc.t => {
  open OptUtil.Syntax;
  let* s = s;
  let* (_, a) = p < 0 ? None : List.nth_opt(List.assoc(s, v), p);
  a;
};

module Slot = {
  /**
   * cf `Walk`
   */
  type t = {
    cmp: Cmp.t,
    reg: Regex.t(Sort.t),
  };
};

/**
  * A sequence of intermediate consecutive prototiles that are expected
  * to appear between a pair of bounding prototiles (not specified here).
  * A walk acts as a witness to the precedence relationship between its bounds,
  * and its contents specify how to error-correct if the two bounds appear
  * on either side of some bottom-up-accumulated middle term (possibly empty)
  * while parsing.
  * The slot between a pair of consecutive prototiles indicates how the two
  * are precedence-related and what possible edit state content may appear
  * there; this info is used to determine where to place the middle term
  * relative to the error-correction-inserted prototiles.
  */
type t = Chain.t(Slot.t, Proto.t);

let enter_eq = {
  let go =
    Core.Memo.general(((bound, from: Dir.t, s)) =>
      List.assoc(s, v)
      |> List.mapi((prec, (r, a)) => {
           let bounded_entry = ((atom, _)) => {
             let well_bounded =
               switch (bound) {
               | None => true
               | Some(bound) => Prec.is_bounded(~on=from, ~a, prec, bound)
               };
             well_bounded || Regex.Atom.is_tok(atom);
           };
           Regex.enter(~from, r, Regex.Unzipped.empty)
           |> List.filter(bounded_entry)
           |> List.concat_map(((a, uz) as z) =>
                switch (Regex.Atom.is_tok(a)) {
                | Some(lbl) => [(lbl, uz)]
                | None => Regex.walk_to_tok(Dir.toggle(from), z)
                }
              );
         })
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

let step_eq: (Util.Dir.t, Proto.t) => list((Walk.t, Proto.t)) =
  Core.Memo.general(((d, Proto.{label, mold})) =>
    Regex.move_to_tok(d, (label, mold.unzipped))
    |> List.map((w, (label, unzipped)) => {
         let mold = {...mold, unzipped};
         (w, Proto.{mold, label});
       })
  )
  |> Util.FunUtil.curry2;

let step_lt: (Util.Dir.t, Proto.t) => _ =
  Core.Memo.general(((d, Proto.{label, mold})) => {
    open Regex;
    let b = Util.Dir.toggle(d);
    let bound = Unzipped.nullable(d, mold.unzipped) ? Some(mold.prec) : None;
    let neighbor_sorts =
      step(d, (Atom(label), mold.unzipped))
      |> List.filter_map(((w, (a, _))) => Atom.is_kid(a))
      |> List.concat_map(s => [s, ...SortDeps.uni_deps(b, s)])
      |> Util.ListUtil.dedup;
    neighbor_sorts |> List.concat_map(enter_eq(~from=b, ~bound));
  })
  |> Util.FunUtil.curry2;

// module Walk = {
//   open Util;

//   module Level = {
//     type t = Chain.t(option(Sort.t), Proto.t);

//     let empty = Chain.of_loop(None);

//     let compare = (l, r) => Int.compare(List.length(l), List.length(r));
//   };
//   type t = Chain.t(Level.t, unit);

//   let compare = (l, r) => {
//     let (m, n) = Chain.(length(l), length(r));
//     if (m < n) {
//       (-1);
//     } else if (m > n) {
//       1;
//     } else {
//       let (l, r) = Chain.(loops(l), loops(r));
//       List.fold_left2(
//         (c, l, r) => c == 0 ? Level.compare(l, r) : c,
//         0,
//         l,
//         r,
//       );
//     };
//   };

//   let add_step = (side: Dir.t, ~kid=?, p: Proto.t) =>
//     switch (side) {
//     | L => Chain.map_fst(Chain.link(kid, p))
//     | R => Chain.map_lst(lvl => Chain.knil(lvl, p, kid))
//     };
//   let add_level = (side: Dir.t, w: t) =>
//     switch (side) {
//     | L => Chain.link(Level.empty, (), w)
//     | R => Chain.knil(w, (), Level.empty)
//     };
// };

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
