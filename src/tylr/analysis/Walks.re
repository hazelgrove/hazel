open Util;

type t = Proto.Map.t(list(Walk.t));

// todo: use List.merge to keep walks ordered by whatever metric
let union2 = Proto.Map.union(
  (_, l, r) => Some(l @ r),
);
let union = List.fold_left(union2, Proto.Map.empty);

let enter_eq = {
  let go =
    Core.Memo.general(((bound, from: Dir.t, s)) =>
      List.assoc(s, v)
      |> List.mapi((prec, (r, a)) =>
           Regex.enter(~from, r, Regex.Unzipped.empty)
           |> List.filter(((atom, _)) =>
             Regex.Atom.is_tok(atom)
             || Prec.lower_bounded(~a, ~side=from, prec, bound)
           )
           |> List.concat_map(((a, uz) as z) =>
                switch (Regex.Atom.is_tok(a)) {
                | Some(lbl) => [(lbl, uz)]
                | None => Regex.walk_to_tok(Dir.toggle(from), z)
                }
              )
         )
      |> List.mapi((prec, zs) =>
           zs
           |> List.map(((label, unzipped)) => {
                let mold = Mold.{sort: s, prec, unzipped};
                Proto.{mold, label};
              })
         )
      |> List.concat
    );
  (~bound=Prec.min, ~from: Dir.t, s: Sort.t) => go((bound, from, s));
};

let rec enter =
    (~ctx=Prec.Ctx.init, ~walked=Walk.empty, ~from: Dir.t, s: Sort.t): t => {
  let p =
    Sort.Map.find(s, Grammar.v)
    |> Precex.frame
    |> Precex.bound
    |> Regex.normalize_failure;

  Regex.enter(~skip_nullable=false, ~from, p)
  |> List.filter_map(
    ((a, unzipped): Regex.Zipper.t(_)) =>
      switch (a) {
      | Tok(label) =>
        let mold = Mold.{sort: s, unzipped};
        let proto = Proto.{label, mold};
        Proto.Map.singleton(proto, walked);
      | Kid({frame, subj: s}) =>
        let entered =






        {
          let walked = Walk.hsup_kid(walked, kid);
          enter(~ctx, ~walked, ~from, s);
        };
        let entered_eq = {

        }
      }
  )
}
and step = (~walked=Walk.empty, ~ctx, z: Regex.Zipper.t(_)): t =>
  Regex.step(~skip_nullable=false, d, z)
  |> List.map(((a, unzipped) as z) =>
    switch (a) {
    | Tok(label) =>
      // complete step
      let mold = {...mold, unzipped};
      let proto = Proto.{label, mold};
      Proto.Map.singleton(proto, walked);
    | Kid(kid) =>
      // fork step
      let stepped_eq = {
        let walked = Walk.hsup_kid(walked, kid);
        go(~walked, ~ctx, z);
      };
      let stepped_lt = {
      };
    }
  )
  |> union;

and step =
    (~ctx=Prec.Ctx.init, ~walked=Walk.empty, d: Dir.t, {label, mold}: Proto.t): t => {
  go((Tok(label), mold.unzipped));
};

let step_lt =
    (~ctx=Prec.Ctx.init, {label, mold}: Proto.t): t => {

}


let step_eq: (Dir.t, Proto.t) => list((Walk.t, Proto.t)) =
  Core.Memo.general(((d, Proto.{label, mold})) =>
    Regex.move_to_tok(d, (label, mold.unzipped))
    |> List.map((w, (label, unzipped)) => {
         let mold = {...mold, unzipped};
         (w, Proto.{mold, label});
       })
  )
  |> FunUtil.curry2;

let step_lt: (Dir.t, Proto.t) => _ =
  Core.Memo.general(((d, Proto.{label, mold})) => {
    open Regex;
    let b = Dir.toggle(d);
    let bound = Unzipped.nullable(d, mold.unzipped) ? Some(mold.prec) : None;
    let neighbor_sorts =
      step(d, (Atom(label), mold.unzipped))
      |> List.filter_map(((w, (a, _))) => Atom.is_kid(a))
      |> List.concat_map(s => [s, ...SortDeps.uni_deps(b, s)])
      |> ListUtil.dedup;
    neighbor_sorts |> List.concat_map(enter_eq(~from=b, ~bound));
  })
  |> FunUtil.curry2;

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
        (~seen=Proto.Set.empty, d: Dir.t, p: Proto.t)
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
  Core.Memo.general(((d, proto)) => walk(d, proto)) |> FunUtil.curry2;
