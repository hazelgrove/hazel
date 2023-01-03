// module Cursor = {
//   module Pointing = {
//     type t =
//       | Space(ZSpace.t)
//       | Chain(ZChain.t);
//   };

//   module Selecting = {
//     type t =
//       | Space(ZZSpace.t)
//       | Segment(ZZSegment.t);
//   };

//   type t =
//     | Pointing(Pointing.t)
//     | Selecting(Selecting.t);
// };

module Selection = {
  type t = {
    focus: Dir.t,
    space: (Space.Z.t, Space.Z.t),
    segment: Segment.t,
  };
};

module Cursor = {
  type t =
    | Pointing(ZChain.t)
    | Selecting(Selection.t);
};

type t = {
  cur: Cursor.t,
  rel: Relatives.t,
};

module Action = {
  type t =
    | Move(Dir.t)
    | Select(Dir.t)
    | Delete(Dir.t)
    | Insert(string);
};

// let normalize_d = (z: t): option((Dir.t, Space.t)) =>
//   switch (z.cur) {
//   | Pointing(Space(zs)) when ZSpace.is_empty(zs) =>
//     Some(Relatives.choose_cur(z.rel))
//   | Pointing(Space(zs)) when ZSpace.is_extreme(L, zs) => Some(L)
//   | Pointing(Space(zs)) when ZSpace.is_extreme(R, zs) => Some(R)
//   | _ => None
//   };
// let normalize = (z: t): t =>
//   switch (normalize_d(z)) {
//   | None => z
//   | Some((d, s)) =>
//     switch (Relatives.(split_zchain(d, push_space(s, z.rel)))) {
//     | None => z
//     | Some((zc, rel)) => {rel, cur: Pointing(Chain(zc))}
//     }
//   };
// exception Abnormal;

// promote empty Selecting to Pointing
// disambiguate Pointing term as needed
let normalize = (_: t): t => failwith("todo normalize");

// result is not necessarily normalized
let move = (d: Dir.t, zipper: t): option(t) => {
  open OptUtil.Syntax;
  let b = Dir.toggle(d);
  switch (zipper.cur) {
  | Selecting(sel) =>
    // unselect
    let (z, _) = Dir.choose(d, sel.space);
    let cur =
      Cursor.Selecting({...sel, space: (z, z), segment: Segment.empty});
    let rel = Relatives.push_seg(b, selected, z.rel);
    return({cur, rel});
  | Pointing(zc) =>
    switch (ZChain.move(d, zc)) {
    | Some(zc) => return({...zipper, cur: Pointing(zc)})
    | None =>
      let (pre, (_, p), suf) = ZChain.split(zc);
      let (affix_d, affix_b) = Dir.choose(d, (pre, suf));
      let ((z, c), rest) = ZChain.enter(b, affix_d);
      let (c, rel) =
        z.rel
        |> Relatives.push_seg(d, rest)
        |> Relatives.push(b, affix_b)
        |> Relatives.push(b, Chain.of_piece(p))
        |> Relatives.pop_kid(b, c);
      return({rel, cur: Pointing((z, c))});
    }
  };
};

let perform = (a: Action.t, z: t): option(t) =>
  switch (a) {
  | Move(d) => normalize(move(d, z))
  | Select(d) => select(d, z)
  };
