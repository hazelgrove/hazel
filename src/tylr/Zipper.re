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

// module Selection = {
//   type t = {
//     focus: Dir.t,
//     space: (Space.Z.t, Space.Z.t),
//     segment: Segment.t,
//   };
// };
// module Cursor = {
//   type t =
//     | Pointing(ZChain.t)
//     | Selecting(Selection.t);
// };
// type t = {
//   cur: Cursor.t,
//   rel: Relatives.t,
// };

type t = {
  sel: Selection.t,
  rel: Relatives.t,
};

module Action = {
  type t =
    | Move(Dir.t)
    | Select(Dir.t)
    | Delete(Dir.t)
    | Insert(string);
};

// promote empty Selecting to Pointing
// // disambiguate Pointing term as needed
// let normalize = (_: t): t => failwith("todo normalize");
// // result is not necessarily normalized
// let move = (d: Dir.t, zipper: t): option(t) => {
//   open OptUtil.Syntax;
//   let b = Dir.toggle(d);
//   switch (zipper.cur) {
//   | Selecting(sel) =>
//     // unselect
//     let (z, _) = Dir.choose(d, sel.space);
//     let cur =
//       Cursor.Selecting({...sel, space: (z, z), segment: Segment.empty});
//     let rel = Relatives.push_seg(b, selected, z.rel);
//     return({cur, rel});
//   | Pointing(zc) =>
//     switch (ZChain.move(d, zc)) {
//     | Some(zc) => return({...zipper, cur: Pointing(zc)})
//     | None =>
//       let (pre, (_, p), suf) = ZChain.split(zc);
//       let (affix_d, affix_b) = Dir.choose(d, (pre, suf));
//       let ((z, c), rest) = ZChain.enter(b, affix_d);
//       let (c, rel) =
//         z.rel
//         |> Relatives.push_seg(d, rest)
//         |> Relatives.push(b, affix_b)
//         |> Relatives.push(b, Chain.of_piece(p))
//         |> Relatives.pop_kid(b, c);
//       return({rel, cur: Pointing((z, c))});
//     }
//   };
// };

let move = (d: Dir.t, z: Zipper.t): option(t) => {
  open OptUtil.Syntax;
  let b = Dir.toggle(d);
  if (!Selection.is_empty(z.sel)) {
    let rel = Relatives.push_seg(b, z.sel.seg, z.rel);
    return({rel, sel: Selection.empty});
  } else {
    let+ (c, rel) = Relatives.pop_char(d, z.rel);
    {...z, rel: Relatives.push_char(b, c, rel)};
  };
};

let select = (d: Dir.t, z: Zipper.t): option(t) => {
  open OptUtil.Syntax;
  let b = Dir.toggle(d);
  if (d == z.sel.foc || Selection.is_empty(z.sel)) {
    let+ (c, rel) = Relatives.pop_char(d, z.rel);
    let sel = Selection.push_char(c, {...sel, foc: d});
    {rel, sel};
  } else {
    // checked for selection empty above
    let (c, sel) = Option.get(Selection.pop_char(z.sel));
    let rel = Relatives.push_char(b, c, z.rel);
    {rel, sel};
  };
};

let delete = (d: Dir.t, z: t): option(t) => {
  open OptUtil.Syntax;
  let+ z = Selection.is_empty(z.sel) ? select(d, z) : return(z);
  let rel = Relatives.remold(z.rel);
  {rel, sel: Selection.empty};
};

let perform = (a: Action.t, z: t): option(t) =>
  switch (a) {
  | Move(d) => move(d, z)
  | Select(d) => select(d, z)
  | Delete(d) => delete(d, z)
  | Insert(s) =>
    let unmolded =
      LangUtil.lex(s)
      |> Aba.map_b(t => Chain.of_piece(T(Tile.unmolded(t))));
    let (molded, rel) = Relatives.remold(~sel=unmolded, z.rel);
    let rel = Relatives.push_seg(L, molded, rel);
    Some({rel, sel: Selection.empty});
  };
