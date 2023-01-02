module Cursor = {
  module Pointing = {
    type t =
      | Space(ZSpace.t)
      | Chain(ZChain.t);
  };

  module Selecting = {
    // when seg empty, ignore Dir.choose(foc, pad)
    type t = {
      foc: Dir.t,
      seg: Segment.t,
      pad: (ZSpace.t, ZSpace.t),
    };
  };

  type t =
    | Pointing(Pointing.t)
    | Selecting(Selecting.t);
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

let normalize_d = (z: t): option((Dir.t, Space.t)) =>
  switch (z.cur) {
  | Pointing(Space(zs)) when ZSpace.is_empty(zs) =>
    Some(Relatives.choose_cur(z.rel))
  | Pointing(Space(zs)) when ZSpace.is_extreme(L, zs) => Some(L)
  | Pointing(Space(zs)) when ZSpace.is_extreme(R, zs) => Some(R)
  | _ => None
  };
let normalize = (z: t): t =>
  switch (normalize_d(z)) {
  | None => z
  | Some((d, s)) =>
    switch (Relatives.(split_zchain(d, push_space(s, z.rel)))) {
    | None => z
    | Some((zc, rel)) => {rel, cur: Pointing(Chain(zc))}
    }
  };
exception Abnormal;

let rec move = (d: Dir.t, z: t): option(t) => {
  open OptUtil.Syntax;
  let b = Dir.toggle(d);
  switch (z.cur) {
  | Selecting({foc: _, seg, pad}) =>
    // switch to Pointing with cursor at directed end of selection
    let (zs_d, zs_b) = Dir.choose(d, pad);
    let rel = Relatives.push_seg(b, seg, ~space=ZSpace.zip(space_b), z.rel);
    Some({rel, cur: Pointing(Space(space_d))});
  | Pointing(Space(zs)) =>
    switch (ZSpace.move(d, zs)) {
    | Some(zs) => Some({...z, cur: Pointing(Space(zs))})
    | None =>
      let* (zc, rel) =
        z.rel
        |> Relatives.push_space(ZSpace.zip(zs))
        |> Relatives.split_zchain(d);
      move(d, {rel, cur: Pointing(Chain(zc))});
    }
  | Pointing(Chain((c_l, zp, c_r))) =>
    switch (ZPiece.move(d, zp)) {
    | Some(zp) => Some({...z, cur: Pointing(Chain((pre, zp, suf)))})
    | None =>
      let p = ZPiece.erase(zp);
      let (c_d, c_b) = Dir.choose(d, (c_l, c_r));
      let (s, rel) =
        z.rel
        |> Relatives.push(d, c_d)
        |> Relatives.push(b, c_b)
        |> Relatives.push_piece(b, p)
        |> Relatives.pop_space;
      let zs = ZSpace.enter(b, s);
      move(d, {rel, cur: Pointing(Space(zs))});
    }
  };
};

let perform = (a: Action.t, z: t): option(t) =>
  switch (a) {
  | Move(d) => normalize(move(d, z))
  | Select(d) => select(d, z)
  };
