module Cursor = {
  module Pointing = {
    type t =
      | Space(ZSpace.t)
      | Chain(ZChain.t);
  };

  module Selecting = {
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

let move = (d: Dir.t, z: t): option(t) =>
  switch (z.cur) {
  | Selecting({foc: _, seg, pad}) =>
    // switch to Pointing with cursor at directed end of selection
    let (space_d, space_b) = Dir.choose(d, pad);
    let rel = Relatives.push(d, seg, ~space=space_b, z.rel);
    // TODO: convert to Chain if space_d is at either extreme
    Some({rel, cur: Pointing(Space(space_d))});
  | Pointing(Space(zs)) =>
    switch (ZSpace.move(d, zs)) {
    | Some(zs) => Some({...z, cur: Pointing(Space(zs))})
    | None =>
      open OptUtil.Syntax;
      let+ (zc, rel) = Relatives.split_zchain(d, z.rel);
      {rel, cur: Pointing(Chain(zc))};
    }
  | Pointing(Chain((pre, zp, suf))) =>
    switch (ZPiece.move(d, zp)) {
    | Some(zp) => Some({...z, cur: Pointing(Chain((pre, zp, suf)))})
    | None =>
      let c = Chain.of_piece(ZPiece.erase(zp));
      let b = Dir.toggle(d);
      let (c_d, c_b) = Dir.choose(d, (pre, suf));
      let rel =
        z.rel
        |> Relatives.push(d, c_d)
        |> Relatives.push(b, c_b)
        |> Relatives.push(b, c);
      failwith("need to check for space");
    }
  }

let perform = (a: Action.t, z: t): option(t) =>
  switch (a) {
  | Move(d) => move(d, z)
  }
