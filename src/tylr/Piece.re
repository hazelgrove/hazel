open Sexplib.Std;
open Util;

module Path = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = int;
  let shift = (n, p) => p + n;
};

// todo: rename to something like Material
// module Shape = {
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type t =
//     | T(Tile.t)
//     | G(Grout.t);

//   let t = t => T(t);
//   let g = g => G(g);
// };

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  paths: list(Path.t),
  material: Material.molded,
  token: Token.t,
};

// well-labeled invariant: for piece p
// !Label.is_empty(p.mold.label) ==> is_prefix(p.token, p.mold.label)
exception Ill_labeled;

let mk = (~id=?, ~paths=[], material, token) => {
  let id =
    switch (id) {
    | None => Id.Gen.next()
    | Some(id) => id
    };
  {id, paths, material, token};
};

let id_ = p => p.id;
let label = p => Material.map(Mold.label, p.material);
let sort = p => Material.map(Mold.sort_, p.material);
let prec = p => Material.map(Mold.prec_, p.material);

let put_label = (_, _) => failwith("todo Piece.put_label");
let put_paths = (paths, p) => {...p, paths};
let put_token = (token, p) => {...p, token};

let is_constant = p =>
  switch (label(p)) {
  | Grout => false
  | Tile(lbl) => Label.is_const(lbl)
  };

// Some(1) if grout
// None if non constant label
let label_length = p =>
  switch (label(p)) {
  | Grout => Some(1)
  | Tile(lbl) => Label.length(lbl)
  };
let token_length = p => Token.length(p.token);

// todo: review uses and replace with one of above
let length = _ => failwith("todo: Piece.length");

let is_grout = p => label_length(p) == 0;

// todo: review uses and rename
let is_empty = _ => failwith("todo Piece.is_empty");

// let tip = (side, p) => Mold.tip(side, mold(p));
// let tips = (side, p) => Mold.tips(side, mold(p));
// let convexable = (side, p) => List.mem(Tip.Convex, tips(side, p));

// let mk = (~id=?, ~paths=[], shape: Shape.t) => {
//   let id = id |> OptUtil.get(() => Id.Gen.next());
//   {id, paths, shape};
// };
// let of_grout = (~id=?, ~paths=[], g) => mk(~id?, ~paths, G(g));
// let of_tile = (~id=?, ~paths=[], t) => mk(~id?, ~paths, T(t));

let add_paths = (ps, p) => {...p, paths: ps @ p.paths};
let clear_paths = p => {...p, paths: []};

let is_finished = p =>
  p.material |> Material.map(_ => label_length(p) == Some(token_length(p)));

// let is_complete = p =>
//   // assumes well-labeled
//   switch (Label.length(label(p))) {
//   | None => true
//   | Some(0) =>
//     assert(is_grout(p));
//     true;
//   | Some(n) => Token.length(p.token) == n
//   };

// let is_porous = p => Token.is_empty(p.token);

let unzip = (n: int, p: t): Result.t((t, t), Dir.t) => {
  switch (label(p), Token.unzip(n, p.token)) {
  | (Grout, Error(L)) => Error(n < 1 ? L : R)
  | (Tile(_), Error(L)) => Error(L)
  | (Tile(lbl), Error(R)) when n == Token.length(p.token) =>
    switch (Label.unzip(n, lbl)) {
    | Error(side) =>
      assert(side == R);
      Error(R);
    | Ok((lbl_l, lbl_r)) =>
      let l = put_label(lbl_l, p);
      let r = put_label(lbl_r, {...p, token: Token.empty});
      Ok((l, r));
    }
  | (_, Error(R)) => Error(R)
  | (Grout, Ok((tok_l, tok_r))) =>
    Ok(({...p, token: tok_l}, {...p, token: tok_r}))
  | (Tile(lbl), Ok((tok_l, tok_r))) =>
    switch (Label.unzip(n, lbl)) {
    | Error(_) => raise(Ill_labeled)
    | Ok((lbl_l, lbl_r)) =>
      let l = put_label(lbl_l, {...p, token: tok_l});
      let r = put_label(lbl_r, {...p, token: tok_r});
      Ok((l, r));
    }
  };
};

let zip = (l: t, r: t): option(t) => {
  open OptUtil.Syntax;
  let* () = OptUtil.of_bool(Id.eq(l.id, r.id));
  let+ l =
    switch (label(l), label(r)) {
    | (Grout, Tile(_))
    | (Tile(_), Grout) => None
    | (Grout, Grout) => Some(l)
    | (Tile(lbl_l), Tile(lbl_r)) =>
      let+ lbl = Label.zip(lbl_l, lbl_r);
      put_label(lbl, l);
    };
  l
  |> put_token(l.token ++ r.token)
  |> put_paths(l.paths @ List.map(Path.shift(token_length(l)), r.paths));
};
let zips = (l, r) => Option.is_some(zip(l, r));

let complement_beyond = (~side as _, _) =>
  failwith("todo: Piece.complement_beyond");
