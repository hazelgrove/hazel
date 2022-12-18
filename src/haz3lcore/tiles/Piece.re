// open Util;
include Base;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = piece;

let whitespace = w => Whitespace(w);
let grout = g => Grout(g);
let tile = t => Tile(t);

let get = (f_w, f_g, f_t, p: t) =>
  switch (p) {
  | Whitespace(w) => f_w(w)
  | Grout(g) => f_g(g)
  | Tile(t) => f_t(t)
  };

let id = get(Whitespace.id, Grout.id, Tile.id);

let sort =
  get(
    _ => (Sort.Any, []),
    _ => (Sort.Any, []),
    t => (t.mold.out, t.mold.in_),
  );

let nibs =
  get(
    _ => None,
    g => {
      let (l, r) = Grout.shapes(g);
      Some(Nib.({shape: l, sort: Any}, {shape: r, sort: Any}));
    },
    t => Some(Tile.nibs(t)),
  );

let nib_sorts =
  get(
    _ => (Sort.Any, Sort.Any),
    _ => (Sort.Any, Sort.Any),
    t => {
      let (l, r) = Tile.nibs(t);
      (l.sort, r.sort);
    },
  );

let sorted_children = get(_ => [], _ => [], Tile.sorted_children);
let children = p => sorted_children(p) |> List.split |> snd;

// let is_balanced =
//   fun
//   | Shard(_) => false
//   | Whitespace(_)
//   | Grout(_)
//   | Tile(_) => true;

let pop_l = (p: t): (t, segment) =>
  switch (p) {
  | Tile(t) => Tile.pop_l(t)
  | Grout(_)
  | Whitespace(_) => (p, [])
  };
let pop_r = (p: t): (segment, t) =>
  switch (p) {
  | Tile(t) => Tile.pop_r(t)
  | Grout(_)
  | Whitespace(_) => ([], p)
  };

let disassemble = (p: t): segment =>
  switch (p) {
  | Grout(_)
  | Whitespace(_) => [p]
  | Tile(t) => Tile.disassemble(t)
  };

// let remold = (p: t) =>
//   switch (p) {
//   | Grout(_)
//   | Whitespace(_) => [p]
//   | Tile(t) => List.map(tile, Tile.remold(t))
//   };

let shapes =
  get(_ => None, g => Some(Grout.shapes(g)), t => Some(Tile.shapes(t)));

let is_grout: t => bool =
  fun
  | Grout(_) => true
  | _ => false;

let is_whitespace: t => bool =
  fun
  | Whitespace(_) => true
  | _ => false;

let is_tile: t => option(Tile.t) =
  fun
  | Tile(t) => Some(t)
  | _ => None;

let label: t => option(Label.t) =
  fun
  | Tile({label, _}) => Some(label)
  | _ => None;

let monotile: t => option(Token.t) =
  fun
  | Tile({label: [t], _}) => Some(t)
  | _ => None;

let is_length_one_monotile: t => bool =
  p =>
    switch (monotile(p)) {
    | Some(t) => String.length(t) == 1
    | None => false
    };

let has_ends = get(_ => true, _ => true, Tile.has_ends);

let is_complete: t => bool =
  fun
  | Tile(t) => Tile.is_complete(t)
  | _ => true;

let get_outside_sorts = (~default_sort=Sort.Any, p: t): list(Sort.t) =>
  //TODO: David please review this
  switch (p) {
  | Whitespace(_) => []
  | Grout({shape: Convex, _}) => []
  | Grout({shape: Concave, _}) => [default_sort, default_sort]
  | Tile({shards: _, _} as t) when !Tile.is_complete(t) =>
    // TODO(andrew): better incomplete tile handling
    // Need to figure out what shape of incomplete tile is
    []
  | Tile(t) =>
    let (sort_l, sort_r) = nib_sorts(p);
    switch ((t.mold.nibs |> fst).shape, (t.mold.nibs |> snd).shape) {
    | (Convex, Convex) => []
    | (Convex, Concave(_)) => [sort_r]
    | (Concave(_), Convex) => [sort_l]
    | (Concave(_), Concave(_)) => [sort_l, sort_r]
    };
  };
