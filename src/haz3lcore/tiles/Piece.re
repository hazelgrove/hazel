// open Util;
include Base;

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = piece('a);

let secondary = w => Secondary(w);
let grout = g => Grout(g);
let tile = t => Tile(t);

let get = (f_w, f_g, f_t: tile('a) => _, f_p: projector('a) => _, p: t('a)) =>
  switch (p) {
  | Secondary(w) => f_w(w)
  | Grout(g) => f_g(g)
  | Tile(t) => f_t(t)
  | Projector(p) => f_p(p)
  };

let proj_id = (projector: projector(Id.t)) => projector.extra;
let id =
  get(Secondary.id, Grout.id, (tile: tile(Id.t)) => tile.extra, proj_id);

let sort: t(Id.t) => (Sort.t, list(Sort.t)) =
  get(
    _ => (Sort.Any, []),
    _ => (Sort.Any, []),
    t => (t.mold.out, t.mold.in_),
    _ => (Sort.Any, []),
  );

let nibs = x =>
  get(
    _ => None,
    g => {
      let (l, r) = Grout.shapes(g);
      Some(Nib.({shape: l, sort: Any}, {shape: r, sort: Any}));
    },
    t => Some(Tile.nibs(t)),
    p => {
      let (l, r) = ProjectorBase.shapes(p);
      Some(Nib.({shape: l, sort: Any}, {shape: r, sort: Any}));
    },
    x,
  );

let nib_sorts = x =>
  get(
    _ => (Sort.Any, Sort.Any),
    _ => (Sort.Any, Sort.Any),
    t => {
      let (l, r) = Tile.nibs(t);
      (l.sort, r.sort);
    },
    _ => (Sort.Any, Sort.Any),
    x,
  );

let sorted_children = x =>
  get(_ => [], _ => [], Tile.sorted_children, _ => [], x);
let children = p => sorted_children(p) |> List.split |> snd;

// let is_balanced =
//   fun
//   | Shard(_) => false
//   | Secondary(_)
//   | Grout(_)
//   | Tile(_) => true;

let pop_l = (p: t('a)): (t('a), segment('a)) =>
  switch (p) {
  | Tile(t) => Tile.pop_l(t)
  | Grout(_)
  | Secondary(_)
  | Projector(_) => (p, [])
  };
let pop_r = (p: t('a)): (segment('a), t('a)) =>
  switch (p) {
  | Tile(t) => Tile.pop_r(t)
  | Grout(_)
  | Secondary(_)
  | Projector(_) => ([], p)
  };

let disassemble = (p: t('a)): segment('a) =>
  switch (p) {
  | Grout(_)
  | Secondary(_)
  | Projector(_) => [p]
  | Tile(t) => Tile.disassemble(t)
  };

// let remold = (p: t) =>
//   switch (p) {
//   | Grout(_)
//   | Secondary(_) => [p]
//   | Tile(t) => List.map(tile, Tile.remold(t))
//   };

let shapes = x =>
  get(
    _ => None,
    g => Some(Grout.shapes(g)),
    t => Some(Tile.shapes(t)),
    p => Some(ProjectorBase.shapes(p)),
    x,
  );

let is_convex = (p: t('a)): bool =>
  switch (shapes(p)) {
  | Some((Convex, Convex)) => true
  | _ => false
  };

let is_grout: t('a) => bool =
  fun
  | Grout(_) => true
  | _ => false;

let is_secondary: t('a) => bool =
  fun
  | Secondary(_) => true
  | _ => false;

let is_tile: t('a) => option(Tile.t('a)) =
  fun
  | Tile(t) => Some(t)
  | _ => None;

let is_projector: t('a) => option(projector('a)) =
  fun
  | Projector(p) => Some(p)
  | _ => None;

let label: t('a) => option(Label.t) =
  fun
  | Tile({label, _}) => Some(label)
  | _ => None;

let monotile: t('a) => option(Token.t) =
  fun
  | Tile({label: [t], _}) => Some(t)
  | Secondary(w) when Secondary.is_comment(w) =>
    Some(Secondary.get_string(w.content))
  | _ => None;

let has_ends = x => get(_ => true, _ => true, Tile.has_ends, x);

let is_complete: t('a) => bool =
  fun
  | Tile(t) => Tile.is_complete(t)
  | _ => true;

let mold_of = (~shape=Nib.Shape.Convex, p: t('a)) =>
  // TODO(d) fix sorts
  switch (p) {
  | Tile(t) => t.mold
  | Grout(g) => Mold.of_grout(g, Any)
  | Secondary(_) => Mold.of_secondary({sort: Any, shape})
  | Projector(p) => ProjectorBase.mold_of(p, Any)
  };

let replace_id = (id: Id.t, p: t(Id.t)): t('a) =>
  switch (p) {
  | Tile(t) => Tile({...t, extra: id})
  | Grout(g) => Grout({...g, id})
  | Secondary(w) => Secondary({...w, id})
  | Projector(p) => Projector({...p, extra: id})
  };

let mk_tile: (Form.t, list(list(t('a)))) => t('a) =
  (form, children) =>
    Tile({
      extra: Id.mk(),
      label: form.label,
      mold: form.mold,
      shards: List.mapi((i, _) => i, form.label),
      children,
    });

let mk_mono = (sort: Sort.t, string: string): t('a) =>
  string |> Form.mk_atomic(sort) |> mk_tile(_, []);

let of_mono = (syntax: t('a)): option(string) =>
  switch (syntax) {
  | Tile({label: [l], _}) => Some(l)
  | _ => None
  };
