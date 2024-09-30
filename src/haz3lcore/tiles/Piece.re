[@deriving (show({with_path: false}), sexp, yojson)]
type t = Base.piece;
module T = Tile;
include Base;
module Tile = T;

let secondary = w => Secondary(w);
let grout = g => Grout(g);
let tile = t => Tile(t);

let get = (f_w, f_g, f_t: Base.tile => _, f_p: Base.projector => _, p: t) =>
  switch (p) {
  | Secondary(w) => f_w(w)
  | Grout(g) => f_g(g)
  | Tile(t) => f_t(t)
  | Projector(p) => f_p(p)
  };

let proj_id = (projector: Base.projector) => projector.id;
let id = get(Secondary.id, Grout.id, tile => tile.id, proj_id);

let sort =
  get(
    _ => (Sort.Any, []),
    _ => (Sort.Any, []),
    t => (t.mold.out, t.mold.in_),
    _ => (Sort.Any, []),
  );

let nibs =
  get(
    _ => None,
    g => {
      let (l, r) = Grout.shapes(g);
      Some(Nib.({shape: l, sort: Any}, {shape: r, sort: Any}));
    },
    t => Some(Tile.nibs(t)),
    p => {
      let (l, r) = ProjectorBase.shapes_p(p);
      Some(Nib.({shape: l, sort: Any}, {shape: r, sort: Any}));
    },
  );

let nib_sorts =
  get(
    _ => (Sort.Any, Sort.Any),
    _ => (Sort.Any, Sort.Any),
    t => {
      let (l, r) = Tile.nibs(t);
      (l.sort, r.sort);
    },
    _ => (Sort.Any, Sort.Any),
  );

let sorted_children = get(_ => [], _ => [], Tile.sorted_children, _ => []);

let disassemble = (p: t): Base.segment =>
  switch (p) {
  | Grout(_)
  | Secondary(_)
  | Projector(_) => [p]
  | Tile(t) => Tile.disassemble(t)
  };

let shapes = ProjectorBase.shapes;

let is_convex = (p: t): bool =>
  switch (shapes(p)) {
  | Some((Convex, Convex)) => true
  | _ => false
  };

let is_grout: t => bool =
  fun
  | Grout(_) => true
  | _ => false;

let is_secondary: t => bool =
  fun
  | Secondary(_) => true
  | _ => false;

let is_tile: t => option(Base.tile) =
  fun
  | Tile(t) => Some(t)
  | _ => None;

let is_projector: t => option(Base.projector) =
  fun
  | Projector(p) => Some(p)
  | _ => None;

let label: t => option(Label.t) =
  fun
  | Tile({label, _}) => Some(label)
  | _ => None;

let monotile: t => option(Token.t) =
  fun
  | Tile({label: [t], _}) => Some(t)
  | Secondary(w) when Secondary.is_comment(w) =>
    Some(Secondary.get_string(w.content))
  | _ => None;

let is_complete: t => bool =
  fun
  | Tile(t) => Tile.is_complete(t)
  | _ => true;

let replace_id = (id: Id.t, p: t): t =>
  switch (p) {
  | Tile(t) => Tile({...t, id})
  | Grout(g) => Grout({...g, id})
  | Secondary(w) => Secondary({...w, id})
  | Projector(p) => Projector({...p, id})
  };

let mk_tile: (Form.t, list(list(t))) => t =
  (form, children) =>
    Tile({
      id: Id.mk(),
      label: form.label,
      mold: form.mold,
      shards: List.mapi((i, _) => i, form.label),
      children,
    });

let mk_mono = (sort: Sort.t, string: string): t =>
  string |> Form.mk_atomic(sort) |> mk_tile(_, []);

let of_mono = (syntax: t): option(string) =>
  switch (syntax) {
  | Tile({label: [l], _}) => Some(l)
  | _ => None
  };

let is_case_or_rule = (p: t) =>
  switch (p) {
  | Tile({label: ["case", "end"], _}) => true
  | Tile({label: ["|", "=>"], _}) => true
  | _ => false
  };
let is_not_case_or_rule_or_space = (p: t) =>
  switch (p) {
  | Tile({label: ["case", "end"], _}) => false
  | Tile({label: ["|", "=>"], _}) => false
  | Secondary(_) => false
  | _ => true
  };
let not_space = (p: t) =>
  switch (p) {
  | Secondary(s) => !Secondary.is_space(s)
  | _ => true
  };
let not_comment_or_space = (p: t) =>
  switch (p) {
  | Secondary(s) => Secondary.is_linebreak(s)
  | _ => true
  };

let is_term = (p: t) =>
  switch (p) {
  | Grout(_)
  | Projector(_)
  | Tile({
      label: [_],
      mold: {nibs: ({shape: Convex, _}, {shape: Convex, _}), _},
      _,
    }) =>
    true
  | Secondary(_) => false // debatable
  | _ => false
  };

let pop_l = Base.Piece.pop_l;

let pop_r = Base.Piece.pop_r;
