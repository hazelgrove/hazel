include Base;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t('p) = piece('p);

let secondary = w => Secondary(w);
let grout = g => Grout(g);
let tile = t => Tile(t);

let get = (f_w, f_g, f_t: tile('p) => _, f_p: projector('p) => _, p: t('p)) =>
  switch (p) {
  | Secondary(w) => f_w(w)
  | Grout(g) => f_g(g)
  | Tile(t) => f_t(t)
  | Projector(p) => f_p(p)
  };

let id =
  get(Secondary.id, Grout.id, tile => tile.id, projector => projector.id, _);

let sort =
  get(
    _ => (Sort.Any, []),
    _ => (Sort.Any, []),
    t => (t.mold.out, t.mold.in_),
    p => (p.mold.out, p.mold.in_),
    _,
  );

let nibs =
  get(
    _ => None,
    g => {
      let (l, r) = Grout.shapes(g);
      Some(
        Nib.(
          {
            shape: l,
            sort: Any,
          },
          {
            shape: r,
            sort: Any,
          },
        ),
      );
    },
    t => Some(Tile.nibs(t)),
    p => Some(p.mold.nibs),
    _,
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
    _,
  );

let sorted_children = get(_ => [], _ => [], Tile.sorted_children, _ => [], _);

let pop_l = (p: t('p)): (t('p), segment('p)) =>
  switch (p) {
  | Tile(t) => Tile.pop_l(t)
  | Grout(_)
  | Secondary(_)
  | Projector(_) => (p, [])
  };
let pop_r = (p: t('p)): (segment('p), t('p)) =>
  switch (p) {
  | Tile(t) => Tile.pop_r(t)
  | Grout(_)
  | Secondary(_)
  | Projector(_) => ([], p)
  };

let disassemble = (p: t('p)): segment('p) =>
  switch (p) {
  | Grout(_)
  | Secondary(_)
  | Projector(_) => [p]
  | Tile(t) => Tile.disassemble(t)
  };

let shapes =
  get(
    _ => None,
    g => Some(Grout.shapes(g)),
    t => Some(Tile.shapes(t)),
    p => Some(p.mold.nibs |> Nibs.shapes),
    _,
  );

let is_convex = (p: t('p)): bool =>
  switch (shapes(p)) {
  | Some((Convex, Convex)) => true
  | _ => false
  };

let is_grout: t('p) => bool =
  fun
  | Grout(_) => true
  | _ => false;

let is_secondary: t('p) => bool =
  fun
  | Secondary(_) => true
  | _ => false;

let is_tile: t('p) => option(Tile.t('p)) =
  fun
  | Tile(t) => Some(t)
  | _ => None;

let is_projector: t('p) => option(projector('p)) =
  fun
  | Projector(p) => Some(p)
  | _ => None;

let label: t('p) => option(Label.t) =
  fun
  | Tile({label, _}) => Some(label)
  | _ => None;

let monotile: t('p) => option(Token.t) =
  fun
  | Tile({label: [t], _}) => Some(t)
  | Secondary(w) when Secondary.is_comment(w) =>
    Some(Secondary.get_string(w.content))
  | _ => None;

let is_complete: t('p) => bool =
  fun
  | Tile(t) => Tile.is_complete(t)
  | _ => true;

let replace_id = (id: Id.t, p: t('p)): t('p) =>
  switch (p) {
  | Tile(t) =>
    Tile({
      ...t,
      id,
    })
  | Grout(g) =>
    Grout({
      ...g,
      id,
    })
  | Secondary(w) =>
    Secondary({
      ...w,
      id,
    })
  | Projector(p) =>
    Projector({
      ...p,
      id,
    })
  };

let mk_tile: (Form.t, list(list(t('p)))) => t('p) =
  (form, children) =>
    Tile({
      id: Id.mk(),
      label: form.label,
      mold: form.mold,
      shards: List.mapi((i, _) => i, form.label),
      children,
    });

let mk_grout = (~id=Id.mk(), shape: Grout.shape): t('p) =>
  grout({
    id,
    shape,
  });

let mk_mono = (sort: Sort.t, string: string): t('p) =>
  string |> Form.mk_atomic(sort) |> mk_tile(_, []);

let of_mono = (syntax: t('p)): option(string) =>
  switch (syntax) {
  | Tile({label: [l], _}) => Some(l)
  | _ => None
  };

let is_case_or_rule = (p: t('p)) =>
  switch (p) {
  | Tile({label: ["case", "end"], _}) => true
  | Tile({label: ["|", "=>"], _}) => true
  | _ => false
  };
let is_not_case_or_rule_or_space = (p: t('p)) =>
  switch (p) {
  | Tile({label: ["case", "end"], _}) => false
  | Tile({label: ["|", "=>"], _}) => false
  | Secondary(_) => false
  | _ => true
  };
let not_comment_or_space = (p: t('p)) =>
  switch (p) {
  | Secondary(s) => Secondary.is_linebreak(s)
  | _ => true
  };

let is_term = (p: t('p)) =>
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

//TODO(andrew): rm if unused
/* If the piece is parentheses, return the child. Otherwise,
 * return a singleton segment consisting of the piece */
let unparenthesize = (piece: t('p)): list(t('p)) =>
  switch (piece) {
  | Tile({
      label: ["(", ")"],
      mold: {nibs: ({shape: Convex, _}, {shape: Convex, _}), _},
      children: [seg],
      _,
    }) => seg
  | _ => [piece]
  };

let is_infix_delimiter_op_prefix = (p: t('p)) =>
  switch (p) {
  | Tile({label: [t], mold, _}) =>
    Mold.is_infix_op(mold) && Form.is_infix_delimiter_op_prefix(t)
  | _ => false
  };
