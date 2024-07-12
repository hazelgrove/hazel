open Util;
include ProjectorBase;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = projector;

[@deriving (show({with_path: false}), sexp, yojson)]
type kind =
  | Fold
  | Info
  | Checkbox
  | Slider
  | SliderF
  | TextArea;

let to_module = (p: projector): core =>
  switch ((p: projector)) {
  | Fold(model) => FoldCore.mk(model)
  | Info(model) => InfoCore.mk(model)
  | Checkbox(model) => CheckboxCore.mk(model)
  | Slider(model) => SliderCore.mk(model)
  | SliderF(model) => SliderFCore.mk(model)
  | TextArea(model) => TextAreaCore.mk(model)
  };

let kind = (p: t): kind =>
  switch (p) {
  | Fold(_) => Fold
  | Info(_) => Info
  | Checkbox(_) => Checkbox
  | Slider(_) => Slider
  | SliderF(_) => SliderF
  | TextArea(_) => TextArea
  };

let init = (f: kind): projector =>
  switch (f) {
  | Fold => Fold()
  | Info => Info(Expected)
  | Checkbox => Checkbox()
  | Slider => Slider()
  | SliderF => SliderF()
  | TextArea => TextArea({inside: false})
  };

let name_of_kind = (p: kind): string =>
  switch (p) {
  | Fold => "fold"
  | Info => "type"
  | Checkbox => "check"
  | Slider => "slide"
  | SliderF => "slidef"
  | TextArea => "text"
  };

/* Needs to be 1-to-1 for menu selection */
let of_name = (p: string): kind =>
  switch (p) {
  | "fold" => Fold
  | "type" => Info
  | "check" => Checkbox
  | "slide" => Slider
  | "slidef" => SliderF
  | "text" => TextArea
  | _ => failwith("Unknown projector kind")
  };

let name = (p: t): string => p |> kind |> name_of_kind;

let shape = (p: t, syntax): shape => {
  let (module P) = to_module(p);
  P.placeholder(syntax);
};

let is_placeholder = (p: Piece.t): bool => {
  //TODO: Revisit
  switch (p) {
  | Tile({label: [s], _}) => s |> String.trim |> String.length == 0
  | _ => false
  };
};

let placeholder_label = (p: t, syntax): list(string) =>
  switch (shape(p, syntax)) {
  | Inline(width) => [String.make(width, ' ')]
  | Block({row, col}) => [String.make(row, '\n') ++ String.make(col, ' ')]
  };

let placeholder = (p: t, info: info): syntax =>
  Piece.Tile({
    id: Piece.id(info.syntax),
    label: placeholder_label(p, info),
    mold: Mold.mk_op(Any, []),
    shards: [0],
    children: [],
  });

/* Currently projection is limited to convex pieces */
let minimum_projection_condition = (syntax: syntax): bool =>
  Piece.is_convex(syntax);

let create = (k: kind, syntax: syntax): option(t) => {
  let p = init(k);
  let (module P) = to_module(p);
  P.can_project(syntax) && minimum_projection_condition(syntax)
    ? Some(p) : None;
};

let piece_is = (ps: Map.t, syntax: option(syntax)): option(Id.t) =>
  switch (syntax) {
  | Some(p) when Map.mem(Piece.id(p), ps) =>
    Map.mem(Piece.id(p), ps) ? Some(Piece.id(p)) : None
  | _ => None
  };

let neighbor_is = (ps, s: Siblings.t): (option(Id.t), option(Id.t)) => (
  piece_is(ps, Siblings.left_neighbor(s)),
  piece_is(ps, Siblings.right_neighbor(s)),
);

module Select = {
  let skip_grow_left =
      ({relatives: {siblings: (ls, rs), _}, _} as z: ZipperBase.t) => {
    let (ls, content) = Segment.push_right((ls, z.selection.content));
    z
    |> ZipperBase.put_selection_content(content)
    |> ZipperBase.put_siblings((ls, rs))
    |> Option.some;
  };

  let skip_grow_right =
      ({relatives: {siblings: (ls, rs), _}, _} as z: ZipperBase.t) => {
    let (content, rs) = Segment.push_left((z.selection.content, rs));
    z
    |> ZipperBase.put_selection_content(content)
    |> ZipperBase.put_siblings((ls, rs))
    |> Option.some;
  };

  let grow =
      ({relatives: {siblings, _}, projectors, _} as z: ZipperBase.t)
      : option(ZipperBase.t) =>
    switch (z.selection.focus, neighbor_is(projectors, siblings)) {
    | (Left, (Some(_), _)) => skip_grow_left(z)
    | (Right, (_, Some(_))) => skip_grow_right(z)
    | _ => None
    };

  let skip_shrink_left =
      ({relatives: {siblings: (ls, rs), _}, _} as z: ZipperBase.t) => {
    let (ls, content) = Segment.push_left((ls, z.selection.content));
    z
    |> ZipperBase.put_selection_content(content)
    |> ZipperBase.put_siblings((ls, rs))
    |> Option.some;
  };

  let skip_shrink_right =
      ({relatives: {siblings: (ls, rs), _}, _} as z: ZipperBase.t) => {
    let (content, rs) = Segment.push_right((z.selection.content, rs));
    z
    |> ZipperBase.put_selection_content(content)
    |> ZipperBase.put_siblings((ls, rs))
    |> Option.some;
  };

  let selection_sides_is =
      (projectors, s: Selection.t): (option(Id.t), option(Id.t)) => (
    piece_is(projectors, ListUtil.hd_opt(s.content)),
    piece_is(projectors, ListUtil.last_opt(s.content)),
  );

  let shrink =
      ({selection, projectors, _} as z: ZipperBase.t): option(ZipperBase.t) =>
    switch (selection.focus, selection_sides_is(projectors, selection)) {
    | (Left, (Some(_), _)) => skip_shrink_left(z)
    | (Right, (_, Some(_))) => skip_shrink_right(z)
    | _ => None
    };
};

module Move = {
  let go = (d: Direction.t, z: ZipperBase.t): option(ZipperBase.t) =>
    switch (d, neighbor_is(z.projectors, z.relatives.siblings)) {
    | (Left, (Some(_), _)) =>
      Some(
        ZipperBase.put_siblings(Segment.push_right(z.relatives.siblings), z),
      )
    | (Right, (_, Some(_))) =>
      Some(
        ZipperBase.put_siblings(Segment.push_left(z.relatives.siblings), z),
      )
    | _ => None
    };
};

module MapPiece = {
  type updater = Piece.t => Piece.t;

  let rec of_segment = (f: updater, seg: Segment.t): Segment.t => {
    seg |> List.map(p => f(p)) |> List.map(of_piece(f));
  }
  and of_piece = (f: updater, piece: Piece.t): Piece.t => {
    switch (piece) {
    | Tile(t) => Tile(of_tile(f, t))
    | Grout(_)
    | Secondary(_) => piece
    };
  }
  and of_tile = (f: updater, t: Tile.t): Tile.t => {
    {...t, children: List.map(of_segment(f), t.children)};
  };

  let of_siblings = (f: updater, sibs: Siblings.t): Siblings.t => (
    of_segment(f, fst(sibs)),
    of_segment(f, snd(sibs)),
  );

  let of_ancestor = (f: updater, ancestor: Ancestor.t): Ancestor.t => {
    {
      ...ancestor,
      children: (
        List.map(of_segment(f), fst(ancestor.children)),
        List.map(of_segment(f), snd(ancestor.children)),
      ),
    };
  };

  let of_generation =
      (f: updater, generation: Ancestors.generation): Ancestors.generation => (
    of_ancestor(f, fst(generation)),
    of_siblings(f, snd(generation)),
  );

  let of_ancestors = (f: updater, ancestors: Ancestors.t): Ancestors.t =>
    List.map(of_generation(f), ancestors);

  let of_selection = (f: updater, selection: Selection.t): Selection.t => {
    {...selection, content: of_segment(f, selection.content)};
  };

  let go = (f: updater, z: ZipperBase.t): ZipperBase.t => {
    ...z,
    selection: of_selection(f, z.selection),
    relatives: {
      ancestors: of_ancestors(f, z.relatives.ancestors),
      siblings: of_siblings(f, z.relatives.siblings),
    },
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type proj_ret = {
  z: ZipperBase.t,
  syntax_map: Id.Map.t(syntax),
};

module Project = {
  let syntax_map: ref(Id.Map.t(syntax)) = ref(Id.Map.empty);

  let placehold = (projectors: Map.t, info_map: Statics.Map.t, syntax: syntax) => {
    let id = Piece.id(syntax);
    switch (Map.find(id, projectors)) {
    | None => syntax
    | Some(pr) =>
      let info: info = {
        syntax,
        status: None, //TODO(andrew)
        ci: Id.Map.find_opt(id, info_map),
      };
      syntax_map := Id.Map.add(id, syntax, syntax_map^);
      placeholder(pr, info);
    };
  };

  let go = (z: ZipperBase.t, info_map: Statics.Map.t): proj_ret => {
    syntax_map := Id.Map.empty;
    if (Id.Map.is_empty(z.projectors)) {
      {z, syntax_map: syntax_map^};
    } else {
      let z = MapPiece.go(placehold(z.projectors, info_map), z);
      {z, syntax_map: syntax_map^};
    };
  };
};

module Syntax = {
  let update_piece = (f, id: Id.t, syntax: syntax) =>
    id == Piece.id(syntax) ? f(syntax) : syntax;

  let sib_has_id = (get, z: ZipperBase.t, id: Id.t): bool => {
    switch (z.relatives.siblings |> get) {
    | Some(l) => Piece.id(l) == id
    | _ => false
    };
  };

  let left_sib_has_id = sib_has_id(Siblings.left_neighbor);
  let right_sib_has_id = sib_has_id(Siblings.right_neighbor);

  let update_left_sib = (f: syntax => syntax, z: ZipperBase.t) => {
    let (l, r) = z.relatives.siblings;
    let sibs = (List.map(f, l), List.map(f, r));
    z |> ZipperBase.put_siblings(sibs);
  };

  let update_right_sib = (f: syntax => syntax, z: ZipperBase.t) => {
    let sibs =
      switch (z.relatives.siblings) {
      | (l, [hd, ...tl]) => (l, [f(hd), ...tl])
      | sibs => sibs
      };
    z |> ZipperBase.put_siblings(sibs);
  };

  let update = (f: syntax => syntax, id: Id.t, z: ZipperBase.t): ZipperBase.t => {
    /* This applies the function to the piece in the zipper having id id, and
     * then replaces the id of the resulting piece with the idea of the old
     * piece, ensuring that the root id remains stable. This function assumes
     * the cursor is not inside the piece to be updated. This is optimized to
     * be O(1) when the piece is directly to the left or right of the cursor,
     * otherwise it is O(|zipper|) */
    let f = syntax =>
      update_piece(
        p => p |> f |> Piece.replace_id(Piece.id(p)),
        id,
        syntax,
      );
    if (left_sib_has_id(z, id)) {
      update_left_sib(f, z);
    } else if (right_sib_has_id(z, id)) {
      update_right_sib(f, z);
    } else {
      MapPiece.go(f, z);
    };
  };
};
