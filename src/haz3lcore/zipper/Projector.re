open Util;
include ProjectorBase;

/* See ProjectorBase for an introduction */

/* After adding a new projector module, add it here so that
 * it can be instantiated. The first-class module created by
 * this function must be reified whenever projector methods
 * are to be called; see `shape` below for an example */
let to_module = (kind: kind): (module Cooked) =>
  switch (kind) {
  | Fold => (module Cook(FoldCore.M))
  | Info => (module Cook(InfoCore.M))
  | Slider => (module Cook(SliderCore.M))
  | SliderF => (module Cook(SliderFCore.M))
  | Checkbox => (module Cook(CheckboxCore.M))
  | TextArea => (module Cook(TextAreaCore.M))
  };

let shape = (p: Map.entry, info: info): shape => {
  let (module P) = to_module(p.kind);
  P.placeholder(p.model, info);
};

/* A projector is replaced by a placeholder in the underlying
 * editor for view purposes. This projector is an all-whitespace
 * monotile. Currently there is no explicit notion of placeholders
 * in the zipper; a tile consisting of any number of whitespaces
 * is considered a placeholder. This could be made more principled.
 * Note that a placeholder retains the UUID of the underlying. */
let placeholder_label = (p: Map.entry, syntax): list(string) =>
  switch (shape(p, syntax)) {
  | Inline(width) => [String.make(width, ' ')]
  | Block({row, col}) => [
      String.make(row - 1, '\n') ++ String.make(col, ' '),
    ]
  };

let placeholder = (p: Map.entry, info: info): syntax =>
  Piece.Tile({
    id: Piece.id(info.syntax),
    label: placeholder_label(p, info),
    mold: Mold.mk_op(Any, []),
    shards: [0],
    children: [],
  });

/* Must be in-sync with placeholder_label / placeholder above */
let is_placeholder = (p: Piece.t): bool =>
  switch (p) {
  | Tile({label: [s], _}) => s |> String.trim |> String.length == 0
  | _ => false
  };

/* Currently projection is limited to convex pieces */
let minimum_projection_condition = (syntax: syntax): bool =>
  Piece.is_convex(syntax);

/* Add a new projector, gated on the predicated on the syntax */
let create = (kind: kind, syntax: syntax): option(Map.entry) => {
  let (module P) = to_module(kind);
  P.can_project(syntax) && minimum_projection_condition(syntax)
    ? Some({kind, model: P.init}) : None;
};

/* Returns the projector at the caret, if any */
let indicated = (z: ZipperBase.t) => {
  open Util.OptUtil.Syntax;
  let* id = Indicated.index(z);
  let+ projector = Map.find(id, z.projectors);
  (id, projector);
};

/* Is a piece of syntax currently projected? */
let syntax_is = (ps: Map.t, syntax: option(syntax)): option(Id.t) =>
  switch (syntax) {
  | Some(p) when Map.mem(Piece.id(p), ps) =>
    Map.mem(Piece.id(p), ps) ? Some(Piece.id(p)) : None
  | _ => None
  };

/* Is neighboring syntax currently projected? */
let neighbor_is = (ps, s: Siblings.t): (option(Id.t), option(Id.t)) => (
  syntax_is(ps, Siblings.left_neighbor(s)),
  syntax_is(ps, Siblings.right_neighbor(s)),
);

/* This handles the logic for selecting around projectors.
 * This amounts to simply selecting over the projected syntax
 * as if it were atomic instead of selecting into it. In principle,
 * this should work the same as if the projected syntax was replaced
 * by its placeholder, the selection was made on that projections,
 * and then the placeholder was replaced by former syntax. In an
 * updated syntax model, it should maybe work this way, or else
 * projected syntax should be represented in the zipper in
 * a more first-class way */
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
    syntax_is(projectors, ListUtil.hd_opt(s.content)),
    syntax_is(projectors, ListUtil.last_opt(s.content)),
  );

  let shrink =
      ({selection, projectors, _} as z: ZipperBase.t): option(ZipperBase.t) =>
    switch (selection.focus, selection_sides_is(projectors, selection)) {
    | (Left, (Some(_), _)) => skip_shrink_left(z)
    | (Right, (_, Some(_))) => skip_shrink_right(z)
    | _ => None
    };
};

/* See Select description above */
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

[@deriving (show({with_path: false}), sexp)]
type proj_ret = {
  z: ZipperBase.t,
  syntax_map: Id.Map.t(syntax),
};

/* Creates a projected version of the zipper for the view.
 * This replaces all projected pieces of syntax with their
 * corresponding placeholders. For convience, it also returns
 * a map from projector/syntax ids to the corresponding syntax */
module Project = {
  let syntax_map: ref(Id.Map.t(syntax)) = ref(Id.Map.empty);

  let placehold = (projectors: Map.t, info_map: Statics.Map.t, syntax: syntax) => {
    let id = Piece.id(syntax);
    switch (Map.find(id, projectors)) {
    | None => syntax
    | Some(pr) =>
      let info: info = {id, syntax, ci: Id.Map.find_opt(id, info_map)};
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

/* Updates the underlying piece of syntax for a projector */
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
